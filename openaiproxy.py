from dotenv import load_dotenv
import os
import requests
import pandas as pd
import logging
import time

LOG_LEVEL = os.getenv("LOG_LEVEL", "INFO").upper()
logging.basicConfig(
    level=LOG_LEVEL,
    format="%(asctime)s | %(levelname)s | %(name)s | %(message)s"
)
logger = logging.getLogger("openai_proxy")
MAX_TEST_ROWS = int(os.getenv("MAX_TEST_ROWS", "10"))


def _shorten(value, max_len: int = 80) -> str:
  text = str(value) if value is not None else ""
  return text if len(text) <= max_len else f"{text[:max_len - 3]}..."

class OpenAIProxy:
  def __init__(self):
    load_dotenv()

    self.api_key = os.environ.get("AIOPSSERVICENOW_APIKEY")
    self.base_url = os.environ.get("AI_PROXY_URL")

    if not self.api_key:
      raise ValueError("API Key nicht gefunden. Bitte .env prüfen.")
    if not self.base_url:
      raise ValueError("Proxy URL nicht gefunden. Bitte .env prüfen.")

    self.headers = {
      "Authorization": f"Bearer {self.api_key}"
    }
    logger.info("OpenAIProxy initialisiert (base_url=%s)", self.base_url)

  def models(self):
    """Get list of available models"""
    url = f"{self.base_url}/models"
    start = time.perf_counter()
    response = requests.get(url, headers=self.headers, timeout=30)
    response.raise_for_status()
    payload = response.json()
    model_count = len(payload.get("data", [])) if isinstance(payload, dict) else 0
    logger.info("Models geladen (%s Modelle, %.2fs)", model_count, time.perf_counter() - start)
    return payload

  def chat(self, model: str, messages: list, temperature: float = 0.0):
    """
    Send a chat request
    messages = [{"role": "user", "content": "Hallo"}]
    """
    url = f"{self.base_url}/openai/chat/completions"
    payload = {
      "model": model,
      "messages": messages,
      "temperature": temperature
    }
    start = time.perf_counter()
    logger.debug("Chat-Request: model=%s, messages=%s", model, len(messages))
    try:
      response = requests.post(url, headers=self.headers, json=payload, timeout=60)
      response.raise_for_status()
    except requests.RequestException:
      logger.exception("Fehler beim Chat-Request (model=%s)", model)
      raise

    result = response.json()
    elapsed = time.perf_counter() - start
    usage = result.get("usage", {}) if isinstance(result, dict) else {}
    logger.info(
        "Chat erfolgreich (model=%s, %.2fs, prompt_tokens=%s, completion_tokens=%s)",
        model,
        elapsed,
        usage.get("prompt_tokens", "n/a"),
        usage.get("completion_tokens", "n/a")
    )
    return result

  def classify_produkt(self, produkt: tuple[str, str], categories: list[str]):
      product_name = _shorten(produkt[0], 60)
      logger.debug("Klassifiziere Produkt: %s", product_name)
      response = self.chat(
          model="gpt-4o-mini",
          messages=[
              {
                  "role": "system",
                  "content": f"""You are a text classification assistant for the OpenFoodFacts dataset.

                  Classify the given product into exactly one category from the list below.
                
                  CATEGORIES:
                  {categories}
                
                  RULES:
                  - Output ONLY the category name. Nothing else.
                  - The output must exactly match one entry from CATEGORIES.
                  - If no category fits well, choose the closest match.
                  - Any type of Protein belongs to the categorie: Vitamintabletten, Magnesiumtabletten o.Ä."""
              },
              {
                  "role": "user",
                  "content": f"{produkt[0]} | {produkt[1]}"
              }
          ],
          temperature=0.0
      )

      new_category = response['choices'][0]['message']['content'].strip()
      if new_category not in categories:
          logger.warning("Modellantwort nicht exakt in Kategorienliste: %s", _shorten(new_category, 60))
      return new_category


if __name__ == "__main__":
    logger.info("Starte Mapping OpenFoodFacts -> Destatis")
    proxy = OpenAIProxy()

    logger.info("Lade Destatis-Daten")
    destatis_df = pd.read_excel('data_raw/sonderauswertung-nahrungsmittel.xlsx', sheet_name='Index_10-Steller', skiprows=5)
    destatis_df = destatis_df.drop(index=[168, 169, 170]).rename(columns={'Unnamed: 0': 'COICOP-Index', 'Unnamed: 1': 'Produkt'})
    categories = destatis_df['Produkt'].tolist()
    logger.info("Destatis-Kategorien geladen: %s", len(categories))

    logger.info("Lade OpenFoodFacts-Daten")
    openfoods_df = pd.read_csv('data_raw/en.openfoodfacts.org.products.tsv', sep='\t',
        on_bad_lines='skip',
        engine='python')
    de_openfoods_df = openfoods_df[openfoods_df['countries_en'].str.contains('Germany', na=False)].copy()
    de_openfoods_df = de_openfoods_df.drop(columns=['countries_en'])
    de_openfoods_df = de_openfoods_df.dropna(subset=['categories', 'product_name'], how='all')
    logger.info("Produkte mit Germany-Filter: %s", len(de_openfoods_df))

    df_unclassified = de_openfoods_df[['product_name', 'categories']]
    total_rows = len(df_unclassified)
    logger.info("Testlauf aktiv: erste %s Zeilen", total_rows)

    results = []
    error_count = 0
    started_at = time.perf_counter()
    for position, (_, row) in enumerate(df_unclassified.iterrows(), start=1):
        produkt = (row.get('product_name'), row.get('categories'))
        try:
            new_cat = proxy.classify_produkt(produkt, categories)
        except Exception:
            error_count += 1
            logger.exception("Klassifizierung fehlgeschlagen bei Zeile %s (%s)", position, _shorten(produkt[0], 50))
            new_cat = None
        results.append(new_cat)
        elapsed = time.perf_counter() - started_at
        rate = position / elapsed if elapsed > 0 else 0
        eta_sec = (total_rows - position) / rate if rate > 0 else 0
        logger.info(
            "[%s/%s] Produkt='%s' | Kategorie='%s' | destatis_category='%s' | Fehler=%s | ETA %.1fs",
            position,
            total_rows,
            _shorten(produkt[0], 40),
            _shorten(produkt[1], 40),
            _shorten(new_cat, 40),
            error_count,
            eta_sec,
        )

    df_unclassified['destatis_category'] = results
    logger.info("Speichere Ergebnis nach data_raw/mapping.csv")
    df_unclassified.to_csv('data_raw/mapping.csv', index=True)
    logger.info("Fertig: %s Zeilen, %s Fehler", total_rows, error_count)





