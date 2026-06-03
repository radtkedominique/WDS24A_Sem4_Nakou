# -----------------------------
# Risikogruppen-Label erzeugen
# -----------------------------
import pandas as pd

df_features = pd.read_csv('data_processed/features_destatis.csv')
start_preise = (
    df_features
    .dropna(subset=['Monat_Nr', 'Preisindex'])
    .sort_values(['Produkt', 'Monat_Nr'])
    .groupby('Produkt')['Preisindex']
    .first()
    .reset_index()
    .rename(columns={'Preisindex': 'start'})
)

end_preise = (
    df_features
    .dropna(subset=['Monat_Nr', 'Preisindex'])
    .sort_values(['Produkt', 'Monat_Nr'])
    .groupby('Produkt')['Preisindex']
    .last()
    .reset_index()
    .rename(columns={'Preisindex': 'end'})
)

preisanstieg = start_preise.merge(end_preise, on='Produkt').assign(
    preisanstieg_pct=lambda x: 100 * (x['end'] / x['start'] - 1)
)

# Schwellenwerte aus deiner Trendanalyse ableiten
def label_risikogruppe(pct):
    if pct >= 41.23:
        return 'hoch'
    elif pct >= 29.23:
        return 'mittel'
    else:
        return 'stabil'

preisanstieg['Risikogruppe'] = preisanstieg['preisanstieg_pct'].apply(label_risikogruppe)
print(preisanstieg['Risikogruppe'].value_counts())


df_features = df_features.merge(
    preisanstieg[['Produkt', 'Risikogruppe', 'preisanstieg_pct']],
    on='Produkt',
    how='left'
)
df_features.to_csv('data_processed/features_destatis.csv', index=False)
print("Fertig.")