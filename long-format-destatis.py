import pandas as pd

destatis_df = pd.read_csv('data_processed/destatis_df.csv')
# Wide → Long
df_long = destatis_df.melt(
    id_vars=['COICOP-Index', 'Produkt'],
    var_name='Monat',
    value_name='Preisindex'
)
df_long = df_long.dropna(subset=['Preisindex'])

# Monatsreihenfolge als Index
monat_order = {m: i for i, m in enumerate(destatis_df.columns[2:])}
df_long['Monat_Nr'] = df_long['Monat'].map(monat_order)
df_long = df_long.sort_values(['Produkt', 'Monat_Nr'])

# Gruppenweise je Produkt berechnen
df_long['lag_1'] = df_long.groupby('Produkt')['Preisindex'].shift(1)
df_long['lag_2'] = df_long.groupby('Produkt')['Preisindex'].shift(2)
df_long['lag_3'] = df_long.groupby('Produkt')['Preisindex'].shift(3)

df_long['rolling_mean_3'] = (
    df_long.groupby('Produkt')['Preisindex']
    .transform(lambda x: x.shift(1).rolling(3).mean())
)
df_long['rolling_mean_6'] = (
    df_long.groupby('Produkt')['Preisindex']
    .transform(lambda x: x.shift(1).rolling(6).mean())
)

start = pd.Timestamp('2020-01-01')
df_long['Datum'] = df_long['Monat_Nr'].apply(
    lambda x: start + pd.DateOffset(months=int(x) - 1) if pd.notna(x) else pd.NaT
)

# Kein dropna() – NaN bleibt erhalten
df_features = df_long.copy()
df_features.to_csv('data_processed/features_destatis.csv', index=False)
