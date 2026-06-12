# ============================================================
# TESINA NDIS — SCRIPT 1: DOWNLOAD DATI OECD
# ============================================================
# Scarica i dati OECD necessari per l'analisi SCM del NDIS:
# - SOCX Aggregate (spesa sociale per categoria)
# - Population by age (per controllo demografico)
# - GDP per capita (per controllo macro)
# ============================================================

library(tidyverse)

# ---- Setup -------------------------------------------------
# Il pacchetto OECD ha avuto cambiamenti di API; proviamo diversi
# approcci in ordine di preferenza.

# OPZIONE A: pacchetto OECD (può richiedere installazione)
if (!requireNamespace("OECD", quietly = TRUE)) {
  install.packages("OECD")
}
library(OECD)


# ============================================================
# 1. ESPLORAZIONE DATASET OECD
# ============================================================
# Cerchiamo i dataset rilevanti
cat("\n=== Datasets disponibili contenenti 'social expenditure' ===\n")
cerca_social <- search_dataset("social expenditure")
print(cerca_social)

cat("\n=== Datasets contenenti 'SOCX' ===\n")
cerca_socx <- search_dataset("SOCX")
print(cerca_socx)


# ============================================================
# 2. METADATI DEL DATASET PRINCIPALE
# ============================================================
# Identifica il dataset SOCX_AGG (Aggregated Social Expenditure)
dataset_id <- "SOCX_AGG"  # da aggiornare in base al risultato della ricerca

cat("\n=== Struttura del dataset SOCX_AGG ===\n")
metadata <- get_data_structure(dataset_id)
print(metadata)


# ============================================================
# 3. SCARICA SPESA INCAPACITY-RELATED
# ============================================================
# Voce: "Incapacity-related benefits" (codice 30000 o simile)
# Unit: % GDP (preferred) o USD PPP per capita
# Paesi: Australia + donor pool (OECD)
# Anni: 1990-2023

paesi_focus <- c("AUS",     # Australia (treated)
                  "CAN",     # Canada
                  "NZL",     # New Zealand
                  "GBR",     # UK
                  "USA",     # United States
                  "IRL",     # Ireland
                  "NOR",     # Norway
                  "SWE",     # Sweden
                  "FIN",     # Finland
                  "DNK")     # Denmark

socx_data <- get_dataset(dataset_id,
                          filter = list(
                            LOCATION = paesi_focus,
                            BRANCH = "30",  # Incapacity-related
                            VAR = "P0_GDP", # % GDP
                            SOURCE = "PUB"   # Public spending
                          ),
                          start_time = 1990,
                          end_time = 2023)

# Salva dati grezzi
write_csv(socx_data, "~/Downloads/TESINA_NDIS_AUSTRALIA/DATI/socx_incapacity_raw.csv")
cat("\n=== Dati SOCX salvati ===\n")
print(socx_data |> head(20))


# ============================================================
# 4. POPOLAZIONE OVER-65 PER PREDITTORE DEMOGRAFICO
# ============================================================
# Dataset OECD per indicatori demografici
dataset_pop_id <- "POPDATA"  # da verificare il codice esatto

# Alternative: World Bank WDI (più affidabile)
if (!requireNamespace("WDI", quietly = TRUE)) {
  install.packages("WDI")
}
library(WDI)

# Indicatore: SP.POP.65UP.TO.ZS — popolazione over-65 (% totale)
over65 <- WDI(indicator = "SP.POP.65UP.TO.ZS",
              country = c("AUS", "CAN", "NZL", "GBR", "USA",
                          "IRL", "NOR", "SWE", "FIN", "DNK"),
              start = 1990, end = 2023)

write_csv(over65, "~/Downloads/TESINA_NDIS_AUSTRALIA/DATI/over65_worldbank.csv")
cat("\n=== Dati over-65 salvati ===\n")
print(over65 |> head(20))


# ============================================================
# 5. PIL PRO-CAPITE
# ============================================================
# Indicatore WDI: NY.GDP.PCAP.PP.KD (GDP per capita PPP, costante)
gdp_pc <- WDI(indicator = "NY.GDP.PCAP.PP.KD",
              country = c("AUS", "CAN", "NZL", "GBR", "USA",
                          "IRL", "NOR", "SWE", "FIN", "DNK"),
              start = 1990, end = 2023)

write_csv(gdp_pc, "~/Downloads/TESINA_NDIS_AUSTRALIA/DATI/gdp_pc_worldbank.csv")
cat("\n=== Dati PIL pro-capite salvati ===\n")
print(gdp_pc |> head(20))


# ============================================================
# 6. SINTESI
# ============================================================
cat("\n=== DOWNLOAD COMPLETATO ===\n")
cat("Dati salvati in: ~/Downloads/TESINA_NDIS_AUSTRALIA/DATI/\n")
cat("- socx_incapacity_raw.csv: spesa per disabilità OECD\n")
cat("- over65_worldbank.csv: popolazione over-65\n")
cat("- gdp_pc_worldbank.csv: PIL pro-capite PPP\n")
cat("\nProssimo step: aprire lo script 2_setup_SCM.R\n")


# ============================================================
# NOTE IMPORTANTI
# ============================================================
# Se il pacchetto OECD dà problemi (API cambiata dal 2024),
# alternative:
#  1. Scaricare manualmente CSV da:
#     https://stats.oecd.org/Index.aspx?DataSetCode=SOCX_AGG
#     - Selezionare: Variable = Public expenditure
#                    Branch = Incapacity-related benefits
#                    Country = Australia, Canada, NZ, UK, USA, ecc.
#                    Year = 1990-2023
#                    Measure = % of GDP
#  2. Mettere il file in DATI/socx_incapacity_raw.csv
#  3. Procedere con lo script 2
#
# Per il pacchetto WDI invece dovrebbe funzionare sempre
# (World Bank API stabile).
# ============================================================
