# ============================================================
# SCRIPT 7b — INDICE COMPOSITO OHCHR ARTICLE 19 (parte 2)
# Download dei due indicatori di OUTCOME dal portale ufficiale
# Eurostat tramite il pacchetto R 'eurostat':
#   - O1: tasso di occupazione delle persone con disabilita'
#         (limitazioni attivita') 16-64 anni
#   - O2: tasso a rischio poverta' delle persone con disabilita'
# I dati vengono scaricati LIVE dalle API Eurostat (richiede
# connessione internet). Ogni paese e' identificato dal codice
# ISO2 (Eurostat) che viene convertito al nostro ISO3 standard.
# ============================================================
# Riferimenti dataset Eurostat:
#   - Disability statistics (hlth_dsi): tema "Disability"
#     https://ec.europa.eu/eurostat/web/health/disability
#   - L'API Eurostat e' libera e non richiede chiavi/autenticazione.
# ============================================================


# ---- 0. Setup -----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "eurostat")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BASE <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
OHCHR_DIR  <- file.path(BASE, "OHCHR_ARTICLE19_INDEX")
DATI_PROC  <- file.path(OHCHR_DIR, "DATI_PROCESSED")
DATI_RAW   <- file.path(OHCHR_DIR, "DATI_RAW")
FIG_OHCHR  <- file.path(OHCHR_DIR, "FIGURE")
stopifnot(dir.exists(OHCHR_DIR))


# ---- 1. Carica il dataset costruito da Script 7 (parte 1) --
# Lo scaffold gia' contiene 31 paesi x 6 indicatori, con P1 e P2
# popolati da OECD SOCX per 7 paesi. Aggiungeremo O1 e O2.
scaffold_pop <- read_csv(file.path(DATI_PROC, "scaffold_indicatori_v1_da_socx.csv"),
                         show_col_types = FALSE)
indicatori   <- read_csv(file.path(DATI_PROC, "framework_indicatori_ohchr.csv"),
                         show_col_types = FALSE)
country_pool <- read_csv(file.path(DATI_PROC, "country_pool.csv"),
                         show_col_types = FALSE)
cat(sprintf("Scaffold caricato: %d righe (%d paesi x %d indicatori)\n",
            nrow(scaffold_pop), nrow(country_pool), nrow(indicatori)))


# ---- 2. Mappa ISO2 (Eurostat) ←→ ISO3 -----------------------
# Eurostat usa codici ISO2 ("IT", "FR", "DE"), il nostro dataset
# usa ISO3 ("ITA", "FRA", "DEU"). Costruiamo la mappa.
mapping_iso <- tibble::tribble(
  ~iso2, ~iso3,
  "AT","AUT","BE","BEL","BG","BGR","HR","HRV","CY","CYP","CZ","CZE",
  "DK","DNK","EE","EST","FI","FIN","FR","FRA","DE","DEU","EL","GRC",
  "GR","GRC","HU","HUN","IE","IRL","IT","ITA","LV","LVA","LT","LTU",
  "LU","LUX","MT","MLT","NL","NLD","PL","POL","PT","PRT","RO","ROU",
  "SK","SVK","SI","SVN","ES","ESP","SE","SWE",
  "UK","GBR","GB","GBR","NO","NOR","CH","CHE"
)
# (Eurostat usa "EL" e talvolta "GR" per la Grecia, "UK" e "GB"
# per il Regno Unito: accettiamo entrambe le varianti.)


# ---- 3. Esplora le serie disabilita' in Eurostat ------------
# Prima di scaricare ciecamente, vediamo quali dataset esistono
# con la parola chiave "disability" o "limitation".
cat("\n=== RICERCA DATASET EUROSTAT 'disability' ===\n")
disab_search <- search_eurostat("disability", type = "dataset")
cat(sprintf("Dataset trovati: %d\n\n", nrow(disab_search)))
print(disab_search |>
        select(code, title) |>
        head(20))


# ---- 4. Download O1: tasso occupazione persone con disab. ---
# Dataset target: hlth_dsi070 oppure hlth_silc_07 (a seconda di
# quale e' attivo al momento dell'esecuzione).
# La strategia: prova il primo, se fallisce passa al secondo.
cat("\n=== DOWNLOAD O1: tasso occupazione persone disab. ===\n")
o1_dataset_id <- NULL
for (try_id in c("hlth_dsi070", "hlth_silc_07", "hlth_dlm200")) {
  o1_try <- try(get_eurostat(try_id, time_format = "num",
                              filters = list()), silent = TRUE)
  if (!inherits(o1_try, "try-error") && nrow(o1_try) > 0) {
    o1_dataset_id <- try_id
    o1_raw <- o1_try
    cat(sprintf("OK - dataset '%s' scaricato (%d righe).\n",
                try_id, nrow(o1_raw)))
    break
  } else {
    cat(sprintf("KO - dataset '%s' non disponibile.\n", try_id))
  }
}
if (is.null(o1_dataset_id)) {
  stop("Nessun dataset Eurostat per O1 e' risultato scaricabile. ",
       "Verificare la connessione o aggiornare i codici dataset.")
}

# Salva una copia raw del dataset scaricato
write_csv(o1_raw, file.path(DATI_RAW, sprintf("eurostat_%s_O1.csv", o1_dataset_id)))
cat(sprintf("Copia salvata in DATI_RAW/eurostat_%s_O1.csv\n", o1_dataset_id))

# Ispeziona le colonne e i livelli delle variabili categoriche
cat(sprintf("\nColonne del dataset:\n"))
print(names(o1_raw))
cat(sprintf("\nPrime righe:\n"))
print(head(o1_raw, 6))


# ---- 5. Download O2: tasso a rischio poverta' persone disab. ----
cat("\n=== DOWNLOAD O2: tasso a rischio poverta' persone disab. ===\n")
o2_dataset_id <- NULL
for (try_id in c("hlth_dsi050", "ilc_di04", "hlth_silc_10")) {
  o2_try <- try(get_eurostat(try_id, time_format = "num",
                              filters = list()), silent = TRUE)
  if (!inherits(o2_try, "try-error") && nrow(o2_try) > 0) {
    o2_dataset_id <- try_id
    o2_raw <- o2_try
    cat(sprintf("OK - dataset '%s' scaricato (%d righe).\n",
                try_id, nrow(o2_raw)))
    break
  } else {
    cat(sprintf("KO - dataset '%s' non disponibile.\n", try_id))
  }
}
if (is.null(o2_dataset_id)) {
  stop("Nessun dataset Eurostat per O2 e' risultato scaricabile.")
}

write_csv(o2_raw, file.path(DATI_RAW, sprintf("eurostat_%s_O2.csv", o2_dataset_id)))
cat(sprintf("Copia salvata in DATI_RAW/eurostat_%s_O2.csv\n", o2_dataset_id))

cat(sprintf("\nColonne del dataset:\n"))
print(names(o2_raw))
cat(sprintf("\nPrime righe:\n"))
print(head(o2_raw, 6))


# ---- 6. STOP TECNICO -----------------------------------------
# Dopo le sezioni 4 e 5 lo script si ferma per ispezione manuale
# perche' i dataset Eurostat hanno strutture e nomi colonne che
# possono variare. Le sezioni 7-9 (filtro disabilita', selezione
# ultima annualita', merge nello scaffold) sono scritte come
# template ma vanno adattate al dataset effettivamente scaricato.
cat("\n\n========================================================\n")
cat("STOP TECNICO — i dataset Eurostat sono stati scaricati.\n")
cat("Adesso ispeziona le colonne e i livelli per capire come\n")
cat("filtrare la 'condizione disabilita'.\n")
cat(sprintf("Dataset O1: %s\n", o1_dataset_id))
cat(sprintf("Dataset O2: %s\n", o2_dataset_id))
cat("========================================================\n")
