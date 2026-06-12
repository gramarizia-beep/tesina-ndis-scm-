# ============================================================
# SCRIPT 7 — INDICE COMPOSITO OHCHR ARTICLE 19
# Costruzione di un indice cross-country di attuazione operativa
# dell'articolo 19 della Convenzione ONU sui Diritti delle Persone
# con Disabilita', basato sugli "illustrative indicators" OHCHR
# (SDG-CRPD Resource Package, 2020-2022).
# ============================================================
# DOMANDA DI RICERCA
# Quale paese UE-27 (+ benchmark esterni: UK, NOR, CHE, AUS) e'
# piu' vicino all'attuazione operativa dell'articolo 19 secondo
# gli indicatori illustrativi OHCHR? Dove si colloca l'Italia in
# classifica? E' coerente con quanto suggerito dalla nostra
# analisi SCM precedente?
# ============================================================
# QUESTO SCRIPT (Parte 1 di 3)
# 1) Definisce il framework degli indicatori (4-6 indicatori scelti)
# 2) Definisce il country pool
# 3) Costruisce lo scaffold del dataset (paesi x indicatori, con NA)
# 4) Popola i primi due indicatori dai dati OECD SOCX TP31 che gia'
#    abbiamo nel progetto (quota in-kind e quota cash della spesa
#    per disabilita').
# 5) Salva un dataset intermedio CSV in DATI_PROCESSED/.
#
# Le parti 2 e 3 (Script 7b e 7c) raccoglieranno gli altri
# indicatori da Eurostat e ANED e costruiranno l'indice composito.
# ============================================================
# RIFERIMENTI
#   - OHCHR (2022). Article 19: List of illustrative indicators
#     on living independently and being included in the community.
#     SDG-CRPD Resource Package, Geneva.
#   - UN Committee on the Rights of Persons with Disabilities (2017).
#     General Comment No. 5 on Article 19.
#   - OECD Social Expenditure Database (SOCX), TP31.
# ============================================================


# ---- 0. Setup -----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "scales")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BASE <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
OHCHR_DIR  <- file.path(BASE, "OHCHR_ARTICLE19_INDEX")
DATI_PROC  <- file.path(OHCHR_DIR, "DATI_PROCESSED")
DATI_RAW   <- file.path(OHCHR_DIR, "DATI_RAW")
FIG_OHCHR  <- file.path(OHCHR_DIR, "FIGURE")
RIS_OHCHR  <- file.path(OHCHR_DIR, "RISULTATI")

# Verifica che le cartelle esistano
stopifnot(dir.exists(OHCHR_DIR))
cat("Cartelle del progetto OHCHR verificate:\n")
cat(sprintf("  %s\n", OHCHR_DIR))


# ---- 1. Framework degli indicatori --------------------------
# Costruiamo 6 indicatori organizzati nei 3 livelli OHCHR
# (structural / process / outcome). I primi 2 li popoliamo subito
# da dati OECD SOCX gia' nel progetto; gli altri 4 in Script 7b/7c.
indicatori <- tibble::tribble(
  ~codice,  ~livello,    ~nome,                                 ~descrizione,                                                   ~fonte,                       ~direzione,
  "S1",     "Structural", "Riconoscimento giuridico Art.19",     "Legge nazionale che riconosca vita indipendente come diritto", "ANED Country Reports",       "+",
  "P1",     "Process",    "Quota in-kind su totale disabilita'", "% spesa pubblica per servizi (in-kind) sul totale TP31",       "OECD SOCX TP31",             "+",
  "P2",     "Process",    "Quota cash su totale disabilita'",    "% spesa pubblica per trasferimenti monetari sul totale TP31",  "OECD SOCX TP31",             "-",
  "O1",     "Outcome",    "Tasso occupazione persone disab.",    "% persone con disabilita' occupate 16-64 anni",                "Eurostat hlth_dlm200",       "+",
  "O2",     "Outcome",    "Rischio poverta' persone disab.",     "% persone con disabilita' a rischio poverta'",                 "Eurostat ilc_di04",          "-",
  "O3",     "Outcome",    "Tasso istituzionalizzazione disab.",  "Persone con disabilita' in istituzioni per 1.000 ab.",          "ANED / EU-SILC",             "-"
)

cat("\n=== FRAMEWORK INDICATORI OHCHR ARTICLE 19 ===\n")
print(indicatori |> select(codice, livello, nome, direzione))

cat("\nLegenda direzione:\n")
cat("  '+' = valori alti = maggiore attuazione Art.19\n")
cat("  '-' = valori alti = minore  attuazione Art.19 (vanno invertiti nell'indice)\n")


# ---- 2. Country pool ----------------------------------------
# 27 UE + benchmark esterni (UK, NOR, CHE, AUS).
# Il codice ISO3 e' lo standard in OECD/Eurostat.
country_pool <- tibble::tribble(
  ~iso3, ~paese,           ~regione,
  "AUT", "Austria",         "UE-27",
  "BEL", "Belgio",          "UE-27",
  "BGR", "Bulgaria",        "UE-27",
  "HRV", "Croazia",         "UE-27",
  "CYP", "Cipro",           "UE-27",
  "CZE", "Repubblica Ceca", "UE-27",
  "DNK", "Danimarca",       "UE-27",
  "EST", "Estonia",         "UE-27",
  "FIN", "Finlandia",       "UE-27",
  "FRA", "Francia",         "UE-27",
  "DEU", "Germania",        "UE-27",
  "GRC", "Grecia",          "UE-27",
  "HUN", "Ungheria",        "UE-27",
  "IRL", "Irlanda",         "UE-27",
  "ITA", "Italia",          "UE-27",
  "LVA", "Lettonia",        "UE-27",
  "LTU", "Lituania",        "UE-27",
  "LUX", "Lussemburgo",     "UE-27",
  "MLT", "Malta",           "UE-27",
  "NLD", "Paesi Bassi",     "UE-27",
  "POL", "Polonia",         "UE-27",
  "PRT", "Portogallo",      "UE-27",
  "ROU", "Romania",         "UE-27",
  "SVK", "Slovacchia",      "UE-27",
  "SVN", "Slovenia",        "UE-27",
  "ESP", "Spagna",          "UE-27",
  "SWE", "Svezia",          "UE-27",
  "GBR", "Regno Unito",     "Benchmark esterno",
  "NOR", "Norvegia",        "Benchmark esterno",
  "CHE", "Svizzera",        "Benchmark esterno",
  "AUS", "Australia",       "Benchmark esterno (gia' analizzato SCM)"
)

cat(sprintf("\n=== COUNTRY POOL ===\n"))
cat(sprintf("Totale paesi nel pool: %d\n", nrow(country_pool)))
cat(sprintf("  UE-27:                 %d\n", sum(country_pool$regione == "UE-27")))
cat(sprintf("  Benchmark esterni:     %d\n", sum(country_pool$regione != "UE-27")))


# ---- 3. Scaffold del dataset --------------------------------
# Costruiamo la matrice paesi x indicatori, riempita di NA.
# Sara' progressivamente popolata dagli script 7a (questo),
# 7b (Eurostat) e 7c (ANED).
scaffold <- expand_grid(
  iso3 = country_pool$iso3,
  codice = indicatori$codice
) |>
  left_join(country_pool, by = "iso3") |>
  left_join(indicatori |> select(codice, nome, fonte, direzione), by = "codice") |>
  mutate(valore = NA_real_,
         anno   = NA_integer_,
         note   = NA_character_) |>
  select(iso3, paese, regione, codice, nome, fonte, direzione, valore, anno, note)

cat(sprintf("\n=== SCAFFOLD DATASET ===\n"))
cat(sprintf("Righe totali: %d (%d paesi x %d indicatori)\n",
            nrow(scaffold), nrow(country_pool), nrow(indicatori)))


# ---- 4. Popolamento P1 e P2 da OECD SOCX --------------------
# Carichiamo il CSV gia' presente nel progetto e calcoliamo la
# composizione cash/in-kind dell'ultima annualita' disponibile
# per ciascun paese.
SOCX_FILE <- file.path(BASE, "DATI", "SOCKS_INCAPACITY_PPPPC.csv")
stopifnot(file.exists(SOCX_FILE))

raw <- read_csv(SOCX_FILE, show_col_types = FALSE)

# Mantieni solo righe valide
long <- raw |>
  filter(!is.na(OBS_VALUE), PRICE_BASE == "V") |>
  transmute(iso3    = REF_AREA,
            anno    = as.integer(TIME_PERIOD),
            tipo    = SPENDING_TYPE,   # "10" = cash, "20" = in-kind, "_T" = totale
            valore  = as.numeric(OBS_VALUE))

# Identifica le tre componenti (totale, cash, in-kind) per ciascun paese-anno
# Codifica OECD effettivamente presente in SOCKS_INCAPACITY_PPPPC.csv:
#   "_T" = totale
#   "C"  = cash
#   "K"  = in-kind
# (Esistono varianti numeriche "10"/"20" su altri file SOCX: qui usiamo C/K).
cat("\nCodifiche SPENDING_TYPE presenti nel dataset:\n")
print(long |> count(tipo) |> arrange(desc(n)))


# ---- 5. Snapshot dell'ultima annualita' disponibile ---------
# Per ogni paese troviamo l'ultimo anno con tutti e tre i dati
ultime <- long |>
  filter(tipo %in% c("_T", "C", "K")) |>
  pivot_wider(names_from = tipo, values_from = valore,
              names_prefix = "spesa_") |>
  rename(totale = spesa__T, cash = spesa_C, in_kind = spesa_K) |>
  filter(!is.na(totale), !is.na(cash), !is.na(in_kind), totale > 0) |>
  group_by(iso3) |>
  filter(anno == max(anno)) |>
  ungroup() |>
  mutate(
    quota_inkind = in_kind / totale * 100,
    quota_cash   = cash    / totale * 100
  )

cat(sprintf("\n=== SNAPSHOT OECD SOCX — ULTIMA ANNUALITA' DISPONIBILE ===\n"))
cat(sprintf("Paesi con dati cash + in-kind: %d\n", nrow(ultime)))
print(ultime |>
        select(iso3, anno, quota_inkind, quota_cash) |>
        arrange(desc(quota_inkind)) |>
        mutate(across(c(quota_inkind, quota_cash), ~ round(.x, 1))))


# ---- 6. Inserimento P1 e P2 nello scaffold ------------------
scaffold_pop <- scaffold |>
  left_join(ultime |> select(iso3, anno_socx = anno, quota_inkind, quota_cash),
            by = "iso3") |>
  mutate(
    valore = case_when(
      codice == "P1" & !is.na(quota_inkind) ~ quota_inkind,
      codice == "P2" & !is.na(quota_cash)   ~ quota_cash,
      TRUE                                  ~ valore
    ),
    anno = case_when(
      codice %in% c("P1", "P2") & !is.na(anno_socx) ~ anno_socx,
      TRUE                                          ~ anno
    ),
    note = case_when(
      codice == "P1" & !is.na(quota_inkind) ~ "da OECD SOCX TP31",
      codice == "P2" & !is.na(quota_cash)   ~ "da OECD SOCX TP31",
      TRUE                                  ~ note
    )
  ) |>
  select(-quota_inkind, -quota_cash, -anno_socx)

# Statistica della copertura
copertura <- scaffold_pop |>
  group_by(codice, nome) |>
  summarise(coperti = sum(!is.na(valore)),
            totali  = n(),
            pct     = round(coperti / totali * 100, 1),
            .groups = "drop")
cat(sprintf("\n=== COPERTURA DATI PER INDICATORE ===\n"))
print(copertura)


# ---- 7. Salva il dataset intermedio --------------------------
out_file <- file.path(DATI_PROC, "scaffold_indicatori_v1_da_socx.csv")
write_csv(scaffold_pop, out_file)
cat(sprintf("\nDataset intermedio salvato in:\n  %s\n", out_file))

# Salva anche il framework e il country pool per riferimento
write_csv(indicatori,   file.path(DATI_PROC, "framework_indicatori_ohchr.csv"))
write_csv(country_pool, file.path(DATI_PROC, "country_pool.csv"))


# ---- 8. Visualizzazione esplorativa ------------------------
# Bar chart: quota in-kind per paese (ranking)
# Solo i paesi con dato disponibile
ranking_inkind <- scaffold_pop |>
  filter(codice == "P1", !is.na(valore)) |>
  arrange(desc(valore))

fig <- ranking_inkind |>
  mutate(paese = fct_reorder(paese, valore),
         italia = (iso3 == "ITA")) |>
  ggplot(aes(x = paese, y = valore, fill = italia)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", valore)),
            hjust = -0.15, size = 3.2, color = "#2C2A26") +
  scale_fill_manual(values = c("FALSE" = "#888888", "TRUE" = "#B85042"),
                    guide = "none") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(ranking_inkind$valore) * 1.18),
                     expand = c(0, 0)) +
  labs(
    title    = "Quota della spesa per disabilita' destinata a servizi (in-kind)",
    subtitle = "% sul totale della spesa pubblica per disabilita' (OECD SOCX TP31, ultima annualita' disponibile)",
    x = NULL,
    y = "Quota in-kind (% sul totale)",
    caption = paste(
      "Indicatore P1 del framework operativo OHCHR Article 19.",
      "Italia NON disponibile nel dataset OECD SOCX corrente: sara' aggiunta da Eurostat",
      "nello Script 7b (parte 2). Pool ridotto: 7 paesi dei 31 del country_pool.",
      "Fonte: OECD SOCX. Elaborazione R.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#2C2A26"),
    plot.subtitle    = element_text(size = 10, color = "grey30", face = "italic"),
    plot.caption     = element_text(size = 8.5, hjust = 0, color = "grey45"),
    axis.text.y      = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(FIG_OHCHR, "Fig01_ranking_inkind.png"),
       plot = fig, width = 9, height = 8, dpi = 170, bg = "white")
cat(sprintf("\nGrafico salvato:\n  %s\n", file.path(FIG_OHCHR, "Fig01_ranking_inkind.png")))

print(fig)


# ---- 9. Sintesi a video --------------------------------------
cat("\n\n=== SINTESI SCRIPT 7 (parte 1) ===\n")
cat(sprintf("Indicatori definiti:        %d (3 livelli OHCHR)\n", nrow(indicatori)))
cat(sprintf("Paesi nel pool:             %d (27 UE + 4 esterni)\n", nrow(country_pool)))
cat(sprintf("Indicatori popolati ora:    2 di 6 (P1 in-kind, P2 cash da SOCX)\n"))
cat(sprintf("Dataset intermedio:         scaffold_indicatori_v1_da_socx.csv\n"))
cat(sprintf("Prossimo passo (Script 7b): popolare O1 (occupazione) e O2 (poverta') da Eurostat\n"))
cat(sprintf("Successivo (Script 7c):     popolare S1 (legge) e O3 (istituzioni) da ANED + costruire indice composito\n\n"))
