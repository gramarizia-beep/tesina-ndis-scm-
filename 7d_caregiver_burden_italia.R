# ============================================================
# SCRIPT 7d — L'ONERE NASCOSTO SUL CAREGIVING ITALIANO
# Stima del trasferimento di onere sul lavoro di cura familiare
# e informale a pagamento (badanti) prodotto da una contrazione
# dell'indennita' di accompagnamento.
# ============================================================
# DOMANDA DI RICERCA
# Se il calo del 13,1% delle domande di invalidita' osservato
# nelle 9 province sperimentali del D.Lgs. 62/2024 si estendesse
# a livello nazionale, quante famiglie italiane assorbirebbero
# il caregiving non piu' finanziato dall'indennita' di accompa-
# gnamento, e quale e' l'ordine di grandezza del costo sociale
# implicito (lavoro di cura non remunerato sottratto al mercato)?
# ============================================================
# Questo script costruisce:
#   1) La classifica europea di prevalenza dei caregiver informali
#      a partire da Eurostat hlth_ehis_ic1e
#   2) La stima quantitativa dell'onere trasferito alle famiglie
#      italiane nei tre scenari di accoglimento del D.Lgs. 62/2024
#   3) Un grafico "bilancio shadow" che confronta risparmio MEF
#      e costo trasferito sulle famiglie
# ============================================================
# Riferimenti:
#   - Eurostat hlth_ehis_ic1e (Informal care providers)
#   - Verbakel et al. 2017, European Journal of Public Health 27(1)
#   - Bom et al. 2019, Journal of Population Ageing 12(3)
#   - Lancet Public Health 2025: "Burden of informal family caregiving
#     in Europe 2000-2050"
#   - INPS Rapporto annuale 2023 (beneficiari indennita' accompagnamento)
#   - IRS-INAPP 2023 (stima badanti attive in Italia)
#   - Bauer & Sousa-Poza 2015 (impatto su salute e lavoro del caregiver)
# ============================================================


# ---- 0. Setup -----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "scales", "ggrepel", "eurostat")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BASE <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
OHCHR_DIR <- file.path(BASE, "OHCHR_ARTICLE19_INDEX")
DATI_PROC <- file.path(OHCHR_DIR, "DATI_PROCESSED")
DATI_RAW  <- file.path(OHCHR_DIR, "DATI_RAW")
FIG_OHCHR <- file.path(OHCHR_DIR, "FIGURE")
RIS_OHCHR <- file.path(OHCHR_DIR, "RISULTATI")
stopifnot(dir.exists(OHCHR_DIR))


# ---- 1. PARTE A — Download prevalenza caregivers Eurostat --
cat("\n=== PARTE A — DOWNLOAD EUROSTAT hlth_ehis_ic1e ===\n")
cg_raw <- get_eurostat("hlth_ehis_ic1e", time_format = "num")
cat(sprintf("Scaricate %d righe dal dataset hlth_ehis_ic1e\n", nrow(cg_raw)))
write_csv(cg_raw, file.path(DATI_RAW, "eurostat_hlth_ehis_ic1e_caregivers.csv"))

cat("\nColonne disponibili:\n")
print(names(cg_raw))

# Ispeziona le dimensioni categoriche
for (col in c("sex", "age", "isced11", "frequenc", "duration", "carer")) {
  if (col %in% names(cg_raw)) {
    cat(sprintf("\nLivelli di '%s':\n", col))
    print(unique(cg_raw[[col]]))
  }
}


# ---- 2. Filtra: persone che forniscono care almeno settimanale ---
# Filtri standard: sex = T (totali), age = Y_GE15 o Y_GE18, frequenc = WEEK+
# La colonna esatta puo' variare; il blocco e' robusto
target_filters <- list()
if ("sex" %in% names(cg_raw))     target_filters$sex <- "T"
if ("age" %in% names(cg_raw)) {
  if ("Y_GE15" %in% cg_raw$age)   target_filters$age <- "Y_GE15"
  else if ("Y_GE18" %in% cg_raw$age) target_filters$age <- "Y_GE18"
  else if ("Y_GE16" %in% cg_raw$age) target_filters$age <- "Y_GE16"
}
if ("isced11" %in% names(cg_raw)) target_filters$isced11 <- "TOTAL"

cat(sprintf("\n\nFiltri applicati: %s\n",
            paste(names(target_filters), target_filters, sep = "=", collapse = ", ")))

cg_filtered <- cg_raw
for (k in names(target_filters)) {
  cg_filtered <- cg_filtered |> filter(.data[[k]] == target_filters[[k]])
}

# Tieni ultima annualita' per paese
cg_pronto <- cg_filtered |>
  filter(!is.na(values)) |>
  group_by(geo) |>
  filter(TIME_PERIOD == max(TIME_PERIOD)) |>
  ungroup() |>
  select(iso2 = geo, anno = TIME_PERIOD, prevalenza = values)

# Mappa ISO2 -> ISO3 (riprendi mapping_iso da Script 7b se in memoria)
if (!exists("mapping_iso")) {
  mapping_iso <- tibble::tribble(
    ~iso2, ~iso3,
    "AT","AUT","BE","BEL","BG","BGR","HR","HRV","CY","CYP","CZ","CZE",
    "DK","DNK","EE","EST","FI","FIN","FR","FRA","DE","DEU","EL","GRC",
    "GR","GRC","HU","HUN","IE","IRL","IT","ITA","LV","LVA","LT","LTU",
    "LU","LUX","MT","MLT","NL","NLD","PL","POL","PT","PRT","RO","ROU",
    "SK","SVK","SI","SVN","ES","ESP","SE","SWE",
    "UK","GBR","GB","GBR","NO","NOR","CH","CHE"
  )
}

cg_pronto <- cg_pronto |>
  left_join(mapping_iso, by = "iso2") |>
  filter(!is.na(iso3))

cat(sprintf("\nPaesi con prevalenza caregiver disponibile: %d\n", nrow(cg_pronto)))


# ---- 3. Visualizzazione: ranking caregiver UE -------------
country_pool <- read_csv(file.path(DATI_PROC, "country_pool.csv"), show_col_types = FALSE)
cg_plot <- cg_pronto |>
  left_join(country_pool |> select(iso3, paese), by = "iso3") |>
  filter(!is.na(paese)) |>
  arrange(desc(prevalenza)) |>
  mutate(italia = (iso3 == "ITA"))

fig_cg_ranking <- cg_plot |>
  mutate(paese = fct_reorder(paese, prevalenza)) |>
  ggplot(aes(x = paese, y = prevalenza, fill = italia)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", prevalenza)),
            hjust = -0.15, size = 3.0, color = "#2C2A26") +
  scale_fill_manual(values = c("FALSE" = "#888888", "TRUE" = "#B85042"),
                    guide = "none") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(cg_plot$prevalenza) * 1.18),
                     expand = c(0, 0)) +
  labs(
    title    = "Prevalenza dei caregivers informali in Europa",
    subtitle = "% popolazione adulta che dichiara di fornire cura informale almeno settimanale (Eurostat hlth_ehis_ic1e)",
    x = NULL, y = "Prevalenza caregivers (%)",
    caption = paste(
      "Italia evidenziata in rosso. Bassa prevalenza dichiarata MA elevato volume di cura familiare e badante",
      "non catturati dall'indagine EHIS (vedi nota metodologica nel testo).",
      "Fonte: Eurostat. Elaborazione R.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = "#2C2A26"),
    plot.subtitle = element_text(size = 10, color = "grey30", face = "italic"),
    plot.caption  = element_text(size = 8.5, hjust = 0, color = "grey45"),
    axis.text.y   = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(FIG_OHCHR, "Fig03_caregiver_prevalence_UE.png"),
       plot = fig_cg_ranking, width = 9, height = 8, dpi = 170, bg = "white")
cat("Grafico salvato: Fig03_caregiver_prevalence_UE.png\n")
print(fig_cg_ranking)


# ---- 4. PARTE B — Stima onere trasferito alle famiglie italiane ---
cat("\n\n=== PARTE B — STIMA ONERE TRASFERITO SUI CAREGIVER ITALIANI ===\n")

# Parametri base (dalla tua stima fiscale Script 6)
DOMANDE_NAZIONALI_ANNUE <- 194251
CALO_DOMANDE_PRES <- 0.131
TASSI_ACCOGLIMENTO <- c(low = 0.50, mid = 0.60, high = 0.70)

# Parametri di letteratura (citazioni esplicite nella tesi)
ORE_CAREGIVING_SETTIMANA <- 24    # Bom et al. 2019 (mediana caregiver mediterraneo per care receiver SM_SEV)
TARIFFA_BADANTE_EUR_ORA  <- 10.0  # Stima IRS-INAPP 2023 (lordo orario medio badante Italia)
SETTIMANE_ANNO           <- 52
FTE_ORE_ANNO             <- 1760

# Calcolo per i tre scenari
shift <- tibble(
  scenario = names(TASSI_ACCOGLIMENTO),
  tasso = TASSI_ACCOGLIMENTO,
  famiglie_escluse_annue = DOMANDE_NAZIONALI_ANNUE * CALO_DOMANDE_PRES * tasso,
  ore_caregiving_annue = famiglie_escluse_annue * ORE_CAREGIVING_SETTIMANA * SETTIMANE_ANNO,
  fte_caregiving = ore_caregiving_annue / FTE_ORE_ANNO,
  valore_shadow_mln_eur = ore_caregiving_annue * TARIFFA_BADANTE_EUR_ORA / 1e6
)

cat("\nParametri di letteratura usati:\n")
cat(sprintf("  - Ore caregiving/settimana per famiglia: %d (Bom et al. 2019)\n", ORE_CAREGIVING_SETTIMANA))
cat(sprintf("  - Tariffa badante EUR/ora (Italia, lordo): %.1f (IRS-INAPP 2023)\n", TARIFFA_BADANTE_EUR_ORA))
cat(sprintf("  - Settimane lavorative/anno: %d\n", SETTIMANE_ANNO))
cat(sprintf("  - FTE equivalente ore/anno: %d\n\n", FTE_ORE_ANNO))

cat("=== STIMA ONERE TRASFERITO SUI CAREGIVER ITALIANI ===\n")
print(shift |>
        mutate(famiglie_escluse_annue = round(famiglie_escluse_annue),
               ore_caregiving_annue_mln = round(ore_caregiving_annue / 1e6, 2),
               fte_caregiving = round(fte_caregiving),
               valore_shadow_mln_eur = round(valore_shadow_mln_eur, 1)) |>
        select(scenario, tasso, famiglie_escluse_annue,
               ore_caregiving_annue_mln, fte_caregiving, valore_shadow_mln_eur))


# ---- 5. Grafico "bilancio shadow" -------------------------
# Confronto: risparmio MEF (Script 6) + costo trasferito famiglie
RISPARMIO_MEF_SCENARI <- c(low = 73.4, mid = 88.0, high = 102.7)  # mln EUR/anno (anno 1)

bilancio <- shift |>
  mutate(
    risparmio_mef_mln = RISPARMIO_MEF_SCENARI[scenario],
    netto_societa_mln = risparmio_mef_mln + valore_shadow_mln_eur  # entrambi sono costi/oneri per la societa'
  )

# Visualizza come due barre stacked per scenario (MEF + famiglie)
bilancio_long <- bilancio |>
  select(scenario, risparmio_mef_mln, valore_shadow_mln_eur) |>
  pivot_longer(c(risparmio_mef_mln, valore_shadow_mln_eur),
               names_to = "tipo", values_to = "valore") |>
  mutate(
    tipo_lab = case_when(
      tipo == "risparmio_mef_mln" ~ "Mancato esborso MEF (anno 1)",
      tipo == "valore_shadow_mln_eur" ~ "Costo shadow lavoro caregiver familiare"
    ),
    scenario_lab = factor(scenario, levels = c("low","mid","high"),
                          labels = c("Accoglimento 50%", "Accoglimento 60%", "Accoglimento 70%"))
  )

fig_bilancio <- ggplot(bilancio_long,
                       aes(x = scenario_lab, y = valore, fill = tipo_lab)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f", valore)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Mancato esborso MEF (anno 1)" = "#2C5E80",
    "Costo shadow lavoro caregiver familiare" = "#B85042"
  )) +
  labs(
    title    = "Il bilancio shadow della contrazione dell'indennita' di accompagnamento",
    subtitle = "Anno 1 di rollout nazionale del D.Lgs. 62/2024 (2027), tre scenari di accoglimento",
    x = NULL, y = "Milioni di EUR/anno",
    fill = NULL,
    caption = paste(
      "Mancato esborso MEF = prestazioni non erogate (Script 6).",
      "Costo shadow caregiver = 15.268 famiglie x 24 ore/settimana x 52 sett. x 10 EUR/ora (Bom 2019; IRS-INAPP 2023).",
      "Le due grandezze sono dello stesso ordine. Lo Stato risparmia 88 mln, le famiglie assorbono lavoro per 190 mln.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#2C2A26"),
    plot.subtitle    = element_text(size = 10, color = "grey30", face = "italic"),
    plot.caption     = element_text(size = 8.5, hjust = 0, color = "grey45"),
    legend.position  = "top",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIG_OHCHR, "Fig04_bilancio_shadow_caregiver.png"),
       plot = fig_bilancio, width = 10, height = 6.3, dpi = 170, bg = "white")
cat("Grafico salvato: Fig04_bilancio_shadow_caregiver.png\n")
print(fig_bilancio)


# ---- 6. Esporta tabella risultati --------------------------
write_csv(bilancio, file.path(RIS_OHCHR, "stima_onere_caregiver_italia.csv"))
cat(sprintf("\nTabella salvata: %s/stima_onere_caregiver_italia.csv\n", RIS_OHCHR))


# ---- 7. Sintesi a video -----------------------------------
cat("\n\n=== SINTESI SCRIPT 7d — ONERE NASCOSTO SUI CAREGIVER ===\n")
cat(sprintf("Scenario centrale (accoglimento 60%%):\n"))
mid <- bilancio |> filter(scenario == "mid")
cat(sprintf("  - %s famiglie/anno escluse dall'indennita'\n",
            format(round(mid$famiglie_escluse_annue), big.mark = ".")))
cat(sprintf("  - %.1f milioni di ore di caregiving/anno trasferite alle famiglie\n",
            mid$ore_caregiving_annue / 1e6))
cat(sprintf("  - %s FTE caregiving informali aggiuntivi NON retribuiti\n",
            format(round(mid$fte_caregiving), big.mark = ".")))
cat(sprintf("  - %.1f mln EUR/anno di costo shadow del lavoro di cura\n",
            mid$valore_shadow_mln_eur))
cat(sprintf("  - %.1f mln EUR/anno di mancato esborso MEF (anno 1)\n",
            mid$risparmio_mef_mln))
cat(sprintf("  - Il costo trasferito alle famiglie e' %.1fx il risparmio del MEF\n",
            mid$valore_shadow_mln_eur / mid$risparmio_mef_mln))
cat("\nLa contrazione amministrativa dell'accesso all'indennita' di accompagnamento\n")
cat("NON e' un risparmio sociale: e' un trasferimento netto di costi dal bilancio\n")
cat("pubblico alle famiglie italiane (caregiving non retribuito + spesa privata badanti).\n\n")
