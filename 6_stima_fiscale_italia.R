# ============================================================
# SCRIPT 6 - STIMA FISCALE ITALIA (v2 - perimetro previdenziale)
# Quante persone non entrerebbero nel sistema di invalidita'
# PREVIDENZIALE e quale sarebbe il "risparmio implicito" del MEF
# se il calo del -13,1% registrato nelle 9 province pilota
# D.Lgs. 62/2024 si estendesse a livello nazionale dal 2027.
# ============================================================
# FONTI:
#  - Baseline e calo: CGIL - Osservatorio Previdenza, 30/05/2026
#    (194.251 domande/anno di invalidita' e inabilita'
#    previdenziale; -13,1% domande presentate; -12,1% accolte)
#  - Importi medi e mix: INPS, Osservatorio statistico
#    "Pensioni vigenti e liquidate", ed. 2026 (pensioni
#    previdenziali LIQUIDATE nel 2025, Tavola 5):
#      Assegno ordinario invalidita': 63.021 pensioni,
#        603,8 mln EUR/anno  -> medio 9.581 EUR/anno
#      Pensione di inabilita':         4.484 pensioni,
#        65,3 mln EUR/anno   -> medio 14.564 EUR/anno
#    Si usano le pensioni LIQUIDATE (nuove) e non lo stock,
#    perche' gli esclusi sarebbero stati NUOVI beneficiari.
# ============================================================
suppressPackageStartupMessages({library(ggplot2); library(tibble); library(dplyr); library(scales)})
BASE <- "."
FIG  <- file.path(BASE, "FIGURE"); if (!dir.exists(FIG)) dir.create(FIG, recursive = TRUE)
OUT  <- file.path(BASE, "STIMA_FISCALE_ITALIA"); if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)
# ---- 1. Costanti ufficiali ---------------------------------
DOMANDE_NAZIONALI_ANNUE <- 194251      # CGIL 30/05/2026
CALO_DOMANDE_PRES <- 0.131             # -13,1% domande presentate
CALO_DOMANDE_ACC  <- 0.121             # -12,1% domande accolte
CALO_DIFF_DID     <- 0.141             # DiD trattamento - controllo
# Importi medi annui da Osservatorio INPS (liquidate 2025)
N_AOI  <- 63021;  IMP_AOI_MLN  <- 603.8
N_INAB <- 4484;   IMP_INAB_MLN <- 65.3
IMPORTO_AOI  <- IMP_AOI_MLN  * 1e6 / N_AOI    # ~9.581 EUR/anno
IMPORTO_INAB <- IMP_INAB_MLN * 1e6 / N_INAB   # ~14.564 EUR/anno
# Mix reale dalle liquidazioni 2025
MIX_AOI  <- N_AOI  / (N_AOI + N_INAB)         # 93,4%
MIX_INAB <- N_INAB / (N_AOI + N_INAB)         #  6,6%
PRESTAZIONE_MEDIA <- MIX_AOI * IMPORTO_AOI + MIX_INAB * IMPORTO_INAB
# Tassi di accoglimento (scenari):
#  - low 35%: rapporto pensioni liquidate 2025 (67.505) /
#    domande annue (194.251) = 34,8% -> ancoraggio documentato
#    (limite inferiore: le domande includono rinnovi e riesami)
#  - mid 50% e high 60%: sensibilita'
TASSI_ACCOGLIMENTO <- c(low = 0.35, mid = 0.50, high = 0.60)
cat(sprintf("\nImporto medio AOI:        %s EUR/anno\n", format(round(IMPORTO_AOI), big.mark=".")))
cat(sprintf("Importo medio inabilita': %s EUR/anno\n", format(round(IMPORTO_INAB), big.mark=".")))
cat(sprintf("Mix AOI/inabilita':       %.1f%% / %.1f%%\n", MIX_AOI*100, MIX_INAB*100))
cat(sprintf("Prestazione media pesata: %s EUR/anno\n", format(round(PRESTAZIONE_MEDIA), big.mark=".")))
# ---- 2. Domande mancate annue ------------------------------
DOMANDE_MANCATE_ANNUE <- DOMANDE_NAZIONALI_ANNUE * CALO_DOMANDE_PRES
cat(sprintf("\nDomande mancate annue (194.251 x 13,1%%): %s\n",
format(round(DOMANDE_MANCATE_ANNUE), big.mark=".")))
# ---- 3-5. Persone escluse, risparmio, proiezione 2027-2031 --
persone_non_entrano <- DOMANDE_MANCATE_ANNUE * TASSI_ACCOGLIMENTO
anni <- 2027:2031
df_proj <- expand.grid(anno = anni, scenario = names(TASSI_ACCOGLIMENTO),
stringsAsFactors = FALSE) |>
arrange(scenario, anno) |> as_tibble() |>
group_by(scenario) |>
mutate(cohort = persone_non_entrano[scenario],
stock_persone = cumsum(cohort),
risparmio_annuo_mln = stock_persone * PRESTAZIONE_MEDIA / 1e6) |>
ungroup()
df_2031 <- df_proj |> group_by(scenario) |>
summarise(stock_finale = stock_persone[anno == 2031],
annuo_2031_mln = risparmio_annuo_mln[anno == 2031],
cumulato_5yr_mln = sum(risparmio_annuo_mln)) |>
mutate(Scenario = paste0("Accoglimento ", round(TASSI_ACCOGLIMENTO[scenario]*100), "%"))
SPESA_DISABILITA_ITALIA_MLD <- 38000
cat("\n=== SINTESI AL 2031 ===\n")
for (s in c("low","mid","high")) {
r <- df_2031[df_2031$scenario == s, ]
cat(sprintf("%-22s persone: %s | annuo 2031: %s mln | cumulato 5a: %s mln | %% spesa: %.2f%%\n",
r$Scenario, format(round(r$stock_finale), big.mark="."),
format(round(r$annuo_2031_mln), big.mark="."),
format(round(r$cumulato_5yr_mln), big.mark="."),
r$annuo_2031_mln / SPESA_DISABILITA_ITALIA_MLD * 100))
}
# ---- 8. Grafico 1: persone fuori dal sistema ---------------
lab_scen <- c(low="Accoglimento 35% (liquidate/domande 2025)",
mid="Accoglimento 50% (centrale)",
high="Accoglimento 60% (alto)")
fig1 <- df_proj |>
mutate(scenario_lab = factor(lab_scen[scenario], levels = lab_scen)) |>
ggplot(aes(anno, stock_persone/1000, color = scenario_lab, shape = scenario_lab)) +
geom_line(linewidth = 1.6) + geom_point(size = 3) +
geom_text(aes(label = formatC(round(stock_persone), big.mark=".", format="d")),
vjust = -1.3, size = 3.4, show.legend = FALSE, color = "#2C2A26") +
scale_color_manual(values = c("#2C5E80", "#B85042", "#D4A056")) +
scale_shape_manual(values = c(16, 17, 15)) +
scale_y_continuous(labels = function(x) formatC(x*1000, big.mark=".", format="d"),
limits = c(0, max(df_proj$stock_persone)*1.18/1000)) +
labs(title = "Persone potenzialmente escluse dall'invalidit\u00e0 previdenziale",
subtitle = "Stock cumulato 2027-2031 se il calo del 13,1% delle nove province pilota si estendesse a livello nazionale",
x = "Anno", y = "Persone fuori dal sistema (cumulato)", color = NULL, shape = NULL,
caption = paste0("Calcolo: 194.251 domande previdenziali/anno x 13,1% x tasso di accoglimento.\n",
"Fonti: CGIL - Osservatorio Previdenza 30/05/2026 (baseline e calo); ",
"scenario 35% = pensioni liquidate 2025 / domande annue (INPS). Elaborazione R.")) +
theme_minimal(base_size = 12) +
theme(plot.title = element_text(face="bold", size=14, color="#2C2A26"),
plot.subtitle = element_text(size=11, color="grey30"),
plot.caption = element_text(size=9, hjust=0, color="grey45"),
legend.position = "top", panel.grid.minor = element_blank())
ggsave(file.path(FIG, "Figura_Italia_Stima_Persone_Escluse.png"), fig1,
width = 11, height = 6.2, dpi = 170, bg = "white")
# ---- 9. Grafico 2: mancato esborso MEF ---------------------
fig2 <- df_proj |>
mutate(scenario_lab = factor(lab_scen[scenario], levels = lab_scen)) |>
ggplot(aes(anno, risparmio_annuo_mln, fill = scenario_lab)) +
geom_col(position = position_dodge(width=0.8), width=0.7, color="white", linewidth=0.4) +
geom_text(aes(label = sprintf("%.0f", risparmio_annuo_mln)),
position = position_dodge(width=0.8), vjust = -0.5, size = 3.4,
color = "#2C2A26", fontface = "bold") +
scale_fill_manual(values = c("#2C5E80", "#B85042", "#D4A056")) +
scale_y_continuous(labels = function(x) formatC(x, big.mark=".", format="d"),
limits = c(0, max(df_proj$risparmio_annuo_mln)*1.18)) +
labs(title = "Mancato esborso annuo del MEF (\"risparmio implicito\") in milioni di EUR",
subtitle = sprintf("Prestazione media pesata: %s EUR/anno (mix reale liquidazioni 2025: %.0f%% assegno ordinario di invalidit\u00e0, %.0f%% pensione di inabilit\u00e0)",
format(round(PRESTAZIONE_MEDIA), big.mark="."), MIX_AOI*100, MIX_INAB*100),
x = "Anno", y = "Mancato esborso annuo (mln EUR)", fill = NULL,
caption = paste0("Importi medi: INPS, Osservatorio statistico Pensioni vigenti e liquidate, ed. 2026 (pensioni previdenziali liquidate nel 2025).\n",
"L'esborso \"evitato\" coincide con il diritto non esercitato. NON e' un risparmio sociale: e' un trasferimento mancato a soggetti che avrebbero avuto titolo.")) +
theme_minimal(base_size = 12) +
theme(plot.title = element_text(face="bold", size=14, color="#2C2A26"),
plot.subtitle = element_text(size=10, color="grey30"),
plot.caption = element_text(size=9, hjust=0, color="grey45"),
legend.position = "top", panel.grid.minor = element_blank())
ggsave(file.path(FIG, "Figura_Italia_Risparmio_Implicito_MEF.png"), fig2,
width = 11, height = 6.2, dpi = 170, bg = "white")
write.csv(df_proj, file.path(OUT, "proiezione_completa_2027_2031.csv"), row.names = FALSE)
cat("\n=== AVVERTENZA METODOLOGICA ===\n")
cat("1. Perimetro: SOLO prestazioni previdenziali (AOI, pensione di\n")
cat("   inabilita'), coerente con il dato CGIL. Restano fuori le\n")
cat("   prestazioni assistenziali (invalidita' civile, accompagnamento):\n")
cat("   la stima e' quindi conservativa per costruzione.\n")
cat("2. Assume estensione uniforme del -13,1% e persistenza nel quinquennio.\n")
cat("3. Non e' un risparmio sociale ma un trasferimento mancato a persone\n")
cat("   che avrebbero avuto titolo di legge a riceverlo.\n")
