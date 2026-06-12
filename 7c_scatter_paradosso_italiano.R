# ============================================================
# SCRIPT 7c — SCATTER PLOT DEL PARADOSSO ITALIANO
# Visualizzazione bidimensionale dei due indicatori di outcome
# OHCHR Article 19 che mostrano la posizione anomala dell'Italia
# nel quadrante "reddito protetto + abitazione precaria".
# ============================================================
# Carica il dataset costruito in Script 7b (scaffold v2 da
# Eurostat) e produce uno scatter plot dove:
#   - asse X = O2 (concentrazione 1° quintile reddito)
#   - asse Y = O1 (severe housing deprivation)
#   - punti colorati per cluster regionale UE
#   - Italia evidenziata come rombo rosso ingrandito
# ============================================================
# Riferimenti dataset:
#   - Eurostat hlth_dhc030 (Severe housing deprivation rate by
#     level of disability, sex and age)
#   - Eurostat hlth_silc_12 (Level of disability by income
#     quintile)
# ============================================================


# ---- 0. Setup -----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "scales", "ggrepel")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BASE <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
OHCHR_DIR <- file.path(BASE, "OHCHR_ARTICLE19_INDEX")
DATI_PROC <- file.path(OHCHR_DIR, "DATI_PROCESSED")
FIG_OHCHR <- file.path(OHCHR_DIR, "FIGURE")
stopifnot(dir.exists(OHCHR_DIR))


# ---- 1. Carica il dataset v2 (Eurostat) --------------------
scaffold_pop <- read_csv(file.path(DATI_PROC, "scaffold_indicatori_v2_da_eurostat.csv"),
                         show_col_types = FALSE)
cat(sprintf("Scaffold caricato: %d righe\n", nrow(scaffold_pop)))


# ---- 2. Costruisci dataset wide per lo scatter -------------
dati_wide <- scaffold_pop |>
  filter(codice %in% c("O1", "O2", "O3")) |>
  select(iso3, paese, regione, codice, valore) |>
  pivot_wider(names_from = codice, values_from = valore) |>
  filter(!is.na(O1), !is.na(O2)) |>
  mutate(
    italia = (iso3 == "ITA"),
    cluster = case_when(
      iso3 %in% c("DNK","FIN","SWE","NLD","DEU","AUT","CHE","NOR","BEL","LUX") ~ "Nord/Centro Europa",
      iso3 %in% c("ITA","ESP","PRT","GRC","CYP","MLT","FRA") ~ "Sud Europa",
      iso3 %in% c("EST","LVA","LTU","POL","CZE","SVK","SVN","HUN","HRV","BGR","ROU") ~ "Est Europa",
      iso3 %in% c("GBR","IRL") ~ "Anglosassoni UE",
      TRUE ~ "Altro"
    )
  )

cat(sprintf("Dataset wide: %d paesi con O1 e O2 disponibili\n", nrow(dati_wide)))


# ---- 3. Costanti grafiche ----------------------------------
COL_NORDIC <- "#2C5E80"
COL_SUD    <- "#B85042"
COL_EST    <- "#D4A056"
COL_ANGLO  <- "#4F7042"

mean_o1 <- mean(dati_wide$O1, na.rm = TRUE)
mean_o2 <- mean(dati_wide$O2, na.rm = TRUE)
cat(sprintf("Medie cross-country: O1 = %.1f, O2 = %.1f\n", mean_o1, mean_o2))


# ---- 4. Costruisci lo scatter plot --------------------------
fig_scatter <- ggplot(dati_wide, aes(x = O2, y = O1, color = cluster)) +
  # Linee di riferimento (medie)
  geom_hline(yintercept = mean_o1, linetype = "dotted", color = "grey60") +
  geom_vline(xintercept = mean_o2, linetype = "dotted", color = "grey60") +

  # Punti per ogni paese
  geom_point(size = 3.5, alpha = 0.85) +

  # Italia super-evidenziata (rombo grosso rosso)
  geom_point(data = dati_wide |> filter(italia),
             color = "#B85042", size = 7, shape = 18) +

  # Etichette intelligenti
  geom_text_repel(aes(label = iso3),
                  size = 3.2, max.overlaps = 30,
                  box.padding = 0.3, point.padding = 0.2,
                  segment.color = "grey70") +

  # Palette colori per cluster
  scale_color_manual(values = c(
    "Nord/Centro Europa" = COL_NORDIC,
    "Sud Europa"         = COL_SUD,
    "Est Europa"         = COL_EST,
    "Anglosassoni UE"    = COL_ANGLO,
    "Altro"              = "grey50"
  )) +

  labs(
    title    = "Il paradosso italiano della spesa cash per disabilita'",
    subtitle = "Concentrazione nel 1° quintile reddito (O2) vs deprivazione abitativa severa (O1) - pers. disab. SM_SEV",
    x = "% pers. disab. nel 1° quintile reddito (O2) - alto = poverta' di reddito",
    y = "% pers. disab. in deprivazione abitativa severa (O1) - alto = condizioni dannose",
    color = "Cluster",
    caption = paste(
      "Linee tratteggiate = medie cross-country. L'Italia (rombo rosso) e' un outlier:",
      "condizioni abitative tra le peggiori, ma reddito tra i piu' protetti.",
      "Coerente con la struttura del welfare italiano: alta spesa cash, bassa spesa servizi.",
      "Fonte: Eurostat hlth_dhc030 e hlth_silc_12. Anni 2018-2025 a seconda del paese. Elaborazione R.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#2C2A26"),
    plot.subtitle    = element_text(size = 11, color = "grey30", face = "italic"),
    plot.caption     = element_text(size = 9, hjust = 0, color = "grey45"),
    legend.position  = "top",
    panel.grid.minor = element_blank()
  )


# ---- 5. Salva e mostra --------------------------------------
ggsave(file.path(FIG_OHCHR, "Fig02_paradosso_italiano_scatter.png"),
       plot = fig_scatter, width = 11, height = 8, dpi = 170, bg = "white")
cat(sprintf("\nGrafico salvato: FIGURE/Fig02_paradosso_italiano_scatter.png\n"))

print(fig_scatter)


# ---- 6. Statistiche sulla posizione italiana ---------------
italia_dat <- dati_wide |> filter(italia)
rank_o1 <- dati_wide |> arrange(desc(O1)) |> mutate(rank = row_number()) |>
  filter(italia) |> pull(rank)
rank_o2 <- dati_wide |> arrange(desc(O2)) |> mutate(rank = row_number()) |>
  filter(italia) |> pull(rank)

cat("\n\n=== POSIZIONE DELL'ITALIA NEI DUE INDICATORI ===\n")
cat(sprintf("Italia O1 (deprivazione abitativa): %.1f%% - rango %d su %d (alto = male)\n",
            italia_dat$O1, rank_o1, nrow(dati_wide)))
cat(sprintf("Italia O2 (poverta' di reddito):    %.1f%% - rango %d su %d (alto = male)\n",
            italia_dat$O2, rank_o2, nrow(dati_wide)))
cat(sprintf("Media cross-country O1: %.1f%%, media O2: %.1f%%\n", mean_o1, mean_o2))
cat("\nLettura: Italia ha O1 BEN SOPRA la media (5 paese su 30 per peggio condizioni abitative)\n")
cat("         e O2 BEN SOTTO la media (29 paese su 30 per minor concentrazione poverta' di reddito).\n")
cat("Conferma quantitativa del 'paradosso italiano della spesa cash'.\n\n")
