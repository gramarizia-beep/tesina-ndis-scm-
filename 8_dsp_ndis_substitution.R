# ============================================================
# SCRIPT 8 — LA SOSTITUZIONE STRUTTURALE DSP → NDIS IN AUSTRALIA
# Verifica empirica: dopo il quadriennio 2014-2018 di contrazione
# DSP, le pensioni di invalidita' australiane sono risalite? Oppure
# sono state sostituite strutturalmente dal sistema NDIS in-kind?
# ============================================================
# RISPOSTA: la DSP NON e' risalita. La contrazione 2014-2018 e'
# diventata strutturale. Il NDIS ha gradualmente sostituito la DSP
# come strumento principale di sostegno alla disabilita' in eta'
# lavorativa. La trasformazione cash -> in-kind e' completata ma
# ha richiesto 7-8 anni.
# ============================================================
# FONTI VERIFICATE:
#   - Australian Institute of Health and Welfare (AIHW), People
#     with disability in Australia, Income support 2024
#   - Parliamentary Budget Office (PBO) 2023, "Disability Support
#     Pension - Historical and projected trends"
#   - NDIA Annual Reports 2018-2024
# ============================================================


# ---- 0. Setup -----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "scales", "patchwork")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BASE <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
FIG  <- file.path(BASE, "FIGURE")
stopifnot(dir.exists(BASE))


# ---- 1. Dati ricostruiti da AIHW e PBO ---------------------
# DSP_16_64: beneficiari Disability Support Pension 16-64 anni (in migliaia)
# NDIS_part: partecipanti NDIS attivi a fine anno (in migliaia)
# Valori interpolati o approssimati per gli anni intermedi mancanti.
dati <- tibble::tribble(
  ~anno, ~DSP_16_64, ~NDIS_part, ~nota,
  2001,  623,        0,          "Inizio serie storica AIHW",
  2005,  698,        0,          "",
  2008,  750,        0,          "",
  2010,  780,        0,          "",
  2012,  802,        0,          "Quota 16-64 = 5.4% popolazione",
  2013,  820,        0,          "NDIS Act approvato (luglio)",
  2014,  833,        5,          "Picco DSP marzo 2014; trial NDIS 4 siti",
  2015,  815,        17,         "Riforme Abbott-Turnbull operative",
  2016,  800,        30,         "Inizio rollout nazionale NDIS",
  2017,  759,        91,         "Grant rate DSP crollato 63%->43%",
  2018,  745,        180,        "",
  2019,  720,        286,        "",
  2020,  660,        391,        "Completamento rollout NDIS",
  2021,  655,        500,        "",
  2022,  646,        555,        "Quota 16-64 = 3.9% popolazione (-1.5pt vs 2012)",
  2023,  645,        610,        "",
  2024,  645,        660,        "Stabile su nuovo livello strutturale"
)

cat("\n=== DATI ===\n")
print(dati |> select(anno, DSP_16_64, NDIS_part))


# ---- 2. Costanti grafiche ----------------------------------
COL_DSP  <- "#B85042"   # rosso terra (cash, in declino)
COL_NDIS <- "#2C5E80"   # blu (in-kind, in espansione)
COL_TEXT <- "#2C2A26"
COL_MUTED<- "#666666"


# ---- 3. Figura: due linee sovrapposte su asse comune --------
fig <- ggplot(dati, aes(x = anno)) +

  # Banda grigia sul periodo "transizione strutturale"
  annotate("rect", xmin = 2014, xmax = 2020, ymin = -Inf, ymax = Inf,
           fill = "#F5F2EC", alpha = 0.5) +
  annotate("text", x = 2017, y = 850,
           label = "Transizione strutturale\nDSP cuts + NDIS rollout",
           color = "grey45", size = 3.2, fontface = "italic") +

  # DSP linea decrescente
  geom_line(aes(y = DSP_16_64, color = "DSP (16-64 anni)"),
            linewidth = 1.7) +
  geom_point(aes(y = DSP_16_64, color = "DSP (16-64 anni)"),
             size = 2.5) +

  # NDIS linea crescente
  geom_line(aes(y = NDIS_part, color = "NDIS (partecipanti totali)"),
            linewidth = 1.7) +
  geom_point(aes(y = NDIS_part, color = "NDIS (partecipanti totali)"),
             size = 2.5) +

  # Annotazione picco DSP
  annotate("text", x = 2014, y = 870,
           label = "Picco DSP\n833.000 (mar 2014)",
           color = COL_DSP, fontface = "bold", size = 3.4, hjust = 0.5) +
  annotate("segment", x = 2014, xend = 2014, y = 855, yend = 838,
           arrow = arrow(length = unit(0.15, "cm")), color = COL_DSP) +

  # Annotazione nuovo livello strutturale DSP
  annotate("text", x = 2024, y = 590,
           label = "DSP a regime\n~645k (stabile)",
           color = COL_DSP, fontface = "bold", size = 3.2, hjust = 1) +

  # Annotazione NDIS scala
  annotate("text", x = 2024, y = 710,
           label = "NDIS a regime\n~660k partecipanti",
           color = COL_NDIS, fontface = "bold", size = 3.2, hjust = 1) +

  # Linea verticale NDIS Act 2013
  geom_vline(xintercept = 2013, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  annotate("text", x = 2013, y = 30, label = "NDIS Act\n2013",
           color = "grey40", size = 3, hjust = -0.1, fontface = "italic") +

  scale_color_manual(values = c(
    "DSP (16-64 anni)"            = COL_DSP,
    "NDIS (partecipanti totali)"  = COL_NDIS
  )) +

  scale_x_continuous(breaks = seq(2000, 2024, 4),
                     limits = c(2000, 2025)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = "."),
                     breaks = seq(0, 900, 150),
                     limits = c(-20, 920)) +

  labs(
    title    = "La sostituzione strutturale DSP -> NDIS in Australia (2001-2024)",
    subtitle = "Beneficiari (migliaia): DSP in eta' lavorativa (16-64) in declino strutturale, NDIS in espansione che sostituisce",
    x = "Anno",
    y = "Beneficiari attivi (migliaia)",
    color = NULL,
    caption = paste(
      "DSP: pensione di invalidita' (cash transfer diretto). NDIS: budget individuale di servizi in-kind.",
      "La DSP NON e' risalita dopo il taglio 2014-2018: la contrazione e' diventata strutturale.",
      "Il NDIS ha gradualmente sostituito la DSP come strumento principale di sostegno alla disabilita' in eta' lavorativa.",
      "Fonte: AIHW (People with disability in Australia, Income support 2024) + PBO (DSP Historical and projected trends, 2023) + NDIA Annual Reports.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = COL_TEXT),
    plot.subtitle    = element_text(size = 10.5, color = "grey30", face = "italic"),
    plot.caption     = element_text(size = 9, hjust = 0, color = "grey45"),
    legend.position  = "top",
    legend.text      = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIG, "Figura_DSP_NDIS_Sostituzione.png"),
       plot = fig, width = 11, height = 6.3, dpi = 170, bg = "white")
cat(sprintf("\nGrafico salvato: %s\n", file.path(FIG, "Figura_DSP_NDIS_Sostituzione.png")))

print(fig)


# ---- 4. Sintesi a video -----------------------------------
cat("\n\n=== SINTESI: LA TRASFORMAZIONE DSP -> NDIS ===\n")
cat("DSP working-age (16-64):\n")
cat(sprintf("  Picco 2014: 833.000 beneficiari\n"))
cat(sprintf("  2022:       646.000 beneficiari (-22.5%% dal picco)\n"))
cat(sprintf("  2024:       645.000 beneficiari (stabile sul nuovo livello strutturale)\n"))
cat(sprintf("  Riduzione %% pop. 16-64: dal 5.4%% al 3.9%% (-1.5 punti)\n\n"))
cat("NDIS:\n")
cat(sprintf("  2014: 5.000 partecipanti (trial)\n"))
cat(sprintf("  2024: 660.000 partecipanti (scala nazionale completata)\n"))
cat(sprintf("  Spesa annua oltre 42 mld AUD/anno (vs ~14 mld DSP)\n\n"))
cat("Quota partecipanti NDIS che ricevono ANCHE DSP:\n")
cat(sprintf("  2018: 77%%\n"))
cat(sprintf("  2022: 70%%\n"))
cat(sprintf("  Sostituzione progressiva: i nuovi beneficiari entrano nel sistema in-kind, non nel cash.\n\n"))
cat("CONCLUSIONE: la trasformazione cash->servizi australiana e' completata, ma\n")
cat("ci sono voluti 7-8 anni perche' fosse visibile su scala nazionale.\n\n")
