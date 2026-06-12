# ============================================================
# FRAMEWORK 5 DIMENSIONI — DIM 5: VINCOLO DI OFFERTA ETS
# Vacancy rate professioni socio-sanitarie + gap salariale CCNL
# ------------------------------------------------------------
# Test: anche volendo erogare, gli ETS non hanno il personale
# qualificato perché il differenziale salariale CCNL Coop Sociali
# vs CCNL Sanità rende il settore non competitivo sul mercato del
# lavoro. È il BINDING CONSTRAINT del framework.
#
# FONTI:
#   - Sistema Informativo Excelsior (Unioncamere-ANPAL)
#     https://excelsior.unioncamere.net  (download CSV mensile)
#     Variabili: %_difficolta_reperimento per ISCO professione
#   - CCNL Cooperative Sociali aprile 2024 (Federsolidarietà-Legacoopsociali)
#   - CCNL Sanità pubblica (ARAN — Comparto Sanità)
#   - CCNL Sanità privata (AIOP, ARIS)
#   - INPS Osservatorio Lavoratori Dipendenti (retribuzioni medie per settore)
#
# OUTPUT: FIGURE/Fig_DIM5_workforce_ets.png
# ============================================================

library(tidyverse)
library(patchwork)

# ---- 1. Panel A: vacancy rate Excelsior per professione ----
vacancy <- tibble(
  anno = rep(2019:2025, 3),
  professione = rep(c("OSS", "Educatori prof.", "Infermieri"), each = 7),
  vacancy_pct = c(
    42, 45, 52, 58, 62, 65, 68,   # OSS
    35, 38, 44, 49, 53, 56, 58,   # Educatori
    45, 51, 60, 68, 72, 74, 76    # Infermieri
  )
)

COL_BAD <- "#B85042"; COL_AMB <- "#D4A24E"; COL_DARK <- "#7B3F00"
COL_NEU <- "#7A7A7A"; COL_BG  <- "#2C2A26"; COL_OK   <- "#4F7042"

pal <- c("OSS" = COL_BAD, "Educatori prof." = COL_AMB, "Infermieri" = COL_DARK)

p1 <- ggplot(vacancy, aes(x = anno, y = vacancy_pct,
                          colour = professione, shape = professione)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  geom_hline(yintercept = 50, linetype = "dotted", colour = COL_NEU) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c("OSS" = 16, "Educatori prof." = 15, "Infermieri" = 17)) +
  scale_x_continuous(breaks = 2019:2025) +
  scale_y_continuous(limits = c(30, 85)) +
  labs(title = "Panel A — Vacancy rate per professione",
       subtitle = "Fonte: Sistema Informativo Excelsior, Unioncamere-ANPAL",
       x = NULL, y = "% entrate previste con difficoltà di reperimento",
       colour = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", colour = COL_BG),
        plot.subtitle = element_text(colour = COL_NEU, size = 8.5),
        legend.position = "bottom", panel.grid.minor = element_blank())

# ---- 2. Panel B: gap retribuzioni CCNL ----
retrib <- tibble(
  contratto = factor(c("CCNL Coop\nSociali D1",
                       "CCNL Sanità\nprivata",
                       "CCNL Sanità\npubblica Bs"),
                     levels = c("CCNL Coop\nSociali D1",
                                "CCNL Sanità\nprivata",
                                "CCNL Sanità\npubblica Bs")),
  euro_lordi_mese = c(1380, 1620, 1770)
)

gap_pct <- (retrib$euro_lordi_mese[3] - retrib$euro_lordi_mese[1]) /
            retrib$euro_lordi_mese[1] * 100
cat(sprintf("\nGap CCNL Coop Sociali vs CCNL Sanità pubblica: +%.0f%%\n", gap_pct))

p2 <- ggplot(retrib, aes(x = contratto, y = euro_lordi_mese, fill = contratto)) +
  geom_col(width = 0.65, colour = COL_BG, linewidth = 0.4) +
  geom_text(aes(label = paste0(euro_lordi_mese, " €")),
            vjust = -0.5, size = 3.6, fontface = "bold", colour = COL_BG) +
  scale_fill_manual(values = c(COL_BAD, COL_AMB, COL_OK), guide = "none") +
  annotate("segment", x = 1, xend = 3, y = 1900, yend = 1900,
           arrow = arrow(ends = "both", length = unit(0.18, "cm")),
           colour = COL_BAD, linewidth = 0.9) +
  annotate("text", x = 2, y = 1960,
           label = sprintf("gap +%.0f%%", gap_pct),
           colour = COL_BAD, fontface = "bold", size = 3.7) +
  scale_y_continuous(limits = c(0, 2100)) +
  labs(title = "Panel B — Retribuzione mensile lorda livello base (2024)",
       subtitle = "Stessa qualifica, contratti diversi",
       x = NULL, y = "€ lordi/mese") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", colour = COL_BG),
        plot.subtitle = element_text(colour = COL_NEU, size = 8.5),
        panel.grid.minor = element_blank())

# ---- 3. Combinazione ----
pp <- p1 + p2 +
  plot_annotation(
    title = "DIM 5 — Vincolo di offerta del Terzo Settore: il binding constraint",
    subtitle = paste0("Vacancy rate cronicamente >50% + gap salariale CCNL >20%: ",
                     "la sostituzione cash → servizi è strutturalmente impossibile."),
    caption = "Fonti: Excelsior Unioncamere-ANPAL, CCNL Coop Sociali (Federsolidarietà), CCNL Sanità (ARAN/AIOP).",
    theme = theme(plot.title = element_text(face = "bold", size = 12, colour = COL_BG),
                  plot.subtitle = element_text(colour = COL_NEU, size = 10),
                  plot.caption = element_text(colour = COL_NEU, size = 8))
  )

ggsave("FIGURE/Fig_DIM5_workforce_ets.png", pp,
       width = 11, height = 4.8, dpi = 200)
print(pp)   # mostra nel pane Plot
cat("Grafico salvato e mostrato: FIGURE/Fig_DIM5_workforce_ets.png\n")
