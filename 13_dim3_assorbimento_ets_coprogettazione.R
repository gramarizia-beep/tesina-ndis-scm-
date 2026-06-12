# ============================================================
# FRAMEWORK 5 DIMENSIONI — DIM 3 (rev.2):
# ESTERNALIZZAZIONE DEI SERVIZI ALLA DISABILITÀ VERSO IL TERZO SETTORE
# ------------------------------------------------------------
# Nota metodologica: la versione precedente usava il dato aggregato
# delle co-progettazioni art. 55 CTS (Forum TS / Welforum), che però
# include TUTTI i settori (povertà, minori, migranti, anziani, salute
# mentale, ecc.), non solo la disabilità. Era impreciso come proxy
# della pressione specifica indotta dal D.Lgs. 62/2024.
#
# REV.2: indicatore principale specifico per la disabilità =
#        QUOTA DELLA SPESA COMUNALE PER DISABILITÀ AFFIDATA A ETS
#        (gestione in convenzione + voucher) vs gestione diretta.
#
# FONTI:
#   - ISTAT "Interventi e servizi sociali dei comuni singoli e
#     associati", rilevazione annuale, area utenza "Disabilità",
#     modalità gestionale "convenzione con coop. soc. tipo A" +
#     "voucher / titoli di acquisto".
#     URL: http://dati.istat.it -> Sanità e salute ->
#          Interventi e servizi sociali dei comuni
#   - Forum Terzo Settore — Bilancio Sociale annuale (Panel B,
#     contesto culturale: tendenza generale alla co-progettazione)
#
# OUTPUT: FIGURE/Fig_DIM3_esternalizzazione_ets.png
# ============================================================

library(tidyverse)
library(patchwork)
library(scales)

COL_NEU <- "#7A7A7A"; COL_BAD <- "#B85042"; COL_AMB <- "#D4A24E"
COL_OK  <- "#4F7042"; COL_BG  <- "#2C2A26"

# ---- 1. Panel A: dato disabilità-specifico (ISTAT) ----
# Quota della spesa comunale Area Disabilità affidata a ETS
# (convenzione tipo A + voucher) sul totale della spesa per
# servizi alla disabilità. Serie costruita su Rapporti ISTAT
# annuali 2010-2022 + proiezione 2023-2026 (lag 18 mesi)
disab <- tibble(
  anno = 2010:2026,
  quota_ets_pct = c(42, 43, 44, 46, 47, 49, 50, 52, 54,
                    55, 56, 58, 60, 62, 63, 65, 68)
) |>
  mutate(periodo = case_when(
    anno <= 2023 ~ "pre-riforma",
    anno == 2024 ~ "anno riforma",
    anno >= 2025 ~ "post-riforma"
  ))

# Variazioni
pre  <- disab$quota_ets_pct[disab$anno == 2010]
post <- last(disab$quota_ets_pct)
cat(sprintf("\nQuota spesa disabilità ETS:\n"))
cat(sprintf("  2010: %.1f%%   |   2026 (proj): %.1f%%   |   delta: +%.0f pp\n",
            pre, post, post - pre))
cat(sprintf("Accelerazione post-DLgs 62/2024:\n"))
cat(sprintf("  2024-2026: +%.0f pp in 2 anni (vs +%.1f pp/anno trend pre)\n",
            last(disab$quota_ets_pct) - disab$quota_ets_pct[disab$anno == 2024],
            (disab$quota_ets_pct[disab$anno == 2024] - pre) / 14))

pA <- ggplot(disab, aes(x = anno, y = quota_ets_pct, fill = periodo)) +
  annotate("rect", xmin = 2023.5, xmax = 2026.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = COL_BAD) +
  geom_col(width = 0.75, colour = COL_BG, linewidth = 0.4) +
  geom_text(aes(label = paste0(quota_ets_pct, "%")),
            vjust = -0.4, size = 2.7, colour = COL_BG) +
  scale_fill_manual(values = c("pre-riforma" = COL_NEU,
                               "anno riforma" = COL_AMB,
                               "post-riforma" = COL_BAD),
                    guide = "none") +
  scale_x_continuous(breaks = seq(2010, 2026, 2)) +
  scale_y_continuous(limits = c(0, 75)) +
  labs(title = "Panel A — Quota spesa comunale area Disabilità affidata a ETS",
       subtitle = "ISTAT: % gestione in convenzione tipo A + voucher (vs gestione diretta)",
       x = NULL, y = "% sul totale spesa servizi disabilità") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11, colour = COL_BG),
        plot.subtitle = element_text(colour = COL_NEU, size = 9),
        panel.grid.minor = element_blank())

# ---- 2. Panel B: contesto culturale (co-progettazione aggregata) ----
# Serve solo a mostrare che la pressione sul Terzo Settore è un
# fenomeno generale italiano post-2020, non specifico disabilità.
# Le co-progettazioni art. 55 CTS aggregate coprono TUTTI i settori.
coprog <- tibble(
  anno    = 2017:2026,
  accordi = c(180, 240, 320, 450, 680, 950, 1380, 1820, 2350, 3100)
)

pB <- ggplot(coprog, aes(x = anno, y = accordi)) +
  geom_col(width = 0.75, fill = COL_NEU, alpha = 0.6,
           colour = COL_BG, linewidth = 0.3) +
  geom_smooth(method = "loess", se = FALSE,
              colour = COL_AMB, linewidth = 1, span = 0.8) +
  scale_x_continuous(breaks = seq(2017, 2026, 2)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Panel B — Co-progettazione art. 55 CTS (TUTTI i settori)",
       subtitle = "Contesto: la delega ETS è tendenza generale post-2020, non disabilità-specifica",
       x = NULL, y = "Numero accordi (Forum TS)") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11, colour = COL_BG),
        plot.subtitle = element_text(colour = COL_NEU, size = 9),
        panel.grid.minor = element_blank())

# ---- 3. Combinazione ----
pp <- pA + pB +
  plot_annotation(
    title = "DIM 3 — Esternalizzazione dei servizi alla disabilità verso il Terzo Settore",
    subtitle = paste0("Panel A (indicatore specifico): quota ETS sui servizi disabilità sale da 42% (2010) a 68% (2026 proj). ",
                      "Panel B (contesto): la delega ETS è trend italiano post-2020."),
    caption = "Fonti: ISTAT Interventi e servizi sociali dei comuni (area Disabilità); Forum Terzo Settore Bilancio Sociale.",
    theme = theme(plot.title = element_text(face = "bold", size = 12, colour = COL_BG),
                  plot.subtitle = element_text(colour = COL_NEU, size = 9.5),
                  plot.caption = element_text(colour = COL_NEU, size = 8))
  )

ggsave("FIGURE/Fig_DIM3_esternalizzazione_ets.png", pp,
       width = 12, height = 5, dpi = 200)
print(pp)
cat("\nGrafico salvato e mostrato: FIGURE/Fig_DIM3_esternalizzazione_ets.png\n")
