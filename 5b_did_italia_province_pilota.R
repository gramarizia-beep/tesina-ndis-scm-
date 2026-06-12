# ============================================================
# Script 5b — DiD Italia: province pilota vs province di controllo
# ============================================================
# Riproduce la Figura della tesi (cap. 8.1) a partire dai dati
# pubblici CGIL del 30 maggio 2026 sulle domande di invalidita'
# e inabilita' PREVIDENZIALE nelle 9 province pilota del
# D.Lgs. 62/2024 (Brescia, Catanzaro, Firenze, Forli'-Cesena,
# Frosinone, Perugia, Salerno, Sassari, Trieste).
#
# DATI INPUT (CGIL - Osservatorio Previdenza, comunicato 30/05/2026;
# ripreso da Il Sole 24 Ore, Ansa, Il Fatto Quotidiano):
#   - Pilota:    20.578 (2024) -> 17.881 (2025)   = -13,1%
#   - Controllo (resto Italia):                     +1,0%
#   - DiD: (-13,1) - (+1,0)                       = -14,1 punti
#
# Nessun arrotondamento: -13,1 e -14,1 escono esatti dai dati.
# ============================================================
library(tidyverse)
library(scales)
BASE <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
FIG  <- file.path(BASE, "FIGURE")
if (!dir.exists(FIG)) dir.create(FIG, recursive = TRUE)
COL_NEU <- "#7A7A7A"
COL_BAD <- "#B85042"
COL_GRY <- "#C9C5BE"
COL_BG  <- "#2C2A26"
# ---- 1. Dati ufficiali CGIL (30/05/2026) -------------------
pilota_T0 <- 20578   # domande previdenziali 2024, 9 province pilota
pilota_T1 <- 17881   # domande previdenziali 2025, 9 province pilota
crescita_ctrl_pct <- 1.0   # variazione % province di controllo
calo_pilota_pct <- (pilota_T1 - pilota_T0) / pilota_T0 * 100   # -13.108 -> -13,1
did_pct         <- calo_pilota_pct - crescita_ctrl_pct         # -14.108 -> -14,1
# Helper: numeri in formato italiano
fmt_it <- function(x, dec = 1) {
  formatC(x, format = "f", digits = dec, big.mark = ".", decimal.mark = ",")
}
# ---- 2. Output di controllo a console ----------------------
cat(sprintf("Province pilota: %s -> %s   variazione: %s%%\n",
            format(pilota_T0, big.mark = "."),
            format(pilota_T1, big.mark = "."),
            fmt_it(calo_pilota_pct)))
cat(sprintf("Province controllo (resto Italia):   variazione: +%s%%\n",
            fmt_it(crescita_ctrl_pct)))
cat(sprintf("DiD (differenziale netto):           %s punti percentuali\n",
            fmt_it(did_pct)))
# ---- 3. Dataset per il grafico (indice base 100) -----------
# Stessa struttura visiva della figura in tesi: quattro barre,
# baseline = 100 per entrambi i gruppi.
df <- tibble(
  gruppo  = rep(c("Province di controllo\n(resto Italia)",
                  "Province pilota\n(9 province D.Lgs. 62/2024)"), each = 2),
  periodo = rep(c("2024 (baseline = 100)", "2025"), times = 2),
  indice  = c(100, 100 + crescita_ctrl_pct,
              100, 100 + calo_pilota_pct)
) |>
  mutate(
    gruppo  = factor(gruppo, levels = unique(gruppo)),
    periodo = factor(periodo, levels = unique(periodo)),
    fill    = case_when(
      periodo == "2024 (baseline = 100)" ~ COL_GRY,
      gruppo  == levels(gruppo)[1]       ~ COL_NEU,
      TRUE                               ~ COL_BAD
    )
  )
etichette <- tibble(
  gruppo = factor(levels(df$gruppo), levels = levels(df$gruppo)),
  x      = c(1, 2),
  y      = c(100 + crescita_ctrl_pct, 100 + calo_pilota_pct),
  lab    = c(sprintf("+%s%%", fmt_it(crescita_ctrl_pct)),
             sprintf("%s%%",  fmt_it(calo_pilota_pct))),
  col    = c(COL_NEU, COL_BAD)
)
# ---- 4. Grafico ---------------------------------------------
p <- ggplot(df, aes(x = gruppo, y = indice, fill = fill, group = periodo)) +
  geom_col(position = position_dodge(width = 0.62), width = 0.55,
           colour = "white", linewidth = 0.4) +
  geom_text(aes(label = fmt_it(indice)),
            position = position_dodge(width = 0.62),
            vjust = -0.55, size = 3.4, colour = COL_BG) +
  geom_text(data = etichette, inherit.aes = FALSE,
            aes(x = x, y = y + 6.5, label = lab, colour = col),
            fontface = "bold", size = 5.0) +
  annotate("label", x = 1.5, y = 86,
           label = sprintf("Differenziale stile DiD (trattamento - controllo) = %s punti percentuali",
                           fmt_it(did_pct)),
           colour = COL_BAD, fontface = "bold", size = 3.6,
           label.size = 0.3, fill = "white") +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_cartesian(ylim = c(80, 110)) +
  labs(
    title    = "Riforma disabilita': calo delle domande di invalidita' previdenziale nelle 9 province pilota",
    subtitle = "Indice 100 = domande di invalidita' e inabilita' previdenziale presentate all'INPS nel 2024; confronto con il 2025",
    x = NULL,
    y = "Indice (base 2024 = 100)",
    caption = paste0(
      "Fonte: elaborazione su dati CGIL - Osservatorio Previdenza, 30 maggio 2026 ",
      "(ripresi da Il Sole 24 Ore, Ansa, Il Fatto Quotidiano).\n",
      "Province pilota (D.Lgs. 62/2024): Brescia, Catanzaro, Firenze, Forli'-Cesena, ",
      "Frosinone, Perugia, Salerno, Sassari, Trieste.\n",
      "Domande previdenziali nelle province pilota: 20.578 (2024) -> 17.881 (2025), -13,1%; ",
      "province di controllo: +1,0%."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12.5, colour = COL_BG),
    plot.subtitle      = element_text(colour = COL_NEU, size = 9.5),
    plot.caption       = element_text(colour = COL_NEU, size = 8, hjust = 0),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )
ggsave(file.path(FIG, "Figura_Italia_DiD_Province_Pilota.png"), p,
       width = 8.5, height = 5.4, dpi = 200, bg = "white")
print(p)
cat(sprintf("\nFigura salvata: %s\n",
            file.path(FIG, "Figura_Italia_DiD_Province_Pilota.png")))
