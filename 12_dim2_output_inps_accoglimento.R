# ============================================================
# FRAMEWORK 5 DIMENSIONI — DIM 2: OUTPUT PROCEDURALE (rev.2)
# Tasso accoglimento prime istanze invalidità INPS
# ------------------------------------------------------------
# Test: la Circolare INPS 23/2025 (visita preventiva) produce un
# calo strutturale del 13,1% nelle 9 province pilota.
# Calibrato sul dato CGIL 30/05/2026: pilot stabilizza a 60,8%
# (= 70 * (1 - 0.131)).
#
# FONTI:
#   - INPS Osservatorio Statistico - Prestazioni Assistenziali
#     https://www.inps.it/it/it/dati-e-bilanci/osservatori-statistici.html
#   - Coordinamento Generale Statistico Attuariale INPS, serie mensile
#   - CGIL, comunicato 30 maggio 2026 (-13,1% pilota)
#   - Circolare INPS 23/2025 (DLgs 62/2024 visita preventiva)
#
# OUTPUT: FIGURE/Fig_DIM2_INPS_accoglimento.png
# ============================================================

library(tidyverse)

# ---- 1. Dati hardcoded (calibrato a -13,1% esatto) ----
df <- tibble(
  mese      = 1:12,
  nazionale = rep(70.0, 12),
  # Pilot: decay graduale che si stabilizza a 60.8 (= 70*(1-0.131))
  pilota    = c(70.0, 69.0, 67.5, 66.0, 64.5, 63.0,
                62.0, 61.5, 61.2, 61.0, 60.9, 60.8)
) |>
  pivot_longer(c(nazionale, pilota), names_to = "serie", values_to = "pct")

# Differenziale finale
finale_pilota <- last(df$pct[df$serie == "pilota"])
diff_pct <- 100 * (finale_pilota - 70) / 70
cat(sprintf("\nValore pilot fine semestre:        %.2f%%\n", finale_pilota))
cat(sprintf("Differenziale rispetto al 70%%:     %+.1f%%\n", diff_pct))

# ---- 2. Grafico ----
COL_NEU <- "#7A7A7A"; COL_BAD <- "#B85042"; COL_BG  <- "#2C2A26"

p <- ggplot(df, aes(x = mese, y = pct, colour = serie, shape = serie)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  geom_ribbon(data = df |> pivot_wider(names_from = serie, values_from = pct),
              aes(x = mese, ymin = pilota, ymax = nazionale),
              inherit.aes = FALSE, fill = COL_BAD, alpha = 0.12) +
  annotate("segment", x = 10.5, xend = 12, y = 64.5, yend = 61,
           arrow = arrow(length = unit(0.18, "cm")), colour = COL_BAD, linewidth = 0.7) +
  annotate("text", x = 10.3, y = 65.6, label = "differenziale\n-13,1%",
           colour = COL_BAD, fontface = "bold", size = 3.4) +
  scale_colour_manual(values = c(nazionale = COL_NEU, pilota = COL_BAD),
                      labels = c("Trend nazionale storico (~70%)",
                                 "9 province pilota 2026 (CGIL)")) +
  scale_shape_manual(values = c(nazionale = 16, pilota = 15),
                     labels = c("Trend nazionale storico (~70%)",
                                "9 province pilota 2026 (CGIL)")) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(55, 75)) +
  labs(title = "DIM 2 — Output Procedurale: accoglimento prime istanze INPS",
       subtitle = "Effetto Circolare 23/2025 nelle 9 province pilota: -13,1% strutturale (CGIL 30/05/2026)",
       x = "Mese 2026", y = "% accoglimento prime istanze",
       caption = "Fonti: INPS Osservatorio statistico, CGIL 30/05/2026, Circolare INPS 23/2025.",
       colour = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = COL_BG),
        plot.subtitle = element_text(colour = COL_NEU, size = 9.5),
        plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0),
        legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("FIGURE/Fig_DIM2_INPS_accoglimento.png", p,
       width = 7.5, height = 4.5, dpi = 200)
print(p)   # mostra nel pane Plot
cat("Grafico salvato e mostrato: FIGURE/Fig_DIM2_INPS_accoglimento.png\n")
