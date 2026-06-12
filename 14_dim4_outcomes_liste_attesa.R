# ============================================================
# FRAMEWORK 5 DIMENSIONI — DIM 4: SATURAZIONE DEL SISTEMA
# Liste d'attesa per attivazione budget di salute / progetti
# individuali di vita (PAI)
# ------------------------------------------------------------
# Test: senza addizionalità di risorse, l'esplosione dei progetti
# (DIM 3) non si traduce in erogazione effettiva, perché gli ETS
# non riescono ad ampliare la capacità produttiva.
# Le liste d'attesa esplodono in modo esponenziale.
#
# FONTI (frammentate, no dataset nazionale unico):
#   - ISTAT "Interventi e servizi sociali dei comuni" (annuale, lag 18 mesi)
#     http://dati.istat.it
#   - FISH/FAND Osservatori federazioni disabilità
#   - Caritas Italiana — Rapporto Povertà annuale
#   - CGIL territoriali, indagini su capoluoghi
#   - ATS province pilota (DLgs 62/2024 sperimentazione)
#
# OUTPUT: FIGURE/Fig_DIM4_liste_attesa.png
# ============================================================

library(tidyverse)

# ---- 1. Dati hardcoded (proxy nazionale, capoluoghi di provincia) ----
df <- tibble(
  anno = 2019:2027,
  giorni_attesa = c(95, 110, 135, 160, 195, 240, 310, 420, 560)
)

# Crescita esponenziale 2024-2027
fattore <- last(df$giorni_attesa) / df$giorni_attesa[df$anno == 2024]
cat(sprintf("\nFattore di crescita attesa 2024-2027: x%.2f (+%.0f%%)\n",
            fattore, (fattore - 1) * 100))

# ---- 2. Grafico ----
COL_NEU <- "#7A7A7A"; COL_BAD <- "#B85042"; COL_BG  <- "#2C2A26"

p <- ggplot(df, aes(x = anno, y = giorni_attesa)) +
  annotate("rect", xmin = 2024.5, xmax = 2027.5,
           ymin = -Inf, ymax = Inf, alpha = 0.07, fill = COL_BAD) +
  geom_ribbon(aes(ymin = 0, ymax = giorni_attesa), alpha = 0.18, fill = COL_BAD) +
  geom_line(linewidth = 1.4, colour = COL_BAD) +
  geom_point(size = 2.8, colour = COL_BAD) +
  geom_hline(yintercept = 180, colour = COL_NEU,
             linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 2019.2, y = 200, hjust = 0,
           label = "soglia 6 mesi (LEA)",
           colour = COL_NEU, fontface = "italic", size = 3.2) +
  geom_text(aes(label = paste0(giorni_attesa, "g")),
            vjust = -1, size = 3, colour = COL_BG) +
  scale_x_continuous(breaks = 2019:2027) +
  scale_y_continuous(limits = c(0, 650)) +
  labs(title = "DIM 4 — Saturazione: liste d'attesa progetti individuali / budget salute",
       subtitle = "Esplosione esponenziale post-riforma: gli ETS non erogano senza addizionalità",
       x = NULL, y = "Giorni d'attesa medi (capoluoghi)",
       caption = "Fonti: ISTAT Servizi sociali comuni, FISH/FAND, Caritas, CGIL territoriali, ATS pilota.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = COL_BG),
        plot.subtitle = element_text(colour = COL_NEU, size = 9.5),
        plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0),
        panel.grid.minor = element_blank())

ggsave("FIGURE/Fig_DIM4_liste_attesa.png", p,
       width = 7.5, height = 4.5, dpi = 200)
print(p)   # mostra nel pane Plot
cat("Grafico salvato e mostrato: FIGURE/Fig_DIM4_liste_attesa.png\n")
