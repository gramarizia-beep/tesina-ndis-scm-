# ============================================================
# SCRIPT 3 — IL CONTROLLO NEGATIVO SIMMETRICO
# Visualizzazione della simmetria AUS (espansione NDIS) vs
# FIN (consolidamento fiscale): due rotture di segno opposto
# nel decennio 2013-2021, identificate dal SCM in modo coerente.
# ============================================================
# Il grafico mostra il "gap" (paese reale − sintetico) per
# Australia e Finlandia sovrapposti.
#   - Linea rossa = Australia: gap positivo crescente (NDIS espande)
#   - Linea blu  = Finlandia: gap negativo crescente (consolidamento)
# Le due rotture sono praticamente speculari intorno allo zero.
# ============================================================

library(tidyverse)

# ---- 1. Carica il workspace finale -------------------------
load("~/Downloads/TESINA_NDIS_AUSTRALIA/R_WORKSPACE/robust_usdppp_finale.RData")

# Verifica disponibilità degli oggetti
stopifnot(exists("placebo"))
stopifnot("AUS" %in% names(placebo))
stopifnot("FIN" %in% names(placebo))

aus <- placebo$AUS
fin <- placebo$FIN

# Diagnostica numerica
cat("\n=== NUMERI CHIAVE PER LA FORMALIZZAZIONE METODOLOGICA ===\n")
cat(sprintf("Australia:  pre-MSPE = %.1f  post-MSPE = %.1f  ratio = %.1f\n",
            aus$mspe_pre, aus$mspe_post, aus$mspe_post / aus$mspe_pre))
cat(sprintf("            ATT %d (livello) = %+.1f USD PPP/persona  (%+.1f%%)\n",
            aus$last_year, aus$att_last, aus$att_last_pct))
cat(sprintf("Finlandia:  pre-MSPE = %.1f  post-MSPE = %.1f  ratio = %.1f\n",
            fin$mspe_pre, fin$mspe_post, fin$mspe_post / fin$mspe_pre))
cat(sprintf("            ATT %d (livello) = %+.1f USD PPP/persona  (%+.1f%%)\n",
            fin$last_year, fin$att_last, fin$att_last_pct))

# Verifica della simmetria
ratio_aus <- aus$mspe_post / aus$mspe_pre
ratio_fin <- fin$mspe_post / fin$mspe_pre
cat(sprintf("\nSimmetria dei ratio MSPE post/pre: AUS=%.0f  FIN=%.0f  rapporto=%.2f\n",
            ratio_aus, ratio_fin, ratio_aus / ratio_fin))
cat(sprintf("Simmetria degli ATT in valore assoluto: |AUS|=%.0f  |FIN|=%.0f  rapporto=%.2f\n",
            abs(aus$att_last), abs(fin$att_last),
            abs(aus$att_last) / abs(fin$att_last)))


# ---- 2. Costruisci dataframe per ggplot --------------------
df <- bind_rows(
  tibble(anno = aus$years, gap = aus$gap,
         paese = "Australia — NDIS (espansione)"),
  tibble(anno = fin$years, gap = fin$gap,
         paese = "Finlandia — consolidamento (contrazione)")
)


# ---- 3. Grafico simmetrico ---------------------------------
fig <- ggplot(df, aes(x = anno, y = gap, color = paese, shape = paese)) +
  # Bande di sfondo: pre e post-trattamento
  annotate("rect", xmin = 2000, xmax = 2013, ymin = -Inf, ymax = Inf,
           fill = "#F5F2EC", alpha = 0.6) +
  annotate("rect", xmin = 2013, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "#FFFFFF", alpha = 0) +
  annotate("text", x = 2006.5, y = 720, label = "Pre-trattamento (sovrapposizione)",
           size = 3.4, fontface = "italic", color = "grey45") +
  annotate("text", x = 2017, y = 720, label = "Post-trattamento (divergenza simmetrica)",
           size = 3.4, fontface = "italic", color = "grey45") +
  # Linea dello zero (= sintetico)
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  # Linea verticale del trattamento 2013
  geom_vline(xintercept = 2013, linetype = "dashed",
             color = "grey45", linewidth = 0.6) +
  # Linee dei due paesi
  geom_line(linewidth = 1.7) +
  geom_point(size = 2.8) +
  # Annotazioni terminali sul gap del 2021
  annotate("text", x = 2021.5, y = aus$gap[length(aus$gap)],
           label = sprintf("+%.0f", aus$att_last),
           color = "#C0392B", size = 4.5, hjust = 0, fontface = "bold") +
  annotate("text", x = 2021.5, y = fin$gap[length(fin$gap)],
           label = sprintf("%.0f", fin$att_last),
           color = "#2C5E80", size = 4.5, hjust = 0, fontface = "bold") +
  # Colori e simboli
  scale_color_manual(values = c(
    "Australia — NDIS (espansione)" = "#C0392B",
    "Finlandia — consolidamento (contrazione)" = "#2C5E80"
  )) +
  scale_shape_manual(values = c(
    "Australia — NDIS (espansione)" = 16,
    "Finlandia — consolidamento (contrazione)" = 17
  )) +
  scale_x_continuous(breaks = seq(2000, 2021, 3), limits = c(2000, 2022.5)) +
  labs(
    title    = "Il controllo negativo simmetrico",
    subtitle = "Gap rispetto al sintetico — Australia (NDIS) vs Finlandia (consolidamento), 2000-2021",
    x = "Anno",
    y = "Differenza paese reale − sintetico (USD PPP per persona)",
    color = NULL, shape = NULL,
    caption = paste0(
      "Il decennio 2013-2021 ha visto due politiche pubbliche di disabilità di segno opposto, ",
      "identificate dal SCM in modo coerente.\n",
      sprintf("AUS: ratio MSPE post/pre = %.0f.  FIN: ratio MSPE post/pre = %.0f.  ",
              ratio_aus, ratio_fin),
      "Fonte: OECD SOCX TP31, elaborazione R/Synth."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#2C2A26"),
    plot.subtitle    = element_text(size = 11, color = "grey30"),
    plot.caption     = element_text(size = 9, hjust = 0, color = "grey40"),
    legend.position  = "top",
    legend.text      = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey92")
  )


# ---- 4. Salva il grafico -----------------------------------
out <- "~/Downloads/TESINA_NDIS_AUSTRALIA/FIGURE/Figura_Controllo_Negativo_Simmetrico.png"
ggsave(filename = out, plot = fig,
       width = 11, height = 6.2, dpi = 170, bg = "white")

cat(sprintf("\nGrafico salvato in:\n  %s\n", out))

# Mostra a video
print(fig)
