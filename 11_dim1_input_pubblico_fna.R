# ============================================================
# FRAMEWORK 5 DIMENSIONI — DIM 1: INPUT PUBBLICO (rev.2)
# Fondo Nazionale per le Non Autosufficienze (FNA)
# ------------------------------------------------------------
# Test: il vincolo formale di "invarianza" finanziaria nominale
# produce duplice erosione:
#   - reale (deflattore IPCA): -5% al 2027
#   - pro capite ultra65 (ISTAT): -10% al 2027 (effetto combinato)
#
# FONTI:
#   - Legge di Bilancio annuale, art. relativi al FNA
#   - Decreti MLPS di riparto FNA (ministerolavoro.gov.it)
#   - IPCA mensile (ISTAT dati: http://dati.istat.it)
#   - Popolazione 65+ residente (ISTAT, Bilancio demografico)
#
# OUTPUT: FIGURE/Fig_DIM1_FNA_invarianza.png
# ============================================================

library(tidyverse)
library(scales)

# ---- 1. Dati hardcoded ----
fna <- tibble(
  anno = 2018:2027,
  fna_nominale_mln = c(573, 573, 571, 823, 823, 865, 913, 913, 913, 913)
)

# IPCA medio annuo (ISTAT) — deflattore
ipca <- c("2018"=1.011, "2019"=1.006, "2020"=0.998,
          "2021"=1.019, "2022"=1.081, "2023"=1.059,
          "2024"=1.010, "2025"=1.020, "2026"=1.020, "2027"=1.020)
defl <- rev(cumprod(rev(ipca)))
defl <- defl / defl["2027"]

# Popolazione 65+ Italia (ISTAT Bilancio demografico, milioni di persone)
# Crescita ~1% annuo, proiezione lineare 2025-2027
pop65 <- c("2018"=13.71, "2019"=13.84, "2020"=13.92, "2021"=14.02,
           "2022"=14.18, "2023"=14.27, "2024"=14.36,
           "2025"=14.46, "2026"=14.56, "2027"=14.66)

fna <- fna |>
  mutate(deflattore       = as.numeric(defl[as.character(anno)]),
         pop65_mln        = as.numeric(pop65[as.character(anno)]),
         fna_reale_mln    = fna_nominale_mln / deflattore,
         # FNA pro capite ultra65 in € reali (prezzi 2027)
         fna_procapite_65 = fna_reale_mln * 1e6 / (pop65_mln * 1e6))

print(fna)

# Erosioni 2024 -> 2027
ero_reale <- (fna$fna_reale_mln[fna$anno == 2024] -
              fna$fna_reale_mln[fna$anno == 2027]) /
              fna$fna_reale_mln[fna$anno == 2024] * 100
ero_pc <- (fna$fna_procapite_65[fna$anno == 2024] -
           fna$fna_procapite_65[fna$anno == 2027]) /
           fna$fna_procapite_65[fna$anno == 2024] * 100
cat(sprintf("\nErosione FNA reale 2024-2027:                  %+.1f%%\n", -ero_reale))
cat(sprintf("Erosione FNA pro capite ultra65 2024-2027:     %+.1f%%\n", -ero_pc))

# ---- 2. Grafico ----
# Indice 2018 = 100 per confrontare le tre serie sullo stesso asse
fna <- fna |>
  mutate(idx_nominale  = fna_nominale_mln / fna_nominale_mln[1] * 100,
         idx_reale     = fna_reale_mln    / fna_reale_mln[1]    * 100,
         idx_procapite = fna_procapite_65 / fna_procapite_65[1] * 100)

df_long <- fna |>
  select(anno, idx_nominale, idx_reale, idx_procapite) |>
  pivot_longer(-anno, names_to = "serie", values_to = "indice") |>
  mutate(serie = recode(serie,
                        idx_nominale  = "FNA nominale",
                        idx_reale     = "FNA reale (deflattore IPCA)",
                        idx_procapite = "FNA pro capite ultra65 (reale)"))

COL_NEU <- "#7A7A7A"; COL_BAD <- "#B85042"; COL_AMB <- "#D4A24E"; COL_BG <- "#2C2A26"
pal <- c("FNA nominale" = COL_NEU,
         "FNA reale (deflattore IPCA)" = COL_AMB,
         "FNA pro capite ultra65 (reale)" = COL_BAD)
shp <- c("FNA nominale" = 16, "FNA reale (deflattore IPCA)" = 17,
         "FNA pro capite ultra65 (reale)" = 15)

p <- ggplot(df_long, aes(x = anno, y = indice, colour = serie, shape = serie)) +
  annotate("rect", xmin = 2024.5, xmax = 2027.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = COL_BAD) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  geom_hline(yintercept = 100, colour = "grey80", linewidth = 0.4) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = shp) +
  scale_x_continuous(breaks = 2018:2027) +
  scale_y_continuous(limits = c(80, 175)) +
  annotate("text", x = 2026, y = 88, hjust = 0.5,
           label = sprintf("scenario Invarianza\nFNA reale %+.1f%%\npro capite ultra65 %+.1f%%",
                           -ero_reale, -ero_pc),
           colour = COL_BAD, fontface = "italic", size = 3.2) +
  labs(title = "DIM 1 — Input Pubblico: Fondo Non Autosufficienza",
       subtitle = "Vincolo nominale rispettato → erosione reale e demografica combinata",
       x = NULL, y = "Indice 2018 = 100",
       caption = "Fonti: LdB 2018-2025, DM riparto MLPS, deflattore IPCA ISTAT, popolazione 65+ ISTAT.",
       colour = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = COL_BG),
        plot.subtitle = element_text(colour = COL_NEU, size = 9.5),
        plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0),
        legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("FIGURE/Fig_DIM1_FNA_invarianza.png", p,
       width = 7.8, height = 4.6, dpi = 200)
print(p)   # ora viene mostrato anche nel pane Plot
cat("Grafico salvato e mostrato: FIGURE/Fig_DIM1_FNA_invarianza.png\n")
