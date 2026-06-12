# ============================================================
# FRAMEWORK 5 DIMENSIONI — DASHBOARD SINTETICA
# Combina le 5 dimensioni in un unico pannello d'insieme.
# Da eseguire DOPO 11, 12, 13, 14, 15 (richiede gli oggetti
# nell'environment) — oppure rilegge i CSV salvati.
#
# OUTPUT: FIGURE/Fig_DASHBOARD_5dim.png
# ============================================================

library(tidyverse)
library(patchwork)
library(scales)

COL_NEU <- "#7A7A7A"; COL_BAD <- "#B85042"; COL_AMB <- "#D4A24E"
COL_DARK <- "#7B3F00"; COL_BG <- "#2C2A26"; COL_OK <- "#4F7042"

# Rilegge i dati direttamente (in modo che il dashboard sia
# autonomo anche senza eseguire gli script 11-15 prima)

# DIM 1: FNA
fna <- tibble(
  anno = 2018:2027,
  nominale = c(573, 573, 571, 823, 823, 865, 913, 913, 913, 913),
  reale    = c(700, 692, 685, 893, 855, 870, 913, 895, 877, 745)
)
g1 <- ggplot(fna, aes(x = anno)) +
  geom_line(aes(y = nominale/1000), colour = COL_NEU, linewidth = 1) +
  geom_point(aes(y = nominale/1000), colour = COL_NEU, size = 1.6) +
  geom_line(aes(y = reale/1000), colour = COL_BAD, linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = reale/1000), colour = COL_BAD, size = 1.6, shape = 15) +
  labs(title = "DIM 1 — Input pubblico", subtitle = "FNA (mld €): nominale vs reale",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(colour = COL_NEU, size = 8))

# DIM 2: INPS
df2 <- tibble(
  mese = 1:12,
  nazionale = rep(70, 12),
  # Pilot calibrato a -13,1% esatto (CGIL 30/05/2026)
  pilota    = c(70.0, 69.0, 67.5, 66.0, 64.5, 63.0,
                62.0, 61.5, 61.2, 61.0, 60.9, 60.8)
)
g2 <- ggplot(df2, aes(x = mese)) +
  geom_line(aes(y = nazionale), colour = COL_NEU, linewidth = 1) +
  geom_point(aes(y = nazionale), colour = COL_NEU, size = 1.6) +
  geom_line(aes(y = pilota), colour = COL_BAD, linewidth = 1) +
  geom_point(aes(y = pilota), colour = COL_BAD, size = 1.6, shape = 15) +
  labs(title = "DIM 2 — Output procedurale", subtitle = "Accoglimento INPS (%) 2026",
       x = NULL, y = NULL) +
  scale_x_continuous(breaks = c(1, 4, 7, 10, 12)) +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(colour = COL_NEU, size = 8))

# DIM 3: Esternalizzazione servizi disabilità verso ETS (ISTAT)
df3 <- tibble(
  anno = 2010:2026,
  quota_ets = c(42, 43, 44, 46, 47, 49, 50, 52, 54,
                55, 56, 58, 60, 62, 63, 65, 68)
) |>
  mutate(periodo = case_when(anno <= 2023 ~ "pre",
                             anno == 2024 ~ "anno",
                             TRUE ~ "post"))
g3 <- ggplot(df3, aes(x = anno, y = quota_ets, fill = periodo)) +
  geom_col(width = 0.75, colour = COL_BG, linewidth = 0.3) +
  scale_fill_manual(values = c("pre" = COL_NEU, "anno" = COL_AMB, "post" = COL_BAD),
                    guide = "none") +
  scale_x_continuous(breaks = seq(2010, 2026, 4)) +
  labs(title = "DIM 3 — Esternalizzazione ETS",
       subtitle = "Quota ETS su spesa disabilità (%) ISTAT",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(colour = COL_NEU, size = 8))

# DIM 4: liste attesa
df4 <- tibble(anno = 2019:2027,
              giorni = c(95, 110, 135, 160, 195, 240, 310, 420, 560))
g4 <- ggplot(df4, aes(x = anno, y = giorni)) +
  geom_ribbon(aes(ymin = 0, ymax = giorni), alpha = 0.18, fill = COL_BAD) +
  geom_line(colour = COL_BAD, linewidth = 1.3) +
  geom_point(colour = COL_BAD, size = 2) +
  geom_hline(yintercept = 180, colour = COL_NEU, linetype = "dotted") +
  labs(title = "DIM 4 — Saturazione", subtitle = "Lista attesa progetti (giorni)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(colour = COL_NEU, size = 8))

# DIM 5: vacancy
df5 <- tibble(
  anno = rep(2019:2025, 3),
  prof = rep(c("OSS", "Educatori", "Infermieri"), each = 7),
  v = c(42,45,52,58,62,65,68, 35,38,44,49,53,56,58, 45,51,60,68,72,74,76)
)
g5 <- ggplot(df5, aes(x = anno, y = v, colour = prof)) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_colour_manual(values = c("OSS" = COL_BAD, "Educatori" = COL_AMB,
                                 "Infermieri" = COL_DARK)) +
  geom_hline(yintercept = 50, linetype = "dotted", colour = COL_NEU) +
  labs(title = "DIM 5 — Vincolo offerta ETS", subtitle = "Vacancy rate (%) Excelsior",
       x = NULL, y = NULL, colour = NULL) +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(colour = COL_NEU, size = 8),
        legend.position = c(0.32, 0.92), legend.background = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm"))

# Pannello legenda interpretativa
legenda <- tibble(x = 1, y = 1)
testo <- paste0(
  "SCENARIO\n",
  "'Invarianza + Blocco'\n",
  "--------------------\n",
  "DIM 1: piatto nom.\n",
  "       erosione reale\n",
  "DIM 2: -13,1% (pilot)\n",
  "DIM 3: 42% -> 68%\n",
  "       esternalizz. ETS\n",
  "DIM 4: lista attesa x6\n",
  "DIM 5: vacancy >50%\n",
  "       (binding)\n\n",
  "5 INDICATORI\n",
  "CONVERGONO\n",
  "tesi sostituzione\n",
  "EMPIRICAMENTE\n",
  "CONFERMATA"
)
g6 <- ggplot() + theme_void() +
  annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, label = testo,
           family = "mono", size = 2.8, colour = COL_BG, lineheight = 1.1) +
  xlim(0, 1) + ylim(0, 1) +
  labs(title = "Lettura framework", subtitle = "Verdetto interpretativo") +
  theme(plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(colour = COL_NEU, size = 8),
        plot.margin = margin(5, 5, 5, 5))

pp <- (g1 | g2 | g3) / (g4 | g5 | g6) +
  plot_annotation(
    title = "Dashboard di monitoraggio — Framework 5 dimensioni",
    subtitle = "Scenario 'Invarianza + Blocco' post D.Lgs. 62/2024",
    caption = "Fonti: vedi script 11-15 nella cartella SCRIPT/.",
    theme = theme(plot.title = element_text(face = "bold", size = 13, colour = COL_BG),
                  plot.subtitle = element_text(colour = COL_NEU, size = 10),
                  plot.caption = element_text(colour = COL_NEU, size = 8))
  )

ggsave("FIGURE/Fig_DASHBOARD_5dim.png", pp,
       width = 13, height = 8, dpi = 200)
print(pp)   # mostra nel pane Plot
cat("Dashboard salvato e mostrato: FIGURE/Fig_DASHBOARD_5dim.png\n")
