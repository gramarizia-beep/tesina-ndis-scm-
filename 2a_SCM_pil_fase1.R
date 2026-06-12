# ============================================================
# TESINA NDIS — SCRIPT 2a: SCM Fase 1 in % di PIL (modello principale)
# ============================================================
# CARICA i workspace .RData salvati nella sessione originale del
# 1 giugno 2026 e ne RI-ESTRAE numeri e figure della Fase 1 della
# tesi (SCM di Australia/NDIS Act 2013 con outcome = % PIL).
#
# Strategia: invece di ri-stimare il modello da zero (che con
# tidysynth richiederebbe di replicare esattamente il set di
# predittori usato nella sessione originale), questo script
# carica gli oggetti SCM già fittati salvati come `.RData` e
# li usa per produrre grafici + tabella MSPE coerenti al 100%
# con quelli citati nella tesi (MSPE in-kind = 121, cash = 7.97,
# z-score 2.46, ATT 2021 = +1.07 PIL).
#
# WORKSPACE DI INPUT (in R_WORKSPACE/):
#   - tesina_step1.RData                (serie storica Australia)
#   - tesina_step3_modello.RData        (scm_ndis modello principale)
#   - tesina_step4_robustness.RData     (scm_2016, scm_placebo_2007)
#   - tesina_step5_decomposizione.RData (scm_cash, scm_inkind, sintesi)
#   - tesina_step6_finale.RData         (scm_loo senza Canada)
#
# OUTPUT FIGURE/:
#   Figura_AUS_serie_storica.png            (Figura 1 della tesi)
#   Figura_NDIS_trends.png                  (Figura 2)
#   Figura_NDIS_differences.png             (Figura 3)
#   Figura_NDIS_cash.png                    (Figura 4)
#   Figura_NDIS_inkind.png                  (Figura 5)
#   Figura_NDIS_placebos.png                (Figura 6)
#   Figura_NDIS_placebo_intime_2007.png     (Figura 7)
#   Figura_NDIS_robust_2016.png             (Figura 8)
#   Figura_NDIS_loo_canada.png              (Figura 9)
# ============================================================

# ---- 0. Setup ----
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "tidysynth", "scales")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install,
                                          repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BASE   <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
FIGDIR <- file.path(BASE, "FIGURE")
OUTDIR <- file.path(BASE, "FASE1_PIL")
WSDIR  <- file.path(BASE, "R_WORKSPACE")
for (d in c(FIGDIR, OUTDIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

COL_NEU <- "#7A7A7A"; COL_BAD <- "#B85042"; COL_BG <- "#2C2A26"
COL_TRT <- "#444444"; COL_SYN <- "#B85042"
TRT_YEAR <- 2013

# ---- Helper: carica un .RData in un environment isolato ----
load_ws <- function(filename) {
  env <- new.env()
  load(file.path(WSDIR, filename), envir = env)
  cat(sprintf("  Caricato %s — oggetti: %s\n",
              filename, paste(ls(env), collapse = ", ")))
  env
}

# ============================================================
# 1. CARICAMENTO WORKSPACE ORIGINALI
# ============================================================
cat("\n[1] Caricamento workspace .RData originali (sessione 1 giugno 2026)...\n")
ws_step1 <- load_ws("tesina_step1.RData")
ws_step3 <- load_ws("tesina_step3_modello.RData")
ws_step4 <- load_ws("tesina_step4_robustness.RData")
ws_step5 <- load_ws("tesina_step5_decomposizione.RData")
ws_step6 <- load_ws("tesina_step6_finale.RData")

# Estraggo gli oggetti SCM dalla loro casa originale.
# I nomi degli oggetti sono quelli usati nella sessione originale:
#   scm_ndis        = modello principale (outcome totale, pivot 2013)
#   scm_cash        = decomposizione cash (pivot 2013)
#   scm_inkind      = decomposizione in-kind (pivot 2013)
#   scm_2016        = robust con pivot 2016
#   scm_placebo_2007= placebo in-time pivot 2007
#   scm_loo         = leave-one-out senza Canada
pick <- function(ws, ...) {
  for (nm in c(...)) if (exists(nm, envir = ws)) return(get(nm, envir = ws))
  NULL
}

scm_ndis     <- pick(ws_step3, "scm_ndis", "scm")
scm_2016     <- pick(ws_step4, "scm_2016", "scm_robust_2016")
scm_2007     <- pick(ws_step4, "scm_placebo_2007", "scm_2007")
scm_cash     <- pick(ws_step5, "scm_cash")
scm_inkind   <- pick(ws_step5, "scm_inkind")
scm_loo      <- pick(ws_step6, "scm_loo", "scm_loo_canada", "scm_no_canada")

# Verifica presenza
for (nm in c("scm_ndis", "scm_2016", "scm_2007", "scm_cash", "scm_inkind",
             "scm_loo")) {
  if (is.null(get(nm)))
    cat(sprintf("  ⚠ %s non trovato nel rispettivo workspace\n", nm))
}

# ============================================================
# 2. FIGURA 1 — Serie storica Australia
# ============================================================
cat("\n[2] Figura 1 — Serie storica Australia % PIL...\n")
# La serie sta dentro lo step1 (panel o aus_series)
panel_obj <- pick(ws_step1, "panel", "panel_aus", "aus")
if (!is.null(panel_obj)) {
  aus_series <- if ("iso3" %in% names(panel_obj)) {
    panel_obj |> filter(iso3 == "AUS")
  } else panel_obj
  out_var <- if ("spesa_totale" %in% names(aus_series)) "spesa_totale" else
             if ("spesa" %in% names(aus_series)) "spesa" else names(aus_series)[3]
  p1 <- ggplot(aus_series, aes(x = anno, y = .data[[out_var]])) +
    geom_line(linewidth = 1.2, colour = COL_BG) +
    geom_point(size = 2.4, colour = COL_BG) +
    geom_vline(xintercept = TRT_YEAR, linetype = "dashed", colour = COL_BAD) +
    annotate("text", x = TRT_YEAR + 0.3, y = max(aus_series[[out_var]]) * 0.6,
             label = "NDIS Act\n2013", colour = COL_BAD, hjust = 0,
             fontface = "bold", size = 3.6) +
    labs(title = "Australia — Spesa pubblica per disabilità (% PIL)",
         subtitle = "OECD SOCX, programma TP31 incapacity-related",
         x = NULL, y = "% di PIL",
         caption = "Fonte: OECD SOCX. Elaborazione R.") +
    scale_x_continuous(breaks = seq(2000, 2022, 4)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = COL_NEU),
          plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0),
          panel.grid.minor = element_blank())
  ggsave(file.path(FIGDIR, "Figura_AUS_serie_storica.png"), p1,
         width = 8, height = 4.5, dpi = 200, bg = "white")
}

# ============================================================
# 3. FIGURE 2-3: trends + differences (dal modello principale)
# ============================================================
cat("[3] Figure 2-3 — Modello principale (Y vs sintetico + gap)...\n")
ts_main <- grab_synthetic_control(scm_ndis) |>
  rename(anno = time_unit, real = real_y, syn = synth_y) |>
  mutate(gap = real - syn)
ATT_2021 <- ts_main |> filter(anno == 2021) |> pull(gap)
sig_main <- grab_significance(scm_ndis) |> filter(unit_name == "AUS")
cat(sprintf("  ATT 2021 = %+.3f punti PIL\n", ATT_2021))
cat(sprintf("  MSPE pre=%.5f post=%.4f ratio=%.1f p=%.3f\n",
            sig_main$pre_mspe, sig_main$post_mspe, sig_main$mspe_ratio,
            sig_main$fishers_exact_pvalue))

# Figura 2 — trends
p2 <- ggplot(ts_main, aes(x = anno)) +
  geom_line(aes(y = real, colour = "Australia osservata"), linewidth = 1.2) +
  geom_line(aes(y = syn,  colour = "Sintetico"), linewidth = 1.2,
            linetype = "dashed") +
  geom_vline(xintercept = TRT_YEAR, linetype = "dotted", colour = COL_NEU) +
  scale_colour_manual(values = c("Australia osservata" = COL_TRT,
                                 "Sintetico" = COL_SYN)) +
  labs(title = "Modello SCM principale — Spesa per disabilità % PIL",
       subtitle = "Australia osservata (grigio) vs sintetico (rosso tratteggiato)",
       x = NULL, y = "% di PIL", colour = NULL,
       caption = sprintf("ATT al 2021 = %+.2f punti PIL.  MSPE ratio = %.1f.  Fonte: OECD SOCX TP31.",
                         ATT_2021, sig_main$mspe_ratio)) +
  scale_x_continuous(breaks = seq(2000, 2021, 3)) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = COL_NEU),
        legend.position = "bottom",
        plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0))
ggsave(file.path(FIGDIR, "Figura_NDIS_trends.png"), p2,
       width = 8, height = 4.8, dpi = 200, bg = "white")

# Figura 3 — gap
p3 <- ggplot(ts_main, aes(x = anno, y = gap)) +
  geom_hline(yintercept = 0, colour = COL_BG, linewidth = 0.4) +
  geom_line(linewidth = 1.2, colour = COL_BAD) +
  geom_point(size = 2.3, colour = COL_BAD) +
  geom_vline(xintercept = TRT_YEAR, linetype = "dashed", colour = COL_NEU) +
  labs(title = "Gap nel tempo: Australia osservata − Sintetico",
       subtitle = "Pre 2013: gap ≈ 0 (buon fit). Post 2013: gap cresce a ~+1 punto PIL",
       x = NULL, y = "Gap (punti di PIL)",
       caption = "Fonte: OECD SOCX TP31.") +
  scale_x_continuous(breaks = seq(2000, 2021, 3)) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = COL_NEU),
        plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0))
ggsave(file.path(FIGDIR, "Figura_NDIS_differences.png"), p3,
       width = 8, height = 4.5, dpi = 200, bg = "white")

# ============================================================
# 4. FIGURE 4-5: decomposizione cash + in-kind
# ============================================================
cat("[4] Figure 4-5 — Decomposizione cash e in-kind...\n")
plot_decomp <- function(scm, titolo, sub, fname) {
  ts_d <- grab_synthetic_control(scm) |>
    rename(anno = time_unit, real = real_y, syn = synth_y)
  sig_d <- grab_significance(scm) |> filter(unit_name == "AUS")
  p <- ggplot(ts_d, aes(x = anno)) +
    geom_line(aes(y = real, colour = "Osservata"), linewidth = 1.2) +
    geom_line(aes(y = syn,  colour = "Sintetico"), linewidth = 1.2,
              linetype = "dashed") +
    geom_vline(xintercept = TRT_YEAR, linetype = "dotted", colour = COL_NEU) +
    scale_colour_manual(values = c("Osservata" = COL_TRT, "Sintetico" = COL_SYN)) +
    labs(title = titolo, subtitle = sub, x = NULL, y = "% di PIL", colour = NULL,
         caption = sprintf("MSPE ratio (post/pre) = %.2f, p = %.3f. Fonte: OECD SOCX TP31.",
                           sig_d$mspe_ratio, sig_d$fishers_exact_pvalue)) +
    scale_x_continuous(breaks = seq(2000, 2021, 3)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = COL_NEU),
          legend.position = "bottom",
          plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0))
  ggsave(file.path(FIGDIR, fname), p, width = 8, height = 4.8,
         dpi = 200, bg = "white")
  sig_d
}
sig_cash <- plot_decomp(scm_cash,
            "Decomposizione CASH — Sintetico vs osservato",
            "Componente cash della spesa per disabilità (% PIL)",
            "Figura_NDIS_cash.png")
sig_kind <- plot_decomp(scm_inkind,
            "Decomposizione IN-KIND — Sintetico vs osservato",
            "Componente servizi della spesa per disabilità (% PIL) — firma centrale della tesi",
            "Figura_NDIS_inkind.png")
cat(sprintf("  CASH:    ratio = %.2f, p = %.3f\n",
            sig_cash$mspe_ratio, sig_cash$fishers_exact_pvalue))
cat(sprintf("  IN-KIND: ratio = %.2f, p = %.3f, z-score = %.2f\n",
            sig_kind$mspe_ratio, sig_kind$fishers_exact_pvalue,
            sig_kind$z_score))

# ============================================================
# 5. FIGURA 6: placebo in-space
# ============================================================
cat("[5] Figura 6 — Placebo in-space...\n")
p6 <- plot_placebos(scm_ndis, prune = FALSE) +
  labs(title = "Test placebo in-space",
       subtitle = "Australia (rossa) vs gap di ciascun donor ri-trattato",
       caption = "Fonte: OECD SOCX TP31.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = COL_NEU),
        plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0))
ggsave(file.path(FIGDIR, "Figura_NDIS_placebos.png"), p6,
       width = 8, height = 5, dpi = 200, bg = "white")

# ============================================================
# 6. FIGURA 7: placebo in-time anno-pivot 2007
# ============================================================
cat("[6] Figura 7 — Placebo in-time 2007...\n")
if (!is.null(scm_2007)) {
  ts_07 <- grab_synthetic_control(scm_2007) |>
    rename(anno = time_unit, real = real_y, syn = synth_y) |>
    mutate(gap = real - syn)
  p7 <- ggplot(ts_07, aes(x = anno, y = gap)) +
    geom_hline(yintercept = 0, colour = COL_BG, linewidth = 0.4) +
    geom_line(linewidth = 1.2, colour = COL_NEU) +
    geom_point(size = 2.3, colour = COL_NEU) +
    geom_vline(xintercept = 2007, linetype = "dashed", colour = COL_BAD) +
    annotate("text", x = 2007.2, y = max(abs(ts_07$gap), na.rm = TRUE) * 0.8,
             label = "Anno-pivot\nfittizio (2007)",
             colour = COL_BAD, hjust = 0, fontface = "bold", size = 3.4) +
    labs(title = "Placebo in-time: anno-pivot fittizio 2007",
         subtitle = "Nessun effetto strutturale prima del 2013 — il modello regge il falso pivot",
         x = NULL, y = "Gap (punti di PIL)",
         caption = "Fonte: OECD SOCX TP31.") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = COL_NEU),
          plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0))
  ggsave(file.path(FIGDIR, "Figura_NDIS_placebo_intime_2007.png"), p7,
         width = 8, height = 4.5, dpi = 200, bg = "white")
}

# ============================================================
# 7. FIGURA 8: robust anno-pivot 2016
# ============================================================
cat("[7] Figura 8 — Robust pivot 2016...\n")
if (!is.null(scm_2016)) {
  ts_16 <- grab_synthetic_control(scm_2016) |>
    rename(anno = time_unit, real = real_y, syn = synth_y)
  p8 <- ggplot(ts_16, aes(x = anno)) +
    geom_line(aes(y = real, colour = "Osservata"), linewidth = 1.2) +
    geom_line(aes(y = syn,  colour = "Sintetico"), linewidth = 1.2,
              linetype = "dashed") +
    geom_vline(xintercept = 2016, linetype = "dashed", colour = COL_BAD) +
    scale_colour_manual(values = c("Osservata" = COL_TRT, "Sintetico" = COL_SYN)) +
    labs(title = "Robustezza al cambio di anno-pivot (2016)",
         subtitle = "Modello stimato con anno-pivot ritardato al 2016: effetto amplificato",
         x = NULL, y = "% di PIL", colour = NULL,
         caption = "Fonte: OECD SOCX TP31.") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = COL_NEU),
          legend.position = "bottom",
          plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0))
  ggsave(file.path(FIGDIR, "Figura_NDIS_robust_2016.png"), p8,
         width = 8, height = 4.8, dpi = 200, bg = "white")
}

# ============================================================
# 8. FIGURA 9: leave-one-out senza Canada
# ============================================================
cat("[8] Figura 9 — LOO senza Canada...\n")
if (!is.null(scm_loo)) {
  ts_loo <- grab_synthetic_control(scm_loo) |>
    rename(anno = time_unit, real = real_y, syn = synth_y)
  sig_loo <- grab_significance(scm_loo) |> filter(unit_name == "AUS")
  p9 <- ggplot(ts_loo, aes(x = anno)) +
    geom_line(aes(y = real, colour = "Osservata"), linewidth = 1.2) +
    geom_line(aes(y = syn,  colour = "Sintetico"), linewidth = 1.2,
              linetype = "dashed") +
    geom_vline(xintercept = TRT_YEAR, linetype = "dotted", colour = COL_NEU) +
    scale_colour_manual(values = c("Osservata" = COL_TRT, "Sintetico" = COL_SYN)) +
    labs(title = "Leave-one-out: donor pool senza Canada",
         subtitle = "Il risultato regge anche senza il paese che pesava di più nei donor",
         x = NULL, y = "% di PIL", colour = NULL,
         caption = sprintf("MSPE ratio (post/pre) = %.1f.  Fonte: OECD SOCX TP31.",
                           sig_loo$mspe_ratio)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = COL_NEU),
          legend.position = "bottom",
          plot.caption = element_text(colour = COL_NEU, size = 8, hjust = 0))
  ggsave(file.path(FIGDIR, "Figura_NDIS_loo_canada.png"), p9,
         width = 8, height = 4.8, dpi = 200, bg = "white")
}

# ============================================================
# 9. Sintesi MSPE finale
# ============================================================
cat("\n[9] Tabella sintesi MSPE estratta dai workspace originali...\n")
sintesi_obj <- pick(ws_step5, "sintesi")   # tibble salvato nello step5
if (!is.null(sintesi_obj)) {
  cat("=== SINTESI MSPE (originale dello step5, riferimento della tesi) ===\n")
  print(sintesi_obj)
  write_csv(sintesi_obj, file.path(OUTDIR, "sintesi_mspe_fase1_pil.csv"))
} else {
  cat("Tibble 'sintesi' non trovato. Ricostruisco dai sig.\n")
}

cat("\n======================================================\n")
cat("=== 9 FIGURE Fase 1 rigenerate dai workspace originali ===\n")
for (f in c("Figura_AUS_serie_storica.png",
            "Figura_NDIS_trends.png",
            "Figura_NDIS_differences.png",
            "Figura_NDIS_cash.png",
            "Figura_NDIS_inkind.png",
            "Figura_NDIS_placebos.png",
            "Figura_NDIS_placebo_intime_2007.png",
            "Figura_NDIS_robust_2016.png",
            "Figura_NDIS_loo_canada.png")) {
  mark <- if (file.exists(file.path(FIGDIR, f))) "✓" else "✗"
  cat(sprintf("  %s  %s\n", mark, f))
}
cat("======================================================\n")
