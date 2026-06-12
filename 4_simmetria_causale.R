# ============================================================
# SCRIPT 4 — SIMMETRIA CAUSALE: AUSTRALIA vs FINLANDIA
# Due modelli SCM speculari, stessa procedura, anno-pivot 2013.
# ============================================================
# CORREZIONE METODOLOGICA IMPORTANTE:
# Il modello SCM per la Finlandia ESCLUDE l'Australia dal donor
# pool, perché l'Australia non è un controllo pulito (ha avuto
# il NDIS nello stesso periodo). Il pool della Finlandia è
# quindi di 8 paesi (anziché 9): CAN, NZL, GBR, USA, IRL, NOR,
# SWE, DNK — tutti i paesi del pool originale tranne AUS e FIN.
# ============================================================

# ---- 0. Setup -----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "Synth")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

BASE  <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
DATA  <- file.path(BASE, "DATI", "SOCKS_INCAPACITY_PPPPC.csv")
FIG   <- file.path(BASE, "FIGURE")
WSDIR <- file.path(BASE, "R_WORKSPACE")


# ---- 1. Costanti --------------------------------------------
PRE      <- 2000:2012
POST     <- 2013:2021
TRT_YEAR <- 2013
ALL_COUNTRIES <- c("AUS","CAN","NZL","GBR","USA","IRL","NOR","SWE","FIN","DNK")


# ---- 2. Carica dati e prepara panel ------------------------
raw <- read_csv(DATA, show_col_types = FALSE)
long <- raw |>
  filter(!is.na(OBS_VALUE), PRICE_BASE == "V") |>
  transmute(country = REF_AREA,
            year    = as.integer(TIME_PERIOD),
            stype   = SPENDING_TYPE,
            value   = as.numeric(OBS_VALUE))
country_id <- tibble(country = ALL_COUNTRIES, id = seq_along(ALL_COUNTRIES))
long <- long |> left_join(country_id, by = "country")
panel_T <- long |> filter(stype == "_T")


# ---- 3. Funzione fit_scm (versione standalone) -------------
fit_scm <- function(panel_long, treated_code, donor_codes,
                    pre = PRE, post = POST) {
  treated_id <- country_id$id[country_id$country == treated_code]
  donors_id  <- country_id$id[country_id$country %in% donor_codes]
  df <- panel_long |>
    filter(country %in% c(treated_code, donor_codes),
           year %in% c(pre, post)) |>
    arrange(id, year) |> as.data.frame()
  special_preds <- lapply(pre, function(y) list("value", y, "mean"))
  d <- dataprep(
    foo = df, predictors = NULL,
    special.predictors = special_preds,
    dependent = "value", unit.variable = "id", time.variable = "year",
    treatment.identifier = treated_id, controls.identifier = donors_id,
    time.predictors.prior = pre, time.optimize.ssr = pre,
    unit.names.variable = "country", time.plot = c(pre, post)
  )
  syn <- synth(data.prep.obj = d, verbose = FALSE, method = "Nelder-Mead")
  Yt <- as.numeric(d$Y1plot)
  Ys <- as.numeric(d$Y0plot %*% syn$solution.w)
  yrs <- as.integer(rownames(d$Y1plot))
  pre_mask <- yrs %in% pre; post_mask <- yrs %in% post
  list(
    treated = treated_code, donors = donor_codes,
    years = yrs, Yt = Yt, Ys = Ys, gap = Yt - Ys,
    weights = setNames(as.numeric(syn$solution.w), donor_codes),
    mspe_pre  = mean((Yt[pre_mask]-Ys[pre_mask])^2),
    mspe_post = mean((Yt[post_mask]-Ys[post_mask])^2),
    att_last = (Yt[post_mask]-Ys[post_mask])[sum(post_mask)],
    att_last_pct = ((Yt[post_mask]-Ys[post_mask])[sum(post_mask)] /
                      Ys[post_mask][sum(post_mask)]) * 100,
    last_year = yrs[post_mask][sum(post_mask)]
  )
}


# ---- 4. Carica il modello AUS esistente (donor pool: 9 paesi) ----
load(file.path(WSDIR, "robust_usdppp_finale.RData"))
aus <- main   # AUS trattata, donor pool = c(CAN,NZL,GBR,USA,IRL,NOR,SWE,FIN,DNK)


# ---- 5. CALCOLA modello FIN PULITO (donor pool: 8 paesi, ESCLUSO AUS) ----
donors_fin <- setdiff(ALL_COUNTRIES, c("AUS", "FIN"))   # 8 paesi
cat("\nDonor pool per Finlandia (esclude AUS):\n")
cat(" ", paste(donors_fin, collapse = ", "), "\n")

cat("\nStima SCM Finlandia trattata, anno-pivot 2013...\n")
fin <- fit_scm(panel_T, treated_code = "FIN", donor_codes = donors_fin)


# ---- 6. Tabella di confronto --------------------------------
cat("\n=== SIMMETRIA CAUSALE: I DUE MODELLI ===\n")
cat(sprintf(
"%-10s  donors=%d   pre-MSPE=%7.1f   post-MSPE=%9.1f   ratio=%6.1f   ATT %d = %+7.1f (%+6.1f%%)\n",
  "Australia", length(aus$donors), aus$mspe_pre, aus$mspe_post,
  aus$mspe_post/aus$mspe_pre, aus$last_year, aus$att_last, aus$att_last_pct))
cat(sprintf(
"%-10s  donors=%d   pre-MSPE=%7.1f   post-MSPE=%9.1f   ratio=%6.1f   ATT %d = %+7.1f (%+6.1f%%)\n",
  "Finlandia", length(fin$donors), fin$mspe_pre, fin$mspe_post,
  fin$mspe_post/fin$mspe_pre, fin$last_year, fin$att_last, fin$att_last_pct))

cat("\nPesi del sintetico (solo quelli > 1%):\n")
cat("  Australia (donor pool 9 paesi):\n")
print(round(aus$weights[aus$weights > 0.01], 3))
cat("  Finlandia (donor pool 8 paesi, esclusa AUS):\n")
print(round(fin$weights[fin$weights > 0.01], 3))


# ---- 7. Funzione di disegno SCM -----------------------------
plot_scm <- function(model, title_text, color_real, ylabel = NULL) {
  df <- bind_rows(
    tibble(anno = model$years, valore = model$Yt, tipo = "Paese reale"),
    tibble(anno = model$years, valore = model$Ys, tipo = "Controfattuale sintetico")
  )
  ggplot(df, aes(x = anno, y = valore, color = tipo,
                 linetype = tipo, shape = tipo)) +
    geom_vline(xintercept = 2013, linetype = "dashed",
               color = "grey55", linewidth = 0.5) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2.6) +
    scale_color_manual(values = c(
      "Paese reale" = color_real,
      "Controfattuale sintetico" = "#444444")) +
    scale_linetype_manual(values = c(
      "Paese reale" = "solid",
      "Controfattuale sintetico" = "dashed")) +
    scale_shape_manual(values = c(
      "Paese reale" = 16,
      "Controfattuale sintetico" = 15)) +
    labs(title = title_text, x = "Anno", y = ylabel,
         color = NULL, linetype = NULL, shape = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank()
    )
}

aus_plot <- plot_scm(aus,
  sprintf("Australia trattata — NDIS espande (ATT %d = +%.0f USD/p, +%.1f%%)",
          aus$last_year, aus$att_last, aus$att_last_pct),
  "#C0392B",
  ylabel = "USD PPP per persona")

fin_plot <- plot_scm(fin,
  sprintf("Finlandia trattata — consolidamento contrae (ATT %d = %.0f USD/p, %.1f%%)",
          fin$last_year, fin$att_last, fin$att_last_pct),
  "#2C5E80")


# ---- 8. Layout affiancato con titolo unico ------------------
fig <- (aus_plot | fin_plot) +
  plot_annotation(
    title    = "Simmetria causale: due politiche pubbliche di senso opposto, stessa procedura SCM",
    subtitle = sprintf(
      "Ratio MSPE post/pre quasi identici (AUS %.0f, FIN %.0f) — il modello identifica due rotture equivalenti di direzione opposta",
      aus$mspe_post / aus$mspe_pre, fin$mspe_post / fin$mspe_pre),
    caption  = paste(
      "Modelli SCM separati: AUS trattata con donor pool di 9 paesi; FIN trattata con donor pool di 8 paesi (esclusa AUS, che ha avuto il NDIS).",
      "Anno-pivot 2013 in entrambi i casi.  Fonte: OECD SOCX TP31, elaborazione R con il pacchetto Synth.",
      sep = "\n"),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14, color = "#2C2A26"),
      plot.subtitle = element_text(size = 11, color = "grey25"),
      plot.caption  = element_text(size = 9, color = "grey45", hjust = 0)
    )
  )


# ---- 9. Salvataggio e visualizzazione ----------------------
out <- file.path(FIG, "Figura_Simmetria_Causale.png")
ggsave(filename = out, plot = fig,
       width = 13, height = 6.5, dpi = 170, bg = "white")
cat(sprintf("\nGrafico salvato:\n  %s\n", out))
print(fig)


# ---- 10. Salva il nuovo modello FIN nel workspace -----------
main_fin_excl_aus <- fin
save(main, decomp, placebo, loo, placebo_tab, loo_tab,
     main_fin_excl_aus,
     file = file.path(WSDIR, "robust_usdppp_finale.RData"))
cat("\nWorkspace aggiornato con 'main_fin_excl_aus' (modello FIN trattata pulito).\n")
