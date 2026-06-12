# ============================================================
# TESINA NDIS — SCRIPT 2: SCM su USD PPP per persona (robustness)
# ============================================================
# Replica del modello principale (% di PIL) usando come outcome
# la spesa pubblica per incapacita TP31 in dollari USA a parita
# di potere d'acquisto per persona (USD PPP per capita).
#
#  Trattata     : Australia (AUS)
#  Donor pool   : CAN, NZL, GBR, USA, IRL, NOR, SWE, FIN, DNK (9 paesi)
#  Pre-trat.    : 2000-2012 (13 anni)
#  Post-trat.   : 2013-2021 (9 anni; 2022 ha dati incompleti)
#  Trattamento  : NDIS Act 2013
#
# Output:
#  - figure PNG e PDF in FIGURE/
#  - CSV con gap, pesi e tabella LOO in ROBUSTNESS_USDPPP/
#  - workspace .RData in R_WORKSPACE/
# ============================================================

# ---- 0. Setup ----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "Synth", "knitr")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

BASE   <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
DATA   <- file.path(BASE, "DATI", "SOCKS_INCAPACITY_PPPPC.csv")
FIGDIR <- file.path(BASE, "FIGURE")
OUTDIR <- file.path(BASE, "ROBUSTNESS_USDPPP")
WSDIR  <- file.path(BASE, "R_WORKSPACE")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(WSDIR,  showWarnings = FALSE, recursive = TRUE)

TREATED  <- "AUS"
DONORS   <- c("CAN","NZL","GBR","USA","IRL","NOR","SWE","FIN","DNK")
PRE      <- 2000:2012
POST     <- 2013:2021
TRT_YEAR <- 2013

# ---- 1. Lettura e pivot ------------------------------------
raw <- read_csv(DATA, show_col_types = FALSE)
cat("\nDimensione raw:", nrow(raw), "x", ncol(raw), "\n")

# Trasformo in long pulito con codici stringa
long <- raw |>
  # PRICE_BASE: "V" = prezzi correnti (usati nella tesina principale),
  #            "Q" = prezzi costanti. Filtriamo solo "V" per evitare duplicati.
  filter(!is.na(OBS_VALUE), PRICE_BASE == "V") |>
  transmute(country = REF_AREA,
            year    = as.integer(TIME_PERIOD),
            stype   = SPENDING_TYPE,
            value   = as.numeric(OBS_VALUE))

cat("\nCopertura paesi e misure:\n")
long |> count(country, stype) |> pivot_wider(names_from = stype, values_from = n) |>
  arrange(country) |> print()

# Aggiungo ID numerico (richiesto da Synth)
country_id <- tibble(country = c(TREATED, DONORS),
                     id      = seq_len(length(DONORS) + 1))
long <- long |> left_join(country_id, by = "country")

# ---- 2. Funzione SCM riutilizzabile -------------------------
fit_scm <- function(panel_long, treated_code, donor_codes,
                    pre = PRE, post = POST,
                    optimx_method = "Nelder-Mead") {
  treated_id <- country_id$id[country_id$country == treated_code]
  donors_id  <- country_id$id[country_id$country %in% donor_codes]

  df <- panel_long |>
    filter(country %in% c(treated_code, donor_codes),
           year %in% c(pre, post)) |>
    arrange(id, year) |> as.data.frame()

  # Predittori = livelli annuali pre-trattamento
  special_preds <- lapply(pre, function(y)
    list("value", y, "mean"))

  d <- dataprep(
    foo                  = df,
    predictors           = NULL,
    special.predictors   = special_preds,
    dependent            = "value",
    unit.variable        = "id",
    time.variable        = "year",
    treatment.identifier = treated_id,
    controls.identifier  = donors_id,
    time.predictors.prior= pre,
    time.optimize.ssr    = pre,
    unit.names.variable  = "country",
    time.plot            = c(pre, post)
  )

  syn <- synth(data.prep.obj = d, verbose = FALSE, method = optimx_method)

  Yt <- as.numeric(d$Y1plot)
  Ys <- as.numeric(d$Y0plot %*% syn$solution.w)
  yrs <- as.integer(rownames(d$Y1plot))
  pre_mask  <- yrs %in% pre
  post_mask <- yrs %in% post

  list(
    treated   = treated_code,
    donors    = donor_codes,
    years     = yrs,
    Yt        = Yt,
    Ys        = Ys,
    gap       = Yt - Ys,
    weights   = setNames(as.numeric(syn$solution.w), donor_codes),
    mspe_pre  = mean((Yt[pre_mask]  - Ys[pre_mask])^2),
    mspe_post = mean((Yt[post_mask] - Ys[post_mask])^2),
    att_mean  = mean(Yt[post_mask] - Ys[post_mask]),
    att_last  = (Yt[post_mask] - Ys[post_mask])[sum(post_mask)],
    att_last_pct = ((Yt[post_mask] - Ys[post_mask])[sum(post_mask)] /
                      Ys[post_mask][sum(post_mask)]) * 100,
    last_year = yrs[post_mask][sum(post_mask)],
    prep      = d,
    synth     = syn
  )
}

# ---- 3. Modello principale (totale _T) ---------------------
cat("\n==== (1) MODELLO PRINCIPALE — outcome _T (USD PPP/persona) ====\n")
panel_T <- long |> filter(stype == "_T")
main <- fit_scm(panel_T, TREATED, DONORS)

cat("Pesi:\n"); print(round(main$weights, 4))
cat(sprintf("MSPE pre=%.2f  post=%.2f  ratio=%.2f\n",
            main$mspe_pre, main$mspe_post, main$mspe_post / main$mspe_pre))
cat(sprintf("ATT medio: %.1f  |  ATT %d: %.1f (+%.1f%%)\n",
            main$att_mean, main$last_year, main$att_last, main$att_last_pct))

# Salva pesi
tibble(donor = names(main$weights), peso = main$weights) |>
  arrange(desc(peso)) |>
  write_csv(file.path(OUTDIR, "scm_usdppp_main_weights.csv"))

# Salva gap
tibble(year = main$years, AUS_real = main$Yt, AUS_synth = main$Ys,
       gap = main$gap) |>
  write_csv(file.path(OUTDIR, "scm_usdppp_main_gap.csv"))

# Figura R1 - AUS reale vs sintetico
plot_main <- function(m, file) {
  png(file, width = 1500, height = 850, res = 175)
  par(mar = c(4.2, 4.5, 3.0, 1.2))
  plot(m$years, m$Yt, type = "o", lwd = 2.4, pch = 19, col = "#c0392b",
       xlab = "Anno", ylab = "USD PPP per persona",
       main = "SCM — Spesa pubblica per incapacita (USD PPP/persona)\nAustralia reale vs controllo sintetico",
       ylim = range(c(m$Yt, m$Ys)) * c(0.95, 1.05))
  lines(m$years, m$Ys, type = "o", lwd = 2.0, pch = 15, lty = 2, col = "#2c3e50")
  abline(v = TRT_YEAR, lty = 3, col = "grey50", lwd = 1.4)
  text(TRT_YEAR + 0.3, max(m$Yt) * 0.97, "NDIS 2013", col = "grey50", pos = 4, cex = 0.85)
  legend("topleft", legend = c("Australia (reale)", "Australia sintetica"),
         lwd = c(2.4, 2.0), pch = c(19, 15), lty = c(1, 2),
         col = c("#c0392b", "#2c3e50"), bty = "n")
  grid(col = "grey85", lty = 3)
  dev.off()
}
plot_main(main, file.path(FIGDIR, "Figura_NDIS_USDPPP_main.png"))
plot_main(main, file.path(OUTDIR, "Figura_NDIS_USDPPP_main.png"))

save(main, file = file.path(WSDIR, "robust_usdppp_step1_main.RData"))

# ---- 4. Decomposizione cash vs in-kind ---------------------
cat("\n==== (2) DECOMPOSIZIONE CASH vs IN-KIND ====\n")
decomp <- list()
for (m_code in c("C", "K")) {
  panel_m <- long |> filter(stype == m_code)
  donors_local <- DONORS[sapply(DONORS, function(d) {
    sub <- panel_m |> filter(country == d, year %in% PRE)
    nrow(sub) == length(PRE)   # tutti gli anni pre presenti
  })]
  if (m_code == "K" && "CAN" %in% donors_local) {
    # CAN non ha in-kind nel dataset OECD per TP31
    donors_local <- setdiff(donors_local, "CAN")
  }
  cat(sprintf("\n  -- misura %s, donor pool (%d): %s\n",
              m_code, length(donors_local), paste(donors_local, collapse=", ")))
  res <- fit_scm(panel_m, TREATED, donors_local)
  decomp[[m_code]] <- res
  cat(sprintf("     MSPE pre=%.2f  post=%.2f  ratio=%.2f  |  ATT %d=%.1f (+%.1f%%)\n",
              res$mspe_pre, res$mspe_post, res$mspe_post/res$mspe_pre,
              res$last_year, res$att_last, res$att_last_pct))
}

# Figura R2 - decomp
plot_decomp <- function(c_mod, k_mod, file) {
  png(file, width = 1700, height = 800, res = 170)
  par(mfrow = c(1, 2), mar = c(4.2, 4.5, 3.0, 1.2))
  for (pair in list(list(c_mod, "CASH (C)"), list(k_mod, "IN-KIND (K)"))) {
    m <- pair[[1]]; ttl <- pair[[2]]
    plot(m$years, m$Yt, type = "o", lwd = 2.2, pch = 19, col = "#c0392b",
         xlab = "Anno", ylab = "USD PPP per persona",
         main = paste0(ttl, " — USD PPP/persona"),
         ylim = range(c(m$Yt, m$Ys)) * c(0.9, 1.1))
    lines(m$years, m$Ys, type = "o", lwd = 1.8, pch = 15, lty = 2, col = "#2c3e50")
    abline(v = TRT_YEAR, lty = 3, col = "grey50")
    legend("topleft", legend = c("AUS reale","AUS sint."),
           col = c("#c0392b","#2c3e50"), lwd = c(2.2,1.8),
           lty = c(1,2), pch = c(19,15), bty = "n", cex = 0.8)
    grid(col = "grey85", lty = 3)
  }
  dev.off()
}
plot_decomp(decomp$C, decomp$K, file.path(FIGDIR, "Figura_NDIS_USDPPP_decomp.png"))
plot_decomp(decomp$C, decomp$K, file.path(OUTDIR, "Figura_NDIS_USDPPP_decomp.png"))

save(main, decomp, file = file.path(WSDIR, "robust_usdppp_step2_decomp.RData"))

# ---- 5. Placebo in-space -----------------------------------
cat("\n==== (3) PLACEBO IN-SPACE ====\n")
units_all <- c(TREATED, DONORS)
placebo <- list()
for (u in units_all) {
  donors_pl <- setdiff(units_all, u)
  donors_pl <- donors_pl[sapply(donors_pl, function(d) {
    sub <- panel_T |> filter(country == d, year %in% PRE)
    nrow(sub) == length(PRE)
  })]
  tryCatch({
    res <- fit_scm(panel_T, u, donors_pl)
    placebo[[u]] <- res
    cat(sprintf("  %s: pre=%.1f  post=%.1f  ratio=%.2f\n",
                u, res$mspe_pre, res$mspe_post, res$mspe_post/res$mspe_pre))
  }, error = function(e) cat("  ", u, ": errore -", conditionMessage(e), "\n"))
}

# p-value approssimato (Abadie): rango di MSPE post/pre dell'AUS
ratios <- sapply(placebo, function(m) m$mspe_post / m$mspe_pre)
pre_mspes <- sapply(placebo, function(m) m$mspe_pre)
keep_idx <- which(pre_mspes <= 5 * pre_mspes[TREATED] | names(pre_mspes) == TREATED)
ratios_keep <- ratios[keep_idx]
rank_AUS <- which(names(sort(ratios_keep, decreasing = TRUE)) == TREATED)
pval <- rank_AUS / length(ratios_keep)
cat(sprintf("\nRank AUS = %d/%d  -> p-value approx = %.3f\n",
            rank_AUS, length(ratios_keep), pval))

plot_placebo <- function(plac, file, threshold = 5.0) {
  pre_v <- sapply(plac, function(m) m$mspe_pre)
  ok    <- names(pre_v)[pre_v <= threshold * pre_v[TREATED] | names(pre_v) == TREATED]
  png(file, width = 1500, height = 850, res = 175)
  par(mar = c(4.2, 4.5, 3.0, 1.2))
  yrs <- plac[[TREATED]]$years
  ylim <- range(sapply(plac[ok], function(m) range(m$gap)))
  plot(NA, xlim = range(yrs), ylim = ylim,
       xlab = "Anno", ylab = "Gap Y - Y_synth (USD PPP/persona)",
       main = sprintf("Placebo in-space — AUS rosso (n grigi = %d)\npre-MSPE <= %g * pre-MSPE(AUS)",
                      length(ok)-1, threshold))
  abline(h = 0, col = "black", lwd = 0.6)
  abline(v = TRT_YEAR, col = "grey50", lty = 3)
  for (u in ok) {
    col <- if (u == TREATED) "#c0392b" else "#888888"
    lwd <- if (u == TREATED) 2.6        else 1.0
    lines(plac[[u]]$years, plac[[u]]$gap, col = col, lwd = lwd)
  }
  legend("topleft", legend = "Australia (trattata)", col = "#c0392b", lwd = 2.6, bty = "n")
  grid(col = "grey90", lty = 3)
  dev.off()
}
plot_placebo(placebo, file.path(FIGDIR, "Figura_NDIS_USDPPP_placebo.png"))
plot_placebo(placebo, file.path(OUTDIR, "Figura_NDIS_USDPPP_placebo.png"))

placebo_summary <- tibble(
  unit       = names(ratios),
  pre_mspe   = pre_mspes,
  post_mspe  = sapply(placebo, function(m) m$mspe_post),
  ratio_pp   = ratios,
  kept_in_pval = names(ratios) %in% names(ratios_keep)
) |> arrange(desc(ratio_pp))
write_csv(placebo_summary, file.path(OUTDIR, "scm_usdppp_placebo.csv"))

save(main, decomp, placebo, file = file.path(WSDIR, "robust_usdppp_step3_placebo.RData"))

# ---- 6. Leave-one-out ---------------------------------------
cat("\n==== (4) LEAVE-ONE-OUT ====\n")
loo <- list(BASELINE = main)
for (d in DONORS) {
  donors_loo <- setdiff(DONORS, d)
  res <- fit_scm(panel_T, TREATED, donors_loo)
  loo[[d]] <- res
  cat(sprintf("  senza %s: ATT %d=%.1f (+%.1f%%)  ratio=%.2f\n",
              d, res$last_year, res$att_last, res$att_last_pct,
              res$mspe_post/res$mspe_pre))
}

loo_tab <- tibble(
  removed       = names(loo),
  ATT_mean      = sapply(loo, function(m) m$att_mean),
  ATT_last_year = sapply(loo, function(m) m$last_year),
  ATT_last      = sapply(loo, function(m) m$att_last),
  ATT_last_pct  = sapply(loo, function(m) m$att_last_pct),
  MSPE_pre      = sapply(loo, function(m) m$mspe_pre),
  MSPE_post     = sapply(loo, function(m) m$mspe_post),
  ratio_pp      = sapply(loo, function(m) m$mspe_post / m$mspe_pre)
)
write_csv(loo_tab, file.path(OUTDIR, "scm_usdppp_loo.csv"))
cat("\nTabella LOO:\n"); print(loo_tab)

plot_loo <- function(loo, file) {
  base <- loo$BASELINE
  png(file, width = 1500, height = 850, res = 175)
  par(mar = c(4.2, 4.5, 3.0, 1.2))
  plot(base$years, base$Yt, type = "o", lwd = 2.4, pch = 19, col = "#c0392b",
       xlab = "Anno", ylab = "USD PPP per persona",
       main = "Leave-one-out — robustezza del sintetico (USD PPP/persona)",
       ylim = range(c(base$Yt, sapply(loo, function(m) m$Ys))) * c(0.95, 1.05))
  lines(base$years, base$Ys, lwd = 2.4, col = "#2c3e50")
  for (d in setdiff(names(loo), "BASELINE")) {
    lines(loo[[d]]$years, loo[[d]]$Ys, col = "#888888", lwd = 0.9, lty = 2)
  }
  abline(v = TRT_YEAR, col = "grey50", lty = 3)
  legend("topleft",
         legend = c("AUS reale", "Sintetico baseline", "Sintetici LOO"),
         col = c("#c0392b", "#2c3e50", "#888888"),
         lwd = c(2.4, 2.4, 0.9), lty = c(1, 1, 2), bty = "n", cex = 0.85)
  grid(col = "grey90", lty = 3)
  dev.off()
}
plot_loo(loo, file.path(FIGDIR, "Figura_NDIS_USDPPP_loo.png"))
plot_loo(loo, file.path(OUTDIR, "Figura_NDIS_USDPPP_loo.png"))

# ---- 7. Salvataggio workspace completo ---------------------
save(main, decomp, placebo, loo, placebo_summary, loo_tab,
     file = file.path(WSDIR, "robust_usdppp_finale.RData"))

cat("\n=========================================================\n")
cat(" ROBUSTNESS USD PPP — completato\n")
cat(" Output:\n")
cat("   FIGURE/   -> 4 figure PNG (main, decomp, placebo, loo)\n")
cat("   ROBUSTNESS_USDPPP/  -> CSV pesi / gap / placebo / LOO\n")
cat("   R_WORKSPACE/robust_usdppp_finale.RData\n")
cat("=========================================================\n")
