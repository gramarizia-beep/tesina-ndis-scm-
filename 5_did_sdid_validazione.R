# ============================================================
# SCRIPT 5 — VALIDAZIONE ECONOMETRICA: DiD pesato + SDiD
# Integrazione del SCM (Fasi 1 e 2) con metodi DiD per produrre
# standard error tradizionali, t-stat e p-value classici.
# ============================================================
# Riferimenti:
#  - Arkhangelsky et al. (2021) AER 111(12): Synthetic DiD
#  - Goodman-Bacon (2021) J. Econometrics: DiD con timing variabile
#  - Callaway & Sant'Anna (2021) J. Econometrics: DiD multi-periodo
# ============================================================

# ---- 0. Setup ----------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("tidyverse", "fixest", "broom")
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# synthdid (Stanford) NON è su CRAN, va da GitHub
has_synthdid <- requireNamespace("synthdid", quietly = TRUE)
if (!has_synthdid) {
  cat("Pacchetto 'synthdid' non disponibile. Tento installazione da GitHub...\n")
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools", repos = "https://cloud.r-project.org")
  }
  tryCatch({
    devtools::install_github("synth-inference/synthdid", upgrade = "never", quiet = TRUE)
    has_synthdid <- requireNamespace("synthdid", quietly = TRUE)
  }, error = function(e) {
    cat("Installazione synthdid fallita:", conditionMessage(e), "\n")
    cat("Proseguo solo con DiD pesato (fixest).\n")
  })
}
if (has_synthdid) library(synthdid)


BASE  <- "~/Downloads/TESINA_NDIS_AUSTRALIA"
DATA  <- file.path(BASE, "DATI", "SOCKS_INCAPACITY_PPPPC.csv")
WSDIR <- file.path(BASE, "R_WORKSPACE")
FIG   <- file.path(BASE, "FIGURE")


# ---- 1. Carica i dati e il workspace SCM -------------------
raw <- read_csv(DATA, show_col_types = FALSE)
long <- raw |>
  filter(!is.na(OBS_VALUE), PRICE_BASE == "V") |>
  transmute(country = REF_AREA,
            year    = as.integer(TIME_PERIOD),
            stype   = SPENDING_TYPE,
            value   = as.numeric(OBS_VALUE))

panel_T <- long |> filter(stype == "_T") |>
  filter(year >= 2000, year <= 2021) |>
  mutate(post = as.integer(year >= 2013),
         treated = as.integer(country == "AUS"))

# Carica modello SCM per i pesi
load(file.path(WSDIR, "robust_usdppp_finale.RData"))
scm_weights <- main$weights  # pesi dei donor in Fase 2

cat("\n=== Pesi SCM del sintetico Australia (Fase 2) ===\n")
print(round(scm_weights, 4))


# ---- 2. DiD CLASSICA (unweighted) come baseline ------------
# Modello: Y_it = α_i + γ_t + β·(treated_i × post_t) + ε_it
cat("\n=== Modello 1: DiD CLASSICA (unweighted) ===\n")
m1 <- feols(value ~ I(treated * post) | country + year,
            data = panel_T,
            cluster = ~country)
print(summary(m1))


# ---- 3. DiD PESATA con i pesi SCM ---------------------------
# I pesi: Australia ha peso 1 (paese trattato), i donor hanno
# il loro peso SCM, gli altri (peso zero nel SCM) non entrano.
# Costruiamo il vettore di pesi a livello paese-anno.
pesi_paese <- tibble(country = c("AUS", names(scm_weights)),
                     w = c(1, as.numeric(scm_weights)))
panel_w <- panel_T |>
  left_join(pesi_paese, by = "country") |>
  filter(!is.na(w), w > 0)

cat("\n=== Modello 2: DiD PESATA con pesi SCM ===\n")
cat("Paesi inclusi (peso > 0):\n")
print(pesi_paese |> filter(w > 0) |> arrange(desc(w)))

m2 <- feols(value ~ I(treated * post) | country + year,
            data = panel_w,
            weights = ~w,
            cluster = ~country)
print(summary(m2))


# ---- 4. Confronto con stima SCM ---------------------------
cat("\n=== CONFRONTO ===\n")
beta1 <- coef(m1)[1]
se1   <- se(m1)[1]
p1    <- pvalue(m1)[1]
beta2 <- coef(m2)[1]
se2   <- se(m2)[1]
p2    <- pvalue(m2)[1]
att_scm <- main$ATT_mean

tab <- tribble(
  ~Modello,                ~Stima_USD_persona, ~SE,        ~`P-value`, ~Note,
  "SCM Fase 2 (ATT medio)", round(att_scm, 1),  NA_real_,   NA_real_,   "Inferenza permutation-based à la Abadie",
  "DiD classica (unweighted)", round(beta1, 1), round(se1, 1), round(p1, 4), "Cluster-robust SE, 10 paesi",
  "DiD pesata con pesi SCM",   round(beta2, 1), round(se2, 1), round(p2, 4), "Cluster-robust SE, donor pool ottimale"
)
print(tab)


# ---- 5. Synthetic DiD (Arkhangelsky et al. 2021) ----------
if (has_synthdid) {
  cat("\n=== Modello 3: SYNTHETIC DiD (Arkhangelsky et al. 2021) ===\n")

  # synthdid richiede una matrice paese x anno
  # Australia = unità trattata, tutti gli altri = controlli
  mat <- panel_T |>
    select(country, year, value) |>
    pivot_wider(names_from = year, values_from = value) |>
    column_to_rownames("country") |>
    as.matrix()

  # Riordina: AUS in ultima posizione, anni 2000-2021
  paesi <- c(setdiff(rownames(mat), "AUS"), "AUS")
  mat <- mat[paesi, ]

  # N0 = numero unità controllo, T0 = numero periodi pre
  N0 <- length(paesi) - 1
  T0 <- which(colnames(mat) == "2012")

  # Stima
  sdid_est <- synthdid_estimate(mat, N0 = N0, T0 = T0)
  cat("Effetto SDiD stimato (USD PPP/persona):\n")
  print(sdid_est)

  # Inferenza placebo per SE
  sdid_se <- sqrt(vcov(sdid_est, method = "placebo"))
  cat(sprintf("Standard error (placebo-based): %.1f\n", sdid_se))
  cat(sprintf("t-statistic: %.2f\n", as.numeric(sdid_est) / sdid_se))

  # Aggiungi alla tabella
  tab <- tab |> add_row(
    Modello = "Synthetic DiD (Arkhangelsky 2021)",
    Stima_USD_persona = round(as.numeric(sdid_est), 1),
    SE = round(sdid_se, 1),
    `P-value` = round(2 * (1 - pnorm(abs(as.numeric(sdid_est) / sdid_se))), 4),
    Note = "Placebo SE su 9 unità di controllo"
  )

  cat("\n=== TABELLA FINALE DI VALIDAZIONE ===\n")
  print(tab)

  # Salva grafico SDiD
  png(file.path(FIG, "Figura_SDiD.png"),
      width = 1700, height = 950, res = 175)
  plot(sdid_est, overlay = 1)
  dev.off()
  cat("\nGrafico SDiD salvato in: FIGURE/Figura_SDiD.png\n")
} else {
  cat("\n[!] Pacchetto synthdid non disponibile. Modello SDiD saltato.\n")
  cat("    Le stime DiD classica e pesata sono comunque sufficienti.\n")
  cat("\n=== TABELLA FINALE DI VALIDAZIONE ===\n")
  print(tab)
}


# ---- 6. Salva la tabella in CSV per la tesi ----------------
write_csv(tab, file.path(BASE, "ROBUSTNESS_USDPPP", "validazione_did_sdid.csv"))
cat("\nTabella salvata in: ROBUSTNESS_USDPPP/validazione_did_sdid.csv\n")
