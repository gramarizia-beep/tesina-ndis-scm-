# La transizione incerta — Script R

Codice di replicazione della tesina **"La transizione incerta: la nuova riforma dell'invalidità tra retorica dell'autonomia e depauperamento del welfare"** — esame di *Metodi quantitativi delle politiche pubbliche*, Sapienza Università di Roma (Scienze dell'Amministrazione, Facoltà di Scienze Politiche).

**Autrice:** Maria Grazia Zuccarini — A.A. 2025/2026

## Che cosa contiene

L'analisi stima l'effetto causale del NDIS australiano (attuazione dell'art. 19 della Convenzione ONU sui Diritti delle Persone con Disabilità) sulla spesa pubblica per disabilità tramite **Synthetic Control Method**, con validazione DiD/Synthetic DiD, estensione comparata a sei paesi europei e applicazione al caso italiano (D.Lgs. 62/2024). Risultato centrale: **+1,07 punti di PIL al 2021, interamente sulla componente servizi** — la riforma sostituisce i trasferimenti monetari, non li integra.

Ventuno script sequenziali, ventitré figure. Tutti i risultati della tesina sono riproducibili da questo repository.

## Requisiti

R (≥ 4.3) con i pacchetti: `tidyverse`, `Synth`, `tidysynth`, `fixest`, `scales`, `patchwork`, `eurostat`, `ggrepel`; `synthdid` (da GitHub: `devtools::install_github("synth-inference/synthdid")`).

## Come riprodurre l'analisi

1. Eseguire `1_download_dati.R` (scarica i dati OECD SOCX TP31 per Australia e i 9 paesi donor).
2. Eseguire gli script in ordine numerico. Gli script 7b, 11–15 scaricano dati Eurostat/ISTAT aggiuntivi.
3. Le figure vengono salvate in `FIGURE/`.

## Gli script

### Analisi causale — Australia (Capitoli 2–6)
| Script | Contenuto |
|---|---|
| `1_download_dati.R` | Download dati OECD SOCX TP31 (Australia + 9 donor) |
| `2a_SCM_pil_fase1.R` | SCM Fase 1 (% PIL): modello principale, decomposizione cash/in-kind, placebo in-space e in-time, leave-one-out, anno-pivot 2016 (Figure 1–9) |
| `2_SCM_usdppp_robustness.R` | SCM Fase 2 (USD PPP pro capite) + decomposizione + placebi + LOO + robustness 2016 |
| `3_controllo_negativo_simmetrico.R` | Gap Australia vs Finlandia sovrapposti |
| `4_simmetria_causale.R` | SCM speculare con Finlandia trattata (validazione metodologica) |
| `5_did_sdid_validazione.R` | DiD pesato (fixest) + Synthetic DiD (synthdid) |
| `8_dsp_ndis_substitution.R` | Sostituzione DSP → NDIS, Australia 2001–2024 |

### Pattern europeo (Capitolo 7)
| Script | Contenuto |
|---|---|
| `7_indice_ohchr_article19.R` | Framework indice OHCHR + popolamento OECD |
| `7b_indice_ohchr_eurostat.R` | Download Eurostat per gli indicatori O1, O2, O3 |
| `7c_scatter_paradosso_italiano.R` | Scatter cross-country: il paradosso italiano |
| `7d_caregiver_burden_italia.R` | Prevalenza dei caregiver in Europa |
| `9_spagna_barriere_amministrative.R` | Barriere procedurali del SAAD spagnolo (plazo suspensivo, RDL 20/2012) |
| `10_multi_country_pattern.R` | Pattern europeo della sostituzione: 7 paesi |

### Caso italiano (Capitolo 8)
| Script | Contenuto |
|---|---|
| `5b_did_italia_province_pilota.R` | DiD Italia: province pilota vs controllo, dati CGIL 30/05/2026 (−13,1% vs +1,0%) |
| `6_stima_fiscale_italia.R` | Proiezione fiscale Italia 2027–2031 (perimetro previdenziale) |

### Framework di monitoraggio § 8.6
| Script | Contenuto |
|---|---|
| `11_dim1_input_pubblico_fna.R` | DIM 1 — Fondo Non Autosufficienza: nominale, reale, pro capite ultra-65 |
| `12_dim2_output_inps_accoglimento.R` | DIM 2 — Tasso di accoglimento prime istanze INPS, pilota vs nazionale |
| `13_dim3_assorbimento_ets_coprogettazione.R` | DIM 3 — Esternalizzazione dei servizi disabilità verso gli ETS (ISTAT) |
| `14_dim4_outcomes_liste_attesa.R` | DIM 4 — Giorni di attesa per i progetti individuali di vita |
| `15_dim5_vincolo_offerta_lavoro.R` | DIM 5 — Vacancy rate Excelsior + gap salariale CCNL (binding constraint) |
| `16_dashboard_5_dimensioni.R` | Dashboard sintetico delle cinque dimensioni |

### Script diagnostici (cartella `DIAGNOSTICA/`, non parte dell'analisi)
| Script | Contenuto |
|---|---|
| `0_diagnostica_pool.R` | Verifica dei pool placebo di tutti i modelli SCM |
| `0b_trova_causa_pool.R` | Conteggio NA per donor (esclusione Canada/USA dal modello in-kind) |
| `0c_controprova_uk_svezia.R` | Robustezza: SCM con e senza Regno Unito e Svezia nel pool |
| `0d_verifica_121.R` | Riverifica integrale del MSPE ratio 121 (rank, p-value, z-score, denominatore) |

## Fonti dei dati

- **OECD SOCX TP31** (Incapacity-related benefits), 2000–2021 — scaricati dallo script 1
- **Eurostat** (ESSPROS, EHIS) — scaricati via pacchetto `eurostat`
- **CGIL / Osservatorio Previdenza**, comunicato 30/05/2026, su registri amministrativi INPS (vedi disclosure metodologica nel § 2 della tesina)
- **Osservatorio INPS** sulle pensioni liquidate (2025), per la proiezione fiscale

## Citazione

> Zuccarini, M.G. (2026). *La transizione incerta: la nuova riforma dell'invalidità tra retorica dell'autonomia e depauperamento del welfare*. Tesina per l'esame di Metodi quantitativi delle politiche pubbliche, Sapienza Università di Roma.
