# test_av_reconcile_prelim.R -------------------------------------------------------
# Runs scripts/av_reconcile_certified.R end to end on synthetic forecast caches
# in throwaway project folders and checks the preliminary-roll calibration
# (PRELIM_TOTAL / PRELIM_NC / PRELIM_PP):
#   - all NULL: uncalibrated; Total excludes NC; label "Total (Certified)"
#   - calibrated: every Total = PRELIM_TOTAL in 2027 and charts/tables agree;
#     real property split in the pipeline's 2027 proportions; PP = script
#     forecast; NC stock 2027 = PRELIM_NC then stock x (1 + g) + CSV flows;
#     2028-2031 growth = the uncalibrated pipeline's
#   - PRELIM_PP override; input validation
# Needs tidyverse (the script's own dependency).
#
#   Rscript scripts/ml/test_av_reconcile_prelim.R
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(here) })
script <- here::here("scripts", "av_reconcile_certified.R")
rscript <- file.path(R.home("bin"), "Rscript")
PT <- 294452027000; PN <- 1631730000

make_root <- function(tag, total = NULL, nc = NULL, pp = NULL) {
  root <- file.path(tempdir(), paste0("recon_", tag))
  unlink(root, recursive = TRUE)
  dir.create(file.path(root, "data", "cache"), recursive = TRUE)
  dir.create(file.path(root, "data", "wrangled"))
  dir.create(file.path(root, "scripts"))
  file.create(file.path(root, ".here"))
  src <- readLines(script)
  setv <- function(src, name, val) {
    i <- grep(paste0("^", name, "\\s*<- NULL"), src)
    stopifnot(length(i) == 1L)
    if (!is.null(val)) src[i] <- paste0(name, " <- ", format(val, scientific = FALSE))
    src
  }
  src <- setv(src, "PRELIM_TOTAL", total)
  src <- setv(src, "PRELIM_NC", nc)
  src <- setv(src, "PRELIM_PP", pp)
  writeLines(src, file.path(root, "scripts", "av_reconcile_certified.R"))
  # synthetic matched-parcel forecasts, distinct growth per track x scenario
  set.seed(2)
  g <- list(res   = c(baseline = .03,  optimistic = .04, pessimistic = .01),
            com   = c(baseline = .02,  optimistic = .03, pessimistic = -.01),
            condo = c(baseline = .025, optimistic = .035, pessimistic = .005))
  for (tr in names(g)) for (sc in names(g[[tr]])) {
    d <- CJ(parcel_id = sprintf("%04d", 1:200), tax_yr = 2026:2031)
    d[, base := runif(200, 2e5, 2e6)[match(parcel_id, unique(parcel_id))]]
    d[, appr_land_val := fifelse(tax_yr == 2026, base * .4, NA_real_)]
    d[, appr_imps_val := fifelse(tax_yr == 2026, base * .6, NA_real_)]
    d[, appr_land_val_filled := base * .4 * (1 + g[[tr]][[sc]])^(tax_yr - 2026)]
    d[, appr_imps_val_filled := base * .6 * (1 + g[[tr]][[sc]] + .002 * (tax_yr - 2026))^(tax_yr - 2026)]
    saveRDS(d, file.path(root, "data", "cache",
                         sprintf("panel_tbl_2006_2031_forecasted_%s_%s.rds", sc, tr)))
  }
  write.csv(data.frame(tax_year = 2026:2031,
                       baseline    = c(4.51e9, 3.0e9, 3.2e9, 3.4e9, 3.5e9, 3.6e9),
                       optimistic  = c(4.51e9, 3.3e9, 3.5e9, 3.7e9, 3.8e9, 3.9e9),
                       pessimistic = c(4.51e9, 2.7e9, 2.9e9, 3.0e9, 3.1e9, 3.2e9)),
            file.path(root, "data", "wrangled",
                      "OERF_New_Construction_Forecast_20261001.csv"), row.names = FALSE)
  root
}

run <- function(root) {
  owd <- setwd(root); on.exit(setwd(owd))
  log <- suppressWarnings(system2(rscript, file.path("scripts", "av_reconcile_certified.R"),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(log, "status")
  w <- file.path(root, "data", "wrangled")
  rd <- function(p) { f <- list.files(w, pattern = p, full.names = TRUE)
                      if (length(f)) fread(f[1]) else NULL }
  list(ok = is.null(status) || status == 0, log = log,
       by_type = rd("^av_certified_by_type_.*\\.csv$"),
       total   = rd("^av_certified_total_summary_.*\\.csv$"))
}
sc3 <- c("baseline", "optimistic", "pessimistic")
val <- function(bt, tr, yr, sc) bt[track == tr & tax_yr == yr][[sc]]

# ---- 1. all NULL: uncalibrated -------------------------------------------------
A <- run(make_root("null"))
stopifnot(A$ok, !any(grepl("calibration ON", A$log)),
          all(A$by_type[track == "Total", series] == "Total (Certified)"),
          !"nc" %in% A$by_type$track)
for (sc in sc3)   # summary total = chart Total + NC stock (Total excludes NC)
  stopifnot(val(A$by_type, "Total", 2027, sc) < A$total[tax_yr == 2027][[sc]])
cat("PASS  PRELIM_* NULL: uncalibrated, Total (Certified) excludes NC, summary adds NC stock\n")

# ---- 2. calibrated -------------------------------------------------------------
B <- run(make_root("cal", total = PT, nc = PN))
stopifnot(B$ok, any(grepl("calibration ON", B$log)),
          all(B$by_type[track == "Total", series] == "Total incl. new construction"))
real <- c("res", "com", "condo")
for (sc in sc3) {
  # every Total hits PRELIM_TOTAL in 2027, and chart Total = summary Total
  stopifnot(abs(val(B$by_type, "Total", 2027, sc) - PT) < 1,
            abs(B$total[tax_yr == 2027][[sc]] - PT) < 1,
            all(abs(B$by_type[track == "Total" & tax_yr >= 2026][order(tax_yr)][[sc]] -
                    B$total[order(tax_yr)][[sc]]) < 1))
  # NC 2027 = PRELIM_NC; PP = script forecast (unchanged); existing = total - NC
  stopifnot(abs(val(B$by_type, "nc", 2027, sc) - PN) < 1,
            all(abs(B$by_type[track == "pp"][order(tax_yr)][[sc]] -
                    A$by_type[track == "pp"][order(tax_yr)][[sc]]) < 1))
  # one scale factor across res / com / condo in 2027 and after; history untouched
  k <- vapply(real, function(tr) val(B$by_type, tr, 2027, sc) / val(A$by_type, tr, 2027, sc), 0)
  stopifnot(max(k) - min(k) < 1e-12)
  for (tr in real) {
    a <- A$by_type[track == tr][order(tax_yr)]; b <- B$by_type[track == tr][order(tax_yr)]
    stopifnot(all(abs(a[tax_yr <= 2026][[sc]] - b[tax_yr <= 2026][[sc]]) < 1),
              all(abs(b[tax_yr >= 2027][[sc]] / a[tax_yr >= 2027][[sc]] - k[[1]]) < 1e-12))
  }
  # NC stock recursion: stock(2028) = PRELIM_NC x (1 + g_real 2028) + flow 2028
  r27 <- sum(vapply(real, function(tr) val(B$by_type, tr, 2027, sc), 0))
  r28 <- sum(vapply(real, function(tr) val(B$by_type, tr, 2028, sc), 0))
  flow28 <- c(baseline = 3.2e9, optimistic = 3.5e9, pessimistic = 2.9e9)[[sc]]
  stopifnot(abs(val(B$by_type, "nc", 2028, sc) - (PN * r28 / r27 + flow28)) < 1)
}
stopifnot(any(grepl("NC 2027 baseline\\s+CSV flow 3,000,000,000 -> PRELIM_NC 1,631,730,000", B$log)),
          sum(grepl("\\(= PRELIM_TOTAL\\)", B$log)) == 3L,
          any(grepl("Calibrated path by type and total", B$log)),
          any(grepl("TY2027 by type, baseline", B$log)))
cat("PASS  calibrated: every Total = PRELIM_TOTAL in 2027, charts = tables, one real-property\n",
    "      scale factor, PP unchanged, NC 2027 = PRELIM_NC then compounds + CSV flows,\n",
    "      2028-2031 growth = pipeline, CSV 2027 NC flow printed next to PRELIM_NC\n", sep = "")

# ---- 3. PRELIM_PP override ---------------------------------------------------------
C <- run(make_root("pp", total = PT, nc = PN, pp = 9.5e9))
stopifnot(C$ok)
for (sc in sc3) {
  stopifnot(abs(val(C$by_type, "pp", 2027, sc) - 9.5e9) < 1,
            abs(val(C$by_type, "Total", 2027, sc) - PT) < 1)
  a <- A$by_type[track == "pp"][order(tax_yr)][[sc]]; c <- C$by_type[track == "pp"][order(tax_yr)][[sc]]
  stopifnot(length(a) == 10L,                                       # 2022-2031
            all(abs(diff(log(a))[6:9] - diff(log(c))[6:9]) < 1e-12))   # 2028-31 growth kept
}
cat("PASS  PRELIM_PP: PP 2027 = override, PP growth after 2027 kept, Total still PRELIM_TOTAL\n")

# ---- 4. validation -----------------------------------------------------------------
D <- run(make_root("half", total = PT))
stopifnot(!D$ok, any(grepl("needs BOTH PRELIM_TOTAL and PRELIM_NC", D$log)))
E <- run(make_root("billions", total = 294.452, nc = PN))
stopifnot(!E$ok, any(grepl("give dollars, not billions", E$log)))
cat("PASS  validation: one of TOTAL/NC alone stops; a total in billions stops\n")

cat("\nAll av_reconcile prelim checks passed.\n")
