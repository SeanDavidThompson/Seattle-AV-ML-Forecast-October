###############################################################################
# overnight_summary.R -- write data/outputs_nwmls_growth/AB_SUMMARY.txt from
# what overnight_monday.R left behind (status.csv, step logs, result files,
# model and forecast caches).  Read-only on everything else.  Tolerates
# missing pieces: anything a failed step did not produce is reported "n/a".
#
#   source("ad_hoc/overnight_summary.R"); write_ab_summary("data/outputs_nwmls_growth/logs")
#
# STYLE: no $ operator. Use x[["name"]].
###############################################################################

suppressPackageStartupMessages(library(data.table))

ab_dirs <- function(root = ".") list(
  level  = list(cache = file.path(root, "data", "cache"),
                model = file.path(root, "data", "model")),
  growth = list(cache = file.path(root, "data", "cache_nwmls_growth"),
                model = file.path(root, "data", "model_nwmls_growth")))

ab_latest <- function(dir, prefix) {
  f <- list.files(dir, pattern = paste0("^", prefix, "_.*\\.rds$"), full.names = TRUE)
  if (!length(f)) return(NULL)
  f[which.max(file.info(f)[["mtime"]])]
}

# Residential total AV (av_fcst_summary definition), matched growth, 2027
# method counts and forecast-year NA counts from one forecasted panel.
ab_res_stats <- function(cache, scenario) {
  f <- file.path(cache, paste0("panel_tbl_2006_2031_forecasted_", scenario, "_res.rds"))
  if (!file.exists(f)) return(NULL)
  d <- as.data.table(readRDS(f))
  pick <- function(a, b) if (a %in% names(d)) fifelse(!is.na(d[[a]]), d[[a]], d[[b]]) else d[[b]]
  lf <- pick("appr_land_val_filled", "appr_land_val")
  im <- pick("appr_imps_val_filled", "appr_imps_val")
  d[, av := fifelse(is.na(lf), 0, as.numeric(lf)) + fifelse(is.na(im), 0, as.numeric(im))]
  tot <- d[tax_yr %in% 2026:2031, .(total_av = sum(av)), by = tax_yr][order(tax_yr)]
  m27 <- d[tax_yr == 2027, .(land_actual = sum(land_method == "actual", na.rm = TRUE),
                             impr_actual = sum(impr_method == "actual", na.rm = TRUE),
                             land_delta  = sum(land_method == "delta",  na.rm = TRUE),
                             impr_delta  = sum(impr_method == "delta",  na.rm = TRUE))]
  nas <- d[tax_yr %in% 2027:2031, .(total_na = sum(is.na(total_assessed_filled))), by = tax_yr]
  list(total = tot[, scenario := scenario], m27 = m27[, scenario := scenario],
       na = nas[, scenario := scenario])
}

ab_warnings <- function(logs) {
  w <- unlist(lapply(logs[file.exists(logs)], function(f) {
    x <- readLines(f, warn = FALSE)
    x[grepl("^Warning", x)]
  }))
  unique(gsub("[0-9]+", "#", w))
}

write_ab_summary <- function(log_dir, root = ".") {
  out <- file.path(root, "data", "outputs_nwmls_growth", "AB_SUMMARY.txt")
  L <- character(0)
  add <- function(...) L <<- c(L, paste0(...))
  tbl <- function(x) L <<- c(L, capture.output(print(x, row.names = FALSE)), "")
  rd  <- function(f) { p <- file.path(log_dir, f); if (file.exists(p)) readRDS(p) else NULL }
  D <- ab_dirs(root)

  add("NWMLS level vs growth A/B summary - written ", format(Sys.time()))
  add(strrep("=", 72)); add("")

  # ---- status ----------------------------------------------------------------
  st <- tryCatch(read.csv(file.path(log_dir, "status.csv"), stringsAsFactors = FALSE),
                 error = function(e) NULL)
  add("STEP STATUS"); add(strrep("-", 72))
  if (!is.null(st)) tbl(st[, c("id", "title", "status", "elapsed_min", "started")])
  else add("status.csv missing", "")
  if (!is.null(st)) add("Total elapsed: ", round(sum(st[["elapsed_min"]], na.rm = TRUE) / 60, 2), " h", "")

  # ---- NWMLS files --------------------------------------------------------------
  add("NWMLS FILE READ BY EACH RUN"); add(strrep("-", 72))
  s0 <- rd("s0_result.rds")
  if (!is.null(s0)) {
    add("Vintage resolved for each scenario (s0):"); tbl(s0[["vintages"]])
    add("data/cache NWMLS forecast cache, 12-month mean price, before vs after s0:")
    tbl(s0[["compare"]])
    if (any(s0[["compare"]][["changed"]], na.rm = TRUE))
      add("NOTE: the previous production NWMLS cache held DIFFERENT forecast prices ",
          "(old modified-time file pick). Previous files: logs/prev_nwmls_fcst_*.rds", "")
  }
  logs <- list.files(log_dir, pattern = "\\.log$", full.names = TRUE)
  for (f in logs) {
    x <- readLines(f, warn = FALSE)
    hit <- unique(trimws(x[grepl("Reading NWMLS from|NWMLS forecast cached to|nwmls cache", x)]))
    add(sprintf("  %-22s %s", sub("\\.log$", "", basename(f)),
                if (length(hit)) paste(hit, collapse = " | ")
                else "no NWMLS read (used the cached NWMLS forecast / not applicable)"))
  }
  add("")

  # ---- CV ---------------------------------------------------------------------------
  add("CV (rolling-CV RMSE, newest residential delta models)"); add(strrep("-", 72))
  cv <- do.call(rbind, lapply(c("lgb_land_delta_cv", "lgb_impr_delta_cv"), function(p) {
    fl <- ab_latest(D[["level"]][["model"]], p); fg <- ab_latest(D[["growth"]][["model"]], p)
    ol <- if (!is.null(fl)) readRDS(fl) else NULL
    og <- if (!is.null(fg)) readRDS(fg) else NULL
    md <- function(o) { m <- attr(o, "nwmls_features", exact = TRUE); if (is.null(m)) "level (unstamped)" else m }
    data.frame(model = p,
               level_file = if (is.null(fl)) NA else basename(fl),
               level_mode = if (is.null(ol)) NA else md(ol),
               level_cv = if (is.null(ol)) NA_real_ else ol[["cv_rmse"]],
               growth_file = if (is.null(fg)) NA else basename(fg),
               growth_mode = if (is.null(og)) NA else md(og),
               growth_cv = if (is.null(og)) NA_real_ else og[["cv_rmse"]])
  }))
  cv[["pct_change"]] <- round(100 * (cv[["growth_cv"]] / cv[["level_cv"]] - 1), 2)
  tbl(cv)

  # ---- sensitivity ------------------------------------------------------------------
  add("SENSITIVITY (NWMLS price x0.95 / x1.05 in forecast months)"); add(strrep("-", 72))
  sens <- list()
  for (m in c("level", "growth")) {
    r <- rd(if (m == "level") "e1_result.rds" else "e2_result.rds")
    sens[[m]] <- if (is.null(r)) NA else isTRUE(r[["pass"]])
    add(sprintf("  %-6s : %s", m, if (is.null(r)) "n/a (step did not finish)"
                else if (isTRUE(r[["pass"]])) "PASS" else "FAIL"))
    if (!is.null(r)) {
      t <- as.data.frame(r[["table"]])
      t <- t[, intersect(c("tax_yr", "total_av_minus5", "total_av_base", "total_av_plus5",
                           "pct_minus5_vs_base", "pct_plus5_vs_base"), names(t))]
      for (cn in grep("^total_av", names(t), value = TRUE)) t[[cn]] <- round(t[[cn]] / 1e9, 3)
      for (cn in grep("^pct_", names(t), value = TRUE)) t[[cn]] <- round(100 * t[[cn]], 3)
      tbl(t)
    }
  }
  add("")

  # ---- residential totals by mode -----------------------------------------------
  add("RESIDENTIAL TOTAL AV BY MODE ($B; pessimistic <= baseline in 2028-2031?)")
  add(strrep("-", 72))
  stats <- list(); pes_ok <- list(); m27 <- list(); nas <- list()
  for (m in c("level", "growth")) {
    ss <- lapply(c("baseline", "optimistic", "pessimistic"),
                 function(sc) ab_res_stats(D[[m]][["cache"]], sc))
    names(ss) <- c("baseline", "optimistic", "pessimistic")
    tt <- rbindlist(lapply(ss, function(z) if (is.null(z)) NULL else z[["total"]]))
    m27[[m]] <- rbindlist(lapply(ss, function(z) if (is.null(z)) NULL else z[["m27"]]))
    nas[[m]] <- rbindlist(lapply(ss, function(z) if (is.null(z)) NULL else z[["na"]]))
    add("  ", m, ":")
    if (nrow(tt)) {
      w <- dcast(tt, tax_yr ~ scenario, value.var = "total_av")
      # compare in dollars, then round for display
      if (all(c("baseline", "pessimistic") %in% names(w))) {
        w[, pes_le_base := pessimistic <= baseline]
        jj <- w[tax_yr %in% 2028:2031]
        pes_ok[[m]] <- nrow(jj) == 4L && all(jj[["pes_le_base"]])
      } else pes_ok[[m]] <- NA
      for (cn in intersect(c("baseline", "optimistic", "pessimistic"), names(w)))
        set(w, j = cn, value = round(w[[cn]] / 1e9, 3))
      tbl(w)
    } else { add("    n/a"); pes_ok[[m]] <- NA }
  }

  add("TY2027 ANCHORING COUNTS (residential)"); add(strrep("-", 72))
  for (m in names(m27)) { add("  ", m, ":"); if (nrow(m27[[m]])) tbl(m27[[m]]) else add("    n/a") }

  # ---- decision rule ------------------------------------------------------------------
  add("DECISION RULE (NWMLS_GROWTH_DECISION.md)"); add(strrep("-", 72))
  v <- function(x) if (is.na(x)) "n/a" else if (isTRUE(x)) "PASS" else "FAIL"
  c1 <- sens[["growth"]]
  c2 <- pes_ok[["growth"]]
  c3 <- if (all(is.finite(cv[["pct_change"]]))) all(cv[["pct_change"]] <= 3) else NA
  # 4: no hard stop in the growth runs, no warning text absent from the
  # level runs, NA counts and TY2027 anchoring identical to level.
  gst <- if (is.null(st)) NA else st[["status"]][match(c("d", "f1", "f2"), st[["id"]])]
  no_stop <- if (is.null(st)) NA else all(gst == "ok")
  wl <- ab_warnings(file.path(log_dir, c("a.log", "g1.log", "g2.log")))
  wg <- ab_warnings(file.path(log_dir, c("d.log", "f1.log", "f2.log")))
  new_w <- setdiff(wg, wl)
  same_na <- if (nrow(nas[["level"]]) && nrow(nas[["growth"]]))
    isTRUE(all.equal(nas[["level"]][order(scenario, tax_yr)], nas[["growth"]][order(scenario, tax_yr)],
                     check.attributes = FALSE)) else NA
  same_anchor <- if (nrow(m27[["level"]]) == 3L && nrow(m27[["growth"]]) == 3L)
    isTRUE(all.equal(m27[["level"]][order(scenario), .(scenario, land_actual, impr_actual)],
                     m27[["growth"]][order(scenario), .(scenario, land_actual, impr_actual)],
                     check.attributes = FALSE)) else NA
  c4 <- if (any(is.na(c(no_stop, same_na, same_anchor)))) NA else
    no_stop && !length(new_w) && same_na && same_anchor
  add(sprintf("  1. Sensitivity (growth passes; level shown)   : %s   [level: %s]", v(c1), v(sens[["level"]])))
  add(sprintf("  2. Scenarios (growth pes <= base, 2028-2031)  : %s   [level: %s]", v(c2), v(pes_ok[["level"]])))
  add(sprintf("  3. CV (growth <= level + 3%%, land and impr)   : %s   [%s]", v(c3),
              paste0(cv[["model"]], " ", cv[["pct_change"]], "%", collapse = ", ")))
  add(sprintf("  4. Clean run                                   : %s", v(c4)))
  add(sprintf("       growth steps d/f1/f2 all ok: %s | NA counts = level: %s | TY2027 anchoring = level: %s | new warning lines: %d",
              v(no_stop), v(same_na), v(same_anchor), length(new_w)))
  if (length(new_w)) { add("       warnings in growth logs not seen in level logs:"); for (x in new_w) add("         ", x) }
  all4 <- c(c1, c2, c3, c4)
  add("")
  add("  RULE: ", if (any(is.na(all4))) "INCOMPLETE - some evidence missing; ship level unless resolved"
      else if (all(all4)) "ALL FOUR PASS - growth may be used for October"
      else "NOT MET - ship level")
  add("")
  add("Optimistic vs baseline is not judged (optimistic NWMLS price input is below baseline).")

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(L, out)
  cat(L, sep = "\n")
  invisible(out)
}
