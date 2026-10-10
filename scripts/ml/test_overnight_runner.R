# test_overnight_runner.R ---------------------------------------------------------
# ad_hoc/overnight_monday.R mechanics with dummy steps in a temporary copy of
# the repo layout: each step runs in its own Rscript process, logs to its own
# file, a failure skips only its dependants, and the dry run parses every
# real step script without running any.
#
#   Rscript scripts/ml/test_overnight_runner.R
# -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(here))
root <- file.path(tempdir(), "overnight_runner_test")
unlink(root, recursive = TRUE)
dir.create(file.path(root, "scripts", "ml"), recursive = TRUE)
dir.create(file.path(root, "ad_hoc"))
file.copy(list.files(here::here("scripts", "ml"), full.names = TRUE),
          file.path(root, "scripts", "ml"))
file.copy(list.files(here::here("ad_hoc"), pattern = "\\.R$", full.names = TRUE),
          file.path(root, "ad_hoc"))
logs <- file.path(root, "data", "outputs_nwmls_growth", "logs")
owd <- setwd(root)

# ---- 1. dry run over the real steps ------------------------------------------
OVERNIGHT_DRY_RUN <- TRUE
out <- capture.output(source(file.path("ad_hoc", "overnight_monday.R")))
stopifnot(any(grepl("parsed OK. Nothing was run", out)),
          !file.exists(file.path(logs, "status.csv")),
          length(list.files(file.path(logs, "steps"), pattern = "\\.R$")) == 18L)
rm(OVERNIGHT_DRY_RUN)
cat("PASS  dry run: 18 real step scripts written and parsed, nothing executed\n")

# ---- 2. dummy steps: isolation, logs, dependency skipping --------------------
OVERNIGHT_STEPS_OVERRIDE <- list(
  list(id = "x1", title = "ok", deps = character(0), always = TRUE,
       code = 'cat("pid", Sys.getpid(), "\\n"); warning("w1")'),
  list(id = "x2", title = "fails", deps = "x1", always = TRUE, code = 'stop("boom")'),
  list(id = "x3", title = "needs x2", deps = "x2", always = TRUE, code = 'cat(1)'),
  list(id = "x4", title = "independent", deps = character(0), always = TRUE,
       code = 'stopifnot(!exists("OVERNIGHT_STEPS_OVERRIDE")); cat("fresh\\n")'),
  list(id = "x5", title = "needs x3", deps = "x3", always = TRUE, code = 'cat(1)'))
invisible(capture.output(source(file.path("ad_hoc", "overnight_monday.R"))))
st <- read.csv(file.path(logs, "status.csv"), stringsAsFactors = FALSE)
stopifnot(identical(st[["status"]], c("ok", "FAILED", "skipped (needs x2)", "ok",
                                      "skipped (needs x3)")))
l1 <- readLines(file.path(logs, "x1.log")); l2 <- readLines(file.path(logs, "x2.log"))
stopifnot(any(grepl("^Warning.*w1", l1)), any(grepl("STEP FAILED: boom", l2)),
          !any(grepl(paste0("pid ", Sys.getpid()), l1)),
          !file.exists(file.path(logs, "x3.log")))
cat("PASS  runner: own process per step, own log, failure skips only dependants\n")
setwd(owd)
cat("\nAll overnight runner checks passed.\n")
