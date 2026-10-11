# Reproducible package-engine timing, not a scientific simulation study.
# Work unit: one design/exponent/method/engine timing shard.
args <- commandArgs(trailingOnly = TRUE)
needed <- c("digest", "jsonlite", "microbenchmark")
for (p in needed) if (!requireNamespace(p, quietly = TRUE)) stop("Missing dependency: ", p)
sha <- function(path) digest::digest(file = path, algo = "sha256")
atomic_rds <- function(value, path) {
  tmp <- paste0(path, ".tmp")
  saveRDS(value, tmp, version = 3)
  if (!file.rename(tmp, path)) stop("Cannot publish ", path)
}
atomic_json <- function(value, path) {
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(value, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  if (!file.rename(tmp, path)) stop("Cannot publish ", path)
}
progress <- function(ctx, phase, running = 0L, failed = 0L, last = NULL) {
  elapsed <- as.numeric(difftime(Sys.time(), ctx$started, units = "secs"))
  throughput <- if (ctx$completed > 0 && elapsed > 0) (ctx$completed - ctx$initial_completed) / elapsed else NA_real_
  remaining <- ctx$total - ctx$completed - failed
  # Case costs differ substantially. Estimate only if every pending cost group
  # has a measured duration; otherwise report estimating rather than extrapolate.
  pending_groups <- unique(ctx$groups[ctx$pending])
  observed_groups <- ctx$groups[names(ctx$durations)]
  means <- if (length(ctx$durations)) tapply(ctx$durations, observed_groups, mean) else numeric()
  eta <- if (length(pending_groups) && all(pending_groups %in% names(means)))
    sum(means[ctx$groups[ctx$pending]]) else NA_real_
  if (ctx$completed == ctx$total) eta <- 0
  state <- list(run_id = basename(ctx$out), phase = phase,
    work_unit = "design/exponent/method/engine shard", total = ctx$total,
    completed = ctx$completed, running = running, failed = failed,
    pending = max(0, remaining - running), percent = 100 * ctx$completed / ctx$total,
    elapsed_seconds = elapsed, units_per_second = throughput,
    estimated_remaining_seconds = eta,
    estimated_completion = if (is.finite(eta)) format(Sys.time() + eta, tz = "UTC", usetz = TRUE) else "estimating",
    eta_scope = "timing shards only; final comparison and manifest validation follow",
    timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE), last_completed = last,
    current_unit = ctx$current)
  atomic_json(state, file.path(ctx$out, "progress.json"))
  history <- file.path(ctx$out, "progress.tsv")
  row <- data.frame(timestamp = state$timestamp, phase = phase, completed = ctx$completed,
    total = ctx$total, running = running, failed = failed, pending = state$pending,
    percent = state$percent, elapsed_seconds = elapsed, units_per_second = throughput,
    estimated_remaining_seconds = eta, current_unit = ctx$current)
  utils::write.table(row, history, sep = "\t", row.names = FALSE,
    col.names = !file.exists(history), append = file.exists(history), quote = TRUE)
  cat(sprintf("%s | %s | %d/%d validated (%.1f%%) | %s\n", state$timestamp,
    phase, ctx$completed, ctx$total, state$percent, ctx$current))
  flush(stdout())
}
make_data <- function(design) {
  set.seed(2019L)
  X <- matrix(rnorm(design$n * design$p), design$n, design$p)
  y <- as.numeric(1 + X %*% c(1, 0.5, -0.2, rep(0, design$p - 3L)) + rnorm(design$n))
  list(X = X, y = y, R = diag(design$p)[(design$p - 1L):design$p, , drop = FALSE],
       r = matrix(0, 2L, 1L))
}
# Each engine is loaded in a fresh R process; installation/loading are not timed.
if (length(args) == 3L && args[1L] == "--worker") {
  job <- readRDS(args[2L])
  .libPaths(c(job$library, .libPaths()))
  suppressPackageStartupMessages(library(rbridge))
  d <- make_data(job$design)
  fit_args <- list(X = d$X, y = d$y, q = job$q, lambda = job$lambda)
  if (grepl("rbridge", job$method, fixed = TRUE)) fit_args <- c(fit_args, d[c("R", "r")])
  if (startsWith(job$method, "cv.")) fit_args <- c(fit_args, list(nfolds = job$folds, num_threads = 1L))
  fit_once <- function() do.call(getExportedValue("rbridge", job$method), fit_args)
  options(warn = 2)
  invisible(fit_once())
  invisible(fit_once())
  times <- numeric(job$repeats)
  timer_notes <- character()
  heartbeat <- Sys.time()
  for (i in seq_len(job$repeats)) {
    times[i] <- withCallingHandlers(
      microbenchmark::microbenchmark(fit_once(), times = 1L)$time / 1e9,
      warning = function(w) {
        if (grepl("less accurate nanosecond times", conditionMessage(w), fixed = TRUE)) {
          timer_notes <<- unique(c(timer_notes, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      })
    if (difftime(Sys.time(), heartbeat, units = "secs") >= 30) {
      progress(job$ctx, "timing", running = 1L)
      heartbeat <- Sys.time()
    }
  }
  fit <- fit_once()
  value <- list(signature = job$signature, id = job$id, elapsed_seconds = times,
    fit = fit, predictions = predict(fit, newx = d$X[1:3, , drop = FALSE]),
    timer_notes = timer_notes, capabilities = if (exists("rbridge_info", asNamespace("rbridge"), inherits = FALSE)) rbridge_info() else NULL)
  atomic_rds(value, args[3L])
  quit(status = 0L)
}
if (length(args) < 4L || !args[1L] %in% c("design", "smoke", "full", "verify")) {
  stop("Usage: benchmark-package.R {design|smoke|full|verify} BASELINE_LIBRARY CANDIDATE_LIBRARY OUTPUT [--resume]")
}
mode <- args[1L]
libs <- vapply(args[2:3], normalizePath, character(1), mustWork = TRUE)
for (lib in libs) if (!dir.exists(file.path(lib, "rbridge"))) stop("Missing installed engine: ", lib)
out <- normalizePath(args[4L], mustWork = FALSE)
resume <- length(args) == 5L && args[5L] == "--resume"
if (length(args) > 5L || (length(args) == 5L && !resume)) stop("Unexpected arguments")
thread_env <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "BLIS_NUM_THREADS",
                "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS")
thread_values <- Sys.getenv(thread_env)
if (any(thread_values != "1")) stop("Set all six BLAS/OpenMP environment variables to 1 before starting R")
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L]))
engine_signature <- function(lib) {
  base <- file.path(lib, "rbridge")
  # Byte ordering makes engine identity independent of LC_COLLATE.
  files <- sort(list.files(base, recursive = TRUE, full.names = TRUE), method = "radix")
  relevant <- files[grepl("/(R|libs)/|/DESCRIPTION$|/NAMESPACE$", files)]
  list(version = read.dcf(file.path(base, "DESCRIPTION"))[1L, "Version"],
       hash = digest::digest(setNames(vapply(relevant, sha, character(1)), substring(relevant, nchar(base) + 2L)), algo = "sha256"))
}
protocol <- function(smoke) list(
  schema = 2L, seed = 2019L, repeats = if (smoke) 3L else 15L, warmup = 2L,
  designs = if (smoke) list(list(id = "small", n = 80L, p = 16L), list(id = "wide", n = 14L, p = 18L)) else
    list(list(id = "small", n = 80L, p = 16L), list(id = "medium", n = 300L, p = 60L),
         list(id = "larger", n = 500L, p = 180L), list(id = "wide", n = 60L, p = 120L)),
  exponents = if (smoke) c(1, 2) else c(0.5, 1, 2),
  lambda = exp(seq(log(10), log(0.1), length.out = if (smoke) 4L else 24L)),
  methods = c("bridge", "rbridge", "cv.bridge", "cv.rbridge"), folds = if (smoke) 2L else 5L,
  coefficient_relative_tolerance = 1e-8, support_threshold = 1e-7, requested_threads = 1L,
  scientific_scope = "synthetic engine timing only; historical preprocessing and estimators preserved",
  convergence_scope = "historical stopping rule; no convergence certificate or accuracy ranking claimed")
metadata <- list(R = as.character(getRversion()), platform = R.version$platform,
  dependencies = setNames(lapply(needed, function(p) as.character(packageVersion(p))), needed),
  numerical_dependencies = setNames(lapply(c("Rcpp", "RcppArmadillo", "Matrix"),
    function(p) as.character(packageVersion(p))), c("Rcpp", "RcppArmadillo", "Matrix")),
  hardware = list(system = unname(Sys.info()["sysname"]),
    architecture = unname(Sys.info()["machine"]), logical_cores = parallel::detectCores(),
    cpu = if (Sys.info()["sysname"] == "Darwin")
      system2("/usr/sbin/sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE) else "record separately"),
  thread_environment = setNames(as.list(thread_values), thread_env),
  engines = setNames(lapply(libs, engine_signature), c("baseline", "candidate")),
  driver_sha256 = sha(script))
if (mode == "verify") {
  if (!file.exists(file.path(out, "config.rds"))) stop("Missing configuration")
  saved <- readRDS(file.path(out, "config.rds"))
  cfg <- saved$config
} else cfg <- protocol(mode == "smoke")
# Paths are execution details; engine content hashes are the scientific identity.
signature <- digest::digest(list(config = cfg, metadata = metadata), algo = "sha256")
if (dir.exists(out)) {
  if (mode != "verify" && !resume) stop("Refusing to overwrite existing output; use --resume")
  saved <- readRDS(file.path(out, "config.rds"))
  if (!identical(saved$signature, signature)) stop("Configuration/code/engine/environment signature mismatch")
} else {
  if (resume || mode == "verify") stop("Cannot resume or verify a missing output directory")
  dir.create(file.path(out, "shards"), recursive = TRUE)
  atomic_rds(list(config = cfg, metadata = metadata, signature = signature), file.path(out, "config.rds"))
  atomic_json(list(config = cfg, metadata = metadata, signature = signature), file.path(out, "config.json"))
}
units <- list()
for (design in cfg$designs) for (q in cfg$exponents) for (method in cfg$methods) for (engine in 1:2) {
  id <- paste(design$id, q, method, c("baseline", "candidate")[engine], sep = "_")
  units[[id]] <- list(id = id, design = design, q = q, method = method, library = libs[engine],
    lambda = cfg$lambda, folds = cfg$folds, repeats = cfg$repeats, signature = signature)
}
ctx <- list(out = out, started = Sys.time(), completed = 0L, initial_completed = 0L,
            total = length(units), current = "preflight", durations = numeric(),
            pending = names(units), groups = setNames(sub("_(baseline|candidate)$", "", names(units)), names(units)))
valid_shard <- function(path, id) {
  if (!file.exists(path) || !file.exists(paste0(path, ".sha256"))) return(FALSE)
  if (!identical(sha(path), readLines(paste0(path, ".sha256"), warn = FALSE))) return(FALSE)
  value <- tryCatch(readRDS(path), error = function(e) NULL)
  !is.null(value) && identical(value$signature, signature) && identical(value$id, id) &&
    length(value$elapsed_seconds) == cfg$repeats && all(is.finite(value$elapsed_seconds)) &&
    all(value$elapsed_seconds > 0) && identical(dim(value$fit$betas),
      c(units[[id]]$design$p + 1L, length(cfg$lambda))) && all(is.finite(value$fit$betas)) &&
    all(is.finite(value$predictions)) &&
    (!startsWith(units[[id]]$method, "cv.") || all(is.finite(value$fit$cve)))
}
manifest <- function() {
  files <- sort(list.files(out, recursive = TRUE, full.names = TRUE), method = "radix")
  files <- files[!grepl("(MANIFEST.tsv|\\.tmp|/worker-job.rds|/worker.log)$", files)]
  table <- data.frame(path = substring(files, nchar(out) + 2L), bytes = file.info(files)$size,
                      sha256 = vapply(files, sha, character(1)))
  utils::write.table(table, file.path(out, "MANIFEST.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
}
check_manifest <- function() {
  path <- file.path(out, "MANIFEST.tsv")
  if (!file.exists(path)) stop("Missing manifest")
  table <- utils::read.delim(path, stringsAsFactors = FALSE)
  files <- file.path(out, table$path)
  if (any(!file.exists(files)) || any(file.info(files)$size != table$bytes) ||
      !identical(unname(vapply(files, sha, character(1))), table$sha256)) stop("Manifest verification failed")
}
if (mode == "design") {
  progress(ctx, "design validated")
  manifest()
  cat("Frozen full timing protocol: ", length(units), " shards; no fits run.\n", sep = "")
  quit(status = 0L)
}
if (mode == "verify") {
  check_manifest()
  ok <- vapply(names(units), function(id) valid_shard(file.path(out, "shards", paste0(id, ".rds")), id), logical(1))
  status <- jsonlite::read_json(file.path(out, "progress.json"), simplifyVector = TRUE)
  comparisons <- utils::read.delim(file.path(out, "comparison.tsv"))
  if (!all(ok) || !identical(status$phase, "COMPLETED") || !all(comparisons$pass)) stop("Incomplete or invalid run")
  cat("Verified: ", sum(ok), "/", length(units), " signature-matching shards, numerical gates and manifest.\n", sep = "")
  quit(status = 0L)
}
existing <- vapply(names(units), function(id) {
  path <- file.path(out, "shards", paste0(id, ".rds"))
  if (file.exists(path) && !valid_shard(path, id)) stop("Invalid existing shard; preserve it and use a new run directory: ", id)
  valid_shard(path, id)
}, logical(1))
ctx$completed <- sum(existing)
ctx$initial_completed <- ctx$completed
ctx$pending <- names(units)[!existing]
last <- if (any(existing)) tail(names(units)[existing], 1L) else NULL
progress(ctx, "timing", last = last)
for (id in names(units)[!existing]) {
  ctx$current <- id
  unit_started <- Sys.time()
  progress(ctx, "timing", running = 1L, last = last)
  job <- c(units[[id]], list(ctx = ctx))
  job_path <- file.path(out, "worker-job.rds")
  atomic_rds(job, job_path)
  path <- file.path(out, "shards", paste0(id, ".rds"))
  pending <- paste0(path, ".pending")
  code <- system2(file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", shQuote(script), "--worker", shQuote(job_path), shQuote(pending)),
    stdout = file.path(out, "worker.log"), stderr = file.path(out, "worker.log"))
  if (code != 0L || !file.exists(pending)) {
    # Retain failed attempts even when a resumed worker replaces worker.log.
    failure_history <- file.path(out, "failure-history.log")
    cat(format(Sys.time(), tz = "UTC", usetz = TRUE), id,
        "exit_status", code, "\n", file = failure_history, append = TRUE)
    cat(readLines(file.path(out, "worker.log"), warn = FALSE), sep = "\n",
        file = failure_history, append = TRUE)
    progress(ctx, "FAILED", failed = 1L, last = last)
    manifest()
    stop("Worker failed; see worker.log: ", id)
  }
  if (!file.rename(pending, path)) stop("Cannot publish shard")
  writeLines(sha(path), paste0(path, ".sha256"))
  if (!valid_shard(path, id)) {
    progress(ctx, "FAILED", failed = 1L, last = last)
    manifest()
    stop("Numerical/shape validation failed: ", id)
  }
  ctx$durations[id] <- as.numeric(difftime(Sys.time(), unit_started, units = "secs"))
  ctx$completed <- ctx$completed + 1L
  ctx$pending <- setdiff(ctx$pending, id)
  last <- id
  progress(ctx, "timing", last = last)
}
progress(ctx, "comparing", last = last)
rows <- list()
for (id in names(units)[grepl("_baseline$", names(units))]) {
  b <- readRDS(file.path(out, "shards", paste0(id, ".rds")))
  other <- sub("_baseline$", "_candidate", id)
  c <- readRDS(file.path(out, "shards", paste0(other, ".rds")))
  fields <- intersect(c("betas", "cve", "cvse", "cvup", "cvlo", "nz"), names(b$fit))
  differences <- vapply(fields, function(field) max(abs(b$fit[[field]] - c$fit[[field]])) /
                          max(1, max(abs(b$fit[[field]]))), numeric(1))
  selection <- all(vapply(intersect(c("lambda.min", "lambda.1se"), names(b$fit)),
    function(field) identical(b$fit[[field]], c$fit[[field]]), logical(1)))
  support <- identical(abs(b$fit$betas) > cfg$support_threshold, abs(c$fit$betas) > cfg$support_threshold)
  prediction_error <- max(abs(b$predictions - c$predictions)) / max(1, max(abs(b$predictions)))
  same_lambda <- identical(b$fit$lambda, c$fit$lambda)
  pass <- all(differences <= cfg$coefficient_relative_tolerance) && selection && support && same_lambda &&
          prediction_error <= cfg$coefficient_relative_tolerance
  rows[[id]] <- data.frame(case = sub("_baseline$", "", id),
    baseline_seconds = median(b$elapsed_seconds), candidate_seconds = median(c$elapsed_seconds),
    speed_ratio = median(b$elapsed_seconds) / median(c$elapsed_seconds),
    max_relative_difference = max(differences), prediction_relative_difference = prediction_error,
    same_selection = selection, same_support = support, same_lambda = same_lambda, pass = pass)
}
comparison <- do.call(rbind, rows)
utils::write.table(comparison, file.path(out, "comparison.tsv"), sep = "\t", row.names = FALSE, quote = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "sessionInfo.txt"))
ctx$current <- "final validation"
if (!all(comparison$pass)) {
  progress(ctx, "FAILED_NUMERICAL_GATE", failed = 0L, last = last)
  manifest()
  stop("Timing complete but numerical comparison failed; no speed claim is justified")
}
progress(ctx, "COMPLETED", last = last)
manifest()
check_manifest()
cat(if (mode == "smoke") "Small timing smoke complete; ratios are diagnostic, not general speed claims.\n" else
  "Timing complete. Ratios apply only to these designs and this recorded environment.\n")
