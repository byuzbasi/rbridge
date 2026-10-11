# Analyze completed timing shards only; this script never fits a model.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: summarize-benchmark.R RUN_DIRECTORY NEW_OUTPUT_DIRECTORY")
for (pkg in c("digest", "jsonlite", "knitr", "ggplot2"))
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing development dependency: ", pkg)
run <- normalizePath(args[1L], mustWork = TRUE)
out <- args[2L]
if (file.exists(out)) stop("Refusing to overwrite an existing analysis")
sha <- function(path) digest::digest(file = path, algo = "sha256")
manifest <- utils::read.delim(file.path(run, "MANIFEST.tsv"))
files <- file.path(run, manifest$path)
if (any(!file.exists(files)) || any(file.info(files)$size != manifest$bytes) ||
    !identical(unname(vapply(files, sha, character(1))), manifest$sha256))
  stop("Source manifest failed validation")
saved <- readRDS(file.path(run, "config.rds"))
cfg <- saved$config
status <- jsonlite::read_json(file.path(run, "progress.json"), simplifyVector = TRUE)
if (!identical(status$phase, "COMPLETED")) stop("Source run is incomplete")
reported <- utils::read.delim(file.path(run, "comparison.tsv"))
rows <- times <- list()
timer_notes <- character()
read_shard <- function(id, design, method) {
  path <- file.path(run, "shards", paste0(id, ".rds"))
  if (!identical(sha(path), readLines(paste0(path, ".sha256"), warn = FALSE)))
    stop("Shard checksum mismatch: ", id)
  x <- readRDS(path)
  stopifnot(identical(x$id, id), identical(x$signature, saved$signature),
    length(x$elapsed_seconds) == cfg$repeats, all(is.finite(x$elapsed_seconds)),
    all(x$elapsed_seconds > 0), all(is.finite(x$fit$betas)),
    identical(dim(x$fit$betas), c(design$p + 1L, length(cfg$lambda))),
    all(is.finite(x$predictions)),
    length(x$predictions) == if (startsWith(method, "cv.")) 3L else 3L * length(cfg$lambda))
  if (startsWith(method, "cv.")) stopifnot(length(x$fit$cve) == length(cfg$lambda),
    all(is.finite(x$fit$cve)), all(is.finite(x$fit$cvse)))
  timer_notes <<- unique(c(timer_notes, x$timer_notes))
  x
}
for (design in cfg$designs) for (q in cfg$exponents) for (method in cfg$methods) {
  id <- paste(design$id, q, method, sep = "_")
  b <- read_shard(paste0(id, "_baseline"), design, method)
  c <- read_shard(paste0(id, "_candidate"), design, method)
  fields <- intersect(c("betas", "cve", "cvse", "cvup", "cvlo", "nz"), names(b$fit))
  differences <- vapply(fields, function(f)
    max(abs(b$fit[[f]] - c$fit[[f]])) / max(1, max(abs(b$fit[[f]]))), numeric(1))
  prediction_error <- max(abs(b$predictions - c$predictions)) / max(1, max(abs(b$predictions)))
  selection <- all(vapply(intersect(c("lambda.min", "lambda.1se"), names(b$fit)),
    function(f) identical(b$fit[[f]], c$fit[[f]]), logical(1)))
  support <- identical(abs(b$fit$betas) > cfg$support_threshold,
                       abs(c$fit$betas) > cfg$support_threshold)
  nz <- identical(b$fit$nz, c$fit$nz)
  grid <- identical(b$fit$lambda, c$fit$lambda)
  passed <- all(differences <= cfg$coefficient_relative_tolerance) &&
    prediction_error <= cfg$coefficient_relative_tolerance && selection && support && nz && grid
  rows[[id]] <- data.frame(case = id, design = design$id, n = design$n, p = design$p,
    q = q, method = method, baseline_seconds = median(b$elapsed_seconds),
    candidate_seconds = median(c$elapsed_seconds),
    speed_ratio = median(b$elapsed_seconds) / median(c$elapsed_seconds),
    max_relative_difference = max(differences), prediction_relative_difference = prediction_error,
    same_selection = selection, same_support = support, same_nz = nz, same_lambda = grid, pass = passed)
  for (engine in c("baseline", "candidate")) {
    value <- if (engine == "baseline") b else c
    times[[paste(id, engine)]] <- data.frame(case = id, engine = engine,
      repeat_id = seq_along(value$elapsed_seconds), seconds = value$elapsed_seconds)
  }
}
comparison <- do.call(rbind, rows)
stopifnot(nrow(comparison) == length(cfg$designs) * length(cfg$exponents) * length(cfg$methods),
  identical(comparison$case, reported$case), all(comparison$pass), all(reported$pass),
  isTRUE(all.equal(comparison$speed_ratio, reported$speed_ratio, tolerance = 1e-12)),
  isTRUE(all.equal(comparison$max_relative_difference, reported$max_relative_difference, tolerance = 1e-12)),
  status$completed == 2L * nrow(comparison), status$pending == 0L, status$failed == 0L)
summarize <- function(field) do.call(rbind, lapply(unique(comparison[[field]]), function(key) {
  x <- comparison$speed_ratio[comparison[[field]] == key]
  data.frame(group = key, median_speedup = median(x), min_speedup = min(x), max_speedup = max(x), cases = length(x))
}))
dir.create(out, recursive = TRUE)
write_tsv <- function(x, name) utils::write.table(x, file.path(out, name),
  sep = "\t", row.names = FALSE, quote = FALSE)
write_tsv(comparison, "comparison.tsv")
write_tsv(do.call(rbind, times), "timings.tsv")
for (field in c("method", "design", "q")) write_tsv(summarize(field), paste0("summary-", field, ".tsv"))
provenance <- list(signature = saved$signature, source_manifest_sha256 = sha(file.path(run, "MANIFEST.tsv")),
  config = cfg, environment = saved$metadata, validated_shards = status$completed,
  comparison_cases = nrow(comparison), maximum_relative_difference = max(comparison$max_relative_difference),
  maximum_prediction_difference = max(comparison$prediction_relative_difference),
  exact_selection_support_counts = TRUE, timer_notices = timer_notes,
  baseline_scope = "first repaired 1.0.3 candidate, not a direct comparison with CRAN 1.0.2",
  timing_scope = "single recorded machine; fixed baseline-then-candidate order; no randomized engine order",
  validation_scope = "historical estimator compatibility, not convergence or global optimality certification",
  numerical_fits_executed_by_analysis = FALSE)
jsonlite::write_json(provenance, file.path(out, "provenance.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA)
comparison$method <- factor(comparison$method, levels = rev(cfg$methods))
comparison$q <- factor(comparison$q, levels = cfg$exponents)
labels <- vapply(cfg$designs, function(d) sprintf("%s: n=%d, p=%d", d$id, d$n, d$p), character(1))
comparison$design_label <- factor(labels[match(comparison$design, vapply(cfg$designs, `[[`, character(1), "id"))], levels = labels)
plot <- ggplot2::ggplot(comparison, ggplot2::aes(q, method, fill = log2(speed_ratio))) +
  ggplot2::geom_tile(color = "white", linewidth = 1.1) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2fx", speed_ratio)), color = "white", size = 4.4) +
  ggplot2::facet_wrap(~design_label, ncol = 2) +
  ggplot2::scale_fill_gradient(low = "#217d82", high = "#142c4c", guide = "none") +
  ggplot2::labs(title = "Measured speed gains", subtitle = "Median time ratios against the first repaired 1.0.3 candidate",
    x = "Bridge penalty exponent q", y = NULL,
    caption = "15 timed fits per engine; single CV/BLAS thread. Synthetic timing designs, not paper-study results.") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold"), plot.caption = ggplot2::element_text(size = 9))
ggplot2::ggsave(file.path(out, "speedups.svg"), plot, width = 10, height = 7.5, bg = "white")
ggplot2::ggsave(file.path(out, "speedups.png"), plot, width = 10, height = 7.5, dpi = 160, bg = "white")
report <- c("rbridge 1.0.3 completed benchmark evaluation", "",
  "All 96 shards and 48 comparisons pass checksum, shape and numerical validation.",
  "Ratios divide the baseline median time by the candidate median time.",
  "Each case uses 15 timed fits per engine, after two warmups; process startup is excluded.",
  "q is the bridge-penalty exponent; n is observations and p is predictors.",
  "The four synthetic designs use seed 2019, q=0.5/1/2, 24 penalties and five CV folds.",
  "CV and BLAS each use one thread; this measures serial engine optimization.", "",
  knitr::kable(summarize("method"), format = "simple", digits = 2), "",
  sprintf("Overall casewise median speedup: %.2fx (range %.2f-%.2fx).", median(comparison$speed_ratio), min(comparison$speed_ratio), max(comparison$speed_ratio)),
  sprintf("Maximum coefficient/CV/count relative difference: %.8g.", max(comparison$max_relative_difference)),
  sprintf("Maximum prediction relative difference: %.8g.", max(comparison$prediction_relative_difference)),
  "Selected penalties, support at 1e-7 and variable counts are exactly preserved.", "",
  "Limits: one machine, one data seed, fixed engine order, no randomized repetitions across sessions.",
  "The reference is the first repaired 1.0.3 candidate, not the original 1.0.2 archive.",
  "No accuracy ranking, convergence certificate, universal speedup or paper reproduction is claimed.",
  "The timer precision notice is retained in provenance.json; solver warnings were errors.",
  "The historical frozen driver and completed source outputs were not modified.")
writeLines(report, file.path(out, "REPORT.txt"))
outputs <- sort(list.files(out, full.names = TRUE), method = "radix")
write_tsv(data.frame(path = basename(outputs), bytes = file.info(outputs)$size,
  sha256 = vapply(outputs, sha, character(1))), "MANIFEST.tsv")
cat("Validated completed outputs and wrote analysis:", out, "\n")
