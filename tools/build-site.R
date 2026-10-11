# Local documentation build. No publication or installation is performed.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L)
  stop("Usage: build-site.R PACKAGE_DIRECTORY OUTPUT_DIRECTORY [--links-only]")
package <- normalizePath(args[1L], mustWork = TRUE)
parent <- normalizePath(dirname(args[2L]), mustWork = TRUE)
destination <- file.path(parent, basename(args[2L]))
if (length(args) == 2L) {
  if (!requireNamespace("pkgdown", quietly = TRUE)) stop("pkgdown is required")
  pkgdown::build_site(package, override = list(destination = destination),
    preview = FALSE, new_process = FALSE, install = FALSE, quiet = FALSE)
} else if (args[3L] != "--links-only") stop("Unexpected option")
# pkgdown emits Rd equations as TeX delimiters. Convert text nodes to native
# MathML with the existing documentation toolchain, without a browser CDN.
math_cache <- new.env(parent = emptyenv())
render_reference_math <- function(html) {
  document <- xml2::read_html(paste(html, collapse = "\n"))
  nodes <- xml2::xml_find_all(document,
    "//text()[not(ancestor::code or ancestor::pre or ancestor::script or ancestor::style or ancestor::math)]")
  pattern <- "(?s)\\$\\$.*?\\$\\$|\\\\[(].*?\\\\[)]"
  converted_count <- 0L
  for (node in nodes) {
    value <- xml2::xml_text(node)
    matches <- gregexpr(pattern, value, perl = TRUE)[[1L]]
    if (matches[1L] < 0L) next
    lengths <- attr(matches, "match.length")
    fragments <- character()
    cursor <- 1L
    for (i in seq_along(matches)) {
      start <- matches[i]
      finish <- start + lengths[i] - 1L
      if (start > cursor)
        fragments <- c(fragments, htmltools::htmlEscape(substr(value, cursor, start - 1L)))
      formula <- substr(value, start, finish)
      if (!exists(formula, envir = math_cache, inherits = FALSE)) {
        input <- tempfile(fileext = ".md")
        output <- tempfile(fileext = ".html")
        markdown <- if (startsWith(formula, "$$")) formula else
          paste0("$", substr(formula, 3L, nchar(formula) - 2L), "$")
        writeLines(markdown, input)
        rmarkdown::pandoc_convert(input, from = "markdown", to = "html5",
          options = c("--mathml", "--wrap=none"), output = output)
        rendered <- trimws(paste(readLines(output, warn = FALSE), collapse = "\n"))
        unlink(c(input, output))
        if (!grepl("<math", rendered, fixed = TRUE))
          stop("Cannot render reference equation: ", formula)
        rendered <- sub("^<p>", "", rendered)
        rendered <- sub("</p>$", "", rendered)
        assign(formula, rendered, envir = math_cache)
      }
      fragments <- c(fragments, get(formula, envir = math_cache, inherits = FALSE))
      cursor <- finish + 1L
      converted_count <- converted_count + 1L
    }
    if (cursor <= nchar(value))
      fragments <- c(fragments, htmltools::htmlEscape(substr(value, cursor, nchar(value))))
    replacement <- xml2::read_html(paste0("<span>", paste(fragments, collapse = ""), "</span>"))
    xml2::xml_add_sibling(node, xml2::xml_find_first(replacement, "//body/span"), .where = "before")
    xml2::xml_remove(node)
  }
  if (!converted_count) return(html)
  cat("Rendered", converted_count, "reference math expressions.\n")
  as.character(document, options = character())
}
# Keep source README links useful in a checkout, and map them to built guides.
home <- file.path(destination, "index.html")
html <- readLines(home, warn = FALSE)
mapping <- c('href="inst/doc/reproducibility.html"' = 'href="articles/reproducibility.html"',
             'href="vignettes/getting-started.Rmd"' = 'href="articles/getting-started.html"',
             'href="NEWS.html"' = 'href="news/index.html"')
for (old in names(mapping)) html <- gsub(old, mapping[[old]], html, fixed = TRUE)
writeLines(html, home)
# Keep guide source links usable both offline and on the published website.
source <- file.path(destination, "source")
benchmarks <- file.path(destination, "benchmarks")
dir.create(benchmarks, recursive = TRUE, showWarnings = FALSE)
benchmark_files <- list.files(file.path(package, "inst", "doc", "benchmarks"), full.names = TRUE)
if (length(benchmark_files) && !all(file.copy(benchmark_files, benchmarks, overwrite = TRUE)))
  stop("Cannot copy completed benchmark evidence")
for (directory in c("R", "vignettes")) {
  dir.create(file.path(source, directory), recursive = TRUE, showWarnings = FALSE)
  files <- list.files(file.path(package, directory), full.names = TRUE)
  files <- files[grepl("\\.(R|Rmd)$", files)]
  if (!all(file.copy(files, file.path(source, directory), overwrite = TRUE)))
    stop("Cannot copy preview source files")
}
for (path in c("inst/CITATION", "CONTRIBUTING.md", "NEWS.md")) {
  dir.create(dirname(file.path(source, path)), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(package, path), file.path(source, path), overwrite = TRUE))
    stop("Cannot copy preview source: ", path)
}
urls <- strsplit(read.dcf(file.path(package, "DESCRIPTION"))[1L, "URL"],
                 "[,[:space:]]+")[[1L]]
repo <- urls[grepl("^https://github.com/[^/]+/[^/]+/?$", urls)]
if (length(repo) != 1L) stop("DESCRIPTION must identify one GitHub source repository")
prefix <- paste0(sub("/$", "", repo), "/blob/HEAD/")
for (page in list.files(destination, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)) {
  text <- readLines(page, warn = FALSE)
  relative <- substring(page, nchar(destination) + 2L)
  depth <- length(strsplit(relative, "/", fixed = TRUE)[[1L]]) - 1L
  text <- gsub(prefix, paste0(strrep("../", depth), "source/"), text, fixed = TRUE)
  text <- gsub('href="inst/doc/benchmarks/', 'href="benchmarks/', text, fixed = TRUE)
  text <- gsub('src="inst/doc/benchmarks/', 'src="benchmarks/', text, fixed = TRUE)
  if (depth) {
    text <- gsub('href="benchmarks/', paste0('href="', strrep("../", depth), 'benchmarks/'), text, fixed = TRUE)
    text <- gsub('src="benchmarks/', paste0('src="', strrep("../", depth), 'benchmarks/'), text, fixed = TRUE)
  }
  if (startsWith(relative, "reference/"))
    text <- render_reference_math(text)
  writeLines(text, page)
}
file.copy(file.path(package, "pkgdown", "extra.css"), destination, overwrite = TRUE)
cat("Local site ready:", destination, "\n")
