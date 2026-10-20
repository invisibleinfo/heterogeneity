# Run from the repository root: Rscript tools/render_resources.R
# Installs the current source into an isolated library, then re-renders the
# tutorial (needs Pandoc, an internet connection, and knitr, rmarkdown and
# gssr). A local paper/ folder, if present, is rendered too, with an Overleaf
# copy (needs bookdown, XeLaTeX and Biber). No packages are downloaded here.
main <- function() {
  stopifnot(file.exists("DESCRIPTION"), dir.exists("tutorial"))
  root <- normalizePath(".")
  paper <- file.path(root, "paper")
  with_paper <- file.exists(file.path(paper, "heterogeneity_draft_v2.Rmd"))
  for (p in c("rmarkdown", "knitr", "gssr", if (with_paper) "bookdown")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Install the package: ", p)
  }
  if (with_paper && !all(nzchar(Sys.which(c("xelatex", "biber"))))) stop("XeLaTeX and Biber must be on PATH to render the paper.")
  lib <- tempfile("heterogeneity-render-library-")
  dir.create(lib)
  old_libs <- .libPaths()
  on.exit({ .libPaths(old_libs); unlink(lib, recursive = TRUE) }, add = TRUE)
  status <- system2(file.path(R.home("bin"), "R"),
                    c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), shQuote(root)))
  if (status != 0) stop("Installing the current source failed.")
  .libPaths(c(lib, old_libs))
  # The *_files directories contain only generated figures. Render into empty
  # ones so charts removed from the source cannot survive, but put the old
  # figures back if rendering fails.
  render_fresh <- function(input, files_dir) {
    backup <- paste0(files_dir, "-backup")
    unlink(backup, recursive = TRUE)
    if (dir.exists(files_dir)) stopifnot(file.rename(files_dir, backup))
    ok <- FALSE
    on.exit(if (ok) unlink(backup, recursive = TRUE) else if (dir.exists(backup)) {
      unlink(files_dir, recursive = TRUE)
      file.rename(backup, files_dir)
    })
    rmarkdown::render(input, envir = new.env(), quiet = TRUE)
    ok <- TRUE
  }
  render_fresh(file.path(root, "tutorial", "quickstart.Rmd"), file.path(root, "tutorial", "quickstart_files"))
  if (!with_paper) return(cat("Rebuilt tutorial/quickstart.md.\n"))

  render_fresh(file.path(paper, "heterogeneity_draft_v2.Rmd"), file.path(paper, "heterogeneity_draft_v2_files"))
  overleaf <- file.path(paper, "overleaf")
  dir.create(file.path(overleaf, "figures"), recursive = TRUE, showWarnings = FALSE)
  tex <- readLines(file.path(paper, "heterogeneity_draft_v2.tex"), warn = FALSE)
  tex <- gsub("heterogeneity_draft_v2_files/figure-latex/", "figures/", tex, fixed = TRUE)
  writeLines(tex, file.path(overleaf, "main.tex"))
  generated <- list.files(file.path(paper, "heterogeneity_draft_v2_files", "figure-latex"),
                           pattern = "[.]pdf$", full.names = TRUE)
  if (!length(generated)) stop("No generated paper figures were found.")
  unlink(list.files(file.path(overleaf, "figures"), pattern = "[.]pdf$", full.names = TRUE))
  figures <- c(generated, list.files(file.path(paper, "figures"), pattern = "[.]png$", full.names = TRUE))
  stopifnot(all(file.copy(figures, file.path(overleaf, "figures"), overwrite = TRUE)),
            file.copy(file.path(paper, "references.bib"), overleaf, overwrite = TRUE))
  # The paper's .tex and figure folder only feed the Overleaf copy.
  unlink(c(file.path(paper, c("heterogeneity_draft_v2.tex", "heterogeneity_draft_v2.log")),
           file.path(paper, "heterogeneity_draft_v2_files")), recursive = TRUE)
  cat("Rebuilt tutorial/quickstart.md, paper/heterogeneity_draft_v2.pdf, and paper/overleaf/.\n")
}
main()
