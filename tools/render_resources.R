# Run from the repository root: Rscript tools/render_resources.R
#
# Installs the current source into an isolated library, then re-renders the
# tutorial (needs Pandoc, knitr and rmarkdown). Nothing is downloaded here.

main <- function() {
  stopifnot(file.exists("DESCRIPTION"), dir.exists("tutorial"))
  root <- normalizePath(".")

  for (p in c("rmarkdown", "knitr")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Install the package: ", p)
    }
  }

  # Install the current source into a temporary library.
  lib <- tempfile("heterogeneity-render-library-")
  dir.create(lib)
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), shQuote(root))
  )
  if (status != 0) {
    stop("Installing the current source failed.")
  }
  .libPaths(c(lib, old_libs))

  # The figure folder contains only generated figures. Render into an empty
  # one so charts removed from the tutorial cannot survive, but put the old
  # figures back if rendering fails.
  files_dir <- file.path(root, "tutorial", "quickstart_files")
  backup <- paste0(files_dir, "-backup")
  unlink(backup, recursive = TRUE)
  if (dir.exists(files_dir)) {
    stopifnot(file.rename(files_dir, backup))
  }
  ok <- FALSE
  restore <- function() {
    if (ok) {
      unlink(backup, recursive = TRUE)
    } else if (dir.exists(backup)) {
      unlink(files_dir, recursive = TRUE)
      file.rename(backup, files_dir)
    }
  }
  on.exit(restore(), add = TRUE)

  rmarkdown::render(
    file.path(root, "tutorial", "quickstart.Rmd"),
    envir = new.env(),
    quiet = TRUE
  )
  ok <- TRUE
  cat("Rebuilt tutorial/quickstart.md.\n")
}

main()
