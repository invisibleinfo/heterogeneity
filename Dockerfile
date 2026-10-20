# The heterogeneity app in a container. A hosting service builds this file
# and sets PORT. To try it locally:
#   docker build -t heterogeneity .
#   docker run --rm -p 3838:3838 heterogeneity    # then open http://localhost:3838
FROM rocker/r-ver:4.6.1

# pak installs binary R packages (from Posit Package Manager) together with
# the system libraries they need. Fonts are for the plots.
RUN apt-get update \
 && apt-get install -y --no-install-recommends fonts-dejavu-core \
 && rm -rf /var/lib/apt/lists/* \
 && R -q -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'

# The package from this repository, with the packages the app suggests. gssr,
# which downloads the GSS example, comes from its own repository.
WORKDIR /heterogeneity
COPY DESCRIPTION NAMESPACE ./
COPY R/ R/
COPY man/ man/
COPY inst/ inst/
RUN R -q -e 'pak::repo_add(gssr = "https://kjhealy.r-universe.dev"); pak::pkg_install(c("local::.", "shiny", "bslib", "haven", "readxl", "ragg", "gssr"))' \
 && rm -rf /tmp/* /root/.cache

# Run as an ordinary user. Uploads live in the session's temporary folder and
# are deleted when the session ends; nothing is written anywhere else.
RUN useradd --create-home app
USER app
ENV PORT=3838
EXPOSE 3838
CMD ["R", "-q", "-e", "heterogeneity::run_app(host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')), launch.browser = FALSE)"]
