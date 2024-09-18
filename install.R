# should be in sync with Docker base image
options(repos = c(CRAN = "https://p3m.dev/cran/__linux__/jammy/latest"))

libraries <- strsplit(
  x = readLines("./requirements.txt"),
  split = "=="
)

# for bslib==0.7.0
remotes::install_version(package = 'htmltools', version = '>= 0.5.8')
remotes::install_version(package = 'sass', version = '>= 0.4.9')

invisible(
  lapply(libraries, function (x) {
    remotes::install_version(package = x[1], version = x[2], upgrade = FALSE)
  })
)
