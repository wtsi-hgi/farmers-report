invisible(loadNamespace('covr'))

src_to_exclude <- c("src/constants.R", "src/ui.R")

src_code <- list.files("src", full.names = T)
src_code <- setdiff(src_code, src_to_exclude)
test_code <- list.files("tests/testthat", full.names = T, pattern = "*.R")

cov <- covr::file_coverage(source_files = src_code, test_files = test_code)
dir.create("reports", showWarnings = F)

for (src in src_code) {
    outfile <- gsub(".R$", ".html", basename(src))
    out_path <- file.path("reports", outfile)
    covr::file_report(x = cov, file = src, out_file = out_path)
}

print(cov)
