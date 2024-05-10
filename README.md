# Farmers report

## Usage
To use within Sanger just load SoftPack environment and start RStudio
```bash
module load HGI/softpack/groups/hgi/farmers-report
module load HGI/common/rstudio
rstudio start -M 8000
```

You can now knit report or serve an app.

## Development
To run tests simply execute within an environment
```bash
Rscript tests/testthat.R
```

To calculate the coverage
```bash
Rscript tests/covr.R
```
The coverage reports can be found in reports/ directory.
