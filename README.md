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

To build a Docker image
```bash
docker build -t mercury/farmers-report:latest .
```

To run tests inside a container
```bash
docker run --rm -v $(pwd):/code -w /code mercury/farmers-report:latest Rscript /code/tests/testthat.R
```
