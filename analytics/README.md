# Analytics Scripts

## WR Analytics

There are two scripts used to generate a report for wr usage:
- `download_wr_stats.R`: This script downloads and pre-aggregates the wr usage data from elastic.
- `plot_wr_stats.Rmd`: This script generates a report from the wr usage data.

To use them, first load an environment:
```bash
module add HGI/softpack/groups/hgi/farmers-report/
```

Then, modify the following input variables in `download_wr_stats.R`: 
- `config_file`: Path to the config file with elasticsearch credentials (do not use go-farmer here).
- `outfolder`: Path to the output folder where the wr usage data will be saved.

Now run the script:
```bash
Rscript analytics/download_wr_stats.R
```

If you do this the first time, it may take a few hours. Consequent runs will download only the new data, which should be much faster.

Next, modify the following input variable in `plot_wr_stats.Rmd`:
- `data_folder`: Path to the folder with the wr usage data (the same as `outfolder` in `download_wr_stats.R`).

Finally, run the script:
```bash
Rscript -e "rmarkdown::render('analytics/plot_wr_stats.Rmd')"
```
