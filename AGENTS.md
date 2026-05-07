# AGENTS.md

## Cursor Cloud specific instructions

### Project Overview
This is an R-based research project that analyzes historical employment data of Japanese local government officials (地方公務員) in Tokyo. It performs time-series analysis of gender-based headcounts by bureaucratic rank.

### Key Files
- `time_series/scripts/TimeSeriesChihouKomuin.R` — Main analysis script (uses hardcoded Windows Box paths for data)
- `time_series/scripts/run_local.R` — Local development runner that uses `time_series/data/merged_data_cleaned.csv`
- `time_series/data/merged_data_cleaned.csv` — Sample data for local development

### Running the Application
From the workspace root:
```bash
Rscript time_series/scripts/run_local.R
```
Output plots are saved to `time_series/output/`.

### Lint / Parse Check
```bash
Rscript -e 'parse(file = "time_series/scripts/TimeSeriesChihouKomuin.R"); cat("OK\n")'
Rscript -e 'parse(file = "time_series/scripts/run_local.R"); cat("OK\n")'
```

### Important Caveats
- The main script (`TimeSeriesChihouKomuin.R`) references Windows Box Sync paths and **cannot run directly** in Cloud environments. Use `run_local.R` instead.
- The sample data in `time_series/data/` is synthetic — it demonstrates the pipeline but is not the actual research dataset.
- R system dependencies required beyond `r-base`: `libuv1-dev`, `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libfontconfig1-dev`, `libharfbuzz-dev`, `libfribidi-dev`, `libfreetype6-dev`, `libpng-dev`, `libtiff5-dev`, `libjpeg-dev`.
- R packages are installed to `/usr/local/lib/R/site-library` (needs write permissions).
