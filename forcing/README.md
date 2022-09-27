NextGen forcing analysis CSV to NetCDF conversion
================
Keith Jennings
9/27/2022

A quick look at NetCDF vs CSV forcing for NextGen

Punch line: they’re both the same, except for slight far decimal
differences.

The below code imports, matches, then analyzes the data.

``` r
# Script for converting NLDAS-2 NETCDF files to CSV
# Data downloaded from NASA GES DISC
# Data are for Irwin Creek, catchment 87

# Keith Jennings
# keith.jennings@noaa.gov
# 2020-11-20

#Load packages
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ncdf4)

# Identify input and output directories
input_dir = "../data/forcing/"

# list files
nc_files <- data.frame(files = list.files(path = input_dir, pattern = ".nc$"), 
                       stringsAsFactors = F)
csv_files <- data.frame(files = list.files(path = input_dir, pattern = ".csv$"),
                        stringsAsFactors = F)

# Open the NetCDF file
nc <- nc_open(paste0(input_dir, nc_files[1, "files"]))

# Read and concatenate all of the csv data
df = data.frame()
ncRAINRATE = ncvar_get(nc, "RAINRATE")
ncT2D = ncvar_get(nc, "T2D")
ncQ2D = ncvar_get(nc, "Q2D")
ncU2D = ncvar_get(nc, "U2D")
ncV2D = ncvar_get(nc, "V2D")
ncPSFC = ncvar_get(nc, "PSFC")
ncSWDOWN = ncvar_get(nc, "SWDOWN")
ncLWDOWN = ncvar_get(nc, "LWDOWN")
ncIDS = ncvar_get(nc, "ids")

for(i in 1:length(csv_files$files)){
  # identify the catchment
  tmpCat = str_extract(csv_files[i, "files"], "[^.]+")
  
  # Get the numeric column corresponding to the catchment in the netcdf file
  tmpIds = which(ncIDS == tmpCat)
 
  # Import the csv and add netcdf data for that catchment
  tmp <- read.csv(paste0(input_dir, csv_files[i, "files"])) %>% 
     mutate(cat = tmpCat,
         ncRAINRATE = ncRAINRATE[, tmpIds],
         ncT2D      = ncT2D[, tmpIds],
         ncQ2D      = ncQ2D[, tmpIds],
         ncU2D      = ncU2D[, tmpIds],
         ncV2D      = ncV2D[, tmpIds],
         ncPSFC     = ncPSFC[, tmpIds],
         ncSWDOWN   = ncSWDOWN[, tmpIds],
         ncLWDOWN   = ncLWDOWN[, tmpIds],)
 
  # Bind to dataframe
  df <- bind_rows(df, tmp)
}

# Make data longer
df_l <- 
  bind_cols(
    df %>% select(RAINRATE:cat) %>% 
      pivot_longer(where(is.numeric), names_to = "var_csv", values_to = "value_csv"),
    df %>% select(ncRAINRATE:ncLWDOWN) %>% 
      pivot_longer(where(is.numeric), names_to = "var_nc", values_to = "value_nc")
  )

# Summarize the data for bias
df_summary <- df_l %>% 
  na.omit() %>% 
  group_by(cat, var_csv) %>% 
  summarise(mean_bias = mean(value_csv - value_nc),
            corr = cor(value_csv, value_nc))
```

    ## `summarise()` has grouped output by 'cat'. You can override using the `.groups`
    ## argument.

Here’s a table of per-catchment and variable biases and correlations:

| cat     | var_csv  | mean_bias | corr |
|:--------|:---------|----------:|-----:|
| cat-100 | LWDOWN   |  1.00e-07 |    1 |
| cat-100 | PSFC     | -1.41e-05 |    1 |
| cat-100 | Q2D      |  0.00e+00 |    1 |
| cat-100 | RAINRATE |  0.00e+00 |    1 |
| cat-100 | SWDOWN   |  0.00e+00 |    1 |
| cat-100 | T2D      |  0.00e+00 |    1 |
| cat-100 | U2D      |  0.00e+00 |    1 |
| cat-100 | V2D      |  0.00e+00 |    1 |
| cat-12  | LWDOWN   |  0.00e+00 |    1 |
| cat-12  | PSFC     | -1.50e-06 |    1 |
| cat-12  | Q2D      |  0.00e+00 |    1 |
| cat-12  | RAINRATE |  0.00e+00 |    1 |
| cat-12  | SWDOWN   |  0.00e+00 |    1 |
| cat-12  | T2D      |  0.00e+00 |    1 |
| cat-12  | U2D      |  0.00e+00 |    1 |
| cat-12  | V2D      |  0.00e+00 |    1 |
| cat-15  | LWDOWN   |  0.00e+00 |    1 |
| cat-15  | PSFC     |  1.01e-05 |    1 |
| cat-15  | Q2D      |  0.00e+00 |    1 |
| cat-15  | RAINRATE |  0.00e+00 |    1 |
| cat-15  | SWDOWN   |  0.00e+00 |    1 |
| cat-15  | T2D      |  1.00e-07 |    1 |
| cat-15  | U2D      |  0.00e+00 |    1 |
| cat-15  | V2D      |  0.00e+00 |    1 |
| cat-16  | LWDOWN   |  0.00e+00 |    1 |
| cat-16  | PSFC     | -2.08e-05 |    1 |
| cat-16  | Q2D      |  0.00e+00 |    1 |
| cat-16  | RAINRATE |  0.00e+00 |    1 |
| cat-16  | SWDOWN   |  0.00e+00 |    1 |
| cat-16  | T2D      |  0.00e+00 |    1 |
| cat-16  | U2D      |  0.00e+00 |    1 |
| cat-16  | V2D      |  0.00e+00 |    1 |
| cat-20  | LWDOWN   |  0.00e+00 |    1 |
| cat-20  | PSFC     |  3.64e-05 |    1 |
| cat-20  | Q2D      |  0.00e+00 |    1 |
| cat-20  | RAINRATE |  0.00e+00 |    1 |
| cat-20  | SWDOWN   |  0.00e+00 |    1 |
| cat-20  | T2D      |  0.00e+00 |    1 |
| cat-20  | U2D      |  0.00e+00 |    1 |
| cat-20  | V2D      |  0.00e+00 |    1 |
| cat-22  | LWDOWN   |  0.00e+00 |    1 |
| cat-22  | PSFC     |  1.46e-05 |    1 |
| cat-22  | Q2D      |  0.00e+00 |    1 |
| cat-22  | RAINRATE |  0.00e+00 |    1 |
| cat-22  | SWDOWN   |  0.00e+00 |    1 |
| cat-22  | T2D      |  0.00e+00 |    1 |
| cat-22  | U2D      |  0.00e+00 |    1 |
| cat-22  | V2D      |  0.00e+00 |    1 |
| cat-28  | LWDOWN   |  0.00e+00 |    1 |
| cat-28  | PSFC     | -1.20e-05 |    1 |
| cat-28  | Q2D      |  0.00e+00 |    1 |
| cat-28  | RAINRATE |  0.00e+00 |    1 |
| cat-28  | SWDOWN   |  0.00e+00 |    1 |
| cat-28  | T2D      |  0.00e+00 |    1 |
| cat-28  | U2D      |  0.00e+00 |    1 |
| cat-28  | V2D      |  0.00e+00 |    1 |
| cat-33  | LWDOWN   |  0.00e+00 |    1 |
| cat-33  | PSFC     | -4.90e-06 |    1 |
| cat-33  | Q2D      |  0.00e+00 |    1 |
| cat-33  | RAINRATE |  0.00e+00 |    1 |
| cat-33  | SWDOWN   |  0.00e+00 |    1 |
| cat-33  | T2D      |  1.00e-07 |    1 |
| cat-33  | U2D      |  0.00e+00 |    1 |
| cat-33  | V2D      |  0.00e+00 |    1 |
| cat-35  | LWDOWN   |  0.00e+00 |    1 |
| cat-35  | PSFC     |  1.47e-05 |    1 |
| cat-35  | Q2D      |  0.00e+00 |    1 |
| cat-35  | RAINRATE |  0.00e+00 |    1 |
| cat-35  | SWDOWN   |  0.00e+00 |    1 |
| cat-35  | T2D      |  0.00e+00 |    1 |
| cat-35  | U2D      |  0.00e+00 |    1 |
| cat-35  | V2D      |  0.00e+00 |    1 |
| cat-4   | LWDOWN   |  0.00e+00 |    1 |
| cat-4   | PSFC     |  2.30e-06 |    1 |
| cat-4   | Q2D      |  0.00e+00 |    1 |
| cat-4   | RAINRATE |  0.00e+00 |    1 |
| cat-4   | SWDOWN   |  0.00e+00 |    1 |
| cat-4   | T2D      |  0.00e+00 |    1 |
| cat-4   | U2D      |  0.00e+00 |    1 |
| cat-4   | V2D      |  0.00e+00 |    1 |
| cat-42  | LWDOWN   |  0.00e+00 |    1 |
| cat-42  | PSFC     |  2.36e-05 |    1 |
| cat-42  | Q2D      |  0.00e+00 |    1 |
| cat-42  | RAINRATE |  0.00e+00 |    1 |
| cat-42  | SWDOWN   |  0.00e+00 |    1 |
| cat-42  | T2D      |  0.00e+00 |    1 |
| cat-42  | U2D      |  0.00e+00 |    1 |
| cat-42  | V2D      |  0.00e+00 |    1 |
| cat-51  | LWDOWN   |  0.00e+00 |    1 |
| cat-51  | PSFC     |  2.46e-05 |    1 |
| cat-51  | Q2D      |  0.00e+00 |    1 |
| cat-51  | RAINRATE |  0.00e+00 |    1 |
| cat-51  | SWDOWN   |  1.00e-07 |    1 |
| cat-51  | T2D      | -1.00e-07 |    1 |
| cat-51  | U2D      |  0.00e+00 |    1 |
| cat-51  | V2D      |  0.00e+00 |    1 |
| cat-54  | LWDOWN   |  0.00e+00 |    1 |
| cat-54  | PSFC     |  1.09e-05 |    1 |
| cat-54  | Q2D      |  0.00e+00 |    1 |
| cat-54  | RAINRATE |  0.00e+00 |    1 |
| cat-54  | SWDOWN   |  0.00e+00 |    1 |
| cat-54  | T2D      |  0.00e+00 |    1 |
| cat-54  | U2D      |  0.00e+00 |    1 |
| cat-54  | V2D      |  0.00e+00 |    1 |
| cat-57  | LWDOWN   |  0.00e+00 |    1 |
| cat-57  | PSFC     | -2.09e-05 |    1 |
| cat-57  | Q2D      |  0.00e+00 |    1 |
| cat-57  | RAINRATE |  0.00e+00 |    1 |
| cat-57  | SWDOWN   |  0.00e+00 |    1 |
| cat-57  | T2D      |  0.00e+00 |    1 |
| cat-57  | U2D      |  0.00e+00 |    1 |
| cat-57  | V2D      |  0.00e+00 |    1 |
| cat-60  | LWDOWN   |  0.00e+00 |    1 |
| cat-60  | PSFC     |  7.50e-06 |    1 |
| cat-60  | Q2D      |  0.00e+00 |    1 |
| cat-60  | RAINRATE |  0.00e+00 |    1 |
| cat-60  | SWDOWN   |  0.00e+00 |    1 |
| cat-60  | T2D      |  0.00e+00 |    1 |
| cat-60  | U2D      |  0.00e+00 |    1 |
| cat-60  | V2D      |  0.00e+00 |    1 |
| cat-61  | LWDOWN   |  0.00e+00 |    1 |
| cat-61  | PSFC     |  2.10e-05 |    1 |
| cat-61  | Q2D      |  0.00e+00 |    1 |
| cat-61  | RAINRATE |  0.00e+00 |    1 |
| cat-61  | SWDOWN   |  0.00e+00 |    1 |
| cat-61  | T2D      |  0.00e+00 |    1 |
| cat-61  | U2D      |  0.00e+00 |    1 |
| cat-61  | V2D      |  0.00e+00 |    1 |
| cat-63  | LWDOWN   |  0.00e+00 |    1 |
| cat-63  | PSFC     |  2.75e-05 |    1 |
| cat-63  | Q2D      |  0.00e+00 |    1 |
| cat-63  | RAINRATE |  0.00e+00 |    1 |
| cat-63  | SWDOWN   |  0.00e+00 |    1 |
| cat-63  | T2D      |  0.00e+00 |    1 |
| cat-63  | U2D      |  0.00e+00 |    1 |
| cat-63  | V2D      |  0.00e+00 |    1 |
| cat-66  | LWDOWN   |  0.00e+00 |    1 |
| cat-66  | PSFC     |  3.68e-05 |    1 |
| cat-66  | Q2D      |  0.00e+00 |    1 |
| cat-66  | RAINRATE |  0.00e+00 |    1 |
| cat-66  | SWDOWN   |  0.00e+00 |    1 |
| cat-66  | T2D      |  0.00e+00 |    1 |
| cat-66  | U2D      |  0.00e+00 |    1 |
| cat-66  | V2D      |  0.00e+00 |    1 |
| cat-68  | LWDOWN   |  1.00e-07 |    1 |
| cat-68  | PSFC     |  9.18e-05 |    1 |
| cat-68  | Q2D      |  0.00e+00 |    1 |
| cat-68  | RAINRATE |  0.00e+00 |    1 |
| cat-68  | SWDOWN   |  0.00e+00 |    1 |
| cat-68  | T2D      |  0.00e+00 |    1 |
| cat-68  | U2D      |  0.00e+00 |    1 |
| cat-68  | V2D      |  0.00e+00 |    1 |
| cat-69  | LWDOWN   |  0.00e+00 |    1 |
| cat-69  | PSFC     | -6.65e-05 |    1 |
| cat-69  | Q2D      |  0.00e+00 |    1 |
| cat-69  | RAINRATE |  0.00e+00 |    1 |
| cat-69  | SWDOWN   |  0.00e+00 |    1 |
| cat-69  | T2D      |  0.00e+00 |    1 |
| cat-69  | U2D      |  0.00e+00 |    1 |
| cat-69  | V2D      |  0.00e+00 |    1 |
| cat-7   | LWDOWN   |  0.00e+00 |    1 |
| cat-7   | PSFC     |  3.20e-06 |    1 |
| cat-7   | Q2D      |  0.00e+00 |    1 |
| cat-7   | RAINRATE |  0.00e+00 |    1 |
| cat-7   | SWDOWN   |  0.00e+00 |    1 |
| cat-7   | T2D      |  0.00e+00 |    1 |
| cat-7   | U2D      |  0.00e+00 |    1 |
| cat-7   | V2D      |  0.00e+00 |    1 |
| cat-78  | LWDOWN   |  0.00e+00 |    1 |
| cat-78  | PSFC     | -2.23e-05 |    1 |
| cat-78  | Q2D      |  0.00e+00 |    1 |
| cat-78  | RAINRATE |  0.00e+00 |    1 |
| cat-78  | SWDOWN   |  0.00e+00 |    1 |
| cat-78  | T2D      |  0.00e+00 |    1 |
| cat-78  | U2D      |  0.00e+00 |    1 |
| cat-78  | V2D      |  0.00e+00 |    1 |
| cat-79  | LWDOWN   |  0.00e+00 |    1 |
| cat-79  | PSFC     | -5.12e-05 |    1 |
| cat-79  | Q2D      |  0.00e+00 |    1 |
| cat-79  | RAINRATE |  0.00e+00 |    1 |
| cat-79  | SWDOWN   |  0.00e+00 |    1 |
| cat-79  | T2D      |  0.00e+00 |    1 |
| cat-79  | U2D      |  0.00e+00 |    1 |
| cat-79  | V2D      |  0.00e+00 |    1 |
| cat-80  | LWDOWN   |  0.00e+00 |    1 |
| cat-80  | PSFC     |  5.01e-05 |    1 |
| cat-80  | Q2D      |  0.00e+00 |    1 |
| cat-80  | RAINRATE |  0.00e+00 |    1 |
| cat-80  | SWDOWN   |  0.00e+00 |    1 |
| cat-80  | T2D      |  0.00e+00 |    1 |
| cat-80  | U2D      |  0.00e+00 |    1 |
| cat-80  | V2D      |  0.00e+00 |    1 |
| cat-82  | LWDOWN   |  0.00e+00 |    1 |
| cat-82  | PSFC     | -3.21e-05 |    1 |
| cat-82  | Q2D      |  0.00e+00 |    1 |
| cat-82  | RAINRATE |  0.00e+00 |    1 |
| cat-82  | SWDOWN   |  0.00e+00 |    1 |
| cat-82  | T2D      |  0.00e+00 |    1 |
| cat-82  | U2D      |  0.00e+00 |    1 |
| cat-82  | V2D      |  0.00e+00 |    1 |
| cat-84  | LWDOWN   |  0.00e+00 |    1 |
| cat-84  | PSFC     |  5.40e-06 |    1 |
| cat-84  | Q2D      |  0.00e+00 |    1 |
| cat-84  | RAINRATE |  0.00e+00 |    1 |
| cat-84  | SWDOWN   |  0.00e+00 |    1 |
| cat-84  | T2D      |  0.00e+00 |    1 |
| cat-84  | U2D      |  0.00e+00 |    1 |
| cat-84  | V2D      |  0.00e+00 |    1 |
| cat-86  | LWDOWN   |  0.00e+00 |    1 |
| cat-86  | PSFC     |  4.27e-05 |    1 |
| cat-86  | Q2D      |  0.00e+00 |    1 |
| cat-86  | RAINRATE |  0.00e+00 |    1 |
| cat-86  | SWDOWN   |  0.00e+00 |    1 |
| cat-86  | T2D      |  0.00e+00 |    1 |
| cat-86  | U2D      |  0.00e+00 |    1 |
| cat-86  | V2D      |  0.00e+00 |    1 |
| cat-89  | LWDOWN   |  0.00e+00 |    1 |
| cat-89  | PSFC     |  7.00e-06 |    1 |
| cat-89  | Q2D      |  0.00e+00 |    1 |
| cat-89  | RAINRATE |  0.00e+00 |    1 |
| cat-89  | SWDOWN   |  0.00e+00 |    1 |
| cat-89  | T2D      |  0.00e+00 |    1 |
| cat-89  | U2D      |  0.00e+00 |    1 |
| cat-89  | V2D      |  0.00e+00 |    1 |
| cat-90  | LWDOWN   |  0.00e+00 |    1 |
| cat-90  | PSFC     |  9.30e-06 |    1 |
| cat-90  | Q2D      |  0.00e+00 |    1 |
| cat-90  | RAINRATE |  0.00e+00 |    1 |
| cat-90  | SWDOWN   |  0.00e+00 |    1 |
| cat-90  | T2D      |  0.00e+00 |    1 |
| cat-90  | U2D      |  0.00e+00 |    1 |
| cat-90  | V2D      |  0.00e+00 |    1 |
| cat-92  | LWDOWN   |  0.00e+00 |    1 |
| cat-92  | PSFC     | -3.76e-05 |    1 |
| cat-92  | Q2D      |  0.00e+00 |    1 |
| cat-92  | RAINRATE |  0.00e+00 |    1 |
| cat-92  | SWDOWN   |  0.00e+00 |    1 |
| cat-92  | T2D      |  0.00e+00 |    1 |
| cat-92  | U2D      |  0.00e+00 |    1 |
| cat-92  | V2D      |  0.00e+00 |    1 |

Most biases are 0 or near 0 and all correlations are 1. For non-zero
biases, the difference appears to be at a very distant decimal point:

``` r
df %>% slice(1) %>% pull(ncT2D) %>% sprintf("%.10f", .)
```

    ## [1] "281.7099914551"

``` r
df %>% slice(1) %>% pull(T2D) %>% sprintf("%.10f", .)
```

    ## [1] "281.7100000000"

Probably an error induced by putting the CSV data into the NetCDF file.
