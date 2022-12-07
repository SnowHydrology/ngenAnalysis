Hydrograph Examples for AGU Town Hall 2022
================
Keith Jennings
2022-12-07

# Get the data

Run the following code chunk to summarize the output data from 3 NextGen
runs.

``` r
# Load packages
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(cowplot); theme_set(theme_cowplot())

# Uncomment the source function if running the summary script
# source("data_summarize.R")
df <- readRDS("../data/flowTownHall2022/qout_dly_summary.RDS")

# Add a path to save figures
plot_path = "figures/"
```

# Compute summary functions

``` r
# Put data into wider format so obs can be compared to sim
df_compare <- 
  left_join(filter(df, mod == "Obs."),
            filter(df, mod != "Obs."),
            by = c("date", "loc"), suffix = c("_obs", "_sim") )

# Make the functions
nse_FUN = function(x,y){1 - sum((x - y)^2)/
    sum((x - mean(x))^2)}
mean_bias_FUN = function(x, y){mean(y) - mean(x)}
rmse_FUN = function(x, y){sqrt(sum((y - x)^2) * 1/length(x))}
mae_FUN = function(x, y){mean(abs(y - x))}
kge_FUN = function(x, y){1 - sqrt(((cor(x,y) - 1)^2) + (((sd(y)/sd(x)) - 1)^2) + (((mean(y)/mean(x)) - 1)^2))}

# Summarize the data
df_objFun <- df_compare %>% 
  group_by(mod_sim, loc) %>% 
  na.omit() %>% 
  summarize(nse = nse_FUN(qout_cms_obs, qout_cms_sim),
            kge = kge_FUN(qout_cms_obs, qout_cms_sim),
            mean_bias = mean_bias_FUN(qout_cms_obs, qout_cms_sim),
            rmse = rmse_FUN(qout_cms_obs, qout_cms_sim),
            mae = mae_FUN(qout_cms_obs, qout_cms_sim))
```

    ## `summarise()` has grouped output by 'mod_sim'. You can override using the
    ## `.groups` argument.

``` r
# Make a table
knitr::kable(df_objFun)
```

| mod_sim  | loc      |        nse |        kge |  mean_bias |      rmse |       mae |
|:---------|:---------|-----------:|-----------:|-----------:|----------:|----------:|
| CFE      | 03364500 |  0.6860091 |  0.8120316 | -0.4154786 | 5.1297524 | 2.3298055 |
| CFE      | 07195800 |  0.4604456 |  0.7405110 |  0.0018799 | 0.8107067 | 0.3584902 |
| CFE      | 10244950 | -0.4105960 |  0.3038112 | -0.0859323 | 0.2248113 | 0.1638194 |
| CFE      | 12117000 |  0.7765349 |  0.8784300 | -0.0997333 | 1.1431417 | 0.7425014 |
| CFE_X    | 03364500 |  0.6833741 |  0.8087494 | -0.4215747 | 5.1512313 | 2.3978718 |
| CFE_X    | 10244950 |  0.7399668 |  0.8215924 |  0.0086335 | 0.0965231 | 0.0539165 |
| CFE_X    | 12117000 |  0.7392351 |  0.8280220 | -0.3760106 | 1.2348659 | 0.8097472 |
| NWM2.1   | 03364500 |  0.6627617 |  0.6308251 | -0.3258854 | 5.3162606 | 1.6743902 |
| NWM2.1   | 07195800 |  0.6956991 |  0.8300378 | -0.0240288 | 0.6088324 | 0.1799366 |
| NWM2.1   | 10244950 | -6.1022667 | -1.4330144 |  0.1586099 | 0.5044467 | 0.2133985 |
| NWM2.1   | 12117000 |  0.6493556 |  0.8023916 | -0.0710322 | 1.4319526 | 0.7616489 |
| TOPMODEL | 03364500 |  0.6838721 |  0.7143009 |  0.2695038 | 5.1471787 | 2.2433056 |
| TOPMODEL | 07195800 |  0.7601842 |  0.8364409 | -0.0058208 | 0.5404867 | 0.1995354 |
| TOPMODEL | 12117000 |  0.4558563 |  0.7361363 | -0.1334381 | 1.7838240 | 1.0650585 |

# Plot the data

``` r
# Make a color scheme
flow_color_scale <- scale_color_manual(name = "Source", 
                                       breaks = c("Obs.", "CFE", "CFE_X", "TOPMODEL", "NWM2.1"),
                                       labels = c("Obs.", "CFE", "CFE_X", "TOPMODEL", "NWM2.1"),
                                       values = c("black", "blue", "purple", "darkorange2", "green4"))

# Add function levels to df to control plotting order
df <- df %>% 
  mutate(source = factor(mod, levels = c("Obs.", "CFE", "CFE_X", "TOPMODEL", "NWM2.1")))


# Plot all
flow_all_plot <- 
  ggplot(filter(df, date >= as.Date("2010-10-01") & date <= as.Date("2011-09-30")), 
         aes(date, qout_cms, color = source)) + 
  geom_line() + 
  facet_wrap(~loc, scales = "free_y", ncol = 2) + 
  flow_color_scale + 
  theme(legend.position = c(0.05, 0.25), 
        legend.title = element_blank(),
        plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) + 
  labs(x = "Date", y = expression("Discharge ("*m^3*" "*s^-1*")"))
save_plot(plot = flow_all_plot, filename = paste0(plot_path, "flow_all.png"), 
          base_height = 4, base_width = 8)
```

    ## Warning: Removed 61 row(s) containing missing values (geom_path).
