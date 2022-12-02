Analysis of Coupled Snow-Runoff Models
================
Keith Jennings
2022-11-17

# Introduction

This directory includes analysis of coupled snow-runoff models run in
the Next Generation Water Resources Modeling Framework.

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
df <- readRDS("../data/snowFlow/qout_dly_summary.RDS")

# Summarize to just the period of overlapping data
df <- df %>% 
  filter(date >= as.Date("2007-10-01") &
           date <= as.Date("2013-09-30") &
           mod != "NWM2.1")

# Import basin metadata and compute area
metadata <- read.csv("../data/snowFlow/basin_attributes(3).csv")
basin_area_m2 = sum(metadata$areasqkm) * (1000^2)

# Compute cumulative discharge
df <- df %>% 
  group_by(mod) %>% 
  mutate(qout_cumsum_m = cumsum(qout_cms * 2600 * 24 / basin_area_m2))

# Add a path to save figures
plot_path = "figures/"
```

# Compute summary functions

``` r
# Put data into wider format so obs can be compared to sim
df_compare <- 
  left_join(filter(df, mod == "Obs."),
            filter(df, mod != "Obs."),
            by = "date", suffix = c("_obs", "_sim") )

# Make the functions
nse_FUN = function(x,y){1 - sum((x - y)^2)/
    sum((x - mean(x))^2)}
mean_bias_FUN = function(x, y){mean(y) - mean(x)}
rmse_FUN = function(x, y){sqrt(sum((y - x)^2) * 1/length(x))}
mae_FUN = function(x, y){mean(abs(y - x))}
kge_FUN = function(x, y){1 - sqrt(((cor(x,y) - 1)^2) + (((sd(y)/sd(x)) - 1)^2) + (((mean(y)/mean(x)) - 1)^2))}

# Summarize the data
df_objFun <- df_compare %>% 
  group_by(mod_sim) %>% 
  summarize(nse = nse_FUN(qout_cms_obs, qout_cms_sim),
            kge = kge_FUN(qout_cms_obs, qout_cms_sim),
            mean_bias = mean_bias_FUN(qout_cms_obs, qout_cms_sim),
            rmse = rmse_FUN(qout_cms_obs, qout_cms_sim),
            mae = mae_FUN(qout_cms_obs, qout_cms_sim))

# Make a table
knitr::kable(df_objFun)
```

| mod_sim  |       nse |       kge | mean_bias |     rmse |      mae |
|:---------|----------:|----------:|----------:|---------:|---------:|
| CFE      | 0.5243340 | 0.7070443 | 0.0188686 | 19.09918 | 10.33364 |
| CFE_X    | 0.5326399 | 0.7135862 | 0.0078376 | 18.93169 | 10.26417 |
| TOPMODEL | 0.5821064 | 0.6800620 | 4.0228252 | 17.90179 | 10.14784 |

``` r
# Make vars for easier plotting
kge_cfe  = df_objFun %>% slice(which(mod_sim == "CFE")) %>% 
  pull(kge) %>% round(., digits = 2)
kge_cfex = df_objFun %>% slice(which(mod_sim == "CFE_X")) %>% 
  pull(kge) %>% round(., digits = 2)
kge_topm = df_objFun %>% slice(which(mod_sim == "TOPMODEL")) %>% 
  pull(kge) %>% round(., digits = 2)
rmse_cfe  = df_objFun %>% slice(which(mod_sim == "CFE")) %>% 
  pull(rmse) %>% round(., digits = 1)
rmse_cfex = df_objFun %>% slice(which(mod_sim == "CFE_X")) %>% 
  pull(rmse) %>% round(., digits = 1)
rmse_topm = df_objFun %>% slice(which(mod_sim == "TOPMODEL")) %>% 
  pull(rmse) %>% round(., digits = 1)
mean_bias_cfe  = df_objFun %>% slice(which(mod_sim == "CFE")) %>% 
  pull(mean_bias) %>% round(., digits = 2)
mean_bias_cfex = df_objFun %>% slice(which(mod_sim == "CFE_X")) %>% 
  pull(mean_bias) %>% round(., digits = 2)
mean_bias_topm = df_objFun %>% slice(which(mod_sim == "TOPMODEL")) %>% 
  pull(mean_bias) %>% round(., digits = 2)
```

# Plot the data

``` r
# Make a color scheme
flow_color_scale <- scale_color_manual(name = "Source", 
                                       breaks = c("Obs.", "CFE", "CFE_X", "TOPMODEL"),
                                       labels = c("Obs.", "CFE", "CFE_X", "TOPMODEL"),
                                       values = c("black", "blue", "purple", "darkorange2"))

# Add function levels to df to control plotting order
df <- df %>% 
  mutate(source = factor(mod, levels = c("Obs.", "CFE", "CFE_X", "TOPMODEL")))


# Plot TOPMODEL
flow_topmod_plot <- 
  ggplot(filter(df, source == "Obs." | source == "TOPMODEL"), aes(date, qout_cms, color = source)) +
  geom_line() +
  flow_color_scale +
  labs(x = "Date", y = expression("Avg. Daily Discharge ("*m^3*" "*s^-1*")")) +
  theme(legend.position = c(0.05, 0.85), plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 180,
           label = paste0("KGE  = ", kge_topm), hjust = 0, color = "darkorange2") +
  # annotate(geom = "text", x = as.Date("2013-01-15"), y = 165,
  #          label = paste0("RMSE = ", rmse_topm), hjust = 0, color = "darkorange2") +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 165,
           label = as.expression(bquote(Bias == .(mean_bias_topm) ~ m^3*"/s")), 
           hjust = 0, color = "darkorange2") +
  coord_cartesian(clip = "off")
save_plot(plot = flow_topmod_plot, filename = paste0(plot_path, "flow_topmod.png"), 
          base_height = 4, base_width = 8)
```

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

``` r
# Plot TOPMODEL and CFE
flow_topmod_cfes_plot <-
  ggplot(filter(df, source == "Obs." | source == "TOPMODEL" | source == "CFE"), aes(date, qout_cms, color = source)) +
  geom_line() +
  flow_color_scale +
  labs(x = "Date", y = expression("Avg. Daily Discharge ("*m^3*" "*s^-1*")"))   +
  theme(legend.position = c(0.05, 0.85), plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 180,
           label = paste0("KGE  = ", kge_topm), hjust = 0, color = "darkorange2") +
  # annotate(geom = "text", x = as.Date("2013-01-15"), y = 165,
  #          label = paste0("RMSE = ", rmse_topm), hjust = 0, color = "darkorange2") +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 165,
           label = as.expression(bquote(Bias == .(mean_bias_topm) ~ m^3*"/s")), 
           hjust = 0, color = "darkorange2") +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 150,
           label = paste0("KGE  = ", kge_cfe), hjust = 0, color = "blue") +
  # annotate(geom = "text", x = as.Date("2013-01-15"), y = 135,
  #          label = paste0("RMSE = ", rmse_cfe), hjust = 0, color = "blue")
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 135,
           label = as.expression(bquote(Bias == .(mean_bias_cfe) ~ m^3*"/s")), 
           hjust = 0, color = "blue")
save_plot(plot = flow_topmod_cfes_plot, filename = paste0(plot_path, "flow_topmod_cfes.png"), 
          base_height = 4, base_width = 8)
```

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

``` r
# Plot all time series
flow_topmod_cfes_cfex_plot <- 
  ggplot(df, aes(date, qout_cms, color = source)) +
  geom_line() +
  flow_color_scale +
  labs(x = "Date", y = expression("Avg. Daily Discharge ("*m^3*" "*s^-1*")")) +
  theme(legend.position = c(0.05, 0.85), plot.margin = unit(c(5.5,12,5.5,5.5), "pt")) +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 180,
           label = paste0("KGE  = ", kge_topm), hjust = 0, color = "darkorange2") +
  # annotate(geom = "text", x = as.Date("2013-01-15"), y = 165,
  #          label = paste0("RMSE = ", rmse_topm), hjust = 0, color = "darkorange2") +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 165,
           label = as.expression(bquote(Bias == .(mean_bias_topm) ~ m^3*"/s")), 
           hjust = 0, color = "darkorange2") +
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 150,
           label = paste0("KGE  = ", kge_cfe), hjust = 0, color = "blue") +
  # annotate(geom = "text", x = as.Date("2013-01-15"), y = 135,
  #          label = paste0("RMSE = ", rmse_cfe), hjust = 0, color = "blue")
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 135,
           label = as.expression(bquote(Bias == .(mean_bias_cfe) ~ m^3*"/s")), 
           hjust = 0, color = "blue") + 
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 120,
           label = paste0("KGE  = ", kge_cfex), hjust = 0, color = "purple") +
  # annotate(geom = "text", x = as.Date("2013-01-15"), y = 105,
  #          label = paste0("RMSE = ", rmse_cfex), hjust = 0, color = "purple")
  annotate(geom = "text", x = as.Date("2013-01-15"), y = 105,
           label = as.expression(bquote(Bias == .(mean_bias_cfex) ~ m^3*"/s")), 
           hjust = 0, color = "purple")
save_plot(plot = flow_topmod_cfes_cfex_plot, filename = paste0(plot_path, "flow_topmod_cfes_cfex.png"), 
          base_height = 4, base_width = 8)
```

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

``` r
# Plot example time periods
date_range1 = seq.Date(from = as.Date("2008-10-20"), to = as.Date("2008-12-15"), by = "1 day")
date_range2 = seq.Date(from = as.Date("2011-06-01"), to = as.Date("2011-12-01"), by = "1 day")

# Plot all models showing baseflow issue
flow_all_baseflow_issue_plot <- 
  filter(df, date %in% date_range1) %>% 
  ggplot(aes(date, qout_cms, color = source)) +
  geom_line() +
  flow_color_scale +
  labs(x = "Date", y = expression("Avg. Daily Discharge ("*m^3*" "*s^-1*")"))+
  #theme(legend.position = c(0.6, 0.8))
  theme(legend.position = "none", plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))
save_plot(plot = flow_all_baseflow_issue_plot, filename = paste0(plot_path, "flow_all_baseflow_issue.png"), 
          base_height = 3, base_width = 4)

# Plot all models showing snow issue
flow_all_snow_issue_plot <-
  filter(df, date %in% date_range2) %>% 
  ggplot(aes(date, qout_cms, color = source)) +
  geom_line() +
  flow_color_scale +
  labs(x = "Date", y = expression("Avg. Daily Discharge ("*m^3*" "*s^-1*")")) +
  theme(legend.position = c(0.6, 0.8))
save_plot(plot = flow_all_snow_issue_plot, filename = paste0(plot_path, "flow_all_snow_issue.png"), 
          base_height = 3, base_width = 4)

# Plot cumulative sums
# TOPMODEL and Obs
cumulative_flow_topmod_plot <-
  ggplot(filter(df, source == "Obs." | source == "TOPMODEL"), 
              aes(date, qout_cumsum_m, color = source)) +
  geom_line(lwd = 1) +
  flow_color_scale +
  labs(x = "Date", y = "Cumulative Discharge (m)") +
  theme(legend.position = c(0.1,0.8), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))
save_plot(plot = cumulative_flow_topmod_plot, filename = paste0(plot_path, "cumulative_flow_topmod.png"), 
          base_height = 4, base_width = 8)

# TOPMODLE, CFE, and OBs
cumulative_flow_topmod_cfe_plot <-
  ggplot(filter(df, source == "Obs." | source == "TOPMODEL" | source == "CFE"), 
              aes(date, qout_cumsum_m, color = source)) +
  geom_line(lwd = 1) +
  flow_color_scale +
  labs(x = "Date", y = "Cumulative Discharge (m)") +
  theme(legend.position = c(0.1,0.8), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))
save_plot(plot = cumulative_flow_topmod_cfe_plot, filename = paste0(plot_path, "cumulative_flow_topmod_cfe.png"), 
          base_height = 4, base_width = 8)
```
