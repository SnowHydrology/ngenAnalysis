# Summarize data from multiple NextGen catchments 
# and multiple runs into one dataframe

# Keith Jennings
# 2023-12-08

# Load packages
library(tidyverse)

# User input
# Specify the data directories
data_dirs <- data.frame(dir = c("data/snowFlow/NOAH_CFE/",
                                "data/snowFlow/NOAH_CFE_X/",
                                "data/snowFlow/NOAH_Topmodel/"),
                        mod = c("CFE",
                                "CFE_X",
                                "TOPMODEL"))
export.dir = "mcdanielAgu2023/"
export.cat = "baseline_vars_dly_summary.RDS"

# Import metadata and compute basin size
metadata <- read.csv("data/snowFlow/basin_attributes(3).csv")
basin_area_m2 = sum(metadata$areasqkm) * (1000^2)

# Create a function for reading in a CSV and adding the filename as column
read_csv_filename <- function(filename){
  df <- read.csv(filename)
  df$source <- filename #EDIT
  df
}

# Create a function for summarizing data
df_create <- function(dir, mod){
  files = list.files(path = dir, pattern =  "^cat")
  tmp = lapply(paste0(dir, files), read_csv_filename)
  tmp = plyr::ldply(tmp) %>% 
    mutate(mod = mod, 
           loc = str_extract(string = source, 
                             pattern = paste0("(?<=", dir, ").*(?=.csv)")))
  tmp
}

# Import the data
df <- data.frame()
for(i in 1:length(data_dirs$mod)){
  tmp = df_create(data_dirs[i, "dir"], data_dirs[i, "mod"]) %>% 
    mutate(datetime = as.POSIXct(Time, tz = "UTC"),
           source = NULL)
  df <- bind_rows(df, tmp)
}

# Convert TOPMODEL AET from accumulated to flux
df <- df %>% 
  group_by(mod, loc) %>% 
  mutate(aet_tmod = c(0, diff(land_surface_water__domain_time_integral_of_evaporation_volume_flux)))

# Summarize by day using UTC time (consistent with other prods)
df_summary <- df %>% 
  mutate(date = as.Date(datetime, tz = "UTC")) %>% 
  group_by(mod, date) %>% 
  summarize(q1 = mean(Q_OUT, na.rm = T),
            q2 = mean(Qout, na.rm = T),
            b1 = mean(NASH_LATERAL_RUNOFF + DEEP_GW_TO_CHANNEL_FLUX, na.rm = T),
            b2 = mean(land_surface_water__baseflow_volume_flux, na.rm = T),
            aet1 = mean(ACTUAL_ET, na.rm = T),
            aet2 = mean(aet_tmod, na.rm = T),
            s1 = mean(GIUH_RUNOFF, na.rm = T),
            s2 = mean(land_surface_water__domain_time_integral_of_overland_flow_volume_flux, na.rm = T)) %>% 
  mutate(qout_cms = ifelse(is.na(q1),
                           q2,
                           q1) * basin_area_m2 / 3600, # convert to m^3/s
         baseflow =  ifelse(is.na(b1),
                            b2,
                            b1) * basin_area_m2 / 3600,
         aet =  ifelse(is.na(aet1),
                       aet2,
                       aet1) * 1000 * 24,
         surface_runoff =  ifelse(is.na(s1),
                                  s2,
                                  s1) * basin_area_m2 / 3600) 
df_summary <- df_summary %>% 
  ungroup() %>% select(mod, date, qout_cms:surface_runoff)


# Export the dataset
saveRDS(df_summary,
        file = paste0(export.dir, export.cat))
