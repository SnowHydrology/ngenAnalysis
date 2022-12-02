# Summarize data from multiple NextGen catchments 
# and multiple runs into one dataframe

# Keith Jennings
# 2022-11-17

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
nwm_data <- "data/snowFlow/nwm_v2.1_chrt.11266500.csv"
obs_data <- "data/snowFlow/usgs_hourly_flow_2007-2019_11266500.csv"
export.dir = "data/snowFlow/"
export.cat = "qout_dly_summary.RDS"

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

# Summarize by day using UTC time (consistent with other prods)
df_summary <- df %>% 
  mutate(date = as.Date(datetime, tz = "UTC")) %>% 
  group_by(mod, date) %>% 
  summarize(q1 = mean(Q_OUT, na.rm = T),
            q2 = mean(Qout, na.rm = T)) %>% 
  mutate(qout_cms = ifelse(is.na(q1),
                       q2,
                       q1) * basin_area_m2 / 3600) # convert to m^3/s
df_summary <- df_summary %>% 
  ungroup() %>% select(mod, date, qout_cms)

# Read in the NWM data and summarize
nwm <- read.csv(nwm_data) %>% 
  mutate(datetime = as.POSIXct(time, tz = "UTC"),
         date = as.Date(datetime, tz = "UTC"),
         qout_cms = flow_cms * 100) # i don't know why you have to multiply NWM output by 100
nwm_summary <- nwm %>% 
  group_by(date) %>% 
  summarize(qout_cms = mean(qout_cms)) %>% 
  mutate(mod = "NWM2.1")

# Read in the obs data and summarize
obs <- read.csv(obs_data) %>% 
  mutate(datetime = as.POSIXct(POSIXct, tz = "UTC"),
         date = as.Date(datetime, tz = "UTC")) %>% 
  rename(qout_cms = q_cms)
obs_summary <- obs %>% 
  group_by(date) %>% 
  summarize(qout_cms = mean(qout_cms)) %>% 
  mutate(mod = "Obs.")

# Bind all
summary_all <- 
  bind_rows(df_summary,
            nwm_summary,
            obs_summary)

# Export the dataset
saveRDS(summary_all,
        file = paste0(export.dir, export.cat))
