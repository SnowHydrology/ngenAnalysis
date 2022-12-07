# Summarize data from multiple NextGen catchments 
# and multiple runs into one dataframe

# Keith Jennings
# 2022-11-17

# Load packages
library(tidyverse)

# User input
# Specify the data directories
data_dir <- "data/flowTownHall2022"
export.dir = "data/flowTownHall2022/"
export.cat = "qout_dly_summary.RDS"
files <- list.files(path = data_dir, full.names = T)


# Create a function for reading in a CSV and adding the filename as column
read_csv_filename <- function(filename){
  df <- read.csv(filename)
  df$source <- filename #EDIT
  df
}

df <- lapply(files, read_csv_filename) %>% 
  plyr::ldply(., bind_rows)
df <- df %>% 
  mutate(loc = str_sub(source, 23, 30),
         mod = ifelse(str_detect(source, "CFE_X"),
                      "CFE_X",
                      ifelse(str_detect(source, "CFE"),
                             "CFE", 
                             "TOPMODEL")),
         datetime = as.POSIXct(time, tz = "UTC"),
         source = NULL)

# Manually lengthen data
df_long <- bind_rows(df %>% select(datetime, qout_cms = q_cms_obs, loc, mod) %>% 
                       mutate(mod = "Obs."),
                     df %>% select(datetime, qout_cms = q_cms_sim, loc, mod),
                     df %>% select(datetime, qout_cms = NWM21_cms, loc, mod) %>% 
                       mutate(mod = "NWM2.1")) %>% 
  distinct() # remove duplicate obs and 2.1 values

# Summarize by day using UTC time (consistent with other prods)
df_summary <- df_long %>% 
  mutate(date = as.Date(datetime, tz = "UTC")) %>% 
  group_by(mod, loc, date) %>% 
  summarize(qout_cms = mean(qout_cms))


# Export the dataset
saveRDS(df_summary,
        file = paste0(export.dir, export.cat))
