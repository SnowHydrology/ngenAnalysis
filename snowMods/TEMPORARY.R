library(tidyverse)
library(cowplot); theme_set(theme_bw(base_size = 14))
devtools::source_url("https://github.com/SnowHydrology/date_functions/blob/main/doy_dowy.R?raw=TRUE")
devtools::source_url("https://github.com/SnowHydrology/date_functions/blob/main/wateryear.R?raw=TRUE")

# User input
cat.output = "data/camels_11266500/snowMods_cat_dly.RDS"
cat.input = "data/camels_11266500/snowMods_cat_forcing_dly.RDS"
runoff.input = "data/camels_11266500/snowMods_cat_runoff_dly.RDS"
meta.input = "data/camels_11266500/basin_attributes.csv"
s_a.input  = "data/camels_11266500/slope_aspect.csv"

# Read in data
cat <- readRDS(cat.output)
meta <- left_join(read_csv(meta.input),
                  read_csv(s_a.input), 
                  by = "ID")
forcing <- readRDS(cat.input)
runoff <- readRDS(runoff.input)

# Add a weight field for calculating basin summaries
meta <- meta %>% 
  mutate(weight = areasqkm / sum(areasqkm))

range(meta$elevation)
range(meta$meanPPT)
range(meta$snowFrac)
range(meta$slope.y)
median(meta$aspect)
ggplot(meta, aes(aspect)) + 
  geom_histogram(bins = 20, fill = "lightblue", color = 'black') +
  coord_polar() +
  scale_x_continuous(limits = c(0,360), breaks = c(0, 90, 180, 270),
                     labels = c("N", "E", "S", "W"))  +
  scale_y_continuous(breaks = seq(0,15, by = 3)) +
  annotate(x = 105, y = 3, label = 3, geom = "text") +
  annotate(x = 105, y = 6, label = 6, geom = "text") +
  annotate(x = 105, y = 9, label = 9, geom = "text") +
  annotate(x = 105, y = 12, label = 12, geom = "text") +
  annotate(
    x = 45, 
    y = 9,
    label = "Aspect\nHistogram",
    geom = "text",
    color = "gray12") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
  
# Add summary date info to cat
cat <- cat %>% 
  mutate(wyear = wyear_FUN(date),
         doy = doy_FUN(date),
         dowy = dowy_FUN(date, doy))

# Compute a time series of daily basin-average SWE by scenario
basin_swe_avg_dly <- cat %>% 
  group_by(scenario, date) %>% 
  left_join(select(meta, ID, weight),
            by = c("loc" = "ID")) %>% 
  summarize(swe = sum(swe * weight))

# Compute time series of other vars, basin avg
basin_avg_dly <- cat %>% 
  group_by(scenario, date) %>% 
  left_join(select(meta, ID, weight),
            by = c("loc" = "ID")) %>% 
  summarize(swe = sum(swe * weight),
            qinsur = sum(qinsur * weight),
            et = sum(et * weight))

basin_forcing_avg_dly <- forcing %>% 
  group_by(date) %>% 
  left_join(select(meta, ID, weight),
            by = c("loc" = "ID")) %>% 
  summarize(tair = sum(tair * weight),
            ppt = sum(ppt * weight),
            q = sum(q * weight))

basin_runoff_avg_dly <- runoff %>% 
  group_by(scenario, date) %>% 
  left_join(select(meta, ID, weight),
            by = c("loc" = "ID")) %>% 
  summarize(qout = sum(qout * weight))

# Make a plot of all
p2015plus <- left_join(filter(basin_avg_dly, scenario == "s-a_binlog"),
                       basin_forcing_avg_dly,
                       by = "date") %>% 
  filter(date > as.Date("2014-09-30")) %>%
  ggplot() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())
plot_grid(
  p2015plus + geom_line(aes(date, ppt * 3600 * 24), color = "royalblue") +
    labs(y = "Precip.\n(mm/d)"),
  p2015plus + geom_line(aes(date, swe), color = "purple") +
    labs(y = "SWE\n(mm)"),
  p2015plus + geom_line(aes(date, et * 3600 * 24 *1000), color = "darkorange3")+
    labs(y = "ET\n(mm/d)"),
  p2015plus + geom_line(aes(date, qinsur * 3600 * 24 *1000), color = "azure4")+
    labs(y = "Water Flux\n(mm/d)", x = "Date") + 
    theme(axis.title.x = element_text(), axis.text.x = element_text()),
  ncol = 1, align = "v", rel_heights = c(1,1,1,1.33)
)

# Make a plot of all
p2008_2013 <- left_join(filter(basin_avg_dly, scenario == "s-a_binlog"),
                       basin_forcing_avg_dly,
                       by = "date") %>% 
  filter(date > as.Date("2007-09-30") & date < as.Date("2013-10-01")) %>%
  ggplot() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())
plot_grid(
  p2008_2013 + geom_line(aes(date, ppt * 3600 * 24), color = "royalblue") +
    labs(y = "Precip.\n(mm/d)"),
  p2008_2013 + geom_line(aes(date, swe), color = "purple") +
    labs(y = "SWE\n(mm)"),
  p2008_2013 + geom_line(aes(date, et * 3600 * 24 *1000), color = "darkorange3")+
    labs(y = "ET\n(mm/d)"),
  p2008_2013 + geom_line(aes(date, qinsur * 3600 * 24 *1000), color = "azure4")+
    labs(y = "Water Flux\n(mm/d)", x = "Date") + 
    theme(axis.title.x = element_text(), axis.text.x = element_text()),
  ncol = 1, align = "v", rel_heights = c(1,1,1,1.33)
)


# Make a plot of runoff
p2008_2013_runoff <- basin_runoff_avg_dly %>% 
  filter(date > as.Date("2007-09-30") & date < as.Date("2013-10-01")) %>%
  ggplot()
p2008_2013_runoff + 
  geom_line(aes(date, qout * 1000 * 24, color = scenario))+
  labs(x = "Date", y = "Runoff (mm/d)")+
  scale_color_manual(values = c("darkorange3", "darkgoldenrod3", "cadetblue4")) +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank())

# Summarize max SWE by wyear and scenario
cat_summary <- cat %>% 
  group_by(scenario, wyear, loc) %>% 
  summarize(max_swe = max(swe),
            max_swe_dowy = which.max(swe),
            scd = sum(swe > 5)) %>% 
  left_join(.,
            select(meta, ID, elevation, slope = slope.y, aspect),
            by = c("loc" = "ID")) %>% 
  filter(wyear != 2007) %>% 
  mutate(aspect_c = ifelse(aspect > 45 & aspect <= 135,
                           "E",
                           ifelse(aspect > 135 & aspect <= 225,
                                  "S",
                                  ifelse(aspect > 225 & aspect <= 315,
                                         "W",
                                         "N"))))
ggplot(cat_summary, aes(max_swe, elevation, color = aspect_c, shape = scenario)) +
  geom_point() +
  scale_color_viridis_d() +
  facet_wrap(~wyear)

# Plot max swe diffs by aspect
slope_diff <- left_join(filter(cat_summary, scenario == "s-a"),
                  filter(cat_summary, scenario == "baseline"),
                  by = c("loc", "wyear", "elevation", "aspect")) %>% 
  mutate(max_swe_diff = max_swe.x - max_swe.y,
         scd_diff = scd.x - scd.y)

# # Plot max swe diffs by aspect
# max_swe_diff_s_a_plot <-
#   slope_diff %>% 
#   filter(wyear %in% c(2008, 2015, 2019)) %>% 
#   ggplot(aes(aspect, abs(max_swe_diff), 
#              fill = (max_swe_diff >= 0), 
#              color = (max_swe_diff >= 0))) + 
#   geom_bar(stat = "identity") +
#   geom_point(alpha = 0.6, size = 2)+
#   coord_polar() +
#   scale_fill_manual(values = c("darkred", "darkblue"),
#                     labels = c("Loss", "Gain")) +
#   scale_color_manual(values = c("darkred", "darkblue"),
#                      labels = c("Loss", "Gain")) +
#   scale_x_continuous(limits = c(0,360), breaks = c(0, 90, 180, 270),
#                      labels = c("N", "E", "S", "W"))  +
#   annotate(x = 135, y = 40, label = 40, geom = "text") +
#   annotate(x = 135, y = 80, label = 80, geom = "text") +
#   annotate(x = 135, y = 120, label = 120, geom = "text") +
#   annotate(x = 135, y = 160, label = 160, geom = "text") +
#   annotate(
#     x = 112.5, 
#     y = 120,
#     label = "Max SWE ∆\n(mm)",
#     geom = "text",
#     color = "gray12",
#     family = "Bell MT"
#   ) +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title = element_blank(),
#         legend.title = element_blank(),
#         axis.text.x = element_text(size = 12),
#         legend.position = c(0.96, 0.1),
#         legend.background = element_blank()) +
#   facet_wrap(~wyear)
# 
# # Plot scd diffs by aspect
# scd_diff_s_a_plot <-
#   slope_diff %>% 
#   filter(wyear %in% c(2008, 2015, 2019)) %>% 
#   ggplot(aes(aspect, abs(scd_diff), 
#              fill = (scd_diff >= 0), 
#              color = (scd_diff >= 0))) + 
#   geom_bar(stat = "identity") +
#   geom_point(alpha = 0.6, size = 2)+
#   coord_polar() +
#   scale_fill_manual(values = c("darkred", "darkblue"),
#                     labels = c("Loss", "Gain")) +
#   scale_color_manual(values = c("darkred", "darkblue"),
#                      labels = c("Loss", "Gain")) +
#   scale_x_continuous(limits = c(0,360), breaks = c(0, 90, 180, 270),
#                      labels = c("N", "E", "S", "W"))  +
#   annotate(x = 135, y = 20, label = 20, geom = "text") +
#   annotate(x = 135, y = 40, label = 40, geom = "text") +
#   annotate(x = 135, y = 60, label = 60, geom = "text") +
#   annotate(x = 135, y = 80, label = 80, geom = "text") +
#   annotate(
#     x = 112.5,
#     y = 60,
#     label = "SCD ∆\n(d)",
#     geom = "text",
#     color = "gray12",
#     family = "Bell MT"
#   ) +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title = element_blank(),
#         legend.title = element_blank(),
#         axis.text.x = element_text(size = 12),
#         legend.position = c(0.96, 0.1),
#         legend.background = element_blank()) +
#   facet_wrap(~wyear)
# 
# # Put the two plots together
# plot_grid(
#   max_swe_diff_s_a_plot,
#   scd_diff_s_a_plot,
#   ncol = 1
# )

# Plot max swe diffs by aspect
max_swe_diff_s_a_plot <-
  slope_diff %>% 
  ggplot(aes(aspect, abs(max_swe_diff), 
             fill = (max_swe_diff >= 0), 
             color = (max_swe_diff >= 0))) + 
  geom_segment(aes( y = 0, xend = aspect, yend = abs(max_swe_diff)),
               alpha = 0.2) +
  geom_point(alpha = 0.6, size = 2)+
  coord_polar() +
  scale_fill_manual(values = c("darkred", "darkblue"),
                    labels = c("Loss", "Gain")) +
  scale_color_manual(values = c("darkred", "darkblue"),
                     labels = c("Loss", "Gain")) +
  scale_x_continuous(limits = c(0,360), breaks = c(0, 90, 180, 270),
                     labels = c("N", "E", "S", "W"))  +
  annotate(x = 115, y = 50, label = 50, geom = "text") +
  annotate(x = 115, y = 100, label = 100, geom = "text") +
  annotate(x = 115, y = 150, label = 150, geom = "text") +
  annotate(x = 115, y = 200, label = 200, geom = "text") +
  annotate(
     x = 310, 
     y = 140,
     label = "Max SWE ∆\n(mm)",
     geom = "text",
     color = "gray12") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.position = "none") +
    ylim(0,200)

# Plot scd diffs by aspect
scd_diff_s_a_plot <-
  slope_diff %>% 
  ggplot(aes(aspect, abs(scd_diff), 
             fill = (scd_diff >= 0), 
             color = (scd_diff >= 0))) + 
    geom_segment(aes( y = 0, xend = aspect, yend = abs(scd_diff)),
                 alpha = 0.2) +
    geom_point(alpha = 0.6, size = 2)+
  coord_polar() +
  scale_fill_manual(values = c("darkred", "darkblue"),
                    labels = c("Loss", "Gain")) +
  scale_color_manual(values = c("darkred", "darkblue"),
                     labels = c("Loss", "Gain")) +
  scale_x_continuous(limits = c(0,360), breaks = c(0, 90, 180, 270),
                     labels = c("N", "E", "S", "W"))  +
  annotate(x = 105, y = 20, label = 20, geom = "text") +
  annotate(x = 105, y = 40, label = 40, geom = "text") +
  annotate(x = 105, y = 60, label = 60, geom = "text") +
  annotate(x = 105, y = 80, label = 80, geom = "text") +
  annotate(
    x = 310, 
    y = 60,
    label = "SCD ∆\n(d)",
    geom = "text",
    color = "gray12" ) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.position = c(0.86, 0.1),
        legend.background = element_blank()) 

# Put the two plots together
plot_grid(
  max_swe_diff_s_a_plot,
  scd_diff_s_a_plot,
  ncol = 2
)

# Plot basin avg SWE
basin_swe_avg_dly %>% 
  filter(scenario %in% c("s-a", "baseline")) %>% 
  ggplot(aes(date, swe, color = scenario)) +
  geom_line(lwd = 1) +
  scale_color_viridis_d(end = 0.6,
                        labels = c("Baseline", "Slope & Aspect"),
                     name = "Scenario") +
  labs(x = "Date", y = "Basin Avg. SWE (mm)") +
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_blank())

# Boxplot of max swe and snow cover duration for s-a and baseline by aspect
cat_summary %>% 
  filter(scenario %in% c("s-a", "baseline")) %>% 
  ggplot(aes(aspect_c, scd, fill = scenario)) +
  geom_boxplot()
# Select catchments to plot
cat %>% 
  filter(scenario %in% c("s-a", "baseline") &
           loc %in% c("cat-40", "cat-238")) %>% 
  ggplot(aes(date, swe, color = scenario)) +
  geom_line(lwd = 1) +
  scale_color_viridis_d(end = 0.6,
                        labels = c("Baseline", "Slope & Aspect"),
                        name = "Scenario") +
  labs(x = "Date", y = "Sub-basin SWE (mm)") +
  facet_wrap(~loc, ncol = 1)+
  theme(legend.position ="top")


# Calculate mean difference between binlog and baseline
phase_diff <- left_join(filter(cat_summary, scenario == "wb"),
                  filter(cat_summary, scenario == "baseline"),
                  by = c("loc", "wyear", "elevation", "aspect")) %>% 
  mutate(max_swe_diff = max_swe.x - max_swe.y,
         scd_diff = scd.x - scd.y)

# Plot max swe differences by phase method
max_swe_diff_phase_plot <-
  ggplot(phase_diff, aes(max_swe_diff ,elevation, color = (max_swe_diff >=0))) +
  geom_point(alpha = 0.5) + 
  geom_segment(aes(x = 0, xend = max_swe_diff, yend = elevation), alpha = 0.5) +
  scale_color_manual(values = c("darkred", "darkblue"),
                     labels = c("Loss", "Gain"),
                     name = element_blank()) +
  labs(x = "Max SWE Difference (mm)",
       y = "Sub-basin Elevation (m)") + 
  theme(legend.position = "none")

# Plot scd differences by phase method
scd_diff_phase_plot <-
  ggplot(phase_diff, aes(scd_diff,elevation, color = (scd_diff >=0))) +
  geom_point(alpha = 0.5) + 
  geom_segment(aes(x = 0, xend = scd_diff, yend = elevation), alpha = 0.5) +
  scale_color_manual(values = c("darkred", "darkblue"),
                     labels = c("Loss", "Gain"),
                     name = element_blank()) +
  labs(x = "SCD Difference (d)",
       y = "Sub-basin Elevation (m)") +
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_blank()) 

# Put the two plots together
plot_grid(
  max_swe_diff_phase_plot,
  scd_diff_phase_plot,
  ncol = 2
)

# Select catchments to plot
cat %>% 
  filter(scenario %in% c("wb", "baseline") &
           loc %in% c("cat-40", "cat-238")) %>% 
  ggplot(aes(date, swe, color = scenario)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("black", "red"),
                     labels = c("Baseline", "Wet Bulb"),
                     name = "Scenario") +
  labs(x = "Date", y = "Sub-basin SWE (mm)") +
  facet_wrap(~loc, ncol = 1)+
  theme(legend.position ="top")
