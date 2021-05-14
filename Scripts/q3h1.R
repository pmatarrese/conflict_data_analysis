#Q3H1

###################################################################################

yr_list <- sort(unique(merged_ged$year))
reg_list <- sort(unique(merged_ged$region))

q3_h1_tbl <-
  merged_ged %>%
  group_by(year) %>%
  mutate(count_yr = n(), #Number of observations that year
         yrs_passed = year - 1989,
         instant_con_yr = length(unique(conflict_new_id)),
         total_con_yr = get_con_yr(year),
         avg_con_yr = total_con_yr/(yrs_passed + 1)) %>% 
  ungroup() %>%
  group_by(year, region) %>%
  mutate(count_con_reg = n(), #Number of observations in that region that year
         instant_con_yr_reg = length(unique(conflict_new_id)),
         total_con_yr_reg = get_con_yr_reg(year, region),
         avg_con_yr_reg = total_con_yr_reg/(yrs_passed + 1)) %>%
  ungroup() %>%
  select(year,
         region,
         conflict_new_id,
         instant_con_yr,
         total_con_yr,
         avg_con_yr,
         instant_con_yr_reg,
         total_con_yr_reg,
         avg_con_yr_reg)

q3_h1_tbl_revised <-
  q3_h1_tbl %>%
  pivot_longer(ends_with("yr"),
               names_to = c("con_yr", ".value"),
               names_pattern = "(total|avg|instant)(_con_yr)") %>%
  pivot_longer(ends_with("reg"),
               names_to = c("con_yr_reg", ".value"),
               names_pattern = "(total|avg|instant)(_con_yr_reg)") %>%
  rename("data_type_yr" = `con_yr`,
         "data_type_yr_reg" = `con_yr_reg`,
         "con_yr" = `_con_yr`,
         "con_yr_reg" = `_con_yr_reg`)

q3_h1_tbl_instant <-
  q3_h1_tbl_revised %>%
  filter(data_type_yr == "instant",
         data_type_yr_reg == "instant") %>%
  select(-data_type_yr, -data_type_yr_reg)

#######################################################################################################

#######################################################################################################

q3_h1_tbl_instant %>%
  ggplot(aes(x = year, y = con_yr)) +
  geom_line(color = "#0072B5FF") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Total Number of Conflicts in each Year (Unique Dyads)",
       subtitle = "This plot follows the trend of total instances of violence.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Number of Conflicts")

ggsave(filename = "Q3H1_plots_world_trend.png",
       plot = last_plot(),
       width = 8,
       height = 6,
       device = "png",
       path = "Plots",
       dpi = 320)

# The plot indicates a brief increase in the number of violent conflicts from 1989-1990, to
# a rate where it remains steady between 150 and 200 conflicts in a given year. 2012-2013 sees
# a large increase in conflicts, passing 200 for the first time. 2012 and afterward sees a
# steady upwards trend in conflict numbers, reaching the overall global peak of 289 unique
# conflicts in 2018.

# The year 2019 represents an anomalous reading at only 33 conflicts, all of which from the
# Middle East. This could be due to how frequently organizations receive and confirm reports
# of these conflicts, but more than likely these reports are confirmed using reported
# information in years after those being observed, and the conflict compilation for 2019 was
# simply unfinished at the time data was gathered. This anomaly will also account for the final
# data point in the graph for the Middle East when we investigate by region.

q3_h1_tbl_instant %>%
  ggplot(aes(x = year, y = con_yr_reg, color = region)) +
  geom_line() +
  scale_y_log10() +
  scale_color_nejm(name = element_blank()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  labs(title = "Number of Conflicts per Year",
       subtitle = "Given in Log Base-10 Form and Separated by Region",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Number of Conflicts (Log-scale)") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q3H1_plots_by_region.png",
       plot = last_plot(),
       width = 8,
       height = 6,
       device = "png",
       path = "Plots",
       dpi = 320)

# There is a consistent upward trend in the number of violent conflicts in both Africa and the
# Middle East, aligning with our hypothesis. Even accounting for the anomalous 2019 data point
# in the Middle East graph, that region still maintains an upward trend for the number of
# violent conflicts in a given year. Like previously mentioned, it is likely that data for 2019
# had not been completed by UCDP at the time data was gathered from their website. This could
# account for the incompleteness of the readings for the year 2019 and the lack of a data point
# for this year for other regions.

# The number of conflicts in Asia and the Americas has been mostly stable for the past 3 decades,
# though the Americas are more susceptible to fluctuations, likely based on activity in Latin
# and South America, both with and without our own government's intervention within those regions.

q3_h1_tbl_instant <-
  q3_h1_tbl_instant %>%
  mutate(decade = merged_ged$decade) %>%
  select(decade, year, everything())

filenames <- sort(unique(q3_h1_tbl_instant$decade))

for(dec in filenames){
  q3_h1_tbl_instant %>%
    filter(decade == dec) %>%
    ggplot(aes(x = year, y = con_yr_reg, color = region)) +
    geom_line() +
    scale_color_nejm(name = element_blank()) +
    scale_x_continuous(breaks = pretty_breaks()) +
    facet_wrap(~ region) +
    labs(title = paste("Total Number of Conflicts in each Year", as.character(dec), collapse = "_"),
         caption = "Source: UCDP dataset",
         x = "Year",
         y = "Number of Conflicts") +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0),
          legend.direction = "vertical")
  
  fname <- paste("Q3H1_plots_by_region_",
                 as.character(dec),
                 ".png",
                 collapse = "_")
  
  ggsave(filename = fname,
         plot = last_plot(),
         width = 8,
         height = 6,
         device = "png",
         path = "Plots",
         dpi = 320)
}

# These 3 sets of graphs, separated by decade, further corroborate the graph of all unique
# conflicts by year separated by region. When pieced together, they still interact with our
# hypothesis in the same manner.

#######################################################################################################