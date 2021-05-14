#################################################
# Question 2 Has conflict become more or less deadly over the years?

### DATA TRANSFORMATION 
# Mutates columns for total, mean and median fatalities by country and year

deadlier <- merged_ged %>%
  group_by(country, year) %>%
  mutate(yearly_fatalities = sum(best),
         mean_fatalities = mean(best),
         median_fatalities = median(best)) %>%
  ungroup()


# Arranged fatality table to refer to the most deadly conflicts. Shows best estimate for fatalities in a unique event, yearly fatalities, mean and median fatalities for the country. 

deadlier_tbl <- deadlier %>%
  select(country, year, yearly_fatalities, mean_fatalities, median_fatalities, best, dyad_name, everything()) %>%
  arrange(desc(best, yearly_fatalities, mean_fatalities, median_fatalities))

# Table that seeks to identify key patterns in conflicts from 2010 onwards by country, 
# arranged by highest fatalities (best estimate), and noting total fatalities that year, yearly mean and type of violence. 
# The Dyad name identifies the particular conflict. 
# From the first 30 rows, we can clearly see Afghanistan, Myanmar and Pakistan as the countries in Asia with the highest fatalities in individual conflicts. 
# Type of violence is primarily state-based conflict between government and Taleban, but Myanmar cases are one-sided conflicts, government vs civilians.

high_asia <- deadlier %>%
  group_by(dyad_name) %>%
  filter(region == "Asia",
         year > "2010") %>%
  arrange(desc(best, yearly_fatalities, mean_fatalities)) %>%
  select(dyad_name, country, year, best, yearly_fatalities, mean_fatalities, type_of_violence) %>%
  distinct()

high_asia %>%
  head(10) %>%
  kable(col.names = c("Dyad Name", "Country", "Year", "Best Estimate of Deaths", "Total Fatalities per Year", "Mean Fatalities", "Type of Violence"),
        caption = "The Top 10 Deadliest Instances of Violence by Year,\nwith mean fatalities and type of violence\n(2010s in Asia)") %>%
  kable_styling() %>%
  save_kable("Results/Q2H1_high_asia.png")

# Compares Taleban related conflict fatalities to the Asian mean values. 
# As you can see, Taleban conflicts have higher fatalities than the average, with a band in the mid-90s seeing an upsurge of taleban related fatalities, 
# and then again from 2005 onwards. This is most likely a consequence of the aftermath of the War in Afghanistan / Operation Enduring Freedom. 

taleban_conflicts <- deadlier %>%
  filter(region == "Asia",
         str_detect(dyad_name, "Taleban")) 

taleban_tbl <- taleban_conflicts %>%
  arrange(desc(best, yearly_fatalities, mean_fatalities)) %>%
  select(dyad_name, country, year, best, yearly_fatalities, mean_fatalities, type_of_violence)

taleban_tbl %>%
  head(10) %>%
  kable(col.names = c("Dyad Name", "Country", "Year", "Best Estimate of Deaths", "Total Fatalities per Year", "Mean Fatalities", "Type of Violence"),
        caption = "Instances of Violence Involving the Taleban") %>%
  kable_styling() %>%
  save_kable("Results/Q2H1_tables-taleban.png")

### PLOTS
#########################################
# Question 2: Has conflict become more or less deadly over the years?

# Basic overview plot to demonstrate the peaks caused by Africa and the Middle East. The following plot will use log10 transformation to show a more even distribution of fatalities. 

deadlier %>%
  ggplot() +
  geom_bar(aes(x = year, y = n_distinct(yearly_fatalities), fill = region), stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  labs(title = "Total fatalities by year and by region (1989-2019)",
       subtitle = "Log transformation will be required to better understand the data, as we have\nmassive peaks in the Middle East and Africa that dwarf the scale of fatality\ncounts in other regions.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Total Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot_1-fatalities_yr_region.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

###
# Changed to log10 scale, to reduce peaks and have a more even view of the fatality distribution. Trend in fatality numbers by region should be much clearer. 

deadlier %>%
  ggplot() +
  geom_bar(aes(x = year, y = log10(n_distinct(yearly_fatalities)), fill = region), stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  labs(title = "Total fatalities by year and by region (1989-2019)",
       subtitle = "Although Africa is experiencing rising fatalities in conflict, the clear\nleader in yearly fatality growth is Asia",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Total Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot_2-fatalities_yr_region_logged.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Filter for Rwanda is used in mean plot as the massive peak still causes scale to shrink for other regions, as mean measurements are easily affected by massive outliers. 

deadlier %>%
  filter(!(country == "Rwanda" & year %in% 1990:1996)) %>%
  ggplot() +
  geom_bar(aes(x = year, y = mean_fatalities, fill = region), stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  labs(title = "Accumulated mean fatalities by year and by region (1989-2019)",
       subtitle = "Mean fatalities have spiked in Africa and the Middle East in the last decade,\nbut are falling over the 31 year time period. In Asia however, mean fatalities are rising.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Accumulated Mean Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot_3-mean_fatalities.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Median fatalities are seeing growth, even considering the fact that they are less affected by outliers like the Rwandan Genocide. As such, Rwanda is not filtered in this case. 

deadlier %>%
  ggplot() +
  geom_bar(aes(x = year, y = median_fatalities, fill = region), stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  labs(title = "Accumulated median fatalities by year and by region (1989-2019)",
       subtitle = "Median fatalities follow a similar trend to mean fatalities, in that they are decreasing\nin most regions, with the exception of a minor rise in the Americas and a much higher\ngrowth rate in Asia.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Accumulated Median Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot_4-median_fatalities.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)


# This plot shows us that the distribution of national mean fatalities by region. Each observation/data point represents the national mean of fatalities from violent conflict in that year. 
# As such, we can see that mean fatalities within regions have been falling compared to the 1990s. For instance, in Africa mean fatalities were quite high, with some countries
# having a yearly mean for violent conflict fatalities above 1,500 deaths. 

ggplot() +
  geom_point(data = deadlier,
             aes(x = year, y = mean_fatalities, fill = region),
             shape = 21,
             stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  labs(title = "Distribution of mean fatalities by year and region (1989-2019)",
       subtitle = "The distribution of mean yearly fatalities shows that fatalities for countries on  year to year basis have \nfallen over the past three decades",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Mean Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot-mean_regional_distribution.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Log10 version to better see the trend in regional mean fatality distribution over time

ggplot() +
  geom_point(data = deadlier,
             aes(x = year, y = log10(mean_fatalities), fill = region),
             shape = 21, 
             stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution of mean fatalities by year and region, Log10 Scale (1989-2019)",
       subtitle = "The distribution of mean yearly fatalities shows that fatalities for countries on  year to year basis have \nfallen over the past three decades",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Mean Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot-mean_log_distribution.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)



# Plot is similar to the above mean regional distribution, but the trends are slightly different. Median yearly fatalities for countries within regions seems to be falling in Africa
# and Asia, but shows some evidence of minor growth in Europe and the Middle East in the past decade. The Americas seems quite steady, with no clear evidence of growth/decline. 

ggplot() +
  geom_point(data = deadlier,
             aes(x = year, y = median_fatalities, fill = region),
             shape = 21,
             stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  labs(title = "Distribution of median fatalities by year and region (1989-2019)",
       subtitle = "The distribution of median yearly fatalities shows a more uneven trend over time across regions, \nwith growth in the Middle East and Europe. However, it seems that yearly medians \nare falling for Asia and Africa.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Median Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot-median_regional_distribution.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Log10 version to better see the trend in regional median fatality distribution over time

ggplot() +
  geom_point(data = deadlier,
             aes(x = year, y = log10(median_fatalities), fill = region),
             shape = 21,
             stat = "identity") +
  scale_fill_nejm(name = element_blank()) +
  scale_y_continuous(labels = comma,
                     breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution of median fatalities by year and region (1989-2019)",
       subtitle = "The distribution of median yearly fatalities shows a more uneven trend over time across regions, \nwith growth in the Middle East and Europe. However, it seems that yearly medians \nare falling for Asia and Africa.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Median Fatalities") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q2H1_plot-median_log_distribution.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

###
# Asia highlighted as a growth area in fatalities since 2010 (higher peak in Middle East, but decline in past few years). Primary region where fatalities have been rising. 
# Shows average fatalities (mean and median) by point value, to reflect the distribution of average deaths in a conflict. 
# When compared with the following graph, this one reveals that while average fatalities in a given conflict event have gone down, the number of conflicts have actually increased. 
# Thereby increasing total fatalities within a given year from 2010 onwards. 

deadlier %>%
  filter(region == "Asia",
         year > "1989") %>%
  ggplot() +
  geom_point(aes(x = year, y = mean_fatalities),
             shape = 21,
             color = "#0072B5FF",
             stat = "identity") +
  geom_point(aes(x = year, y = median_fatalities),
             shape = 21,
             color = "#BC3C29FF",
             stat = "identity") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Distribution of average fatalities from conflict by year in Asia (1989-2019)",
       subtitle = "The distribution of both mean (in blue) and median (in red) fatalities of Asian conflicts\nare decreasing in number of fatalities but as we will see in later plots, this does not mean\ntotal yearly fatalities are falling.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Fatalities")

ggsave(filename = "Q2H1_plot_5-asia_distribution.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Bar plot to show cummulative mean of fatalities in all conflicts in Asia by year, to identify which years have seen the highest growth in average fatalities,
# while properly reflecting the number of conflicts that have occured in a year. 

deadlier %>%
  filter(region == "Asia") %>%
  ggplot() +
  geom_bar(aes(x = year, y = mean_fatalities), fill = "#0072B5FF", stat = "identity") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Mean fatalities in Asia accumulated by year (1989-2019)",
       subtitle = "The mean fatalities of conflicts in Asia (accumulated by year), have been on the rise\nand exceed mean fatalities pre-2000.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Accumulated Mean Fatalities") 

ggsave(filename = "Q2H1_plot_6-asia_mean_fatalities.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Plot showing distribution of Asian mean (orange) and median (red) by year, and Afghanistan fatality counts (in blue), which are clearly higher than the Asian averages, 
# notably from 2005 to 2018. Two Afghan conflict events have fatality counts higher than 1500 casualties (outliers in 2014 and 2015).

ggplot() +
  geom_point(data = taleban_conflicts, aes(x = year, y = best), color = "#0072B5FF", stat = "identity") +
  geom_point(data = deadlier, aes(x = year, y = mean_fatalities), color = "#E18727FF", stat = "identity") +
  geom_point(data = deadlier, aes(x = year, y = median_fatalities), color = "#BC3C29FF", stat = "identity") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Comparison of Taleban related conflict fatalities to Asian averages (1989-2019)",
       subtitle = "Violent conflict events in Afghanistan (in blue) have a much higher fatality count than\nthe Asian mean (in orange) or median (in red)",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Fatalities") 

ggsave(filename = "Q2H1_plot_7-taleban_fatalities_count.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)
