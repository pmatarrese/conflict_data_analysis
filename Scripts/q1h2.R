#################################################
# Question 1: "What kinds of conflicts have become more prevalent worldwide in the past 
# three decades?"
# Hypothesis 1: "There has been a reduction in violence perpatrated by government forces,
# but an increase in violent conflicts by and between non-state actors in recent years."

# transform data

q1_h2_tbl <- merged_ged %>%
  group_by(year) %>%
  mutate(count_yr = n(),
         total_deaths_yr = sum(best),
         ave_death_instance_yr = total_deaths_yr/count_yr) %>%
  ungroup() %>%
  group_by(year, region) %>%
  mutate(count_yr_region = n(),
         total_deaths_yr_region = sum(best),
         ave_death_instance_yr_region = total_deaths_yr_region/count_yr_region) %>%
  ungroup()

q1_h2_tbl_woR <- merged_ged_wo_rwanda %>%
  group_by(year) %>%
  mutate(count_yr = n(),
         total_deaths_yr = sum(best),
         ave_death_instance_yr = total_deaths_yr/count_yr) %>%
  ungroup() %>%
  group_by(year, region) %>%
  mutate(count_yr_region = n(),
         total_deaths_yr_region = sum(best),
         ave_death_instance_yr_region = total_deaths_yr_region/count_yr_region) %>%
  ungroup()

# generate plots

q1_h2_tbl %>%
  ggplot(aes(x = year, y = count_yr, color = ave_death_instance_yr)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_color_gradient(high = "#BC3C29FF",
                       low = "#0072B5FF",
                       name = "Average Number of Deaths\nper Instance (by year)") +
  labs(title = "Reported Instances of Violence Worldwide (1989 - 2019)",
       subtitle = "How much of the upward trend is related to better reporting?",
       caption = "Source: UCDP dataset",
       y = "Number of Instances",
       x = "Year")

# This overview plot raises some interesting questions.  Firstly, the graph shows a pretty steady
# upward trend in instances (with a huge spike in the last 5 years).  Now, is this a true trend
# or could it possibly be attributed to better reporting?  The world of telecommunication today
# is a far cry from what it was in 1989.  Additionally, the huge dip in 2019 certainly can't be
# an accurate representation of conflicts.  The easiest explanation for this is that there is a
# lag in reporting, as the process for data gathering involves validating auto-gathered data, as
# well as translation from local news sources.  It's reasonable to assume that further updates
# to the UCDP dataset will include more complete numbers.

ggsave(filename = "Q1H2_plot_1-world_trend.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

q1h2_plots("1990s")
q1h2_plots("2000s")
q1h2_plots("2010s")

# These plots show that Europe and the Americas have seen an overall decrease across decades, 
# while all other regions have seen a steady increase across our dataset.  Of particular interest
# is the spike in the Middle East.  Again, we want to take a look at the validity of our dataset.

# First Level Findings:
# I want to look further into why the overall number of of instances has seen a steady increase.
# I could possibly bring in data from the World Bank.  Let's drill down.

# drill down

q1_h2_tbl %>%
  group_by(year) %>%
  summarise(total_reports = n()) %>%
  ggplot(aes(x = year, y = total_reports)) +
  geom_col(fill = "#0072B5FF") +
  geom_smooth(color = "#BC3C29FF") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(title = "Number of Observations in UCDP Dataset",
       subtitle = "We should expect many more reported instances for 2019",
       caption = "Source: UCDP dataset",
       x = element_blank(),
       y = "Number of Reports")

# This plot gives us the overall trend in reporting (at least in our dataset).  I added a loess
# line to give a good visual cue.  As we can see, the 2019 observations are well under what we 
# would expect them to be.

ggsave(filename = "Q1H2_plot_3-observations.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

highest_reports <- q1_h2_tbl %>%
  group_by(year, country) %>%
  summarise(total_reports = n()) %>%
  arrange(desc(total_reports)) %>%
  head(20)

highest_reports %>%
  kable(col.names = c("Year", "Country", "Total Number of Reports of Violence"),
        caption = "The Top 20 Country-Years of Reported Violence in the UCDP dataset") %>%
  kable_styling() %>%
  save_kable("Results/Q1H2_tables_top_country_yr.png")

# This table gave us a list of countries that had the highest count of observations in any given
# year.  Now if I bring in World Bank Development indicators pertaining to cellphone and internet
# users in those countries, I can see if more cases is correlated with increased telecomm usage
# in said country.  A second-level hypothesis would be that in developing countries, when there
# is more cellphone and internet usage, there is more opportunity to report violence, and 
# therefore higher observations in our dataset.

q1h2_drilldown("Sri Lanka")
q1h2_drilldown("Nepal")
q1h2_drilldown("Afghanistan")
q1h2_drilldown("India")

# The WB data didn't have recent numbers on Syria, so that was excluded.  But from the other plots
# we can at least see that there could be a correlation between increased telecomm and reported
# violence.  India is the only country that bucks the trend, but that could possibly be because
# of India's population (any high-population country will have different trends than a population 
# of average sized countries).  Further investigation to this correlation is warrented.
