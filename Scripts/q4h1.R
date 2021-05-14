#################################################
# Question 4 Which type of conflict has become more prevalent in the regions with the highest
# growth in conflicts?
### Second layer of analysis details rise of violent conflict in the Americas as we have
# already covered growth of conflict in other regions. Focus is on War on Drugs. 

### DATA TRANSFORMATION
# Count of number of conflicts by region by counting type of violence, to easily see trends by
# groupings. More useful than counting by dyads.    

number_conflicts_type <- merged_ged %>%
  group_by(region, year) %>%
  count(type_of_violence) %>%
  ungroup() %>%
  rename(number_of_conflicts = n)

number_conflicts_type %>%
  filter(type_of_violence == 2) %>%
  ggplot(aes(x = year, y = number_of_conflicts)) +
  geom_line(color = "#0072B5FF") +
  scale_y_log10() +
  facet_wrap(~ region) +
  labs(title = "Non-State Violence by Region (1989 - 2019)",
       subtitle = "The rise in non-state violence in the Americas is of particular interest",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Number of Instances of Violence (Log-scale)")

ggsave(filename = "Q4H1_plot_0-non_state_conflicts_yr_region.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)



# Count of number of conflicts in the Americas by type of violence to illustrate sudden rise in
# number of type 2 conflicts from 2016 to 2018 

americas_conflict <- number_conflicts_type %>%
  filter(region == "Americas",
         year > "2010") %>%
  arrange(desc(year))

# Deadlier dataframe used to highlight the conflicts with the highest casualties in the Americas.
# Note the heavy presence of cartel vs cartel violence. 

high_americas <- deadlier %>%
  group_by(dyad_name) %>%
  filter(region == "Americas",
         year > "2010") %>%
  arrange(desc(best, yearly_fatalities, mean_fatalities)) %>%
  select(dyad_name, country, year, best, yearly_fatalities, mean_fatalities, type_of_violence, everything()) %>%
  distinct()

high_americas %>%
  arrange(desc(yearly_fatalities)) %>%
  select(year, dyad_name, country, yearly_fatalities) %>%
  head(15) %>%
  kable(col.names = c("Year", "Dyad Name", "Country", "Total Fatalities per Year"),
        caption = "The Top 15 Deadliest Years by Dyad Pairing\n(2010s in the Americas)") %>%
  kable_styling() %>%
  save_kable("Results/Q4H1_tables-deadliest_yr_dyad.png")

# narco_conflict assigns a regex to detect key phrases, drug war focused sources, and 'signature'
# calling cards of Narco-Cartels to str_detect most likely cases of cartel violence

narco_conflict <- "(drug|drugs|cartel|cartels|gang|gangs|Borderland Beat|narco|execution|executions|beheading|beheadings|
headless|tortured|decapitated|hang|hangs|hanged|hanging|dismembered|warning|mutilated|mutilation|FACTBOX-Security developments in Mexico|dumped|organized crime)"

# Filters region to the Americas, post 2010 (to analyse recent spike in Type 2 Violence), groups
# by dyad name to identify unique conflicts. Mutate then creates an additional variable that
# return TRUE/FALSE depending on whether str_detect found key strings from the source_article
# column. Out of 2,765 rows, it returns over 1,600 rows identified as true. Narco regex is
# also not completely comprehensive, as a lot of articles are ambigious, with many simply
# involving gunmen. Can't widen the regex further, as generalisation would also pick up on
# insurgencies, normal murders and other conflicts unrelated to the war on drugs. 

war_on_drugs <- deadlier %>%
  filter(region == "Americas",
         year > "2010") %>%
  group_by(conflict_name) %>%
  mutate(drug_conflicts = str_detect(source_article, regex(narco_conflict, ignore_case = TRUE))) %>%
  ungroup() %>%
  select(dyad_name, drug_conflicts, source_article, year, yearly_fatalities, mean_fatalities, everything())

# Use this to compare the numbers between cartel and non-cartel related conflict
war_on_drugs %>% filter(drug_conflicts == "TRUE")


### PLOTS
#########################################
# Question 4 Which type of conflict has become more prevalent in the regions with the highest
# growth in conflicts?
# As we have covered the rising trend in conflicts and fatalities in Asia and the Middle East,
# this set of tables will look into the rising trend of conflict in the Americas. 

# Barplot to show changing trend in number of conflicts by region, type and year. 

number_conflicts_type %>%
  ggplot() +
  geom_bar(aes(x = year, y = number_of_conflicts, fill = as.factor(type_of_violence)),
           position = "dodge",
           stat = "identity") +
  scale_fill_nejm(name = "Type of Violence",
                  labels = c("State-based conflict",
                             "Non-state conflict",
                             "One-sided violence")) +
  scale_y_log10() +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region, nrow = 5) +
  labs(title = "Number of conflicts by region and type of violence (1989-2019)",
       subtitle = "State-based conflict remains the modal type of conflict in Africa, Asia and\nthe Middle East.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Number of Conflicts (Log-scale)")

ggsave(filename = "Q4H1_plot_1-number_conflicts_yr_region_type.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Number of conflicts line chart to better understand trends over time than the above chart. 
# We can see a clear peak though later decline in the Middle East, while other regions have
# seen a steady upsurge in Africa, the Americas and Asia. 

number_conflicts_type %>%
  ggplot() +
  geom_line(aes(x = year, y = number_of_conflicts, color = as.factor(type_of_violence))) +
  scale_color_nejm(name = "Type of Violence",
                   labels = c("State-based conflict",
                              "Non-state conflict",
                              "One-sided violence")) +
  scale_y_log10() +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region, nrow = 3) +
  labs(title = "Number of conflicts by region and type of violence (1989-2019)",
       subtitle = "Trends in conflicts by type seem somewhat stable in Africa and Asia, while there has\nbeen a dip in the Middle East over the past 5 years. However, we can see a clear\nupsurge in non-state conflict in the Americas",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Number of Conflicts (Log-scale)") +
  theme(legend.position= c(1,0),
        legend.justification = c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q4H1_plot_2-trends_type_regions.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Barplot using deadlier dataframe to show fatalities by type of conflict over the years.
# Log10 scale for y_var keeps y-axis label and presents data more nicely. 

deadlier %>%
  ggplot() +
  geom_bar(aes(x = year,
               y = log10(yearly_fatalities),
               fill = as.factor(type_of_violence)),
           stat = "identity") +
  scale_fill_nejm(name = "Type of Violence",
                  labels = c("State-based conflict",
                             "Non-state conflict",
                             "One-sided violence")) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region, ncol = 3) +
  labs(title = "Fatalities by region and type of violence (1989-2019)",
       subtitle = "While we can see an upsurge in non-state conflicts in three world regions, it has\nbecome the most prevalent type of conflict in the Americas.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Yearly fatalities (Log-scale)") +
  theme(legend.position = c(1,0),
        legend.justification =  c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q4H1_plot_3-fatalities_type_regions_bar.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

deadlier %>%
  ggplot() +
  geom_bar(aes(x = year,
               y = log10(yearly_fatalities),
               fill = as.factor(type_of_violence)),
           stat = "identity",
           position = "fill") +
  scale_fill_nejm(name = "Type of Violence",
                  labels = c("State-based conflict",
                             "Non-state conflict",
                             "One-sided violence")) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region, ncol = 3) +
  labs(title = "Fatalities by region and type of violence (1989-2019)",
       subtitle = "While we can see an upsurge in non-state conflicts in three world regions, it has\nbecome the most prevalent type of conflict in the Americas.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Yearly fatalities (proportion)") +
  theme(legend.position = c(1,0),
        legend.justification =  c(1,0),
        legend.direction = "vertical")

ggsave(filename = "Q4H1_plot_3-fatalities_type_regions_bar_proportion.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Barplot utilising War on Drugs dataframe, which has run a str_detect to identify most
# likely cases of conflicts being related to the War on Drugs. Plot is to distinguish between
# the two types of conflict inolved, war on drugs related, or other conflicts. 

war_on_drugs %>%
  mutate(number_of_conflicts = n()) %>%
  ggplot() +
  geom_bar(aes(x = year,
               y = log10(number_of_conflicts),
               fill = as.factor(drug_conflicts)),
           stat = "identity") +
  scale_fill_nejm(name = "Conflict type",
                  labels = c("Other Conflict",
                             "War on Drugs conflict")) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~ region, nrow = 5) +
  labs(title = "Number of War on Drugs conflicts compared to other conflicts (2010-2018)",
       subtitle = "Although drug conflicts are not the dominant form of conflict in 2018, they have \nclearly been on the rise in the past decade, as is the number of conflicts in general.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Number of Conflicts (Log-scale)") +
  theme(legend.position="bottom",
        legend.direction = "vertical")

ggsave(filename = "Q4H1_plot_4-war_on_drugs.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# War on drugs map

war_on_drugs %>%
  filter(drug_conflicts == TRUE,
         country == "Mexico") %>%
  ggplot() +
  geom_sf(data = world, fill = "antiquewhite") +
  annotation_scale(location = "bl",
                   width_hint = 0.5,
                   pad_x = unit(0.3, "in"),
                   pad_y = unit(0.3, "in")) +
  annotation_north_arrow(location = "bl",
                         which_north = "true", 
                         pad_x = unit(0.75, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_jitter(aes(x = longitude, y = latitude, color = best, size = best), alpha = 0.35) +
  geom_text(data= world_points,
          aes(x=X, y=Y, label=name),
          color = "black",
          check_overlap = TRUE) +
  scale_color_gradient(high = "#BC3C29FF",
                       low = "#0072B5FF",
                       name = "Number of Victims (Best Estimate)") +
  scale_size(guide = "none") +
  coord_sf(xlim = c(min(war_on_drugs$longitude[war_on_drugs$drug_conflicts == TRUE & war_on_drugs$country == "Mexico"]) - 2,
                    max(war_on_drugs$longitude[war_on_drugs$drug_conflicts == TRUE & war_on_drugs$country == "Mexico"]) + 2),
           ylim = c(min(war_on_drugs$latitude[war_on_drugs$drug_conflicts == TRUE & war_on_drugs$country == "Mexico"]) - 1,
                    max(war_on_drugs$latitude[war_on_drugs$drug_conflicts == TRUE & war_on_drugs$country == "Mexico"]) + 1)) +
  labs(title = "The War on Drugs (Mexico in the 2010s)",
       subtitle = "Cartels often perpetrate violence along the U.S.-Mexico Border to gain control\nof the illicit drug smuggling channels.",
       caption = "Source: UCDP dataset",
       x = element_blank(),
       y = element_blank()) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed', 
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

ggsave(filename = "Q4H1_plot_5-war_on_drugs_map.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)


