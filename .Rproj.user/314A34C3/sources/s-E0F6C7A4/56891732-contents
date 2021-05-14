#################################################
# Q1 H1: "There is likely to be a large reduction in inter-state wars, 
# but an increase in intra-state conflicts (civil wars) in recent years." 

#################################################
# transform data

q1_h1_tbl <- merged_ged %>%
  
  # create a variable that marks gov v gov, gov v non-state, and non-state v non-state
  mutate(gov_action = case_when(
    grepl("Government", side_a, ignore.case = TRUE) & grepl("Government", side_b, ignore.case = TRUE) ~ "gov_gov",
    grepl("Government", side_a, ignore.case = TRUE) & !grepl("Government", side_b, ignore.case = TRUE) ~ "gov_other",
    TRUE ~ "other")) %>%
  
  # add a binary version of above variable for proportion plot
  mutate(gov_action_binary = ifelse(gov_action == "other", 0, 1)) %>%
  mutate(gov_action = as.factor(gov_action),
         gov_action_binary = as.factor(gov_action_binary)) %>%
  
  # create variables for total deaths, total instances of violence, and ave death per instance
  # group by year and binary variable
  group_by(year, gov_action_binary) %>%
  mutate(total_deaths_yr_govb = sum(best),
         total_conflicts_yr_govb = n(),
         ave_deaths_yr_govb = total_deaths_yr_govb/total_conflicts_yr_govb) %>%
  ungroup() %>%
  
  # same variables but grouped by year and non-binary indicator variable
  group_by(year, gov_action) %>%
  mutate(total_deaths_yr_gova = sum(best),
         total_conflicts_yr_gova = n(),
         ave_deaths_yr_gova = total_deaths_yr_gova/total_conflicts_yr_gova) %>%
  ungroup() %>%
  
  # same variables but grouped by region, year, and binary indicator
  group_by(year, gov_action_binary, region) %>%
  mutate(total_deaths_yr_govb_region = sum(best),
         total_conflicts_yr_govb_region = n(),
         ave_deaths_yr_govb_region = total_deaths_yr_govb_region/total_conflicts_yr_govb_region) %>%
  ungroup() %>%
  
  # same variables but grouped by region, year, and non-binary indicator
  group_by(year, gov_action, region) %>%
  mutate(total_deaths_yr_gova_region = sum(best),
         total_conflicts_yr_gova_region = n(),
         ave_deaths_yr_gova_region = total_deaths_yr_gova_region/total_conflicts_yr_gova_region) %>%
  ungroup()

#################################################
# plot graphs

# plot a graph to get a view of total number of instances of violence and if it's gov or non-state
q1_h1_tbl %>%
  ggplot(aes(x = year, y = total_conflicts_yr_govb, fill = gov_action_binary)) +
  geom_col() +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  scale_fill_nejm(name = "Perpetrator of Violence",
                  breaks = c(0,1),
                  labels = c("Non-Government Actors", "Government Forces")) +
  labs(title = "Who is perpetrating violence in the world?",
       subtitle = "The number of total instances of violence seems to have\nincreased over the last 30 years.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Total Instances of Violence") +
  theme(legend.position = "bottom",
        legend.direction = "vertical")

# This graph shows that there has actually been an increase in instances of violence perpetrated
# by government forces.  So the first half of our initial hypothesis is incorrect.  It may be
# that our initial feeling was more based on traditional state vs state conflict; it feels as if
# there hasn't been a "traditional" war in some time.  But this graph accounts for government
# forces taking action against non-state groups like ISIS.

ggsave(filename = "Q1H1_plot_1-all_years.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# plot a proportion graph to see if there is a trend for more or less gov violence or non-state violence
q1_h1_tbl %>%
  ggplot(aes(x = year, y = total_conflicts_yr_govb, fill = gov_action_binary)) +
  geom_col(position = "fill") +
  scale_fill_nejm(name = "Perpetrator of Violence",
                  breaks = c(0,1),
                  labels = c("Non-Government Actors", "Government Forces")) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Who is perpetrating violence in the world?",
       subtitle = "In the last decade, it seems like there is a higher proportion of non-state actors perpetrating violent\nacts, though the majority of violent acts are still carried out by government forces.",
       caption = "Source: UCDP dataset",
       x = "Year",
       y = "Proportion of Total Instances of Violence") +
  theme(legend.position = "bottom",
        legend.direction = "vertical")

# I wanted to see a proportion graph of the first plot.  In this way, I can see if there is any
# trend about state-sponsored violence vs non-state actors.  We can see that, while the majority
# of violence is still perpetrated by government forces, there has been a trend in the last 10
# years for a higher proportion of non-state actor violence.  So the second half of our hypothesis
# was half correct.  There's been an increase in the overall number of non-state violence, as well
# as an increase in the proportion of total instances done by non-state actors.

ggsave(filename = "Q1H1_plot_2-all_years_proportion.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# generate plots for each of the regions, showing who the perp/victims are, faceted by decade
q1h1_plots("Americas")

# Americas: There has been a general decrease in government sponsored violence across the decades.
# The spike in the 2000s was attributed mostly to the Government of Colombia and their armed
# conflict with FARC rebels.

q1h1_plots("Asia")

# Asia: There's actually been as steady increase in government action in Asia.  We look into this
# in later questions/hypotheses.  There's also an increase in non-state action, and this is the
# one region that has pretty much constant gov v gov violence.  This could be attributed to the
# various border conflicts around India/Pakistan.

q1h1_plots("Africa")

# Africa: The 1990s and 2000s are very similar in nature; they don't have too much movement either
# up or down (there is a large spike in the 1990s).  But in the last decade, there has been a 
# steady increase in both government action and non-state action.

q1h1_plots("Europe")

# Europe: Europe hasn't seen gov v gov action (as noted in the dataset) in the last 31 years. But
# the 1990s did see a huge number of instances of government violence.  Since then, there has been
# a steady decline in government action.

q1h1_plots("Middle East")

# Middle East: As we can see from our overview plots at the beginning, there has been a huge jump
# in the number of instances of violence.  This is mirrored in our Middle East plot; the data is
# mostly accounted for by violence in Syria and northern Iraq.
  
# First level findings:
# Our initial hypothesis might only be half-true.  Government against government conflicts are rare
# across the whole dataset, but even moreso in the last decade.  However, that doesn't meand that
# governments aren't perpetrating violent acts.  Our proportion plot shows that there is a trend
# towards a higher proportion of acts being conducted by non-state actors, but our other summary
# plot shows that overall, the total number of reported violent acts has had a dramatic rise (we
# will look into one possible explanation of this in the second hypothesis).  One interesting trend
# in the Middle East graph showed a spike in the non-state actor instances.  I wanted to drill down
# on finding who the most violent non-state actors are in each decade.

#################################################
# drill down
#
# In this drilldown, I wanted to initially find out what non-state group was the deadliest in the
# world in each of the decades, and see where they operated.

# find the deadliest group by totalling the number of deaths by decade and by perpetrator
q1_h1_tbl %>%
  # ensures that we are not looking at any government forces as the perp
  filter(gov_action == "other") %>%
  group_by(decade, side_a) %>%
  summarise(total_deaths_decade_actor = sum(best)) %>%
  arrange(desc(total_deaths_decade_actor)) %>%
  
  # once I summarise, I have to group_by again to find the top unique entry for each decade
  group_by(decade) %>%
  
  # slice gives me the first entry for each unique decade
  slice(1) %>%
  
  # make pretty and save
  kable(col.names = c("Decade", "Non-State Actor", "Total number of Victims (Best Estimate)"),
        caption = "The Deadliest Non-State Actors by Decade") %>%
  kable_styling() %>%
  save_kable("Results/Q1H1_tables_deadliest_grps_decade.png")

# I want to create objects for the earliest and latest year that each group operated for passing
# into strings in each plot
LRA_first <- min(q1_h1_tbl$year[q1_h1_tbl$side_a == "LRA"])
LRA_last <- max(q1_h1_tbl$year[q1_h1_tbl$side_a == "LRA"])
AFDL_first <- min(q1_h1_tbl$year[q1_h1_tbl$side_a == "AFDL"])
AFDL_last <- max(q1_h1_tbl$year[q1_h1_tbl$side_a == "AFDL"])
IS_first <- min(q1_h1_tbl$year[q1_h1_tbl$side_a == "IS"])
IS_last <- max(q1_h1_tbl$year[q1_h1_tbl$side_a == "IS"])
# tested to see if IS had different dates for only their action in the Middle East
IS_first_me <- min(q1_h1_tbl$year[q1_h1_tbl$side_a == "IS" & q1_h1_tbl$region == "Middle East"])
IS_last_me <- max(q1_h1_tbl$year[q1_h1_tbl$side_a == "IS" & q1_h1_tbl$region == "Middle East"])

# I wanted to generate maps of each group's activity, and show how deadly each event was
q1_h1_tbl %>%
  filter(side_a == "LRA") %>%
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
            check_overlap = FALSE) +
  scale_color_gradient(high = "#BC3C29FF",
                       low = "#0072B5FF",
                       name = "Number of Victims (Best Estimate)") +
  scale_size(guide = "none") +
  
  # I only wanted the map to show the part of the globe that the group committed violence in
  coord_sf(xlim = c(min(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "LRA"]) - 4,
                    max(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "LRA"]) + 4),
           ylim = c(min(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "LRA"]) - 2,
                    max(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "LRA"]) + 2)) +
  labs(title = "Instances of Violence perpetrated by the Lord's Resistance Army (LRA)",
       subtitle = str_c("(", LRA_first, " - ", LRA_last, ")"),
       caption = "Source: UCDP dataset",
       x = element_blank(),
       y = element_blank()) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed', 
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

ggsave(filename = "Q1H1_plot_5-LRA_map.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# repeat plot for AFDL
q1_h1_tbl %>%
  filter(side_a == "AFDL") %>%
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
            check_overlap = FALSE) +
  scale_color_gradient(high = "#BC3C29FF",
                       low = "#0072B5FF",
                       name = "Number of Victims (Best Estimate)") +
  scale_size(guide = "none") +
  coord_sf(xlim = c(min(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "AFDL"]) - 7,
                    max(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "AFDL"]) + 7),
           ylim = c(min(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "AFDL"]) - 3,
                    max(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "AFDL"]) + 3)) +
  labs(title = "Instances of Violence perpetrated by the\nAlliance of Democratic Forces for the\nLiberation of Congo-Zaire (AFDL)",
       subtitle = str_c("(", AFDL_first, " - ", AFDL_last, ")"),
       caption = "Source: UCDP dataset",
       x = element_blank(),
       y = element_blank()) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

ggsave(filename = "Q1H1_plot_4-AFDL_map.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Repeat plot for IS worldwide actions
q1_h1_tbl %>%
  filter(side_a == "IS") %>%
  ggplot() +
  geom_sf(data = world, fill = "antiquewhite") +
  geom_jitter(aes(x = longitude, y = latitude, color = best, size = best), alpha = 0.55) +
  scale_color_gradient(high = "#BC3C29FF",
                       low = "#0072B5FF",
                       name = "Number of Victims (Best Estimate)") +
  scale_size(guide = "none") +
  coord_sf(xlim = c(min(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "IS"]) - 1,
                    max(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "IS"]) + 1),
           ylim = c(min(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "IS"]) - 1,
                    max(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "IS"]) + 1)) +
  labs(title = "Instances of Violence perpetrated by the Islamic State (IS)",
       subtitle = str_c("(", IS_first, " - ", IS_last, ")"),
       caption = "Source: UCDP dataset",
       x = element_blank(),
       y = element_blank()) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed', 
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

# As opposed to the first two non-state actors, IS is shown to have a much further reach.  It is
# not isolated to a particular country or region.  The difficulty in combatting non-state actors
# partially comes from their lack of specific geo-political goals.

ggsave(filename = "Q1H1_plot_6-IS_map1.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Repeated the IS plot but specifically in the Middle East
q1_h1_tbl %>%
  filter(region == "Middle East") %>%
  filter(side_a == "IS") %>%
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
  geom_jitter(aes(x = longitude, y = latitude, color = best, size = best), alpha = 0.55) +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "black",
            check_overlap = TRUE) +
  scale_color_gradient(high = "#BC3C29FF",
                       low = "#0072B5FF",
                       name = "Number of Victims (Best Estimate)") +
  scale_size(guide = "none") +
  coord_sf(xlim = c(min(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "IS" & q1_h1_tbl$region == "Middle East"]) - 7,
                    max(q1_h1_tbl$longitude[q1_h1_tbl$side_a == "IS" & q1_h1_tbl$region == "Middle East"]) + 7),
           ylim = c(min(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "IS" & q1_h1_tbl$region == "Middle East"]) - 1,
                    max(q1_h1_tbl$latitude[q1_h1_tbl$side_a == "IS" & q1_h1_tbl$region == "Middle East"]) + 1)) +
  labs(title = "Instances of Violence perpetrated by IS in the Middle East",
       subtitle = str_c("(", IS_first_me, " - ", IS_last_me, ")"),
       caption = "Source: UCDP dataset",
       x = element_blank(),
       y = element_blank()) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed', 
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

# This map focuses in on IS action in the Middle East.  Again, we can see a heavy grouping in
# Syria and northern Iraq.

ggsave(filename = "Q1H1_plot_7-IS_map2.png",
       plot = last_plot(),
       device = "png",
       path = "Plots",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Second Level Findings:
# The deadliest non-state group in the 1990s was The Alliance of Democratic Forces for the
# Liberation of Congo-Zaire, or the AFDL.  The Rwandan Genocide had left the eastern part of then-
# Zaire destabilized, and the AFDL successfully defeated the government to form the Democratic
# Republic of the Congo after the First Congo War.  Much of the conflict along the borders of
# Uganda, Rwanda, and Burundi reflect the violence in this period.
#
# In the 2000s, the Lord's Resistance Army (LRA) had massive amounts of conflict in central Africa.
# Lead by Joseph Kony, they had not only abducted an estimated 25,000 children to act as child
# soldiers, but are responsible for thousands of deaths in southern Sudan, northern Uganda, and the
# DRC, as well as the displacement of hundreds of thousands of Congolese.
#
# In the 2010s, the Islamic State (IS) has clearly taken over as the most violent group.  We would
# need to gather additional data on IS to drill down deeper into the conflict dataset.


