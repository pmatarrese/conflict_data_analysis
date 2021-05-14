#################################################
# Creating subdirectories

create_subdirectories <- function(subdirectory_name) {
  n_subdirectories <- length(subdirectory_name)
  
  for (i in 1:n_subdirectories) {
    if (!dir.exists(subdirectory_name[i])) {
      dir.create(subdirectory_name[i])
    }
  }
}

#################################################
# Download zipped file and extract .csv

get_data <- function(data_file, data_dir, data_URL, data_name) {
  
  data_loc <- paste(data_dir, "/", data_file, sep = "")
  temp <- tempfile()
  
  if(!file.exists(data_loc)) {
    data_URL %>%
      download.file(destfile = temp)
    data_name <- fread(unzip(temp, files = data_file)) 
    rm(temp)
    data_name %>%
      write_csv(path = data_loc)
  }
  
  data_name <- read_csv(data_loc)
  return(data_name)
}

#################################################
# function to clean World Bank data
# The data that was downloaded from the World Bank website was not tidy.  It had each year as its
# own variable.  The tidying is rather simple, but some modification of the classes of variables
# needed to happen to be able to combine them later in the data transformation process.

tidy_wb <- function(data, index) {
  index <- as.character(index) 
  tidy_index <- data %>%
    pivot_longer(cols = c(`1989 [YR1989]`: `2019 [YR2019]`), # change the year columns into one variable
                 names_to = "year",
                 values_to = str_c('"', index, '"', sep = "")) %>%
    mutate(year = str_sub(year, 1, 4), # only keep the four digits we want
           country = as.factor(`Country Name`)) %>%
    rename(country_code = `Country Code`) %>%
    select(country, country_code, year, str_c('"', index, '"', sep = "")) %>% # trim down to 4 variables
    mutate(year = as.integer(year),
           rownum = row_number()) %>%
    select(rownum, everything())
}

#################################################
# Set world data for mapping using geom_map

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

#################################################
# Q1H1 plots function
# I wanted to create a function that would do all of my plotting for me.  The idea is that
# the data is pretty huge, and we would want to look at different subsets of it.  The function
# would take an argument for the subset of data I want to look at; in this instance, I want to 
# break down the data to produce graphs for each region.  Each graph will show the total number of
# instances of violence over time for a particular region.  The plots will also break that data
# down by who is perpetrating the violence, and on average how many deaths there are per instance
# of violence.  Each decade will be broken out to provide an additional framework for viewing the
# data.  Each graph will then be saved to the Plots folder for rendering in the .Rmd file.


q1h1_plots <- function(region_var) {
  region_var <- as.character(region_var)
  
  # after initially graphing each plot, I made some general observations, and included them here
  # for use as subtitles in each plot depending on the region.
  
  americas_sub <- "While the Americas has seen a steady decrease in government-sponsored violence,\nthere has been a steady increase in non-state violence, with a severe spike in recent\nyears."
  asia_sub <- "Government-sponsored violence in Asia has been on a steady rise, and\ngovernment v government violence has been regularly occurring over the last 30 years."
  africa_sub <- "The number of violent acts has been on the rise in the last decade, though\nthe average number of deaths per action is not as high as it was during the 1990s."
  europe_sub <- "Europe has not seen government v government violence in the last 30 years, and overall\nthe number of violent acts has seen a decline in recent decades."
  me_sub <- "Relative to the previous two decades, the 2010s has seen a huge spike in violence,\nparticularly in the last five years."
  
  q1_h1_tbl %>%
    filter(region == as.character(region_var)) %>% # filter according to the region_var argument
    ggplot(aes(x = year, y = total_conflicts_yr_gova_region, color = gov_action)) +
    geom_point(aes(size = ave_deaths_yr_gova_region)) +
    geom_line() +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_size_continuous(name = "Average number of deaths per\ninstance of violence (per year)") +
    facet_wrap(~ region, ncol = 2) +
    labs(title = ifelse(region_var == "Americas" | region_var == "Middle East",
                        str_c("Violence in the ", region_var),
                        str_c("Violence in ", region_var)),
         subtitle = case_when(
           region_var == "Asia" ~ asia_sub,
           region_var == "Africa" ~ africa_sub,
           region_var == "Europe" ~ europe_sub,
           region_var == "Americas" ~ americas_sub,
           TRUE ~ me_sub),
         x = "Year",
         y = "Total Instances of Violence") +
    scale_color_nejm(name = "Actors in Violent Conflict\n(Perpetrator/Victim)",
                     breaks = c("gov_gov", "gov_other", "other"),
                     labels = c( "Government Forces/Government Forces",
                                 "Government Forces/Non-Government Actors",
                                 "Non-Government Actors/Non-Government Actors")) +
    theme(legend.position = "bottom",
          legend.direction = "vertical") +
    facet_wrap(~ decade, scales = "free_x") # free_x to allow the x axis to fit the faceted data
  
  ggsave(filename = str_c("Q1H1_plot_3-", region_var, ".png"),
         plot = last_plot(),
         device = "png",
         path = "Plots",
         width = 8,
         height = 6,
         units = "in",
         dpi = 320)
}

#################################################
# Q1H2 plots function
# For this set of plots, I had the decade be the function argument, allowing for three plots that
# will then be faceted by the region.  The logic

q1h2_plots <- function(decade_var) {
  decade_var <- as.character(decade_var)
  q1_h2_tbl_woR %>%
    filter(decade == decade_var) %>%
    ggplot(aes(x = year, y = count_yr_region, color = ave_death_instance_yr_region)) +
    geom_point() +
    geom_line() +
    scale_y_log10() +
    scale_color_gradient(high = "#BC3C29FF",
                         low = "#0072B5FF",
                         name = "Average Number of Deaths\nper Instance (by year)") +
    facet_wrap(~ region) +
    scale_x_continuous(breaks = pretty_breaks()) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          axis.text.x = element_text(angle = 90)) +
    labs(title = str_c("Instances of Violence by Region (", decade_var, ")"),
         subtitle = "Numbers were calculated after removing data of instances in Rwanda\nbetween 1990 and 1996, to avoid heavily skewed graphs",
         caption = "Source: UCDP dataset",
         y = "Number of Instances (log-scale)",
         x = "Year")
  
  ggsave(filename = str_c("Q1H2_plot_2-", decade_var,".png"),
         plot = last_plot(),
         device = "png",
         path = "Plots",
         width = 8,
         height = 6,
         units = "in",
         dpi = 320)
}

#################################################
# Q1H2 Drilldown function
#
#

q1h2_drilldown <- function(country_var) {
  country_var <- as.character(country_var)
  india_comment <- "India's numbers for reported instances of violence sees a rise that predates an\nexpanded use of cellphones and internet."
  af_comment <- "The number of incidents per capita in Afghanistan seem to correlate with the \ndevelopment of telecommunications infrastructure."
  sl_comment <- "Sri Lanka's incidents do see a large spike around the same time that internet and\ncellphone usage begin to grow throughout the country."
  nepal_comment <- "Nepal's number of incidents per capita track an increase with the introduction\nof the internet."
  merged_ged %>%
    filter(country == country_var) %>%
    group_by(year) %>%
    mutate(total_reports_yr = n()) %>%
    ungroup() %>%
    mutate(reports_per_capita = total_reports_yr/country_pop,
           cellphone_per_capita = cellphone_users/country_pop) %>%
    rename('Number of Cellphone Users (per capita)' = cellphone_per_capita,
           'Internet Users (% Population)' = internet_users,
           'Number of Reported Incidents (per capita)' = reports_per_capita) %>%
    pivot_longer(cols = c('Number of Cellphone Users (per capita)', 'Internet Users (% Population)', 'Number of Reported Incidents (per capita)'),
                 names_to = "q1h2_drilldown",
                 values_to = "q1h2_drilldown_value") %>%
    ggplot(aes(x = year, y = q1h2_drilldown_value, color = q1h2_drilldown)) +
    geom_point() +
    geom_line() +
    scale_color_nejm(guide = "none") +
    scale_y_continuous(breaks = pretty_breaks(),
                       labels = comma) +
    facet_wrap(~ q1h2_drilldown,
               scales = "free",
               ncol = 2) +
    labs(title = "Does the number of reported incidents per capita increase with the\nintroduction of telecommunication infrastructure?",
         subtitle = case_when(
           country_var == "Afghanistan" ~ af_comment,
           country_var == "India" ~ india_comment,
           country_var == "Sri Lanka" ~ sl_comment,
           TRUE ~ nepal_comment),
         caption = "Source: UCDP dataset and World Bank dataset",
         x = element_blank(),
         y = element_blank()) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0),
          legend.direction = "vertical")
  
  ggsave(filename = str_c("Q1H2_plot_4-telecomm_", country_var, ".png"),
         plot = last_plot(),
         device = "png",
         path = "Plots",
         width = 8,
         height = 6,
         units = "in",
         dpi = 320)
}

#################################################
# Q3H1 functions

get_con_yr <- function(year){
  con_num <- 0
  for(i in range(1, match(year, yr_list))){
    comp_tbl <- merged_ged %>%
      filter(year == yr_list[i])
    
    con_num <- con_num + length(unique(comp_tbl$conflict_new_id))
  }
  con_num
}

get_con_yr_reg <- function(year, reg){
  con_num <- 0
  for(i in range(1, match(year, yr_list))){
    comp_tbl <- merged_ged %>%
      filter(year == yr_list[i],
             region == reg_list[match(reg, reg_list)])
    
    con_num <- con_num + length(unique(comp_tbl$conflict_new_id))
  }
  con_num
}



