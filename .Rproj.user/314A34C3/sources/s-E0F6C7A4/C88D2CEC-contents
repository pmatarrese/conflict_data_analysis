#################################################
# Tidy the World Bank datasets

tidy_pop <- tidy_wb(wb_population, "population")
tidy_cellphones <- tidy_wb(wb_cellphones, "cellphones")
tidy_internet <- tidy_wb(wb_internet, "internet")

# join the different WB sets together
tidy_wb_tbl <- tidy_pop %>%
  left_join(tidy_cellphones, by = c("rownum", "country", "country_code", "year")) %>%
  left_join(tidy_internet, by = c("rownum", "country", "country_code", "year"))

# convert from tibble to dataframe for handling in countrycode()
tidy_wb_tbl <- as.data.frame(tidy_wb_tbl)

# use countrycode() to convert ISO3 character country code to Gleditsch & Ward numeric
# to match with UCDP dataset
tidy_wb_tbl$country_code <- countrycode(as.character(tidy_wb_tbl$country_code),
                                    origin = "wb",
                                    destination = "gwn")

# get rid of countries which didn't couldn't convert country code and
# rename country_code to country_id to match UCDP for join
tidy_wb_tbl <- tidy_wb_tbl %>%
  filter(!is.na(country_code)) %>%
  rename(country_id = country_code) %>%
  select(- rownum)

#################################################
# Merge datasets

ged_syria$source_date <- as.Date(ged_syria$source_date)
ged191$date_start <- as.Date(ged191$date_start)
ged191$date_end <- as.Date(ged191$date_end)

# get ged191 and ged_syria to match variables
ged191 <- ged191 %>%
  select(- source_date, #source_date variable unsuitable for converting from chr to date
         - gwnob,
         - gwnoa)

ged_syria <- ged_syria %>%
  select(- source_date,
         - gwnob)

# combine the main UCDP dataset with the Syria-specific dataset
merged_ged <- bind_rows(ged191, ged_syria)

merged_ged <- merged_ged %>%
  mutate(region = as.factor(region),
         decade = ifelse(year %in% 1989:1999, "1990s",
                         ifelse(year %in% 2000:2009, "2000s", "2010s"))) %>%
  mutate(decade = as.factor(decade),
         dyad_new_id = as.factor(dyad_new_id),
         conflict_new_id = as.factor(conflict_new_id),
         country = as.factor(country))

# combine tidied World Bank data to the UCDP dataset

merged_ged <- merged_ged %>%
  left_join(tidy_wb_tbl, by = c("year", "country", "country_id")) %>%
  rename(country_pop = '"population"',
         cellphone_users = '"cellphones"',
         internet_users = '"internet"')

# create a subset without Rwandan Genocide data
merged_ged_wo_rwanda <- merged_ged %>%
  filter(!(country == "Rwanda" & year %in% 1990:1996))

#################################################
# remove extra csv files

file.remove("ged_syria.csv")
file.remove("ged191.csv")


