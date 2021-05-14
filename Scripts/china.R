china_set <- merged_ged %>%
  mutate(country = as.factor(country)) %>%
  filter(country == "China")

china_first <- min(china_set$year)
china_last <- max(china_set$year)

china_set %>%
  ggplot() +
  geom_sf(data = world, fill = "antiquewhite") +
  geom_jitter(aes(x = longitude, y = latitude, color = best, size = best), alpha = 0.55) +
  scale_color_gradient(high = "#BC3C29FF",
                       low = "#0072B5FF",
                       name = "Number of Victims (Best Estimate)") +
  scale_size(guide = "none") +
  coord_sf(xlim = c(min(china_set$longitude) - 3,
                    max(china_set$longitude) + 17),
           ylim = c(min(china_set$latitude) - 4,
                    max(china_set$latitude) + 10)) +
  labs(title = "Instances of Violence in China",
       subtitle = str_c("(", china_first, " - ", china_last, ")"),
       caption = "Source: UCDP dataset",
       x = element_blank(),
       y = element_blank()) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed', 
                                        size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))
