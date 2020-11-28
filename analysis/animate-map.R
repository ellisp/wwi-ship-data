# create an animated map of the world with the location of Royal Navy ships
# on each day.


all_dates <- sort(unique(vessel_logs_sel$date))
all_dates <- all_dates[all_dates >= "1914-01-01" & all_dates <= "1920-12-31"]

d2 <- vessel_logs_sel %>%
  mutate(vessel_type_lumped = case_when(
    vessel_type %in% c(
      "armed merchant cruiser",
      "light cruiser",
      "armoured cruiser",
      "river gunboat",
      "sloop",
      "destroyer",
      "battleship"
    ) ~ str_to_sentence(vessel_type),
    grepl("cruiser", vessel_type) ~ "Other cruiser",
    TRUE ~ "Other"
  )) %>%
  mutate(vessel_type_lumped = as.factor(vessel_type_lumped))

pal <- brewer.pal(9, "Set1")
names(pal) <- unique(d2$vessel_type_lumped)

for(i in 1:length(all_dates)){
  the_date <- all_dates[i]
  d <- d2 %>%
    filter(date == the_date)
  
  m <- d %>%
    ggplot(aes(x = long, y = lat)) +
    borders(fill = "grey", colour = NA) +
    geom_point(aes(colour = vessel_type_lumped), size = 0.8) +
    coord_sf() +
    theme_void(base_family = "Roboto") +
    scale_colour_manual(values = pal, labels = names(pal), drop = FALSE) +
    labs(subtitle = "",
         title = glue("Royal Navy ship locations 1914 to 1920: {format(the_date, '%d %B %Y')}"),
         colour = "",
         caption = "Locations from log books compiled by naval-history.net; map by freerangestats.info") +
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Sarala", hjust = 0.5),
          plot.caption = element_text(colour = "grey70"),
          panel.background = element_rect(fill = "grey98", colour = NA))
  
  CairoPNG(glue("tmpimg/{i + 1000}.png"), 2000, 1300, res = 200)
  print(m)
  dev.off()         
}
vessel_logs_sel %>%
  count(vessel_type, sort = TRUE)
