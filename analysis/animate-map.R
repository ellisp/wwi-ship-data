# create an animated map of the world with the location of Royal Navy ships
# on each day.

events <- tribble(~start_date, ~event,
                 "1914-08-01", "Commencement of war",
                 "1914-08-26", "First battle of Heligoland Bight",
                 "1914-08-30", "",
                 "1914-10-31", "Battle of Coronel",
                 "1914-11-02", "Dardanelles campaign commences",
                 "1914-11-11", "Chase and Battle of Falkland Islands",
                 "1914-12-10", "",
                 "1915-01-22", "Battle of Dogger Bank",
                 "1915-01-26", "",
                 "1916-05-29", "Battle of Jutland",
                 "1916-06-03", "",
                 "1917-11-16", "Second battle of Heligoland Bight",
                 "1917-11-19", "",
                 "1918-11-05", "Armistice",
                 "1918-11-18", "Demobilisation in main theatres"
                 ) %>%
  mutate(start_date = as.Date(start_date))


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
  mutate(vessel_type_lumped = fct_infreq(vessel_type_lumped)) %>%
  left_join(events, by = c("date" = "start_date")) %>%
  arrange(date) %>%
  fill(event, .direction = "down") %>%
  mutate(event = replace_na(event, replace = ""))

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
    labs(subtitle = unique(d$event),
         title = glue("Royal Navy ship locations 1914 to 1920:      {format(the_date, '%d %B %Y')}"),
         colour = "",
         caption = "Locations from log books compiled by naval-history.net; map by freerangestats.info") +
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Sarala", hjust = 0.5),
          plot.caption = element_text(colour = "grey70"),
          panel.background = element_rect(fill = "#F2F9FC", colour = NA))
  
  CairoPNG(glue("tmpimg/{i + 1000}.png"), 2000, 1300, res = 200)
  print(m)
  dev.off()         
}
