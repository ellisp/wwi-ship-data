# create an animated map of the world with the location of Royal Navy ships
# on each day.

# Adapted from
# https://www.loc.gov/collections/stars-and-stripes/articles-and-essays/a-world-at-war/timeline-1914-1921/
# and https://en.wikipedia.org/wiki/Allied_intervention_in_the_Russian_Civil_War
phases <- tribble(~start_date, ~phase,
                 "1912-10-08", "First Balkan War",
                 "1913-05-30", "Between the Balkan wars",
                 "1913-06-29", "Second Balkan War",
                 "1913-08-10", "Tension in the aftermath of the Balkan wars",
                 "1914-06-28", "Archduke Francis Ferdinand assassinated, and tension builds",
                  "1914-08-01", "War commences",
                 "1914-09-13", "Stalemate in the west after the First Battle of the Marne",
                 "1914-11-02", "Dardanelles build-up commences",
                 "1915-04-25", "Allied troops on Gallipoli Peninsula",
                 "1916-02-21", "Battle of Verdun commences",
                 "1916-07-01", "Battle of the Somme commences",
                 "1917-02-01", "Germany returns to unrestricted submarine warfare",
                 "1917-04-06", "The USA enters the war",
                 "1917-12-15", "Russia signs armistice with Germany",
                 "1918-03-04", "British troops intervene in the Russian civil war",
                 "1918-03-21", "Final German offensive",
                 "1918-09-26", "Final allied offensive begins at Meusse-Argonne",
                 "1918-11-05", "Armistice",
                 "1918-12-01", "British and American forces enter Germany",
                 "1919-01-18", "Peace conference begins at Paris",
                 "1919-06-28", "Treat of Versailles signed",
                 "1919-09-27", "Last Allied troops leave Archangel, effectively ending involvement in the Russian civil war",
                 "1919-11-19", "US Senate fails to ratify Treaty of Versailles"
                 ) %>%
  mutate(start_date = as.Date(start_date))

# From https://en.wikipedia.org/wiki/List_of_naval_battles#World_War_I_(1914%E2%80%9318)
# Note the battle of Penang, Cocos, Cape Sarych, Gotland, Kirpen Island 
# are excluded as no UK warships there
battles <- tribble(~date, ~battle, ~duration, ~lat, ~long,
                   "1914-08-16", "Battle of Antivari", 1, 42 + 10/ 60, 19 + 10 / 60,
                  "1914-08-28", "First battle of Heligoland Bight", 1, 54+19/60, 7 + 51/60,
                  "1914-11-01", "Battle of Coronel", 1, -(36 + 59.15/60), -(73 + 48.14/60),
                  "1914-12-08", "Battle of Falkland Islands", 1, -(52 + 29.95/60), -(56 + 9.98/60),
                  "1915-01-24", "Battle of Dogger Bank", 1, 54 + 33.5/60, 5 + 27.85/60,
                  "1915-02-17", "Dardanelles campaign", 327, 40 + 2/60, 26 + 4/60,
                  "1915-05-01", "Battle off Noordhinder Bank", 1, 51 + 39/60, 2 + 41/60,
                  "1916-05-31", "Battle of Jutland", 2, 56 + 42/60, 5 + 52/60,
                  "1916-10-26", "Battle of Dover Strait", 2, 51, 1 + 27/60,
                  "1917-04-20", "Second Battle of Dover Strait", 2, 51 + 1/60, 1+29/60,
                  "1917-05-14", "Raid on the Otranto Barrage", 2, 40.2167, 18.9167,
                  "1917-11-17", "Second battle of Heligoland Bight", 1, 54 + 10/60, 8 + 4/60,
                  "1918-01-20", "Battle of Imbros", 1, 40 + 14/60, 25 + 58 / 60
                   )

# We make a long version that  has a row for every day of the battle, plus five days
# of "aftermath" for fading away on the
battles_long <- battles[rep(1:nrow(battles), battles$duration + 5), ] %>%
  group_by(battle) %>%
  mutate(date = as.Date(date) - 1 + 1:n()) %>%
  mutate(status = ifelse(max(date) - date >= 5, "Battle", "Aftermath"),
         point_size = ifelse(status == "Battle", 5.5, max(date) - date + 0.5))

all_dates <- sort(unique(vessel_logs_sel$date))
all_dates <- all_dates[all_dates >= "1913-07-01" & all_dates <= "1920-12-31"]

d2 <- vessel_logs_sel %>%
  mutate(vessel_type = tolower(vessel_type)) %>%
  mutate(vessel_type_lumped = case_when(
    vessel_type %in% c(
      "armed merchant cruiser",
      "armoured cruiser"
    ) ~ str_to_sentence(vessel_type),
    grepl("battleship", vessel_type) |
      grepl("battlecruiser", vessel_type) |
      grepl("dreadnought", vessel_type) ~ "Battleship or battlecruiser",
    grepl("gunboat", vessel_type) ~ "Gunboat",
    grepl("cruiser", vessel_type) ~ "Other cruiser",
    grepl("sloop", vessel_type) ~ "Sloop",
    grepl("destroyer", vessel_type) ~ "Destroyer",
    TRUE ~ "Other"
  )) %>%
  mutate(vessel_type_lumped = fct_infreq(vessel_type_lumped)) %>%
  left_join(phases, by = c("date" = "start_date")) %>%
  arrange(date) %>%
  fill(phase, .direction = "down") %>%
  mutate(phase = replace_na(phase, replace = ""))

#---------------Colours-----------------

pal <- brewer.pal(8, "Dark2")
names(pal) <- levels(d2$vessel_type_lumped)
pal["Other"] <- "#E6FA05"
pal["Sloop"] <- "#7CEA7C"

battle_col <- "black"
sea_col <- "#DCEAFA"
comment_col <- "grey50"

#-------------main loop------------------
for(i in 1:length(all_dates)){
  the_date <- all_dates[i]
  ships_data <- d2 %>%
    filter(date == the_date)
  
  battle_data <- filter(battles_long, date == the_date)
  date_col <- case_when(
    the_date < "1914-08-01" ~ "blue",
    the_date > "1918-11-11" ~ "blue",
    TRUE ~ "red"
  )
  date_sum_text <- case_when(
    the_date < "1914-08-01" ~ "Before World War I",
    the_date > "1918-11-11" ~ "After World War I; Russian Civil War continues",
    TRUE ~ "World War I"
  ) %>% toupper()
  
  m <- ships_data %>%
    ggplot(aes(x = long, y = lat)) +
    borders(fill = "grey", colour = NA) +
    geom_point(aes(colour = vessel_type_lumped), size = 0.8) +
    geom_point(data = battle_data,
               aes(size = point_size),
               shape = 1, colour = battle_col) +
    geom_text(data = battle_data, 
              aes(label = battle), 
              family = main_family, 
              hjust = 0,
              nudge_x = 5,
              size = 2,
              colour = battle_col) +
    scale_size_identity() +
    coord_sf() +
    theme_void(base_family = main_family) +
    # The date, in the South Atlantic:
    annotate("text", x = 22, y = -60, label = format(the_date, '%e %B %Y'), 
             colour = date_col, hjust = 1) +
    # Summary text (in WWI or not) just below the date:
    annotate("text", x = 22, y = -67, label = date_sum_text, colour = comment_col, 
             hjust = 1, size = 2.5) +
    scale_colour_manual(values = pal, labels = names(pal), drop = FALSE) +
    labs(subtitle = unique(ships_data$phase),
         title = glue("Daily locations of Royal Navy Ships 1914 to 1920"),
         colour = "",
         caption = str_wrap("Locations of 314 UK Royal Navy from log books compiled by 
         naval-history.net; map by freerangestats.info. Ships that survived the 
         war and that travelled out of home waters were more likely to be selected 
         for transcription, which was conduct by volunteers for the 'Zooniverse Old Weather Project'", 160)) +
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Sarala", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, colour = comment_col),
          plot.caption = element_text(colour = "grey70", size = 8, hjust = 0),
          legend.spacing.x = unit(0, "cm"),
          legend.text = element_text(hjust = 0, margin = margin(l = -2, r = 15)),
          legend.background = element_rect(fill = sea_col, colour = NA),
          # sea colour:
          panel.background = element_rect(fill = sea_col, colour = NA))
  
  CairoPNG(glue("tmpimg/{i + 1000}.png"), 2000, 1300, res = 200)
  print(m)
  dev.off()         
}
