# create an animated map of the world with the location of Royal Navy ships
# on each day.


all_dates <- sort(unique(vessel_logs$date))

for(i in 1:length(all_dates)){
  the_date <- all_dates[i]
  d <- vessel_logs %>%
    filter(date == the_date)
  
  m <- d %>%
    ggplot(aes(x = long, y = lat)) +
    borders(fill = "grey", colour = NA) +
    geom_point() +
    coord_sf() +
    theme_void() +
    labs(subtitle = format(the_date, "%d %B %Y"),
         title = "Royal Navy ship locations in World War I")
  
  CairoPNG(glue("tmpimg/{i + 1000}.png"), 2000, 1000, res = 200)
  print(m)
  dev.off()         
}

setwd(glue("{here()}/tmpimg"))

system("D:/other-program-files/ImageMagick-7.0.10-Q16-HDRI/magick *.png wwishipsmovie.gif")

setwd(here())