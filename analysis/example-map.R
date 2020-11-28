




m <- vessel_logs %>%
  leaflet() %>%
  addProviderTiles(provider = providers$GeoportailFrance.orthos, group = "Satellite imagery")

shown_vessels <- sample(1:max(vessel_logs$vessel_id), 3)
shown_vessels_names <- vessel_logs %>%
  filter(vessel_id %in% shown_vessels) %>%
  distinct(vessel) %>%
  pull(vessel)

for (i in shown_vessels){
  d <- filter(vessel_logs, vessel_id == i) %>%
    arrange(date) %>%
    mutate(hover_lab = paste(vessel, date, position_description),
           click_lab = log_entry)
  
  if(nrow(d) == 0)  {next()}
  
  m <- m %>%
    addMarkers(lng = ~long, lat = ~lat,
               label = ~hover_lab,
               data = d,
               group = unique(d$vessel)
    ) %>%
    addPolylines(lng = ~long, lat = ~lat,
                 data = d,
                 group = unique(d$vessel))
}

m %>%
  addLayersControl(options = layersControlOptions(collapsed = FALSE),
                   overlayGroups = shown_vessels_names)
