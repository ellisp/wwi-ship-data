
url1 <- "https://www.naval-history.net/OWShips-LogBooksWW1.htm"




p1 <- read_html(url1)


all_links <- p1 %>%
  html_nodes("a") %>%
  str_extract("OWShips-WW1.*?\\.htm") %>%
  unique()


vessel_logs_l <- list()
all_links <- all_links[!is.na(all_links)]

# There is an error: JMS Welland, should be HMS Welland. URL is correct but
# <a href= is wrong.
all_links <- gsub("JMS_", "HMS_", all_links)

i = 1
for(i in i:length(all_links)){
  cat(i, " ")
  the_url <- glue("https://www.naval-history.net/{all_links[i]}")
  
  the_vessel <- str_sub(all_links[i], 16, -5) %>%
    str_replace("_", " ")
  
  this_ship_page <- read_html(the_url)
  
  vessel_type <- this_ship_page %>%
    html_nodes("title") %>%
    html_text() %>%
    str_replace(" - British warships of World War 1", "") %>%
    str_replace(".*, ", "")
  
  txt <- this_ship_page %>%
    html_nodes("p") %>%
    html_text()
  
  d <- tibble(txt) %>%
    mutate(txt2 = gsub("\r", " ", txt, fixed = TRUE),
           txt2 = gsub("\n", " ", txt2, fixed = TRUE),
           txt2 = str_squish(txt2)) %>%
    mutate(is_date = grepl("^[0-9]+ [a-zA-Z]+ 19[0-9][0-9]$", txt2),
           entry_id = cumsum(is_date),
           is_position = grepl("^Lat.*Long", txt2),
           is_position_description = lag(is_date),
           last_date = ifelse(is_date, txt2, NA),
           last_date = as.Date(last_date, format = "%d %b %Y")) %>%
    fill(last_date) %>%
    filter(entry_id >= 1)
  
  vessel_logs_l[[i]] <- d %>%
    group_by(entry_id) %>%
    summarise(date = unique(last_date),
              position = txt2[is_position],
              # position_description is a bit of a guess, sometimes there are 0,
              # 1 or 2 of them (not necessarily correct), so we just take the
              # first one and hope for the best.
              position_description = txt2[is_position_description][1],
              log_entry = paste(txt2, collapse = "\n"),
              .groups = "drop") %>%
    mutate(url = the_url,
           vessel = the_vessel,
           vessel_type = vessel_type,
           vessel_id = i,
           lat = str_extract(position, "Lat.*?\\.[0-9]+"),
           long = str_extract(position, "Lon.*?\\.[0-9]+"),
           lat = as.numeric(gsub("Lat ", "", lat)),
           long = as.numeric(gsub("Long ", "", long)))
}

vessel_logs <- bind_rows(vessel_logs_l)
save(vessel_logs, file = "data/vessel_logs.rda")

vessel_logs_sel <- select(vessel_logs, -log_entry)
save(vessel_logs_sel, file = "data/vessel_logs_sel.rda")