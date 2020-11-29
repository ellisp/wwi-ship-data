library(rvest)
library(tidyverse)
library(glue)
library(leaflet)
library(ggmap)
library(maps)
library(mapproj)
library(Cairo)
library(here)
library(RColorBrewer)
library(extrafont)
library(sf)

main_family <- "Roboto"

ggplot2::update_geom_defaults("text", list(family = main_family))
