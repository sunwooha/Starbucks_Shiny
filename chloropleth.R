library(geojson)
library(geojsonio)
library(leaflet)
library(tidyverse)
library(sp)

url <- "http://leafletjs.com/examples/choropleth/us-states.js"
mapbox_token <- "pk.eyJ1Ijoic3Vud29vaGEiLCJhIjoiY2plYWFoNGlwMDJuZDJ6bWtzcTJwOTBmayJ9.JxBnjC9j23-HIAYfuWIYZg"

# Loading starbucks data
starbucks <- read.csv("final-starbucks-us.csv")
starbucks <- starbucks %>% select(state, value)

# Loading cities dataset
cities <- read.csv("starbucks_original.csv")

# filtering out only US locations and selecting only long, lat columns
cities <- cities %>%
  filter(Country == "US") %>%
  select(City, Longitude, Latitude)

# read as text file
doc <- readLines(url)

# remove the javascript assignment at the front 
doc2 <- gsub("var statesData = ", "", doc)

# write out as a temp file and read
write(doc2, file = "tempgeo.json")
states <- geojson_read("tempgeo.json", what = "sp")

# making bins for choropleth
bins <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, Inf)

# color palatte
pal <- colorBin("Greens", domain = starbucks$value, bins = bins)

# merging dataset for states and starbucks
states <- merge(states, starbucks, by.x="name", by.y="state")

# tool tip
labels <- sprintf(
  "<strong>%s</strong><br/>%g Starbucks Locations per 10,000 People",
  states$name, states$value
) %>% lapply(htmltools::HTML)

# creating the map
leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = mapbox_token)) %>%
  addPolygons(
    fillColor = ~pal(value),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 5,
      color = "#696969",
      dashArray = "",
      fillOpacity = 0.8,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright") %>%
  addCircles(lng = cities$Longitude, lat = cities$Latitude, group = "Locations", color = "#006400", label = cities$City) %>%
  addLayersControl(
    overlayGroups = c("Locations"), options = layersControlOptions(collapsed = FALSE)
  )