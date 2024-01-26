# Capture map with inset map included

# package names
packages<-c("tidyverse", "here", "leaflet", 
            "mapview", "rnaturalearth")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

df<-read_csv(here("data/capture_coordinates.csv"))

# state boundaries
states<-ne_states(country = "United States of America")
provinces<-ne_states(country="Canada")

# start basemap (note the argument to hide the zoom buttons)
leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
   
   # add basemap
   addProviderTiles(providers$Esri.WorldPhysical) %>% 

   # add state boundaries and labels
   addPolygons(data=states,
               fill=NA,
               weight=2,
               color="lightgrey",
               opacity=1) %>% 
   
   # add province boundaries
   addPolygons(data=provinces,
               fill=NA,
               weight=2,
               color="lightgrey",
               opacity=1) %>% 
   
# focus map on proper area and zoom
   setView(-90, 45, zoom=5) %>% 
   
   # add scale bar
   addScaleBar(position="bottomright") %>% 
   
  # add inset map
  addMiniMap(
    tiles = providers$Esri.WorldPhysical,
    position = 'topright', 
    width = 175, height = 175,
    toggleDisplay = FALSE) %>%
  
  
  # add graticules
   addSimpleGraticule(interval = 10) %>%
 
  
  # add points
  addCircleMarkers(data=df, ~Longitude, ~Latitude,
                   weight = 0.5,
                   col = 'black', 
                   fillColor = 'darkslategrey',
                   radius = 2, 
                   fillOpacity = 0.8)

