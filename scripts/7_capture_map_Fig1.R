# Capture map with inset map included

# package names
packages<-c("tidyverse", "here", "sf", "ggmap","terra", "tidyterra",
             "rnaturalearth", "ggspatial")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# read in capture points
df<-read_csv(here("data/capture_coordinates.csv"))

# download the following raster at www.naturalearthdata.com

# pull in raster to use as basemap
earth<-terra::rast("data/NE1_50M_SR_W/NE1_50M_SR_W.tif")

# define bounding box
box<-make_bbox(c(-101,-79), c(52,34))

# crop basemap
base_crop<-crop(earth, box)

# state boundaries
states<-ne_states(country = "United States of America")
provinces<-ne_states(country="Canada")

# add in lakes
lakes <- rnaturalearth::ne_download(type = 'lakes', 
                                    scale=10,
                                    category = 'physical') %>% 
  sf::st_as_sf(lakes110, crs = 4263) %>% 
  select(name) %>% 
  filter(name == 'Lake Huron' 
         | name == 'Lake Ontario'
         | name == 'Lake Michigan'
         | name == 'Lake Erie'
         | name == 'Lake Superior'
         | name == 'Lake Winnipeg')

caps_sp<-df %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)

box<-make_bbox(c(-101,-79), c(52,34))

capture_plot<-ggplot()+
  layer_spatial(base_crop, alpha=0.5)+
  geom_sf(data=states, fill=NA)+
  geom_sf(data=provinces, fill=NA)+
  geom_sf(data=lakes, color="black", fill="lightblue")+
  geom_sf(data=caps_sp, fill="black")+
  coord_sf(xlim=c(box[[1]], box[[3]]), ylim=c(box[[2]], box[[4]]), expand=F)+
  annotation_scale()+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw()+
  scale_x_continuous(label=I)+
  scale_y_continuous(label=I)+
  labs(x="\nLongitude", y="Latitude\n")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))

 
 ggsave("figures/capture_map.tiff", capture_plot,
        width=9, height=11, units = "in",
        dpi=300, compression="lzw")
 

 