# package names
packages<-c("tidyverse", "here", "lubridate", "data.table",
             "sf", "ggmap","terra", "tidyterra",
                    "rnaturalearth", "ggspatial")

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# use the most recent dataset (pulled 4/28/2023)
df<-fread(here("data/full_dataset_4_28_2023/full_w_nsd.csv"))

# julian date
df$yday<-yday(df$timestamp)

# concatenate year and julian date
df$jdate<-as.Date(paste(as.character(df$year), as.character(df$yday), sep="-"), "%Y-%j")

# change capture state to factor
df$capture_state<-as.factor(df$capture_state)

# remove 1 outlier date
df <- df %>% 
  filter(year<2024)

# make swan-year variable to join with other dataset for migration categories
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1


# bring in the migration category information
migs<-read_csv(here("output/post_march_2024/migration_categories.csv"))

# add on migration categories
df <- left_join(df, migs[,c("swan_yr", "mig_cat")], by="swan_yr")

# now spatial stuff

# compute daily averages to thin dataset for mapping

# if you want to average to one location a day
# sub<-df %>% 
#   group_by(swan_yr, jdate) %>% 
#   filter(!is.na(mig_cat)==T) %>% 
#   mutate(avg_lat=mean(lat), avg_long=mean(lon)) %>% 
#   select(swan_yr, id, state_ID, mig_cat, avg_lat, avg_long, jdate, year, yday) %>% 
#   distinct()

# If you want to average down to one location a day
#sub_locs<-st_as_sf(sub, coords = c('avg_long', 'avg_lat')) %>% st_set_crs(4326)

sub<-df %>%
  group_by(swan_yr, jdate) %>%
  filter(!is.na(mig_cat)==T) %>%
  select(swan_yr, id, state_ID, mig_cat, lat, lon, jdate, year, yday) 

sub_locs<-st_as_sf(sub, coords = c('lon', 'lat')) %>% st_set_crs(4326)

sub_lines<-sub_locs%>% group_by(id, mig_cat) %>%arrange(year, yday) %>% summarise(do_union=FALSE) %>% st_cast("LINESTRING")

locals<-sub_lines %>% filter(mig_cat=="local")
regs<-sub_lines %>% filter(mig_cat=="regional")
longs<-sub_lines %>% filter(mig_cat=="long-distance")

# take out one swan with wrong fixes
regs<-regs %>% 
  filter(!id=="2M")

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

# comprehensive map with all 3 migration categories together
# I think this makes it too hard to see anything but the long-distance migrants
ggplot()+
  layer_spatial(base_crop, alpha=0.5)+
  geom_sf(data=states, fill=NA)+
  geom_sf(data=provinces, fill=NA)+
  geom_sf(data=lakes, color="black", fill="lightblue")+
  geom_sf(data=sub_lines, aes(color=mig_cat), alpha=0.7)+
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


# 3 separate plots
pal <- c("#d95f02","#1b9e77","#7570b3")


local_map<-ggplot()+
  layer_spatial(base_crop)+
  geom_sf(data=states, fill=NA)+
  geom_sf(data=provinces, fill=NA)+
  geom_sf(data=lakes, color="black", fill="lightblue")+
  geom_sf(data=locals, color="#d95f02")+
  coord_sf(xlim=c(box[[1]], box[[3]]), ylim=c(box[[2]], box[[4]]), expand=F)+
  annotation_scale()+
  labs(title="A) Local Movement")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw()+
  scale_x_continuous(label=I)+
  scale_y_continuous(label=I)+
  labs(x="\nLongitude", y="Latitude\n")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none")

regional_map<-ggplot()+
  layer_spatial(base_crop)+
  geom_sf(data=states, fill=NA)+
  geom_sf(data=provinces, fill=NA)+
  geom_sf(data=lakes, color="black", fill="lightblue")+
  geom_sf(data=regs, color="#1b9e77")+
  coord_sf(xlim=c(box[[1]], box[[3]]), ylim=c(box[[2]], box[[4]]), expand=F)+
  annotation_scale()+
  labs(title="B) Regional Migration")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw()+
  scale_x_continuous(label=I)+
  scale_y_continuous(label=I)+
  labs(x="\nLongitude", y="Latitude\n")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none")

longs_map<-ggplot()+
  layer_spatial(base_crop)+
  geom_sf(data=states, fill=NA)+
  geom_sf(data=provinces, fill=NA)+
  geom_sf(data=lakes, color="black", fill="lightblue")+
  geom_sf(data=longs, color="#7570b3")+
  coord_sf(xlim=c(box[[1]], box[[3]]), ylim=c(box[[2]], box[[4]]), expand=F)+
  #labs(color="Long-Distance Migrants")+
  annotation_scale()+
  labs(title="C) Long-Distance Migration")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw()+
  scale_x_continuous(label=I)+
  scale_y_continuous(label=I)+
  labs(x="\nLongitude", y="Latitude\n")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none")



all_maps<-local_map+regional_map+longs_map
ggsave("figures/paths_by_mig_categories.tiff", all_maps,
       dpi=600, compression="lzw")



