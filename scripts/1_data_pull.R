# Data Pull from Movebank for Annual MVMT analysis
# 4/28/2023

packages<-c('tidyverse', 'lubridate', 'sf', 'move', 'stringr', 'here', 'amt', 'furrr')

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# source movebank password from file
source("movebank_password.R")

# Note, this next line doesn't use my actual password
login<-movebankLogin(username=move_usr, password=move_pwd)

main_studyID<-getMovebankID(study = "Interior_Population_Trumpeter_Swans", login=login)
mi_studyID<-getMovebankID(study="trumpeter_swan_Michigan", login=login)

# This works to get 1) timestamp, 2) lat/long, 
#  3)ind_id (which is a unhelpful string), 
#  and 4) tag_id (which is an unhelpful string of numbers)

main_df<-getMovebank(entity_type = "event", login = login, 
                     study_id=main_studyID )
mi_df<-getMovebank(entity_type = "event", login = login, 
                   study_id=mi_studyID )

# This retrieves the alpha-numeric ID's, which we'll use going forward
main_indNames<-getMovebank("individual", login=login, 
                           study_id=main_studyID)[, c("id", "local_identifier")]
mi_indNames<-getMovebank("individual", login=login, 
                         study_id=mi_studyID)[, c("id", "local_identifier")]


main_df<-merge(main_df,main_indNames,by.x="individual_id", by.y="id")
mi_df<-merge(mi_df,mi_indNames,by.x="individual_id", by.y="id")

# Add capture state to keep track
mi_df$capture_state<-"MI"
main_df$capture_state<-NA

# Assign capture state
main_df<-main_df %>% 
  mutate(capture_state=ifelse(grepl("[7-9]L", local_identifier)|
                                grepl("0H_2nd", local_identifier), "AR",
                              ifelse(grepl("9H_2nd", local_identifier)|
                                       grepl("2H_2nd", local_identifier)|
                                       grepl("0N_2nd", local_identifier)|
                                       grepl("6P", local_identifier), "MN",
                              ifelse(grepl("A", local_identifier)|
                                       grepl("E", local_identifier)|
                                       grepl("R", local_identifier)|
                                       grepl("T",local_identifier)|
                                       grepl("L", local_identifier),"MN",
                              ifelse(grepl("M", local_identifier)|
                                       grepl("N", local_identifier), "OH",
                              ifelse(grepl("H", local_identifier), "MB",
                              ifelse(grepl("C", local_identifier), "IA",
                              ifelse(grepl("P", local_identifier), "WI",
                              ifelse(grepl("J", local_identifier)|
                                       grepl("K", local_identifier), "MI",
                                            "flag")))))))))

# Merge both datasets
df<-rbind(main_df, mi_df)

# Append state to ID for mapping later
df$state_ID<-paste(df$capture_state, df$local_identifier, sep="-")

# add sex info
sex<-read_csv(here("swan_sex_codes.csv"))
df<-left_join(df, sex, by="local_identifier")

#don't need individual_id and tag_id
df <- df %>% rename(id="local_identifier", lat="location_lat",
                      long="location_long") 

df <- df %>% dplyr::select('id', 'lat','long', 'timestamp', 'capture_state', 'state_ID', 'sex')

# Create timestamp in correct format, year
df$timestamp<-as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
df$year<-as.factor(year(df$timestamp))

# check, for dups
df <- df %>% distinct 

# Remove outliers with 0,0 coordinates
df<-df[df$lat>25,] #this also fixes longitude outliers


# NSD
df_sp<-st_as_sf(df, coords = c('long', 'lat'), crs=4326)

# now transform from lat/long to UTM's
df_sp<-st_transform(df_sp, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

# get UTM coordinates and add directly to dataframe
df_sp$utm_E<-st_coordinates(df_sp)[,1]
df_sp$utm_N<-st_coordinates(df_sp)[,2]

# switch back to lat/long
df_sp<-st_transform(df_sp, crs = 4326)

# add lat and long to dataframe as well
df_sp$lon<-st_coordinates(df_sp)[,1]
df_sp$lat<-st_coordinates(df_sp)[,2]

# geometry unnecessary and slows things down
df_sp<-st_drop_geometry(df_sp)

trk<-mk_track(df_sp, .x = utm_E, .y = utm_N, .t = timestamp, id = id, all_cols = T)

# calculate NSD
trk2<-trk %>% nest(data=-id) %>%
  mutate(nsd=map(.x=data, .f=amt::nsd)) %>% 
  unnest(cols=c(data, nsd))


#don't preserve spatial column because already have both sets of coordinates
df_nsd<-data.frame(id=trk2[[1]], x=trk2[[2]], y=trk2[[3]], timestamp=trk2[[4]],
                     capture_state=trk2[[5]], state_ID=trk2[[6]], sex=trk2[[7]], 
                     year=trk2[[8]],lon=trk2[[9]], lat=trk2[[10]], nsd=trk2[[11]])
# 
# # save out to file
write_csv(df_nsd, here("data/full_dataset_4_28_2023/full_w_nsd.csv"))

