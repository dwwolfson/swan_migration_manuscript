# Condense full dataset down

# package names
packages<-c("tidyverse", "here", "lubridate")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# Import data (pulled from movebank by script #1)
df<-read_csv(here("data/full_dataset_4_28_2023/full_w_nsd.csv"))

# get in posixct format
df$timestamp<-as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%OS")

# remove CTT incorrect dates
# this date is fixed because I repulled entire dataset on 4/28/2023
df<-df %>% filter(timestamp<"2024-12-31 00:00:00")

# pull off julian day
df$yday<-yday(df$timestamp)

# create unique year-day combinations for each id category
df$year_day<-paste(df$year, df$yday, sep="-")

# calculate daily nsd average
df<-df %>% 
  group_by(id, year_day) %>% 
  mutate(nsd_daily_mean=mean(nsd))

# distill dataset down to one observation a day
# because the combination of the rest of the variables are not distinct for each day, 
# the first observation of each day has been retained (this shouldn't matter, but good to note)
df<-df %>% 
  group_by(id) %>% 
  distinct(year_day, .keep_all = T)

# write out csv to file for future analyses
write_csv(df, here("data/full_dataset_4_28_2023/daily_nsd.csv"))





