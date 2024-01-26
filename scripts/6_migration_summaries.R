# Summarizing migration categories

# logit latent state model

# package names
packages<-c("tidyverse", "here", "lubridate")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# most updated version of dataset
df<-read_csv(here("data/full_dataset_4_28_2023/daily_nsd.csv"))
# 126 separate collar deployments

# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

original_swan_yrs<-unique(df$swan_yr)
original_ids<-unique(df$id)

df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(num_days=n())

# filter out swans that had years with less than 90 days
df<-df %>% 
  filter(num_days>90)

# filtered out 21 swan-years with less than 90 days; 231 swan-year combinations

# Convert NSD to simple displacement in kilometers
df$sqrt<-sqrt(df$nsd_daily_mean)
df$rescale<-df$sqrt/1000

#########################################################################

# Pull out the maximum displacement value for each swan-year
df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(max_nsd=max(rescale)) %>% 
  select(id, capture_state, state_ID, sex, swan_yr, max_nsd, num_days) %>% 
  distinct()

# others to exclude
exclude<-c(
  "1P-2020-2021", # taken into custody, year all screwy
  "9J (swan originally collared as 5J)-2021-2022", # collar died before winter
  "5L-2020-2021" ,"5L-2021-2022", # the cygnet that went up to Hudson Bay
  "6M-2021-2022", "6M-2022-2023", # Ohio disperser
  "7M-2021-2022", # Dropped 7M because it made a big movement NE into Pennsylvania (and then back to territory)
  "8P-2021-2022", # big summer dispersal N and then collar died
  "9N-2021-2022", # big summer dispersal
  "9N-2022-2023"  # big summer dispersal
)

# add in breeding lat and other info
ids<-read_csv(here("ids.csv"))
df<-df %>% 
  left_join(., ids) %>% 
  select(-comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         mass='mass (kg)', skull='skull (mm)')

df<-df %>% 
  filter(!swan_yr%in%exclude)
# excluded 10 more swan-year datasets, from 231 to 221

filtered_swan_yrs<-unique(df$swan_yr)
filtered_ids<-unique(df$id)

# filtering summaries
removed_swan_yrs<-setdiff(original_swan_yrs,filtered_swan_yrs) # taken from 27 individuals
removed_ids<-setdiff(original_ids, filtered_ids) # 10 individuals dropped out entirely

#########################################################################

# Migration categories
df<-df %>% 
  mutate(mig_cat=ifelse(max_nsd<25, "local",
                        ifelse(max_nsd>25&max_nsd<100, "regional",
                               ifelse(max_nsd>100, "long-distance","flag"))))
  
df %>% 
  group_by(mig_cat) %>% 
  summarise(count=n(),
            prop=count/nrow(df))

# Summarize by individual instead of by swan-year
switching<-df %>% 
  drop_na(mig_cat) %>% 
  group_by(id) %>% 
  summarize(num_mig_cats=length(unique(mig_cat)))

multiples<-switching %>% 
  filter(num_mig_cats>1)

# after going through by hand, these are the 'switchers' and their switch:
# local and regional, n=4, OT_2nd, 1A, 2M, 5R
# local and long-distance, n=0
# regional and long-distance, n=3, 1C, 4J, 9A

# how many swan_year datasets from swans that only had one migration category were in each category
df %>% 
  group_by(id) %>% 
  mutate(num_mig_cats=length(unique(mig_cat))) %>% 
  ungroup %>% 
  filter(num_mig_cats==1) %>% 
  group_by(mig_cat) %>% 
  summarize(num=n())

# how many swans that only had one migration categories were in each category
df %>% 
  group_by(id) %>% 
  mutate(num_mig_cats=length(unique(mig_cat))) %>% 
  ungroup %>% 
  filter(num_mig_cats==1) %>%
  group_by(mig_cat) %>% 
  distinct(id) %>% 
  summarize(num_cat=n())
