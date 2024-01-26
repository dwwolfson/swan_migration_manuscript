# breeding latitude vs migration timing

library(here)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(patchwork)

# third round (post apr/may 2023)
param_df<-read_csv(here("output/migration_metrics_3rd.csv"))

# Merge additional info onto dataframe
ids<-read_csv(here("ids.csv"))
param_df<-param_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         id_year=year)


# Translate back to dates from julian day
p_dates<-param_df %>% 
  mutate(across(c(fall_mig_onset, 
                  first_departure,
                  furthest_seg_arrival, 
                  furthest_seg_departure, 
                  spring_arrival),
                ~ifelse(.<186, .+181, .-185)))

# remove a swan with only partial info
p_dates<-p_dates %>% 
  filter(!id_year%in%"8P-2021-2022")

p_dates<-p_dates %>% 
  mutate(across(c(fall_mig_onset, 
                  first_departure,
                  furthest_seg_arrival, 
                  furthest_seg_departure, 
                  spring_arrival),
                ~as.Date(., origin="2019-12-31")))

# Add specific years for the fall and spring events (fall_onset and spring_arrival) to track yearly variation
p_dates<-p_dates %>% 
  mutate(fall_yr=map_chr(strsplit(.$id_year, "-"), ~.x[2]),
         spring_yr=map_chr(strsplit(.$id_year, "-"), ~.x[3]))

# add column for entire year cycle
p_dates<-p_dates %>% 
  mutate(entire_yr=paste(map_chr(strsplit(.$id_year, "-"), ~.x[2]),
                         map_chr(strsplit(.$id_year, "-"), ~.x[3]), sep="-"))

# Or, just load this instead
# third round (post apr/may 2023)
# p_dates<-read_csv(here("output/metrics_3rd_round_manuscript_ready.csv")) %>% 
#   filter(!id_year%in%"8P-2021-2022")


# plot for timing of migration vs breeder/non-breeder/paired
autumn_depts<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  filter(fall_yr!=2019) %>% 
  mutate(breeding_status=fct_relevel(breeding_status, "breeder", "paired", "non_breeder")) %>% 
  ggplot(., aes(breeding_lat, fall_mig_onset, color=fall_yr))+
  scale_y_date(date_labels = "%b %d")+
  geom_point(shape=21, aes(fill=fall_yr), color="black")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           label.x.npc="middle",
           label.y=as.Date("2020-12-10"),
           size = 5)+
  labs(x="", y="\nDate of Autumn Departure\n")+
  scale_x_continuous(breaks = seq(42,52, 2),limits=c(41,53)) +
  theme_pubr()+
  theme(legend.position = "none",
        text=element_text(size=16),
        strip.text.x = element_text(size=16))+
  facet_wrap(~fall_yr)


spring_arrival<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  filter(spring_yr!=2020) %>% 
  mutate(breeding_status=fct_relevel(breeding_status, "breeder", "paired", "non_breeder")) %>% 
  ggplot(., aes(breeding_lat, spring_arrival, color=spring_yr))+
  scale_y_date(date_labels = "%b %d")+
  geom_point(shape=21, aes(fill=spring_yr),color="black")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           label.x.npc="middle",
           label.y=as.Date("2020-04-24"),
           size = 5)+
  labs(x="\nBreeding\\Capture Latitude", y="Date of Spring Arrival\n")+
  scale_x_continuous(breaks = seq(42,52, 2),limits=c(41,53)) +
  theme_pubr()+
  theme(legend.position = "none",
        text=element_text(size=16),
        strip.text.x = element_text(size=16))+
  facet_wrap(~spring_yr)


 breeding_timing<-autumn_depts/spring_arrival
ggsave(here("figures/figs_for_manuscript/timings_latitude.tiff"),
            dpi=300, compression="lzw")





