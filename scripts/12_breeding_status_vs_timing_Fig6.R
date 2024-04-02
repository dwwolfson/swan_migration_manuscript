# Breeding status vs migration timing

library(here)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(patchwork)

# read in migration metrics output
param_df<-read_csv(here("output/post_march_2024/migration_metrics_v4.csv"))

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
                  spring_arrival),
                ~ifelse(.<186, .+181, .-185)))

# remove a swan with only partial info
p_dates<-p_dates %>% 
  filter(!id_year%in%"8P-2021-2022")

p_dates<-p_dates %>% 
  mutate(across(c(fall_mig_onset, 
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

# plot for timing of migration vs breeder/non-breeder/paired
autumn_onset<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  mutate(breeding_status=fct_relevel(breeding_status, "breeder", "paired", "non_breeder")) %>% 
  ggplot(., aes(breeding_status, fall_mig_onset,fill=breeding_status))+
  scale_x_discrete(labels=c("Breeder", "Paired", "Non-Breeder"))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_date(date_labels = "%b %d")+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="A)")+
  labs(x="", y="Date")+
  theme_pubr()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=17),
        axis.title.y=element_text(size=18),
        text=element_text(size=18, face='bold'),
        axis.text.x=element_text(angle=-25, vjust=0.5),
        panel.grid.major = element_line(colour="lightgrey"))+
  ggtitle("A) Autumn Departure")

spring_arrival<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  mutate(breeding_status=fct_relevel(breeding_status, "breeder", "paired", "non_breeder")) %>% 
  ggplot(., aes(breeding_status, spring_arrival,fill=breeding_status))+
  scale_x_discrete(labels=c("Breeder", "Paired", "Non-Breeder"))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_date(date_labels = "%b %d")+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="B)")+
  labs(x="", y="Date")+
  theme_pubr()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=17),
        axis.title.y=element_text(size=18),
        text=element_text(size=18, face='bold'),
        axis.text.x=element_text(angle=-25, vjust=0.5),
        panel.grid.major = element_line(colour="lightgrey"))+
  ggtitle("B) Spring Arrival")

breeding_duration<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  mutate(breeding_status=fct_relevel(breeding_status, "breeder", "paired", "non_breeder")) %>% 
  ggplot(., aes(breeding_status, mig_duration,fill=breeding_status))+
  scale_x_discrete(labels=c("Breeder", "Paired", "Non-Breeder"))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="C)")+
  labs(x="", y="Number of Days")+
  theme_pubr()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=17),
        axis.title.y=element_text(size=18),
        text=element_text(size=18, face='bold'),
        axis.text.x=element_text(angle=-25, vjust=0.5),
        panel.grid.major = element_line(colour="lightgrey"))+
  ggtitle("C) Duration of Non-Breeding Period")

autumn_onset+spring_arrival+breeding_duration

breeding_timing<-autumn_onset+spring_arrival+breeding_duration

ggsave(here("figures/figs_for_manuscript/breeding_timing.tiff"),
       dpi=300, compression="lzw")