# Supplemental Tables

library(here)
library(tidyverse)
library(flextable)
library(lubridate)
library(ggpubr)
library(patchwork)
library(lme4)
library(emmeans)
library(modelsummary)

### Migration Phenology Summary Statistics:

#### Autumn Departure

# We estimated autumn departure dates for all swans that traveled \>100 km 
# from the breeding/capture territory by 30 December. 

# Read in migration phenology data
p_dates<-read_csv(here("output/post_march_2024/metrics_4th_round_manuscript_ready.csv"))

# first, show a table with summary stats for fall departure and spring arrival
# all swans and years, fall departures
fall_table<-p_dates %>% 
  drop_na(fall_mig_onset) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_fall_depart=format(mean(as.POSIXct(fall_mig_onset)), "%B %d"),
            stan_dev_fall_depart=round(sd(fall_mig_onset)),
            earliest_fall_depart=format(range(fall_mig_onset)[1], "%B %d"),
            latest_fall_depart=format(range(fall_mig_onset)[2], "%B %d"))

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
total_tracked<-data.frame(total_tracked=length(unique(p_dates$swan_ID)))

fall_table<-cbind.data.frame(total_tracked, fall_table)

fall_table %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "total_tracked"="Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "num_swan_years" = "Number of Fall Departure Events",
    "average_fall_depart" = "Average Autumn Departure",
    "stan_dev_fall_depart" = "Standard Deviation (days)",
    "earliest_fall_depart" = "Earliest Departure",
    "latest_fall_depart" = "Latest Departure")) %>% 
  set_table_properties(layout="autofit") %>% 
  fontsize(size=12, part="all") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S1. Compiled migration phenology of all autumn departures from 2019-2022.")

# Split out by year
fall_yearly<-p_dates %>% 
  drop_na(fall_mig_onset) %>% 
  # filter(!fall_yr==2019) %>% 
  group_by(fall_yr) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            average_fall_depart=format(mean(as.POSIXct(fall_mig_onset)), "%B %d"),
            stan_dev_fall_depart=round(sd(fall_mig_onset)),
            earliest_fall_depart=format(range(fall_mig_onset)[1], "%B %d"),
            latest_fall_depart=format(range(fall_mig_onset)[2], "%B %d"))

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
tracked_by_year<-p_dates %>% 
  group_by(fall_yr) %>% 
  summarize(tracked_swans=length(unique(swan_ID)))

fall_yearly<-cbind.data.frame(tracked_by_year, fall_yearly[,2:6])

fall_yearly %>% 
  flextable() %>% 
  colformat_num(big.mark="") %>% 
  set_header_labels(values=list(
    "tracked_swans"="Total Swans Tracked",
    "fall_yr" = "Year",
    "num_swans" = "Number of Long-Distance Migrants",
    "average_fall_depart" = "Average Autumn Departure",
    "stan_dev_fall_depart" = "Standard Deviation (days)",
    "earliest_fall_depart" = "Earliest Departure",
    "latest_fall_depart" = "Latest Departure")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S2. Yearly summaries of migration phenology of autumn departures from 2019-2022.")

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
tracked_by_breeding<-p_dates %>% 
  drop_na(breeding_status) %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>%    #drop cygnets
  mutate(breeding_status=recode(.$breeding_status, breeder="Breeder", non_breeder="Non-Breeder", paired="Paired")) %>% 
  group_by(breeding_status) %>% 
  summarize(tracked_swans=length(unique(swan_ID)))

# Split by breeding status
# Fall
fall_breeding<-p_dates %>% 
  drop_na(fall_mig_onset) %>% 
  drop_na(breeding_status) %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>%
  group_by(breeding_status) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_fall_depart=format(mean(as.POSIXct(fall_mig_onset)), "%B %d"),
            stan_dev_fall_depart=round(sd(fall_mig_onset)),
            earliest_fall_depart=format(range(fall_mig_onset)[1], "%B %d"),
            latest_fall_depart=format(range(fall_mig_onset)[2], "%B %d"))

fall_breeding<-cbind.data.frame(tracked_by_breeding, fall_breeding[,2:7])

fall_breeding %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "breeding_status" = "Breeding Status",
    "tracked_swans" = "Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "num_swan_years" = "Number of Autumn Departure Events",
    "average_fall_depart" = "Average Autumn Departure",
    "stan_dev_fall_depart" = "Standard Deviation (days)",
    "earliest_fall_depart" = "Earliest Departure",
    "latest_fall_depart" = "Latest Departure")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S3. Autumn departure dates of long-distance migrants by breeding status")
#########################################################################################################


#### Spring Arrival

# We estimated spring arrival for all swans that traveled >100 km from the breeding/capture territory during the non-breeding period, 
# left their territory by 30 December, and returned to within <30 km of their previous year's territory. 

p_dates<-read_csv(here("output/post_march_2024/metrics_4th_round_manuscript_ready.csv"))

# all swans and years, spring arrivals
spring_table<-p_dates %>% 
  drop_na(spring_arrival) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_spring_arrival=format(mean(as.POSIXct(spring_arrival)), "%B %d"),
            stan_dev_spring_arrival=round(sd(spring_arrival)),
            earliest_spring_arrival=format(range(spring_arrival)[1], "%B %d"),
            latest_spring_arrival=format(range(spring_arrival)[2], "%B %d"))

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
total_tracked<-data.frame(total_tracked=length(unique(p_dates$swan_ID)))

spring_table<-cbind.data.frame(total_tracked, spring_table)

spring_table %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "total_tracked"="Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "num_swan_years" = "Number of Spring Arrival Events",
    "average_spring_arrival" = "Average Spring Arrival",
    "stan_dev_spring_arrival" = "Standard Deviation (days)",
    "earliest_spring_arrival" = "Earliest Arrival",
    "latest_spring_arrival" = "Latest Arrival")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S4. Compiled migration phenology of all spring arrivals from 2020-2023.")

spring_yearly<-p_dates %>% 
  drop_na(spring_arrival) %>% 
  group_by(spring_yr) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            average_spring_arrival=format(mean(as.POSIXct(spring_arrival)), "%B %d"),
            stan_dev_spring_arrival=round(sd(spring_arrival)),
            earliest_spring_arrival=format(range(spring_arrival)[1], "%B %d"),
            latest_spring_arrival=format(range(spring_arrival)[2], "%B %d"))

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
tracked_by_year<-p_dates %>% 
  group_by(spring_yr) %>% 
  summarize(tracked_swans=length(unique(swan_ID)))

spring_yearly<-cbind.data.frame(tracked_by_year, spring_yearly[,2:6])

spring_yearly %>% 
  flextable() %>% 
  colformat_num(big.mark="") %>% 
  set_header_labels(values=list(
    "spring_yr" = "Year",
    "tracked_swans" = "Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "average_spring_arrival" = "Average Spring Arrival",
    "stan_dev_spring_arrival" = "Standard Deviation (days)",
    "earliest_spring_arrival" = "Earliest Arrival",
    "latest_spring_arrival" = "Latest Arrival")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S5. Yearly summaries of migration phenology of spring arrivals from 2020-2023.")

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
tracked_by_breeding<-p_dates %>% 
  drop_na(breeding_status) %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>%    #drop cygnets
  mutate(breeding_status=recode(.$breeding_status, breeder="Breeder", non_breeder="Non-Breeder", paired="Paired")) %>% 
  group_by(breeding_status) %>% 
  summarize(tracked_swans=length(unique(swan_ID)))

# Split by breeding status
# Spring
spring_breeding<-p_dates %>% 
  drop_na(spring_arrival) %>% 
  drop_na(breeding_status) %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>%
  group_by(breeding_status) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_spring_arrival=format(mean(as.POSIXct(spring_arrival)), "%B %d"),
            stan_dev_spring_arrival=round(sd(spring_arrival)),
            earliest_spring_arrival=format(range(spring_arrival)[1], "%B %d"),
            latest_spring_arrival=format(range(spring_arrival)[2], "%B %d"))



spring_breeding<-cbind.data.frame(tracked_by_breeding, spring_breeding[,2:7])

spring_breeding %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "breeding_status" = "Breeding Status",
    "tracked_swans" = "Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "num_swan_years" = "Number of Spring Arrival Events",
    "average_spring_arrival" = "Average Spring Arrival",
    "stan_dev_spring_arrival" = "Standard Deviation (days)",
    "earliest_spring_arrival" = "Earliest Arrival",
    "latest_spring_arrival" = "Latest Arrival")) %>%  
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S6. Spring arrivals of long-distance migrants by breeding status") %>% 
  width(., width = dim(.)$widths * 6.5 / (flextable::flextable_dim(.)$widths)) 

#########################################################################################################


#### Duration of non-breeding period

#  We estimated duration of non-breeding period for all swans that had an autumn departure
#  (i.e., traveled >100 km from territory) followed by a spring arrival the following year.
#  This migration metric represents the span of time absent from the breeding/capture territory 
#  during the non-breeding period, and is calculated by the difference in days between spring
#  arrival and the previous year's autumn departure. 

p_dates<-read_csv(here("output/post_march_2024/metrics_4th_round_manuscript_ready.csv"))

# all swans and years, migration duration
duration_table<-p_dates %>% 
  drop_na(mig_duration) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_migration_duration=round(mean(mig_duration)),
            stan_duration=round(sd(mig_duration)))

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
total_tracked<-data.frame(total_tracked=length(unique(p_dates$swan_ID)))

duration_table<-cbind.data.frame(total_tracked, duration_table)

duration_table%>% 
  flextable() %>% 
  set_header_labels(values=list(
    "total_tracked"="Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "num_swan_years" = "Number of Annual Cycles",
    "average_migration_duration" = "Average Duration of Non-breeding Period (days)",
    "stan_duration" = "Standard Deviation (days)")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S7. Compiled duration of non-breeding period for all swans from 2019-2023.")


# all swans and years, migration duration
duration_table<-p_dates %>% 
  drop_na(mig_duration) %>% 
  group_by(spring_yr) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            average_migration_duration=round(mean(mig_duration)),
            stan_duration=round(sd(mig_duration)))

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
tracked_by_year<-p_dates %>% 
  group_by(spring_yr) %>% 
  summarize(tracked_swans=length(unique(swan_ID)))

duration_table<-cbind.data.frame(tracked_by_year, duration_table[,2:4])

duration_table%>% 
  flextable() %>% 
  colformat_num(big.mark="") %>% 
  set_header_labels(values=list(
    "spring_yr" = "Year",
    "tracked_swans"="Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "average_migration_duration" = "Average Duration of Non-breeding Period (days)",
    "stan_duration" = "Standard Deviation (days)")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S8. Yearly summaries of duration of non-breeding period from 2019-2020 until 2022-2023.")

# all swans and years, migration duration
duration_table<-p_dates %>% 
  drop_na(mig_duration, breeding_status) %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>%    #drop cygnets
  group_by(breeding_status) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            average_migration_duration=round(mean(mig_duration)),
            stan_duration=round(sd(mig_duration)))

# retain the total number of tracked swans (not just those that fit the rules for fall departure)
tracked_by_breeding<-p_dates %>% 
  drop_na(breeding_status) %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>%    #drop cygnets
  mutate(breeding_status=recode(.$breeding_status, breeder="Breeder", non_breeder="Non-Breeder", paired="Paired")) %>%
  group_by(breeding_status) %>% 
  summarize(tracked_swans=length(unique(swan_ID)))

duration_table<-cbind.data.frame(tracked_by_breeding, duration_table[,2:4])

duration_table%>% 
  flextable() %>% 
  colformat_num(big.mark="") %>% 
  set_header_labels(values=list(
    "breeding_status" = "Breeding Status",
    "tracked_swans"="Total Swans Tracked",
    "num_swans" = "Number of Long-Distance Migrants",
    "average_migration_duration" = "Average Duration of Non-breeding Period (days)",
    "stan_duration" = "Standard Deviation (days)")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S9. Summaries by breeding status of duration of non-breeding period from 2019-2020 until 2022-2023.")

#########################################################################################################
#########################################################################################################

### Migration Phenology Model Output

#### Linear mixed models

param_df<-read_csv(here("output/post_march_2024/migration_metrics_v4.csv"))

# Merge additional info onto dataframe
ids<-read_csv(here("ids.csv"))
param_df<-param_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         id_year=year)

# drop cygnets
param_df<-param_df %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired"))

# Autumn departure
fall_lmer<-lmer(fall_mig_onset~sex+breeding_status+breeding_lat+
                  (1|swan_ID),
                data=param_df)

####################################################
# Spring arrival
spring_lmer<-lmer(spring_arrival~sex+breeding_status+breeding_lat+
                    (1|swan_ID), 
                  data=param_df)


####################################################
# Migration duration
duration_lmer<-lmer(mig_duration~sex+breeding_status+breeding_lat+
                      (1|swan_ID),
                    data=param_df)

models<-c(fall_lmer, spring_lmer, duration_lmer)
names(models)<-c("Autumn Departure", "Spring Arrival", "Duration of Non-breeding Period")


modelsummary(models, 
             output='flextable', 
             #coef_omit = "Intercept", 
             statistic = 'conf.int', 
             stars=T,
             coef_rename=c("sexM"="Sex",
                           "breeding_statusnon_breeder"="Breeder - Non-Breeder",
                           "breeding_statuspaired"="Breeder - Paired",
                           "breeding_lat"="Breeding/Capture Latitude"),
             gof_omit="AIC|BIC|RMSE") %>% 
  width(., width=2.5) %>% 
  set_caption("Table S10. Model summaries from 3 linear mixed models fit using the 
              3 migration metrics (autumn departure, spring arrival, and duration of 
              non-breeding period) as the response variables. 95% confidence intervals 
              for each coefficient are shown in square brackets.")

#########################################################################################################

#### Pairwise contrasts of migration timing by breeding status, adjusted using Tukey's HSD

# autumn departure
autumn_contrasts<-emmeans(fall_lmer, "breeding_status")
autumn<-as.data.frame(pairs(autumn_contrasts, infer=c(T,T))) %>% 
  mutate(across(where(is.numeric), round, digits=2)) %>% 
  mutate(contrast=recode(contrast, 'breeder - non_breeder'='Breeder - Non-Breeder',
                         'breeder - paired' = 'Breeder - Paired',
                         'non_breeder - paired' = 'Non-Breeder - Paired'))
autumn %>% 
  flextable() %>% 
  colformat_num(big.mark="") %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S11. Pairwise contrasts of autumn departure dates by long-distance migrants considered by breeding status.")

# spring arrival
spring_contrasts<-emmeans(spring_lmer, "breeding_status")
spring<-as.data.frame(pairs(spring_contrasts, infer=c(T,T))) %>% 
  mutate(across(where(is.numeric), round, digits=2)) %>% 
  mutate(contrast=recode(contrast, 'breeder - non_breeder'='Breeder - Non-Breeder',
                         'breeder - paired' = 'Breeder - Paired',
                         'non_breeder - paired' = 'Non-Breeder - Paired'))
spring %>% 
  flextable() %>% 
  colformat_num(big.mark="") %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S12. Pairwise contrasts of spring arrival dates by long-distance migrants considered by breeding status.")


# migration duration
duration_contrasts<-emmeans(duration_lmer, "breeding_status")
duration<-as.data.frame(pairs(duration_contrasts, infer=c(T,T))) %>% 
  mutate(across(where(is.numeric), round, digits=2)) %>% 
  mutate(contrast=recode(contrast, 'breeder - non_breeder'='Breeder - Non-Breeder',
                         'breeder - paired' = 'Breeder - Paired',
                         'non_breeder - paired' = 'Non-Breeder - Paired'))
duration %>% 
  flextable() %>% 
  colformat_double(digits=1) %>% 
  colformat_num(big.mark="") %>% 
  #border_outer() %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Table S13. Pairwise contrasts of duration of non-breeding period by long-distance migrants considered by breeding status.")












