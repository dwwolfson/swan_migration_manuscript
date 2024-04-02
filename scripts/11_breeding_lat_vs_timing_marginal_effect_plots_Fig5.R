# Breeding latitude by migration timing Figure 5

library(here)
library(tidyverse)
library(lme4)
library(sjPlot)
library(patchwork)
library(ggpubr)
library(emmeans)

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


# Methodology is to fit a LMM with sex and breeding status and if the variance for the random intercept is 0,
# then drop the random effect and fit a LM instead.

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


# Marginal effects plot for latitude (by each response)
fall_marg<-plot_model(fall_lmer, type="pred", terms = "breeding_lat", show.data = T)
spring_marg<-plot_model(spring_lmer, type="pred", terms = "breeding_lat",show.data = T)
duration_marg<-plot_model(duration_lmer, type="pred", terms="breeding_lat",show.data = T)


# Julian date positions for Sep-Dec 1 dates on y axis
fall_dates<-c(64,94,125, 155)
fall_dates_text<-c("Sep 1", "Oct 1", "Nov 1", "Dec 1")

fall_plot<-fall_marg+
  scale_y_continuous(breaks=fall_dates, labels=fall_dates_text)+
  theme_pubr()+
  labs(x="\nBreeding/Capture Latitude", y="Date")+
  theme(text=element_text(size=18, face='bold'),
        plot.title=element_text(size=18),
        panel.grid.major = element_line(colour="lightgrey"))+
  #aspect.ratio = 1)+
  ggtitle("A) Autumn Departure")+coord_fixed(ratio=.1)



# spring dates for plotting
spring_dates<-c(217, 246, 277)
spring_dates_text<-c("Feb 1", "Mar 1", "Apr 1")

spring_plot<-spring_marg+
  scale_y_continuous(breaks=spring_dates, labels=spring_dates_text)+
  theme_pubr()+
  labs(x="\nBreeding/Capture Latitude", y="Date")+
  theme(text=element_text(size=18, face='bold'),
        plot.title=element_text(size=18),
        panel.grid.major = element_line(colour="lightgrey"))+
  #aspect.ratio = 1)+
  ggtitle("B) Spring Arrival")+coord_fixed(ratio=.14)

duration_plot<-duration_marg+
  theme_pubr()+
  labs(x="\nBreeding/Capture Latitude", y="Number of Days")+
  theme(text=element_text(size=18, face='bold'),
        plot.title=element_text(size=17),
        panel.grid.major = element_line(colour="lightgrey"))+
  #aspect.ratio = 1)+
  ggtitle("C) Duration of Non-Breeding Period")+coord_fixed(ratio=.08)


fall_plot+spring_plot+duration_plot




marginals<-fall_plot+spring_plot+duration_plot+plot_layout(widths=c(1,1,1), heights=c(1,1,1))

marginals<-fall_plot+spring_plot+duration_plot
ggsave(marginals, file=here("figures/figs_for_manuscript/post_march_2024/marginal_effect_plots.tiff"),
       dpi=300, compression="lzw")
