# Phenology models

library(here)
library(tidyverse)
library(lme4)
library(sjPlot)
library(patchwork)
library(ggpubr)
library(emmeans)

# load data
param_df<-read_csv(here("output/post_march_2024/migration_metrics_v4.csv"))

# Merge additional info onto dataframe
ids<-read_csv(here("ids.csv"))
param_df<-param_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)') %>% 
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
#####################################################
# Plot model coefficients
p1<-plot_model(fall_lmer,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Nonbreeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4,
               order.terms = c(1,2,3,4),
               group.terms=c(1,2,3,4),
               colors = c("purple", "orange", "purple", "orange"))+
  theme_pubclean()+
  ylim(-32,36)+
  labs(y="")+
  ggtitle("Autumn Departure")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black", face='bold'),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

p2<-plot_model(spring_lmer,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Nonbreeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4,
               order.terms = c(1,2,3,4),
               group.terms=c(1,2,3,4),
               colors = c("purple", "purple", "purple", "orange"))+
  theme_pubclean()+
  ylim(-32,36)+
  labs(y="")+
  ggtitle("Spring Arrival")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black", face='bold'),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

p3<-plot_model(duration_lmer,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Nonbreeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4,
               order.terms = c(1,2,3,4),
               group.terms=c(1,2,3,4),
               colors = c("purple", "purple", "purple", "orange"),
               show.p=T)+
  theme_pubclean()+
  ylim(-32,36)+
  labs(y="\nCoefficient Estimates")+
  ggtitle("Duration of Nonbreeding Period")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black", face='bold'),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

phenology<-p1/p2/p3

ggsave(phenology, file=here("figures/phenology_models.tiff"),
       dpi=300, compression="lzw")




