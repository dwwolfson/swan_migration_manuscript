# Phenology models

library(here)
library(tidyverse)
library(lme4)
library(sjPlot)
library(patchwork)
library(ggpubr)
library(emmeans)

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

# drop cygnets
param_df<-param_df %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired"))


# Methodology is to fit a LMM with sex and breeding status and if the variance for the random intercept is 0,
# then drop the random effect and fit a LM instead.

# Autumn departure
fall_lmer<-lmer(fall_mig_onset~sex+breeding_status+breeding_lat+
                  (1|swan_ID),
                data=param_df)
# The random effect variance is 0, so switch to LM instead of LMM
fall_lm<-lm(fall_mig_onset~sex+breeding_status+breeding_lat,
            data=param_df)

####################################################
# Spring arrival
spring_lmer<-lmer(spring_arrival~sex+breeding_status+breeding_lat+
                    (1|swan_ID), 
                  data=param_df)
# The variance of the random effect is not 0, so I'll keep the mixed model.

####################################################
# Migration duration
duration_lmer<-lmer(mig_duration~sex+breeding_status+breeding_lat+
                      (1|swan_ID),
                    data=param_df)
# The random effect variance is 0, so switch to LM instead of LMM

duration_lm<-lm(mig_duration~sex+breeding_status+breeding_lat,
                data=param_df)
###########################################################################
# Plot model coefficients

p1<-plot_model(fall_lm,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Non-Breeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4)+
  theme_pubr()+
  labs(y="")+
  ggtitle("Autumn Departure")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black"),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

p2<-plot_model(spring_lmer,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Non-Breeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4)+
  theme_pubr()+
  labs(y="")+
  ggtitle("Spring Arrival")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black"),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

p3<-plot_model(duration_lm,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Non-Breeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4)+
  theme_pubr()+
  labs(y="\nCoefficient Estimates")+
  ggtitle("Migration Duration")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black"),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

new_phenology<-p1/p2/p3

ggsave(new_phenology, file=here("figures/figs_for_manuscript/updated_phenology_models.tiff"),
       dpi=300, compression="lzw")


# Multiple comparisons
# autumn departure
autumn_contrasts<-emmeans(fall_lm, "breeding_status")
autumn_contrasts
pairs(autumn_contrasts, infer=c(T,T))

# spring arrival
spring_contrasts<-emmeans(spring_lmer, "breeding_status")
spring_contrasts
pairs(spring_contrasts, infer=c(T,T))

# migration duration
duration_contrasts<-emmeans(duration_lm, "breeding_status")
duration_contrasts
pairs(duration_contrasts, infer=c(T,T))

