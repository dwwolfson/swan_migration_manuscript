# Sensitivity analysis for autumn departure and spring arrival estimation thresholds
# Supplemental Figure S2

# package names
packages <- c("tidyverse", "here", "ggpubr")

# load packages
invisible(lapply(packages, library, character.only = TRUE))


# Analysis 1: Distance threshold for autumn departure (and long-distance migrant classification)
# Previous thresholds to retain:

# rule 1: segments must be at least 2 km from each other in displacement      
dist_threshold<-2  #kilometers
# 
# # rule 2: segments must be at least 2 days separate from each other in time   
time_threshold<-2 #days
# 
# latest date to be considered fall migration onset/ earliest date to be considered spring return
fall_spring_threshold<-184  #translates to Jan 1 as the cutoff (remember that julian dates for each swan-year start at July 1, not Jan 1)


# parameter to vary
fall_onset_threshold<-seq(0, 500, 1)


########
# Estimate autumn departure

# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
autumn_sensitivity_params<-list()
actual_autumn_distances<-list()
skipped_vec<-vector()

mod_df<-mod_df %>% 
  filter(!id%in%c("7L", "8L", "9L")) # Arkansas captures don't fit pipeline

# these are the assessments of whether the mcp fit is adequate to extract model parameters
assess<-read_csv(here("data/mcp_assessment.csv"))

# loop by values of fall onset threshold
for (k in seq_along(fall_onset_threshold)){
  
  # loop by individual swans
  for (i in seq_along(ids)){
    
    # filter dataset for each swan
    tmp <- mod_df %>%
      filter(id == ids[[i]])
    
    years <- unique(tmp$year)
    
    # loop by swan-year dataset
    for (j in seq_along(years)) {
      
      # check to see that mcp fit reasonably well from visual check 
      if(assess[assess$year==years[[j]],"mcp_fit_well"]=="Y"){
        
        tmp_yr <- tmp %>%
          filter(year == years[[j]])
        
        out_params <- list()
        
        # Data requirement: individuals have to have a full year of data
        out_params["swan_ID"]<-ids[[i]]
        out_params["year"]<-years[[j]]
        out_params["autumn_onset_threshold"]<-fall_onset_threshold[[k]]
        
        # estimate autumn departure
        if (length(grep("int", tmp_yr$name))==2){  #if only 1 changepoint, (exclude residents with only one intercept)
          out_params["fall_mig_onset"]<-NA
          out_params["fall_mig_onset_comment"]<-"can't consider a migrant with only 1 changepoint"
        } else if (length(grep("int", tmp_yr$name))>2){ # at least 2 changepoints
          if((tmp_yr[tmp_yr$name == "int_2", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 (dist b/w segments) passes for 1st 2 segments
             tmp_yr[tmp_yr$name=="int_2", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
             tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold &&    # must be between July and December
             # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold 
             abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[k]]
          ){out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"] # rules are satisfied; estimate an autumn migration onset
          
          } else if ((tmp_yr[tmp_yr$name == "int_3", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 3rd segment
                     tmp_yr[tmp_yr$name=="int_3", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
                     tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
                     # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
                     abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[k]]
          ){# rules are satisfied; estimate an autumn migration onset
            out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"]   #skip 1st segment
            
          }else if (length(grep("int", tmp_yr$name))>3){
            if((tmp_yr[tmp_yr$name == "int_4", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 4rd segment
               tmp_yr[tmp_yr$name=="int_4", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
               tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
               # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
               abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[k]]
            ){# rules are satisfied; estimate an autumn migration onset
              out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"]   #skip 1st segment
              
            } else {
              out_params["fall_mig_onset"]<-NA
              out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
            }
          }else if (length(grep("int", tmp_yr$name))==1){
            out_params["fall_mig_onset"] <-NA
            out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
          }else {
            out_params["fall_mig_onset"]<-NA
            out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
          }
          
          # Append 
          out_df<-bind_rows(out_params)
          autumn_sensitivity_params<-append(autumn_sensitivity_params, list(out_df))
          
        }}else{
          skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
          skipped_vec<-c(skipped_vec, skipped) # 
        }
      
    } # end loop for years
  } # end loop for individuals
  cat("Working on fall threshold value ", k, "\n" )
} # end loop for varying parameter values

autumn_df<-autumn_sensitivity_params %>% bind_rows()

# save out csv and figure
# write_csv(autumn_df, here("output/sensitivity_analysis/autumn2_v2_500pts.csv"))

######################################################################################
# Spring sensitivity



# Previous thresholds to retain:

# rule 1: segments must be at least 2 km from each other in displacement      
dist_threshold<-2  #kilometers
# 
# # rule 2: segments must be at least 2 days separate from each other in time   
time_threshold<-2 #days
# 
# latest date to be considered fall migration onset/ earliest date to be considered spring return
fall_spring_threshold<-184  #translates to Jan 1 as the cutoff (remember that julian dates for each swan-year start at July 1, not Jan 1)

# fall migration onset threshold
# In order for 'traditional' fall migration to start, 
# the animal must move a certain distance between breeding/capture zone and the furthest eventual segment.
# This is to exclude the movements of locally resident animals from those of migrants
fall_onset_threshold<-100 

# this is equivalent to the fall threshold; used to make sure when swans return to their previous summer 
# territory, that they had traveled >100km during the nonbreeding period
spring_distance_threshold<-100

# set range of spring migration thresholds
# In order to be considered a return to the breeding/capture site, the last segment ('presumably return to breeding site') must be 
# within a certain distance of the first segment ('presumable breeding site during first summer')
spring_proximity_threshold<-seq(0,100, 1)

spring_sensitivity_params<-list()


# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
skipped_vec<-vector()

mod_df<-mod_df %>% 
  filter(!id%in%c("7L", "8L", "9L")) # Arkansas captures don't fit pipeline

# these are the assessments of whether the mcp fit is adequate to extract model parameters
assess<-read_csv(here("data/mcp_assessment.csv"))

# loop by values of spring proximity threshold
for (k in seq_along(spring_proximity_threshold)){               
  
  # loop by individual swans
  for (i in seq_along(ids)){
    
    # filter dataset for each swan
    tmp <- mod_df %>%
      filter(id == ids[[i]])
    years <- unique(tmp$year)
    
    # loop by swan-year dataset
    for (j in seq_along(years)) {
      
      # check to see that mcp fit reasonably well from visual check 
      if(assess[assess$year==years[[j]],"mcp_fit_well"]=="Y"){
        
        tmp_yr <- tmp %>%
          filter(year == years[[j]])
        out_params <- list()
        
        # Data requirement: individuals have to have a full year of data
        out_params["swan_ID"]<-ids[[i]]
        out_params["year"]<-years[[j]]
        out_params["spring_proximity_value"]<-spring_proximity_threshold[[k]]
        
        # estimate autumn departure
        if (length(grep("int", tmp_yr$name))==2){  #if only 1 changepoint, (exclude residents with only one intercept)
          out_params["fall_mig_onset"]<-NA
          out_params["fall_mig_onset_comment"]<-"can't consider a migrant with only 1 changepoint"
        } else if (length(grep("int", tmp_yr$name))>2){ # at least 2 changepoints
          if((tmp_yr[tmp_yr$name == "int_2", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 (dist b/w segments) passes for 1st 2 segments
             tmp_yr[tmp_yr$name=="int_2", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
             tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold &&    # must be between July and December
             # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold 
             abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold
          ){out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"] # rules are satisfied; estimate an autumn migration onset
          
          } else if ((tmp_yr[tmp_yr$name == "int_3", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 3rd segment
                     tmp_yr[tmp_yr$name=="int_3", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
                     tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
                     # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
                     abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold
          ){# rules are satisfied; estimate an autumn migration onset
            out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"]   #skip 1st segment
            
          }else if (length(grep("int", tmp_yr$name))>3){
            if((tmp_yr[tmp_yr$name == "int_4", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 4rd segment
               tmp_yr[tmp_yr$name=="int_4", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
               tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
               # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
               abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold
            ){# rules are satisfied; estimate an autumn migration onset
              out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"]   #skip 1st segment
              
            } else {
              out_params["fall_mig_onset"]<-NA
              out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
            }
          }else if (length(grep("int", tmp_yr$name))==1){
            out_params["fall_mig_onset"] <-NA
            out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
          }else {
            out_params["fall_mig_onset"]<-NA
            out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
          }
        }# end of autumn departure section
        
        # estimate spring arrival
        if (length(grep("int", tmp_yr$name))>2){       # have at least 3 intercepts
          num_ints<-length(grep("int", tmp_yr$name))
          first_int<-tmp_yr[tmp_yr$name=="int_1", "mean"]
          last_int<-tmp_yr[tmp_yr$name==paste0("int_", num_ints), "mean"]
          if(abs(first_int-last_int)<spring_proximity_threshold[[k]]){    # the spring proximity threshold is satisfied
            num_cp<-length(grep("cp", tmp_yr$name))
            if(tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"]>fall_spring_threshold  && # the last changepoint is during the winter/spring/summer
               last_int<tmp_yr[tmp_yr$name==paste0("int_", num_ints-1), "mean"] && # last change in intercepts moved towards the origin
               abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tail(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean, n=1))>spring_distance_threshold                                                                    # overall migration extent was enough to not be considered resident
            ){    
              out_params["spring_arrival"]<-tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"] # spring arrival is the last changepoint
            }else{
              out_params["spring_arrival"]<-NA
              out_params["spring_arrival_comment"]<-glue::glue("last changepoint not during winter/spring/summer (Dec-July),
                                                   or last movement away from origin, or 
                                                   difference between max and last int not above {spring_distance_threshold} km")
            }
          } else if(abs(first_int-last_int)>spring_proximity_threshold[[k]]){
            out_params["spring_arrival"]<-NA
            out_params["spring_arrival_comment"]<-glue::glue("first and last intercepts not within {spring_proximity_threshold} km")
          }} else if (length(grep("int", tmp_yr$name))<3){
            out_params["spring_arrival"]<-NA
            out_params["spring_arrival_comment"]<-"no spring arrival date because less than 3 intercepts"
          } #end of spring estimation; but append params to list before closing loop for 'did mcp model fit'
        
        
        # Append 
        out_df<-bind_rows(out_params)
        spring_sensitivity_params<-append(spring_sensitivity_params, list(out_df))
        
      } # end of 'did mcp model fit well' test
      else{skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
      skipped_vec<-c(skipped_vec, skipped)}
      
    } # end loop for years
  } # end loop for individuals
  cat("Working on spring threshold value ", k, "\n" )
} # end loop for varying parameter values


spring_df<-spring_sensitivity_params %>% bind_rows()


# save out csv and figure
# write_csv(spring_df, here("output/sensitivity_analysis/spring_v2.csv"))

######################################################################################
# Visualization

# Autumn
# autumn_df<-read_csv(here("output/sensitivity_analysis/autumn2_v2_500pts.csv"))

autumn_gg1<-autumn_df %>%
  group_by(autumn_onset_threshold) %>%
  summarise(autumn_onset_threshold=autumn_onset_threshold,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            prop_years_filtered=sum(is.na(fall_mig_onset))/total_swan_years,
            prop_swan_years_retained=sum(!is.na(fall_mig_onset))/total_swan_years) %>%
  distinct() %>%
  ggplot(., aes(autumn_onset_threshold, prop_swan_years_retained))+
  geom_line()+
  theme_bw()+
  labs(x="\n Distance threshold (km)",
       y="Proportion of swan-year datasets retained\n")+
  ggtitle("A) Autumn Departure")+
  theme(text=element_text(size=20))+
  geom_vline(xintercept=100, color="red")


# Spring
# spring_df<-read_csv(here("output/sensitivity_analysis/spring_v2.csv"))

spring_v2_gg<-spring_df %>%
  group_by(spring_proximity_value) %>%
  summarise(spring_proximity_value=spring_proximity_value,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            num_swan_years_retained=sum(!is.na(spring_arrival)),
            prop_years_retained=sum(!is.na(spring_arrival))/sum(!is.na(fall_mig_onset))) %>%
  distinct() %>%
  ggplot(., aes(spring_proximity_value, prop_years_retained))+
  geom_line()+
  theme_bw()+
  labs(x="\n Proximity threshold (km)",
       y="\nProportion of long-distance migrants retained\n")+
  ggtitle("B) Spring Arrival")+
  theme(text=element_text(size=20))+
  geom_vline(xintercept=30, color="red")

autumn_gg1+spring_v2_gg
ggsave(here("figures/figs_for_manuscript/post_march_2024/filtering_2_panel.png"),
       dpi=300)