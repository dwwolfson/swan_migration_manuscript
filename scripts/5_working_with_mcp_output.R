# working with mcp model output

# package names
packages <- c("tidyverse", "here")

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# Now pick apart all the migration metrics from mcp output

# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
master_params<-list()
skipped_vec<-vector()

mod_df<-mod_df %>% 
  filter(!id%in%c("7L", "8L", "9L")) # Arkansas captures (flipped NSD pattern) don't fit pipeline

# these are the assessments of whether the mcp fit is adequate to extract model parameters
assess<-read_csv(here("data/mcp_assessment.csv"))

for (i in seq_along(ids)) {
  
  # filter dataset for each swan
  tmp <- mod_df %>%
    filter(id == ids[[i]])
  
  years <- unique(tmp$year)
  for (j in seq_along(years)) {
    # check to see that mcp fit reasonably well from visual check 
    if(assess[assess$year==years[[j]],"mcp_fit_well"]=="Y"){
      
      tmp_yr <- tmp %>%
        filter(year == years[[j]])
      
      out_params <- list()
      
      # Data requirement: individuals have to have a full year of data
      out_params["swan_ID"]<-ids[[i]]
      out_params["year"]<-years[[j]]
      
      ######################################################################
      
      # Thresholds
      
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
      spring_proximity_threshold<-30
      
      
      ################################################################################################################################
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
      
      ################################################################################################################################
      # estimate spring arrival
      if (length(grep("int", tmp_yr$name))>2){       # have at least 3 intercepts
        num_ints<-length(grep("int", tmp_yr$name))
        first_int<-tmp_yr[tmp_yr$name=="int_1", "mean"]
        last_int<-tmp_yr[tmp_yr$name==paste0("int_", num_ints), "mean"]
        if(abs(first_int-last_int)<spring_proximity_threshold){    # the spring proximity threshold is satisfied
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
        } else if(abs(first_int-last_int)>spring_proximity_threshold){
          out_params["spring_arrival"]<-NA
          out_params["spring_arrival_comment"]<-glue::glue("first and last intercepts not within {spring_proximity_threshold} km")
        }} else if (length(grep("int", tmp_yr$name))<3){
          out_params["spring_arrival"]<-NA
          out_params["spring_arrival_comment"]<-"no spring arrival date because less than 3 intercepts"
        } #end of spring estimation
      
      ################################################################################################################################
      # estimate migration duration: How long was the duration of time between fall onset and spring arrival
      # considered in the context of 'traditional' longer-distance migration
      if (length(grep("int", tmp_yr$name))>2){          # have at least 3 intercepts
        if (!is.na(out_params[["fall_mig_onset"]]) &&     # have a legit fall departure (traveled far enough away)
            !is.na(out_params[["spring_arrival"]])){        # have a legit spring arrival (returned within proximity of previous year)
          out_params["mig_duration"]<-out_params[["spring_arrival"]]-out_params[["fall_mig_onset"]]
        } else {
          out_params["mig_duration"]<-NA
          out_params["mig_duration_comment"]<-"lack of fall departure and/or spring arrival"
        }} else{
          out_params["mig_duration"]<-NA
          out_params["mig_duration_comment"]<-"need at least 3 intercepts to get migration duration"
        } # end of migration duration 
      
      # Append 
      out_df<-bind_rows(out_params)
      master_params<-append(master_params, list(out_df))
      
    } # end of 'did mcp model fit well' test
    else{skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
    skipped_vec<-c(skipped_vec, skipped)
    }
  } # end loop for years
} # end loop for individuals

# convert to dataframe
param_df<-master_params %>% bind_rows()

# Fix 5C-2021-2022 because it left the summer territory in august then came back before it actually migrated
param_df[param_df$year=="5C-2021-2022","fall_mig_onset"]<-159

# add in arkansas-captured phenology dates
ar<-read_csv(here("output/arkansas_phenology_by_hand.csv"))

param_df<-rbind.data.frame(param_df, ar)  

# see how many swan-year datasets got fall/spring estimation
colSums(!is.na(param_df))

# write dataset to file
# write_csv(param_df, "output/post_march_2024/migration_metrics_v4.csv")

# Merge additional info onto dataframe
ids<-read_csv(here("ids.csv"))
param_df<-param_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)') %>% 
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

# save this to file as well
# write_csv(p_dates, here("output/post_march_2024/metrics_4th_round_manuscript_ready.csv"))
