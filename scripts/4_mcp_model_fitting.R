# Rerun models from first mcp round

# package names
packages<-c("tidyverse", "here", "mcp", "lubridate", "loo", "doFuture")


# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


# load packages
invisible(lapply(packages, library, character.only = TRUE))
source(here("ggplot_custom_function.R"))
options(scipen = 999)

registerDoFuture()
plan(multisession)



# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

df$sqrt<-sqrt(df$nsd_daily_mean)
df$rescale<-df$sqrt/1000


# model syntax
one_int<-list(rescale~1)
two_int<-list(rescale~1,
              ~1)
three_int<-list(rescale~1,
                ~1,
                ~1)
four_int<-list(rescale~1,
               ~1,
               ~1,
               ~1)
five_int<-list(rescale~1,
               ~1,
               ~1,
               ~1,
               ~1)
six_int<-list(rescale~1,
              ~1,
              ~1,
              ~1,
              ~1,
              ~1)
seven_int<-list(rescale~1,
                ~1,
                ~1,
                ~1,
                ~1,
                ~1,
                ~1)
int_mods<-list(one_int, two_int, three_int, four_int, five_int, six_int, seven_int)
int_mod_vec<-c("one_int", "two_int", "three_int", "four_int", "five_int", "six_int", "seven_int")



# Create objects to store the results
res<-data.frame(id=NA, year=NA, name=NA, mean=NA, lower=NA, upper=NA, Rhat=NA, n.eff=NA)
write_csv(res, here("rerun4_output/best_mod_params.csv"))

model_comparison<-data.frame(id=NA, year=NA, 
                             one_int_loo=NA, 
                             two_int_loo=NA,
                             three_int_loo=NA,
                             four_int_loo=NA,
                             five_int_loo=NA,
                             six_int_loo=NA,
                             seven_int_loo=NA)
write_csv(model_comparison, here("rerun4_output/model_comparisons.csv"))


###
# fit models
for(i in seq_along(ids)){
  # filter dataset for each swan
  tmp<-df %>% 
    filter(id==ids[[i]])
  
  # filter each "swan-year"
  years<-unique(tmp$swan_yr)
  
  for(j in seq_along(years)){
    tmp_yr<-tmp %>% 
      filter(swan_yr==years[[j]])
    
    # some years are too few locations
    if(nrow(tmp_yr)>30){
      
      # a numeric index by rows wouldn't include data gaps
      # instead, translate July1 -> July1 with julian dates converted to 1 -> 366
      
      tmp_yr<-tmp_yr %>% 
        mutate(date = ifelse(181<yday& yday<367, yday-181, yday+185))
      
      # fit mcp models
      out_mods<-list()
      
      # fit mcp models
      out_mods<-foreach(mm=1:length(int_mods), .errorhandling = 'pass')%dopar%{
        out_mods<-.GlobalEnv$out_mods
        
        tryCatch(out_mods[[mm]]<-mcp(model = int_mods[[mm]], 
                                     data = tmp_yr[,c("rescale", "date")],
                                     par_x = "date",
                                     adapt=10000, # amount of burn-in
                                     iter=15000), # number of MCMC iterations
                 error = function(e) NULL)
      }
      
      # determine if models fit well or not based on if any Rhat values are higher than 1.1
      loo_list<-list()
      
      for(k in 1:length(out_mods)){
        if(length(out_mods[[k]])!=0){
          mod<-as.data.frame(summary(out_mods[[k]]))
          out_mods[[k]]$rhat_fail<-any(mod$Rhat>1.1)
        }}
      
      # extract parameters for models that had an acceptable fit
      for(nn in 1:length(out_mods)){
        if(length(out_mods[[nn]])!=0){
          if(out_mods[[nn]]$rhat_fail==F){
            out_mods[[nn]]$loo<-loo(out_mods[[nn]])
            loo_list[[nn]]<-out_mods[[nn]]$loo$estimates[[1]]
          }else{
             cat("Skipped model ", years[[j]], int_mod_vec[[nn]], "\n",
                 file=here("rerun4_output/skipped_mods.txt"), append=T)
             loo_list[[nn]]<-(-9999) # this is to reflect that it didn't pass rhat check but stay in numeric for which.max
          }
          }}
      
      
      # Save the relative fit of each model
      loo_vec<-unlist(loo_list)
      # cbind together with id as 1st col and year as 2nd
      mods<-c(ids[[i]], years[[j]],loo_vec)
      # write out to file
      write_csv(as.data.frame(t(mods)), here("rerun4_output/model_comparisons.csv"), append = T)
      
      # pick the best model based on loo
      best_mod<-out_mods[[which.max(loo_list)]]
      params<-as.data.frame(summary(best_mod))
      
      # cbind together with id as 1st col and year as 2nd
      params<-cbind.data.frame(id=ids[[i]], year=years[[j]], params)
      # write out to file
      write_csv(params, here("rerun4_output/best_mod_params.csv"), append = T)
      
      # save out plot of best model
      p<-plot(best_mod, q_fit=T)+
        labs(y = "displacement in km", 
             x = "Date, starting from July 1",
             title = glue::glue("The best model for {years[[j]]} has {length(best_mod$model)} intercepts"))
      
      ggsave(plot = p, filename = here(glue::glue("rerun4_output/best_mod_plots/{years[[j]]}.png")))
      
      # keep track of progress 
      cat("Working on year", j, "out of", length(years), "\n")
    }else{
      # keep track of which swans didn't have enough data to fit a model
      cat("Dataset for ", years[[j]], "skipped because of insufficient sample size.\n",
          file=here("rerun4_output/skipped_swan_years.txt"), append=T)
    }}
  # keep track of progress
  cat("Working on swan", i, "out of", length(ids), "\n")
}



























































