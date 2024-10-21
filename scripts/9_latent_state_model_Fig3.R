# latent state mixture model

# package names
packages<-c("tidyverse", "here", "lubridate", "R2jags", 
            "mcmcplots", "loo", "MCMCvis", "viridis", "ggpubr")
options(mc.cores=7)

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# most updated version of dataset
df<-read_csv(here("data/daily_nsd.csv"))
# 126 separate collar deployments

# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is ordinal day for july 1

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
# Now fit latent mixture model

# get index variable for individual
ind_index<-consecutive_id(df$id)

# Model 1: Just latitude and max_nsd 
migs<-df$max_nsd
lats<-df$breeding_lat-min(df$breeding_lat)
df$bl2<-df$breeding_lat-min(df$breeding_lat)
n_obs<-nrow(df)
n_inds<-length(unique(df$id))

latent_model<-function(){
  #Random effects
  for(j in 1:n_inds){
    z[j] ~ dbern(pi) # latent variables, held constant across years for each individual
  }
  pi ~ dunif(0, 1)  # prior for random intercepts
  
  # Other Priors
  alpha ~ dnorm(400, 0.001) # intercept for linear model
  beta1 ~ dnorm(150, 0.001) # slope for linear model
  
  
  a ~ dnorm(30, 0.001)
  exp ~ dunif(0, 30)
  c ~ dunif(0,100)
  
  sigma1 ~ dunif(1,200)
  tau1 <- 1 / (sigma1*sigma1)
  sigma2 ~ dunif(1,200)
  tau2 <- 1 / (sigma2*sigma2)
  
  # Likelihood
  for (i in 1:n_obs){        
    Y[i] ~ dnorm(mu[i], z[ind_index[i]]*tau1+(1-z[ind_index[i]])*tau2) 
    
    # take either functional form based on value of z[i]
    mu[i] <- z[ind_index[i]]*(alpha+beta1*x[i])+
      (1-z[ind_index[i]])*(c+a*x[i]^exp)
  }}

# JAGS data object
jags.dat<-(list(x = lats, Y = migs, n_obs=n_obs, n_inds=n_inds, ind_index=ind_index))

# Parameters and computed values to track
params <- c("mu","alpha", "beta1", "a", "c", "sigma1", "sigma2", "z", "exp")

# Run jags
jagsfit <- jags.parallel(data=jags.dat, parameters.to.save=params,
                         model.file=latent_model,
                         n.thin=10, n.chains=3, n.burnin=10000, n.iter=30000) 

MCMCsummary(jagsfit, params = c("alpha", "beta1", "a", "c", "sigma1", "sigma2", "exp"))


betas<-MCMCpstr(jagsfit, params=c("alpha", "beta1", "a","exp", "c"), type="chains")

#########################################
# Credible Interval plotting
#########################################

# get range of latitude values
lats_pred<-seq(from=min(df$bl2),
               to=max(df$bl2),
               length=100)

# number of mcmc samples
nmcmc<-dim(betas$alpha)[2]

# number of values to predict
nlats<-length(lats_pred)

# matrix to hold 95% CI for each value of lats_pred
conf.int1<-matrix(NA, nlats, 2)
conf.int2<-matrix(NA, nlats, 2)

# loop over values for lats_pred
for(i in 1:nlats){
  # Estimate the migration extent for each breeding lat and for 
  #   each MCMC sample of beta0 and beta1
  mig1_hats <- betas$alpha + rep(lats_pred[i], nmcmc)*betas$beta1
  mig2_hats <- betas$c+betas$a*rep(lats_pred[i],nmcmc)^betas$exp
  
  
  conf.int1[i,] <- quantile(mig1_hats, prob = c(0.025, 0.975))
  conf.int2[i,] <- quantile(mig2_hats, prob = c(0.025, 0.975))
  
}

betas_hat<-MCMCpstr(jagsfit, params = c("alpha", "beta1", "a", "c", "z", "exp"), func=median)



#  Pull out z estimation
zdat<-data.frame(id=unique(df$id), groupID=jagsfit$BUGSoutput$mean$z)
df<-left_join(df, zdat)


mu_hats1<-data.frame(est=rep(betas_hat$alpha, nlats)+
                       rep(betas_hat$beta1, nlats)*lats_pred,
                     LCL=conf.int1[,1],
                     UCL=conf.int1[,2],
                     latitudes=lats_pred)

mu_hats2<-data.frame(est=betas_hat$c+betas_hat$a*lats_pred^betas_hat$exp,
                     LCL=conf.int2[,1],
                     UCL=conf.int2[,2],
                     latitudes=lats_pred)

# use raw latitude values
mu_hats1$raw_lats<-mu_hats1$latitudes+min(df$breeding_lat)
mu_hats2$raw_lats<-mu_hats2$latitudes+min(df$breeding_lat)

ggplot(mu_hats1, aes(raw_lats, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  geom_point(data=df, aes(breeding_lat, max_nsd, color=groupID), size=2)+
  geom_line(aes(x=lats_pred+min(df$breeding_lat), y=betas_hat$c+betas_hat$a*lats_pred^betas_hat$exp))+
  scale_color_continuous(low='red', high='blue', breaks=c(0,1))+
  labs(x="\nBreeding/Capture Latitude", 
       y="Migration Extent (km)\n", 
       color="Probability of Group 1  ")+
  theme_pubr()+
  theme(text=element_text(size=20, face='bold'),
        plot.title=element_text(size=22),
        panel.grid.major = element_line(colour="lightgrey"))

ggsave(here("output/updated_latent_jan_2024/no_randoms.tiff"),
       compression="lzw", dpi=300)

# size with aspect ratio closer to 1 instead of longer horizontal
ggsave(here("figures/figs_for_manuscript/latent_plot.tiff"),
       compression="lzw", dpi=300, width=6383, height=5195, units="px")









