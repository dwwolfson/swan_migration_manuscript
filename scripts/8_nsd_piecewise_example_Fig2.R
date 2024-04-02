# Pull a single year for showing the NSD and piecewise example together

packages <- c("lubridate", "dplyr", "here", "readr", "ggplot2", "ggpubr", "mcp")
# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# read in full dataset
df <- read_csv(here("data/daily_nsd.csv"))

# create variable for 'swan-year' using summer as endpoints
df$yr <- year(df$timestamp)

df <- df %>%
  filter(yr < 2024)
# removed one more outlier

df<-df %>% 
  filter(id=="1C")

df$julian <- yday(df$timestamp)

df$jdate <- as.Date(paste(as.character(df$yr), as.character(df$julian), sep = "-"), "%Y-%j")

df <- df %>%
  mutate(swan_yr = ifelse(julian < 182, paste(id, yr - 1, yr, sep = "-"),
                          paste(id, yr, yr + 1, sep = "-")
  )) # 182 is julian day for july 1

# calculate average daily nsd value
df <- df %>%
  group_by(jdate) %>%
  mutate(daily_nsd = mean(nsd))

# Reduce down to a single point a day
nsd_sub <- df %>%
  distinct(jdate, daily_nsd)
nsd_sub$yr <- year(nsd_sub$jdate)

yrs <- unique(nsd_sub$yr)
nsd_sub<-nsd_sub %>% 
  filter(yr%in%c(2020,2021)) %>% 
  filter(jdate<"2021-07-01")

p1<-ggplot(nsd_sub, aes(jdate, sqrt(daily_nsd) / 1000)) +
  geom_line() +
  labs(y = "Displacement (km)\n", x = "Date", ) +
  theme_pubr()
  
ggsave(p1, file = "figures/figs_for_manuscript/nsd_example.jpeg", width = 6, height = 9, dpi=300, units = "in",bg="white")  

# Now refit the mcp model
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

df<-nsd_sub
df$sqrt<-sqrt(df$daily_nsd)
df$rescale<-df$sqrt/1000
df$year<-year(df$jdate)

df$yday<-yday(df$jdate)
df<-df %>% 
  mutate(date = ifelse(181<yday& yday<367, yday-181, yday+185))

id<-"1C"
df<-df %>% 
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")))

######################################
# Fit piecewise regression model with 5 intercepts

m5<-mcp(model=int_mods[[5]],
        data=df[,c("rescale", "date")],
        par_x = "date",
        adapt=10000, # amount of burn-in
        iter=15000)

########################################
# Plot results

# pull off confidence interval from fitted model
gg_fits<-ggplot_build(plot(m5, lines=0, q_fit=T, cp_dens=F))
gg_bands<-gg_fits$data[[2]]

# Raw points
gg_pts<-gg_fits$data[[1]]

#convert from julian to actual date
gg_pts<-gg_pts %>% 
  mutate(jdate=as.Date(x, origin="2020-07-01"))


lowers<-gg_bands %>% filter(group==1)
lowers<-lowers %>% 
  mutate(jdate=as.Date(x, origin="2020-07-01"))

uppers<-gg_bands %>% filter(group==2)
uppers<-uppers %>% 
  mutate(jdate=as.Date(x, origin="2020-07-01"))

# base version
plot(gg_pts$jdate, gg_pts$y)
lines(lowers$jdate, lowers$y)
lines(uppers$jdate, uppers$y)

#ggplot
p1<-ggplot(gg_pts, aes(jdate, y))+
  geom_point()+
  #geom_ribbon(aes(ymin=lowers$y, ymax=uppers$y))  #doesn't work b/c wrong length
  geom_line(data=lowers, aes(jdate, y), size=0.7, color='red', linetype=2)+
  geom_line(data=uppers, aes(jdate, y), size=0.7, color='red', linetype=2)+
  theme_minimal()+
  labs(x="Date\n", y="Displacement (km)\n")+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=18))
ggsave(p1, file = "figures/piecewise_example.png", width = 9, height = 6, dpi=600, units = "in", bg="white")

