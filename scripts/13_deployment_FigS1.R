# Figure of deployment and data transmission

# package names
packages<-c("tidyverse", "here", "lubridate", "scales", "data.table", "ggpubr")

# load packages
invisible(lapply(packages, library, character.only = TRUE))
# source(here("scripts/ggplot_custom_function.R"))
options(scipen = 999)

# use the most recent dataset (pulled 4/28/2023)
df<-fread(here("data/full_dataset_4_28_2023/full_w_nsd.csv"))

# julian date
df$yday<-yday(df$timestamp)

# concatenate year and julian date
df$jdate<-as.Date(paste(as.character(df$year), as.character(df$yday), sep="-"), "%Y-%j")

# change capture state to factor
df$capture_state<-as.factor(df$capture_state)

# remove 1 outlier date
df<-df %>% 
  filter(year<2024)

sub<-df %>% distinct(jdate, id, capture_state)
sub<-sub %>% 
  group_by(id) %>% 
  mutate(days=n())

sub$id<-factor(sub$id, levels=sort(unique(sub$id[order(sub$days)])))

rect1<-data.frame(xstart=as.Date("2019-07-25"), xend=as.Date("2019-08-29"))
rect2<-data.frame(xstart=as.Date("2020-06-23"), xend=as.Date("2020-09-08"))
rect3<-data.frame(xstart=as.Date("2021-07-06"), xend=as.Date("2021-08-22"))
rect4<-data.frame(xstart=as.Date("2021-12-15"), xend=as.Date("2021-12-30"))

ggplot(sub, aes(jdate, fct_reorder(id, days, .desc=F), color=capture_state))+
  theme_pubr()+
  geom_point(size=1.5)+
  theme(axis.text.y=element_blank())+
  theme(axis.text.x=element_text(size=12, face="bold"))+
  facet_grid(fct_relevel(capture_state, 'MI','MN', 'MB', 'IA', 'OH', 'WI', 'AR')~., scales="free")+
  geom_rect(data=rect1, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf,
  ), alpha=0.3, inherit.aes=F)+
  geom_rect(data=rect2, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf,
  ), alpha=0.3, inherit.aes=F)+
  geom_rect(data=rect3, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf,
  ), alpha=0.3, inherit.aes=F)+
  geom_rect(data=rect4, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf,
  ), alpha=0.3, inherit.aes=F)+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x=element_blank())+
  theme(strip.text.y = element_text(size=12, face="bold"))+
  theme(strip.background = element_rect(color="black", fill="white"))+
  guides(colour="none", fill="none")+
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), color="black", size=1)+
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), color="black", size=1)+
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), color="black", size=1)+
  geom_vline(xintercept = as.numeric(ymd("2023-01-01")), color="black", size=1)+
  theme(panel.spacing.y = unit(0, "lines"))

# write to file
ggsave(filename = here("figures/figs_for_manuscript/deployments_reduced_labels.tiff"),
       plot = p, compression = "lzw")
