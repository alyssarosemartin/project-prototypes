library(rnpn)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(lme4)
rm(list=ls())
setwd("~/Documents/My Files/USA-NPN/Data/Analysis/R_default/Obs/GRSM/2022")

#this is exploratory code looking at climate drivers of leaf out at the Great Smokies

df <- npn_download_status_data(
  request_source = 'Alyssa', 
  network_ids = c(72),
  years = c(2009:2022), 
  species_ids = c(3, 98, 61, 82, 1187, 97, 1172, 823, 100, 79, 1189), 
  additional_fields = c("Site_Name", "Network_Name", "Phenophase_Category", "Observed_Status_Conflict_Flag"),
  climate_data = TRUE
)

write.csv(df, file="GRSMTreeDatawClimate2022.csv")

df <- (read.csv("GRSMTreeDatawClimate2022.csv"))

#General formatting & clean up
df=df %>%
  dplyr::mutate(year = lubridate::year(observation_date), 
                month = lubridate::month(observation_date), 
                day = lubridate::day(observation_date))

df$elev_bands <- cut(df$elevation_in_meters, c(-Inf,800,1300,Inf), c("1:Low <800m", "2:Med 800-1300m", "3:High >1300m"))


#Keep only Leaves Category (BLB, Leaves, Colored Leaves, Falling Leaves) and 2010 bc seems they started in Sept, only colored leaves would work, and some unneeded columns
df = df %>% 
  subset(phenophase_category == 'Leaves' & year != 2010) %>%
  subset(select = -c(kingdom, intensity_category_id, abundance_value, update_datetime, state))
  
#remedy NAs in climate data
df = df %>% 
  mutate(tmax_spring = na_if(tmax_spring, "-9999")) %>%
  mutate(tmax_fall = na_if(tmax_fall, "-9999")) %>%
  mutate(tmax_winter = na_if(tmax_winter, "-9999"))%>%
  mutate(tmax_summer = na_if(tmax_summer, "-9999"))%>%
  mutate(prcp_spring = na_if(prcp_spring, "-9999")) %>%
  mutate(prcp_summer = na_if(prcp_summer, "-9999")) %>%
  mutate(prcp_winter = na_if(prcp_winter, "-9999")) %>%
  mutate(prcp_fall = na_if(prcp_fall, "-9999")) 


#subset down to just yeses, and then to just the earliest yes within a year, ind, spp and phenophase
df1 <- df %>%
  subset(phenophase_status == 1) %>%
  group_by(year, individual_id, species_id, phenophase_description) %>%
  filter(day_of_year == min(day_of_year))

#if the phenophase is colored leaves or falling leaves, drop records before day 200, otherwise drop records after June 30
df1 <- df1 %>%
  filter(if(phenophase_id == c(498, 471)) day_of_year > 200 else day_of_year < 182)


#Phenology Data Cleaning
#Conflicting records (see vignette) - go back to the full dataset - df, with 0/1s
conflict_summary <- df %>%
  count(phenophase_category, observed_status_conflict_flag) %>%
  group_by(phenophase_category) %>%
  mutate(observed_status_conflict_flag=recode(
    observed_status_conflict_flag,'MultiObserver-StatusConflict'='Multi', 'OneObserver-StatusConflict'='One')) %>%
  mutate(Percent_Conflict = n / sum(n))

conflicts <- conflict_summary %>%
  filter(observed_status_conflict_flag != '-9999')


p <- ggplot(conflicts,aes(observed_status_conflict_flag, Percent_Conflict)) +
  facet_wrap(~conflicts$phenophase_category) +
  geom_bar(stat = "identity")
plot(p + labs(title = "Percent Multi and One Observer Status Conflict by Phenophase") #need to fix this in vignette
     + scale_y_continuous(labels = scales::percent_format(scale = 100)))

#Decide not to remove any conflicting records, because represent less than 1% of records, thus Yes will override


#Limit data set, first to those individual plants with a 10 year record, then remove outliers based on distribution across years
df2 <- df1 %>% #now going back to the dataset with just first yeses
  group_by(individual_id) %>% 
  filter(n_distinct(year) > 9) %>% 
  mutate(ind_spp_pp = paste(individual_id, species_id, phenophase_description, sep = '_'))

quantiles <- as.data.frame(df2 %>%
                             group_by(ind_spp_pp) %>%
                             summarize(Q1 = quantile(day_of_year, .25), 
                                       Q3 = quantile(day_of_year, .75),
                                       IQR = IQR(day_of_year)))

df3 <- df2 %>% 
  right_join(quantiles, by = "ind_spp_pp")


df4 <- subset(
  df3, (df3$day_of_year > (Q1 - 1.5*df3$IQR) & 
        df3$day_of_year < (Q3 + 1.5*df3$IQR))
)


#Create a dataset of just the Leaves phenophase across species
Leaves  <- df6 %>%
  subset(phenophase_description == c('Leaves'))

#Plot a linear model of first day that "leaves" were observed against spring tmax
#Note this method is not great, because doesn't account for species and 
#sites, which are likely to behave similarly, random effects model likely better

ggplot(data = Leaves, aes(x = tmax_spring, y = day_of_year)) +
  stat_cor() +
  geom_point() +
  stat_smooth(method = "lm", formula = y~x , size = 1)

#create a model object for this linear model and then summarize it 
model <- lm(day_of_year~tmax_spring, data = df)
summary(model)

# look for normally distributed residuals
hist(model$residuals, main = "Residuals Histogram")

#look for pattern, for model assumptions to be met, it should look random
plot(model$residuals, pch = 16, col = "pink", main = "Residuals Plot")

#check for normality in distribution of residuals with Shapiro Wilks
shapiro.test(model$residuals) 
#doesn't pass this test, need to ask about this

