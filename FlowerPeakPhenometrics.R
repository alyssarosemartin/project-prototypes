library(rnpn)
library(dplyr)
library(httr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemr) # still explorign these pretty plots
librar
rm(list= ls())
rm(list=ls()[ls()!= "df"])

devtools::install_github('Mikata-Project/ggthemr')

#### This script formats data, applies rules, to generate peak phenometrics. It outputs a PDF for exploring
#### discontinuous peaks and a csv file with peak onset/end duration and a flag for discontinuous peaks and multiple observers
### Contributors: Hayley Limes, Alyssa Rosemartin, Jeff Oliver

setwd("~/Documents/My Files/USA-NPN/Data/Analysis/R_default/TimetoRestore/Peak/Data")


#Download data
df <- npn_download_status_data(request_source="Alyssa",
                                years=c(2012:2022),
                                species_ids = 1827, # species codes
                                additional_fields = c("observedby_person_id"),
                                phenophase_ids= c(501,500), # open flowers and flowers or flower buds
                                climate_data = FALSE)



#Format data
#Code NAs correctly, extract year from the date
#Code midpoints to values to facilitate calculating the number of flowers
df <- df %>%
  mutate(year = lubridate::year(observation_date)) %>%
  mutate(intensity_value = na_if(intensity_value, "-9999")) %>%
  mutate(midpoint=recode(intensity_value, 'Less than 3'= 2, '3 to 10'= 7, '11 to 100'= 56, '101 to 1,000' = 551, '1,001 to 10,000' = 5510, 'More than 10,000'= 10000, '95% or more'= 0.95, '75-94%'= 0.85, '50-74%' = 0.62, '25-49%'= 0.37, '5-24%'= 0.15, 'Less than 5%'= 0.05))

#I just removed this piece of the recode for midpoint, I don't think any spp have it - More than 10'= 10,

#Get a count of observers who contributed to observing a given ind plant in a year
df <- df %>%
  group_by(individual_id, year) %>%
  mutate(number_observers = n_distinct(observedby_person_id))

#Remove unneeded columns
colnames(df) 
df <- subset(df, select = -c(3,10,11,13,22))

#This removes rows that are 100% dupes, excepting observation_id, observer_id, and intensity type (for coyotebrush 2013-14 removed 31 rows)
df <- df %>% 
  distinct(individual_id,observation_date,phenophase_description,phenophase_status,intensity_value,.keep_all = TRUE)

#Checks if you have an error we might expect to cause issues, where observers report No to Flowers or Flower buds
# but Yes to Open Flowers
df <- df %>% 
  group_by(observedby_person_id, individual_id, observation_date) %>% 
  mutate(FlowersNo = ifelse(phenophase_status == 0 && phenophase_id ==500, 1, NA)) %>% 
  mutate(OpenFlowersYes= ifelse(phenophase_status == 1 && phenophase_id ==501, 1, NA)) %>% 
  mutate(FlowersError = ifelse(FlowersNo == 1 && OpenFlowersYes == 1, 1, NA)) %>%
  ungroup()

#Check the data. It's not happening for this dataset, so I just go ahead and drop these fields
df <- subset(df, select = -c(FlowersNo,OpenFlowersYes,FlowersError))
  
#This identifies repeated observations on the same date/individual/phenophase
df <- df %>% 
  group_by(individual_id,observation_date, phenophase_id) %>% 
  mutate(same_date_ind_pp = n()>1)  %>% 
  ungroup()
df$same_date_ind_pp <- as.integer(df$same_date_ind_pp == "TRUE")

#View the number of records with the same date/ind/pp - presumed conflicts
same_date_ind_pp_summary <- df %>%
  count(phenophase_description, same_date_ind_pp) %>%
  group_by(phenophase_description) %>%
  mutate(freq = n / sum(n))

#appear to be 1% of records for Flowers/Flower buds and 0.6% for open flowers the same date/phenophase/individual plant, for these spp
#have a peak at these records
same_date_records <- df %>% 
  subset(same_date_ind_pp == 1)

#Decided to handle these conflicts by averaging the values for conflicts 
#Convert NAs in Intensity to 0 if status value is 0 (eg, a no to status = a legit 0 for intensity)
#Create a new field with the average intensity values across observation records for the date/ind/phenophase
#So, 0s are averaged in, taking info from people who said yes and no.

df <- df %>% 
  mutate(midpoint2 = ifelse(phenophase_status == 0, 0, midpoint))
  
df <- df %>%       
  group_by(individual_id,observation_date,phenophase_id) %>% 
  mutate(ave_midpoint = mean(midpoint2))  %>% 
  ungroup()

#filter to just the unique records - just one record per date, ind, phenophase
df <- df %>% 
  distinct(individual_id,observation_date,phenophase_description,.keep_all = TRUE)

#Now multiply the two values together - the average midpoint for flowers with with average midpoint for open flowers
#to get the Estimate of Open Flowers
df <- df %>%       
  group_by(individual_id,observation_date) %>% 
  mutate(open_flower_estimate = prod(ave_midpoint))  %>% 
  ungroup()

#Now we can drop the individuals-year combos that have no information at all, on any dates, for open flower estimates
df <- df %>%  
  group_by(year, individual_id) %>%
  filter(any(open_flower_estimate != 0))%>%
  ungroup()

#Now remove the columns you no longer need, focused on Open Flower Estimate by Ind/date
colnames(df) 
df <- subset(df, select = c(3,4,5,6,7,8,9,10,11,12,13,14,18,20,24))

#creates column to flag all observations with over 500 flowers - substantial floral resources available
df <- df %>%  
  group_by(year, individual_id) %>%
  mutate(over_500 = if_else(open_flower_estimate >= 500, 1, 0, missing = NULL))%>%
  ungroup()

#creates column to print the maximum number of flowers for each individual in each year
df <- df %>%  
  group_by(year, individual_id) %>%
  mutate(max_flowers = max(open_flower_estimate, na.rm = TRUE))%>% 
  ungroup()

#flags "near peak" ie within 75% of the max in each ind and year
#creates a peak column which flags all observations which fall within the peak qualifications
#which for this project, include the over 500 resource availability feature
df <- df %>%  
  group_by(year, individual_id) %>%
  mutate(near_max_threshold = max_flowers*0.75)%>%
  mutate(is_near_max = if_else(open_flower_estimate > near_max_threshold, 1, 0))%>%
  mutate(peak = if_else(is_near_max == "1"| over_500 == "1", 1, 0))%>% 
  ungroup()

#Add count of records in and out of peak
df$peak <- as.numeric(df$peak) # watch out - seems like this guy sometimes changes peaks from 0/1 to 1/2
df <- df %>%  
  group_by(year, individual_id) %>%
  mutate(num_records_for_ind_year = n())%>%
  mutate(num_peak_records_for_ind_year = sum(peak))%>% #not working, sometimes gives NA bc there are NAs in peak
  ungroup()

# For each individual and year, want to know peak duration (in number of days) 
# and a flag if the peaks were discontinuous; for these calculations, treat 
# peak values of NA as zeros (not peak)
# From here forward df is the full dataframe, that will eventually get used to print graphs
# int_pmetric is the summary table - which gives the start/end/duration and discont flag by ind-year
int_pmetric <- df %>%
  mutate(peak = if_else(is.na(peak), 0, peak)) %>%
  filter(peak == 1) %>%
  group_by(state, site_id, common_name, individual_id, year, number_observers) %>%
  summarize(onset_doy = min(day_of_year, na.rm = TRUE),
            end_doy = max(day_of_year, na.rm = TRUE)) %>%
  mutate(duration = end_doy-onset_doy + 1) %>% # the +1 is so that there is no duration of zero, 1 day peaks
  ungroup()

# Add flags if any of the not peak values lie within the window of 
# peak values for a particular individual / year combination

# New column will hold flag information
int_pmetric$discontinuous_peak <- NA
# Iterate over each row of data with peak duration information
for (i in 1:nrow(int_pmetric)) {
  # Pull out individual ID and year for this row
  ind.i <- int_pmetric$individual_id[i]
  year.i <- int_pmetric$year[i]
  # Pull out start and end of peak for this individual / year combination
  onset.i <- int_pmetric$onset_doy[i]
  enddate.i <- int_pmetric$end_doy[i]
  #cat("i=", i, "\n")- this is code to help you watch the iterating loop go by
  
  # Subset the original data (df) for just this individual / year
  # Also, set all NA values of peak to 0 and drop the peak rows; for this 
  # comparison we only need days that were not peak
  Non_Peaks_i <- df %>%
    filter(year == year.i,
           individual_id == ind.i) %>%
    mutate(peak = if_else(is.na(peak), 0, peak)) %>%
    filter(peak == 0)
  
  # See if any of the non-peak values lie within the peak duration; if so, mark 
  # these as Discontinuous peaks
  if (any(Non_Peaks_i$day_of_year > onset.i & 
          Non_Peaks_i$day_of_year < enddate.i)) {
    int_pmetric$discontinuous_peak[i] <- 1
  }
}

#With the code above we have successfully flagged our Intensity phenometric data with discontinuous flags
#the resulting int_pmetric file is essentially the prototype for individual intensity phenometrics 
write_csv(int_pmetric, 'intensity_phenometrics_wildparsnip_2013-2022.csv')

#Now back over to the base dataset, df, where we want to get the discontinuous flag and plot data, to reality check
#Our intensity phenometrics, decide if we want to include all ind-year combos
#Populate a new field for discontinuous flag in df from the reference file int_pmetric using a new
#field called common_factor
df <- df %>%
  mutate(common_factor = paste(individual_id,year, sep = '_'))

int_pmetric <- int_pmetric %>%
  mutate(common_factor = paste(individual_id,year, sep = '_'))

df$discontinuous_flag <- int_pmetric$discontinuous_peak[match(df$common_factor, int_pmetric$common_factor)]


#Make Graphs for every individual / year in the dataset
#First some formatting to support display/graphs
df$peak <- as.factor(df$peak)
#df$peak_factor <- recode(df$peak, "1" = 'Peak', "0" = "Not Peak")
df$peak[is.na(df$peak)] <- 0 #turning NAs into 0s
df$discontinuous_flag[is.na(df$discontinuous_flag)] <- 0
df$discontinuous_flag <-as.character(df$discontinuous_flag)
df$discontinuous <- recode(df$discontinuous_flag, "1" = 'yes', "0" = "no")

#Prototype - get a good plot with one ind/year
df_sub <- subset(df, individual_id == 17967 & year == 2013)

p <-ggplot() +
  geom_point(data = df_sub, aes(x = day_of_year, y = open_flower_estimate, color = peak))
print(plot(p + labs(title = paste0(df_sub$common_name," individual #", df_sub$individual_id, " in ", df_sub$year),  
                    subtitle = paste0("Discontinuous peak: ", df_sub$discontinuous, ". ", df_sub$number_observers, " observers contributed ", df_sub$num_records_for_ind_year, " records."), 
                    colour = "Peak", y = "Estimate of Open Flowers", x = "Day of Year")))




#PDF of all Plots

#Create Plots
plot_list = list()
for (i in unique(df$common_factor)){
  df_subset <- df[df$common_factor==i,] #keep only rows for a given ind/year
  
  p = ggplot() +
      geom_point(data = df_subset, aes(x = day_of_year, y = open_flower_estimate, color = peak)) +
      labs(title = paste0(df_subset$common_name," individual #", df_subset$individual_id, " in ", df_subset$year),  
                      subtitle = paste0("Discontinuous peak: ", df_subset$discontinuous, ". ", df_subset$number_observers, " observers contributed ", df_subset$num_records_for_ind_year, " records."), 
                      colour = "Peak", y = "Estimate of Open Flowers", x = "Day of Year")
  plot_list[[i]] = p
}

#Create PDF
pdf("peak_plots_wildparsnip_2012-2022.pdf")
for (i in unique(df$common_factor)){
  print(plot_list[[i]])
}
dev.off()

