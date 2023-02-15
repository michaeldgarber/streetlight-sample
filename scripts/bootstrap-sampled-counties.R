#Bootstrap the sample
# February 3, 2023
#Updated Feb 15 2023 to calculate number of unique counties in each bootstrap 
#replicate and average (and SD) number of times a given county appears 
#in each bootstrap replicate

library(here) #for managing working directory
library(tidyverse)
library(readr) #for working with CSVs

# Load data---------

#(In my personal project folder (not all on GitHub), 
# I have a folder called data-processed
# data-processed in the same parent directory where my R project file is located.
setwd(here("data-processed")) #Set working directory. 
        
#In this folder, I put an .RData version of the sampled counties
load("samp_298.RData")
#Note if the data were in Excel, then we could
readr::read_csv("samp_298.csv")


# Add two made-up variables that could be correlated with one another-------
samp_298_2_vars = samp_298 %>% 
  mutate(
    x=rnorm(n=n(), mean=10, sd=5),
    y=rnorm(n=n(), mean=x+2, sd=3) #make it somewhat correlated with x
  )

names(samp_298_2_vars)
cor(samp_298_2_vars$x, samp_298_2_vars$y) #yes, correlated

# Check correlation by_group() %>% summarise()------
# a tidyverse way to assess correlation by group
corr_made_up_vars_by_group = samp_298_2_vars %>% 
  #Say we wanted to assess correlation within each urban-rural-region strata
  group_by(urban_rural_6, region_4_no) %>% 
  summarise(
    corr_pears = cor( x, y, method = "pearson", use="complete.obs"),
    corr_spear = cor(x, y, method = "spearman", use="complete.obs")
    ) %>% 
  ungroup()

corr_made_up_vars_by_group

# Bootstrap-------
#https://michaeldgarber.github.io/teach-r/monte-carlo-sim-bootstrapping-purrr.html

## Write a function to re-sample observations by group-----
#Only the 230 counties were actually sampled; 68 were simply selected rather than
#sampled, so those 68 probably shouldn't factor into the sampling variability.  
#So let's make a new dataset with just the 230 that were sampled

samp_230 = samp_298_2_vars %>% 
  filter(urban_rural_6!=1) #exclude counties in this category

nrow(samp_230) #are there 230 left?

#So that we can add back in the 68, also create a dataset filtered to just those:
samp_68 = samp_298_2_vars %>% 
  filter(urban_rural_6==1) 

samp_68
nrow(samp_68)
boot_fun = function(rep_id_val){
  boot_df = samp_230 %>% 
    group_by(urban_rural_6, region_4_no) %>% #group by the urban-rural-region strata 
    slice_sample(prop=1,replace=TRUE) %>% #sample by group with replacement
    ungroup() %>% 
    
    #Add the 68 urban counties back in so that each sample has 298 counties
    bind_rows(samp_68) %>% 
    mutate(rep_id = rep_id_val)  #to keep track of replications
}

## Run the function once to see what happens------
boot_test = boot_fun(1) #the argument for the function is a value for rep_id
boot_test
#what's the mean value for the made-up x variable?
mean(boot_test$x)
#Does it change every time we run the above function?
#Yes
## Run the function lots of times------
#Define the number of replications
rep_id_val_list = 1:1000 #create a sequence of numbers between 1 and 1,000
        #This will create a new "rep_id" for every number

#Run the bootstrapping function lots of times.
boot_lots  = rep_id_val_list %>% 
  map_dfr(boot_fun) #map_dfr is in the purrr() family of functions\
                    #map_dfr specifically row binds each iteration together
                    #(vs map_dfc, which would column bind)

#Check out the resulting dataset.
#It should be 1,000 samples stacked on top of one another,
#so n, rows= 298*1,000
289*1000
nrow(boot_lots)
boot_lots
#Are there 100 distinct values for the rep_id?
n_distinct(boot_lots$rep_id)


## Summarize correlation over replications--------
### First calculate the correlation in each of the samples------
summarize_corr_by_boot_rep = boot_lots %>% 
  group_by( rep_id) %>% 
  summarise(
    corr_pears = cor( x, y, method = "pearson", use="complete.obs"),
    corr_spear =  cor(x, y, method = "spearman", use="complete.obs")#spearman rank
  ) %>% 
  ungroup()

### Then summarize over those samples to get a confidence interval----
summarize_corr_over_boot_reps = summarize_corr_by_boot_rep %>% 
  #we no longer have anything to group by (they've been collapsed), so we can hack it
  #If there were still stratum of interest (say, regions, etc., those could be used here)
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    #Find the 2.5th and 97.5th percentiles
    corr_pears_ll = quantile(corr_pears, probs =c(0.025), na.rm=TRUE),
    corr_pears_ul = quantile(corr_pears, probs =c(0.975), na.rm=TRUE),
    corr_spear_ll = quantile(corr_spear, probs =c(0.025), na.rm=TRUE),
    corr_spear_ul = quantile(corr_spear, probs =c(0.975), na.rm=TRUE)
  ) %>% 
  ungroup()
    
#Print confidence intervals
summarize_corr_over_boot_reps

# How many unique counties are there in each replicate?------
#Feb 15 2023
#The object, 'boot_lots', is the dataset of samples stacked on top of one another.
#In each sample, we can find the number of times a county appears (it might be not at all)
n_times_county_appears_each_sample = boot_lots %>% 
  #first filter to the 230 counties that were sampled,
  #as the other 68 will appear in every replicate
  
  #the "sampled" variable refers to whether it's one of the 230 or not,
  #not whether it was sampled in that particular bootstrap replicate.
  filter(sampled==1) %>% 
  #group by the id for the replicate and the county_id
  group_by(rep_id,county_fips) %>% 
  #Use n() within summarise() to count the number of 
  #times a county_id appears in each replicate
  summarise(n_times_in_sample = n()) %>% 
  ungroup()


n_times_county_appears_each_sample

#How many unique counties are in each sample?
#We can answer this by counting the number of times county_fips appears at all
#in a given rep_id
n_unique_counties_each_sample = n_times_county_appears_each_sample %>% 
  group_by(rep_id) %>% 
  summarise(n_unique_counties_each_rep = n()) %>% 
  ungroup() %>% 
  #summarize (mean, sd) that variable over all datas
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    n_unique_counties_each_rep_mean = mean(n_unique_counties_each_rep,na.rm=TRUE),
    n_unique_counties_each_rep_sd = sd(n_unique_counties_each_rep,na.rm=TRUE),
    n_unique_counties_each_rep_var = var(n_unique_counties_each_rep,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  #Remember this is just the sampled counties. The other 68 will always be there,
  #so we could just add 68 to the mean, as it's a constant, if we wanted to include
  #those 68 in this number.
  #The SD and variance would be the same.
  mutate(
    n_unique_counties_each_rep_mean_inc_68 = n_unique_counties_each_rep_mean+68,
    n_unique_counties_each_rep_sd_inc_68 = n_unique_counties_each_rep_sd ,
    n_unique_counties_each_rep_var_inc_68 = n_unique_counties_each_rep_var
  )
  

n_unique_counties_each_sample
#To see all var names better
View(n_unique_counties_each_sample)



#We may also want to calculate the average number of times counties appear
#in each sample (rather than unique counties)

#To make sure that we count replicates where a county is not included,
#I'm going to begin with a dataset of replications where each of the 230 counties are 
#included in every replicate, and we can then link the bootstrap samples to this dataset
#to count replicates where a county was not sampled as well
samp_230_1000 =samp_298 %>% 
  filter(sampled==1) %>% 
  #uncount is a way to stack identical datasets on top of one another
  #n times
  uncount(1000) %>%
  #generate an equivalent "rep_id" variable here
  #so we can link the bootstrapped data in
  
  #Group by county and then take the row_number() to do this
  group_by(county_fips) %>% 
  mutate(rep_id = row_number()) %>%
  ungroup() %>%  #back to ungrouped
  #remove variables other than county_fips and rep_id before left_join()
  dplyr::select(county_fips, rep_id)

#Okay, now we can link n_times_county_appears_each_sample
#to samp_230_1000 in which we know every county appears exactly once

n_times_county_appears_linked = samp_230_1000 %>% 
  #begin with samp_230_1000 and then left_join()
  #n_times_county_appears_each_sample to it
  #left join by both county fips and rep id
  left_join(n_times_county_appears_each_sample, by = c("county_fips","rep_id")) %>% 
  #now, if the variable n_times_in_sample is missing (NA), we know it wasn't included
  #in that replicate. Let's make a new variable that makes those instances
  #zero instead.
  mutate(
    #This code says... if n_times_in_sample is missing, make it zero
    #else, set it equal to n_times_in_sample
    n_times_in_sample_linked = case_when(
      #small nuance: n_times_in_sample is an integer, so we need the as.integer()
      is.na(n_times_in_sample) ==TRUE~as.integer(0),
      TRUE ~ n_times_in_sample
    )
  )

#Compare n_times_in_sample with n_times_in_sample_linked
n_times_county_appears_linked

#Now summarize n_times_in_sample_linked by county
n_times_in_sample_summary_by_county = n_times_county_appears_linked %>% 
  group_by(county_fips) %>% 
  summarise(
    n_times_in_sample_mean=mean(n_times_in_sample_linked, na.rm=TRUE),#mean
    n_times_in_sample_sd=sd(n_times_in_sample_linked, na.rm=TRUE),#sd
    n_times_in_sample_var=var(n_times_in_sample_linked, na.rm=TRUE),#variance
    n_times_in_sample_min=min(n_times_in_sample_linked, na.rm=TRUE),#minimum
    n_times_in_sample_max=max(n_times_in_sample_linked, na.rm=TRUE)#maximum
    )

n_times_in_sample_summary_by_county
#can then summarize each of those variables
#on average, a county is sampled once( mean,n_times_in_sample_mean=1)
summary(n_times_in_sample_summary_by_county)
    
  

