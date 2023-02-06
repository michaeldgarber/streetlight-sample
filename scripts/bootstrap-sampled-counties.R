#Bootstrap the sample
# February 3, 2023

library(here) #for managing working directory
library(tidyverse)
library(readr) #for working with CSVs

# Load data---------

setwd(here("data-processed")) #Set working directory. 
          #(In my personal project folder (not all on GitHub), I have a folder called data-processed
          # data-processed in the same parent directory where my R project file is located.

#In this folder, I put an .RData version of the sampled counties
load("samp_298.RData")
#Note if the data were in Excel, then we could
readr::read_csv("samp_298.csv")

samp_298 #check it to make sure

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
  group_by(urban_rural_6, region_4_no) %>% #group by the urban-rural-region strata, say
  summarise(
    corr_pears = cor( x, y, method = "pearson", use="complete.obs"),
    corr_spear =  #spearman rank
      cor(x, y, method = "spearman", use="complete.obs")
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
  filter(urban_rural_6==1) #exclude counties in this category

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
                    #(vs map_dfc, which would colum bind)

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

    
  

