#A simple script version where the counties are sampled
#For the more complete steps, see
#streetlight-sample/docs/sample-streetlight-counties.Rmd

library(tidyverse)
library(sf)
library(mapview)
library(RColorBrewer)
library(viridis)
library(tidycensus)
library(here)

#Load the wrangled county data, created in the RMarkdown

setwd(here("data-processed"))
load("county_wrangle_nogeo.RData")

# The 68 counties in the most urban stratum------
names(county_wrangle_nogeo)
samp_ur_1= county_wrangle_nogeo %>% 
  filter(ur_1==1) %>% #and limit to a few vars
  dplyr::select(county_fips, county_name, urban_rural_6, region_4_no)

# Sample the other 230 following method in RMarkdown------
load("urban_rural_region_strata.RData")
set.seed(123) #set seed so that the sample is the same each time.
samp_230 = county_wrangle_nogeo %>% 
  filter(ur_1==0) %>% 
  filter(pop_above_ur_6_10th==1) %>% 
  left_join(urban_rural_region_strata, by = c("urban_rural_6", "region_4_no")) %>%
  group_by(urban_rural_6, region_4_no) %>% 
  sample_n(
    size=n_counties_to_sample_rnd[1], 
    replace=FALSE) %>%
  mutate(sampled=1) %>% 
  dplyr::select(county_fips, county_name, urban_rural_6, region_4_no, sampled)

samp_298 = samp_ur_1 %>% 
  bind_rows(samp_230) %>% 
  arrange(county_fips)

# save sample
save(samp_298, file = "samp_298.RData")


