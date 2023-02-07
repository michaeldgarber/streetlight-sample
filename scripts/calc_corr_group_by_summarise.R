
#Feb 7 2023

#Demo for calculating correlations using group_by() %>% summarise()
library(tidyverse)
#Generate samp_298 using random variables
samp_298 = 1:298 %>% 
  as_tibble() %>% 
  mutate(
    #creating demo ACS / Streetlight variables
    var_1_acs = rnorm(n=n(), mean=1, sd=2),
    var_1_streetlight = rnorm(n=n(), mean=var_1_acs +1, sd=2),
    
    var_2_acs = rnorm(n=n(), mean=2, sd=2),
    var_2_streetlight = rnorm(n=n(), mean=var_2_acs +2, sd=2),
    
    var_3_acs = rnorm(n=n(), mean=3, sd=2),
    var_3_streetlight = rnorm(n=n(), mean=var_3_acs +2, sd=3)  
    )

# Calculate correlation between ACS / Streetlight variables ------
samp_298_corrs = samp_298 %>% 
  mutate(dummy=1) %>% #1 for every obs so that we can group by the whole dataset
  group_by(dummy) %>% #now group by that dummy variable; this will collapse the dataset into one observation
  summarise(
    corr_spear_var_1 = cor(var_1_acs, var_1_streetlight, method = "spearman", use="complete.obs"),
    corr_spear_var_2 = cor(var_2_acs, var_2_streetlight, method = "spearman", use="complete.obs"),
    corr_spear_var_3 = cor(var_3_acs, var_3_streetlight, method = "spearman", use="complete.obs")
    #etc
  )

samp_298_corrs