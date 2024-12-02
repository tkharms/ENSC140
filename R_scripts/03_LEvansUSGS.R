### Lake Evans outflow from USGS ###

library(here)
library(tidyverse)

############
### Data ###
############

dat <- read.csv(here("data", "LEvansUSGS.csv"))

dat <- dat %>% select(Location_Identifier, Location_Name, Location_Type, Location_Latitude, Location_Longitude, Activity_StartDate, Activity_StartTime, Result_Characteristic, Result_CharacteristicUserSupplied, Result_Measure, Result_MeasureUnit)

dat <- dat %>% pivot_wider(names_from = c(Result_CharacteristicUserSupplied, Result_MeasureUnit), values_from = Result_Measure) 

# rename chems
names(dat) <- c("USGSSite", "Location", "Type", "lat", "lon", "Date", "Time", 
                "analytecode", "Bromide_mgL", "Fluoride_mgL", "agency", "Sulfate_mgL", "pH", "Chloride_mgL", "SPC", "NH4_DON_mgNL", "NO3_NO2_mgNL", "Hion_mgL", "NO3_mgNL", "NO2_mgNL", "NH4_mgNL", "NO2_mgNL2", "PO4_mgL", "DON_mgL", "SPC2", "dD_pmil", "d18O_pmil", "PO4_mgL2", "NO3_mgNL2", "DOC_mgL", "TDN_mgL", "DO_mgL", "Tair_C", "Twater_C", "NH4_mgNL3", "DO_pctsat", "baro_mmHG")

# Format concentrations as numeric & summarize to single row
dat.sum <- dat %>% mutate(across(Bromide_mgL:baro_mmHG, as.numeric)) %>%
                   summarize(across(where(is.numeric), list(mn = mean, SD = sd), na.rm = TRUE))
  
summarize(across(where(is.numeric),
                                             list(mn = mean, SD = sd), na.rm = TRUE))