### Grouped data summary ###

library(tidyverse)
library(here)

############
### Data ###
############
dat <- read.csv(here("data", "ENSC140_lakechems.csv"))

### Data summary ###
## Mean & SD of analytical replicates
dat.sum <- dat %>% select(-rep) %>% group_by(Sample) %>%
                   summarize(across(where(is.numeric),
                                    list(mn = mean, SD = sd), na.rm = TRUE))

dat.sum <- dat.sum %>% mutate(Sample = ifelse(Sample == "S", 0, Sample)) %>%
                       mutate(Sample = as.numeric(Sample))
  
############                             
### Plot ###
############
pl <- dat.sum %>% ggplot(aes(x = NH4_mgNL_mn, y = Sample)) +
                      geom_pointrange(aes(xmin = NH4_mgNL_mn - NH4_mgNL_SD,  xmax = NH4_mgNL_mn + NH4_mgNL_SD)) +
                      scale_y_reverse()

