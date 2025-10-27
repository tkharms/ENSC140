### Analysis of SRP ###
## T.K. Harms, 10/2025

### Libraries ###
library(here)
library(tidyverse)
library(googlesheets4)
library(ggpmisc)

############
### Data ###
############
## standards
srp_url <- "https://docs.google.com/spreadsheets/d/1IjABOfYTFSefa801nU9kF8WexBbae0FlndZHQFop_H4/edit?gid=1472257806#gid=1472257806"
stds <- read_sheet(srp_url, sheet = "standards")

## samples
dat <- read_sheet(srp_url, sheet = "samples")

######################
### Standard curve ###
######################
## Plot standard curve ##
ggplot(stds, aes(x = absorbance, y = concentration)) +
    geom_smooth(method = "lm") +
    stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                 parse = TRUE) +
    geom_point()

## Extract calibration coefficients ##
std.curve <- stds %>% lm(concentration ~ absorbance, data = .)
  
intercept <- std.curve$coef[1]
slope <- std.curve$coef[2]

###############################  
### Calculate concentration ###
###############################
dat <- dat %>% mutate(SRP_uM = absorbance * slope + intercept)

#############
### Plots ###
#############