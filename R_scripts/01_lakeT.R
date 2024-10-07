### Lake temperature profiles ###
## T.K. Harms, 10/2024

## Visualize lake temperature data ##
# Data from: Sorensen, T., Espey, E., Kelley, J.G.W. et al. A database of in situ water temperatures for large inland lakes across the coterminous United States. Sci Data 11, 282 (2024). https://doi.org/10.1038/s41597-024-03103-8

library(here)
library(tidyverse)

############
### Data ###
############
dat <- read.csv(here("data", "lakeT.csv"))

# Format date/time
dat <- dat %>% mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#####################
### Example plots ###
#####################
## L Washington ##
## Temperature profile on a single date/time
wash.dep <- dat %>% filter(site == "washington") %>%
  filter(dateTime >= "2013-08-21 08:00:00" & dateTime <= "2013-08-21 12:00:00") %>%
  ggplot(aes(x = temp, y = depth_m)) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_reverse() +
  scale_x_continuous(position = "top")

## 2-D time series
wash.ts <- dat %>% filter(site == "washington") %>%
  ggplot(aes(x = dateTime, y = temp)) +
  geom_point(aes(color = depth_m))

# single year
wash.yr <- dat %>% filter(site == "washington") %>%
  filter(dateTime > "2018-12-31" & dateTime < "2020-01-01") %>%
  ggplot(aes(x = dateTime, y = temp)) +
  geom_point(aes(color = depth_m))

## depth profiles
wash.3d <- dat %>% filter(site == "washington") %>%
  ggplot(aes(x = dateTime, y = depth_m)) +
  geom_line(aes(color = temp), linewidth = 6) +
  scale_y_reverse() +
  scale_color_viridis()

################
### Exercise ###
################
### 1) Does the lake stratify? Hypothesize about how the lake does or does not stratify.
        
### 2) What are the depths of the epilimnion, metalimnion, and hypolimnion (as relevant)? 

### 3) Does the lake mix? How often? When? Hypothesize about why the does or doesn't mix.