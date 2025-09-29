### Analysis of USGS discharge records ###
## T.K. Harms, 9/30/24

## Retrieve, summarize, and plot USGS river discharge monitoring data ##

library(here)
library(tidyverse)
library(dataRetrieval)

############
### Data ###
############
## Use the dataRetrieval package to download river flow data from USGS
sites <- c( "05200510", #Miss
            "09510200", #SYC
            "12113346", #SpringbrookWA
            "02225270") #OhoopeeGA

pCode <- "00060"
# this is the parameter code for discharge, in cubic feet per second

# Download data from USGS.
Qdat <- readNWISuv(siteNumbers = sites,
                   parameterCd = pCode,
                   startDate = "2008-10-01",
                   endDate = "2025-09-27")

write.csv(Qdat, here("data", "Qdat.csv"), row.names = FALSE)

## Read in downloaded data
Qdat <- read.csv(here("data", "Qdat.csv"))

# Rename discharge columns
Qdat <- Qdat %>% rename(Q_cfs = X_00060_00000)

# Format datetime
Qdat$dateTime <- as.POSIXct(Qdat$dateTime, format = "%Y-%m-%d %H:%M:%S")

# Add site names
Qdat <- Qdat %>% mutate(site = ifelse(site_no == 2225270, "Ohoopee River, GA",
                                  ifelse(site_no == 5200510, "Upper Mississippi, MN",
                                    ifelse(site_no == 9510200, "Sycamore Creek, AZ", "Springbrook Creek, WA"
                                      ))))


##########################
### Visualize the data ###
##########################
# basic time series plot
ts.pl <- Qdat %>% ggplot(aes(x = dateTime, y = Q_cfs)) +
                    geom_line() + 
                    facet_wrap(vars(site_no), ncol = 2)
     
# Different y-axis scales to visualize variation within each site
ts.scale.pl <- Qdat %>% ggplot(aes(x = dateTime, y = Q_cfs)) +
                            geom_line() + 
                            facet_wrap(vars(site), ncol = 2, scales = "free_y")

# Clean up plots
ts.times.pl <- Qdat %>% ggplot(aes(x = dateTime, y = Q_cfs)) +
                          geom_line() + 
                          ylab("discharge (cfs)") +
                          facet_wrap(vars(site), ncol = 2, scales = "free_y") +
                          theme_bw() +
                          theme(panel.grid = element_blank(),
                                panel.border = element_rect(color = "black", linewidth = 1),
                                axis.text = element_text(size = 18),
                                axis.title.y = element_text(size = 18),
                                axis.title.x = element_blank(),
                                strip.text = element_text(size = 18),
                                strip.background = element_rect(color="black", linewidth = 1),
                                plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm")
                                )
                          
# Export pdf of figure
ggsave(ts.times.pl, path = "plots", file = "Qtimeseries.pdf", width = 8, height = 5, units = "in")

# Examine last year of data
ts.WY25.pl <- Qdat %>% ggplot(aes(x = dateTime, y = Q_cfs)) +
                        geom_line() + 
                        scale_x_datetime(limits = as.POSIXct(c("2024-10-01", "2025-09-27"))) + #y-axis still scaled to full dataset 
                        ylab("discharge (cfs)") +
                        facet_wrap(vars(site), ncol = 2, scales = "free_y") +
                        theme_bw() +
                        theme(panel.grid = element_blank(),
                          panel.border = element_rect(color = "black", linewidth = 1),
                          axis.text = element_text(size = 18),
                          axis.title.y = element_text(size = 18),
                          axis.title.x = element_blank(),
                          strip.text = element_text(size = 18),
                          strip.background = element_rect(color="black", linewidth = 1),
                          plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm")
  )

# filter data to most recent water year
ts.WY25.pl <- Qdat %>% filter(dateTime >= "2024-10-01" & dateTime <= "2025-09-27") %>%
                       ggplot(aes(x = dateTime, y = Q_cfs)) +
                          geom_line() + 
                          ylab("discharge (cfs)") +
                          facet_wrap(vars(site), ncol = 2, scales = "free_y") +
                          theme_bw() +
                          theme(panel.grid = element_blank(),
                            panel.border = element_rect(color = "black", linewidth = 1),
                            axis.text = element_text(size = 18),
                            axis.title.y = element_text(size = 18),
                            axis.title.x = element_blank(),
                            axis.text.x = element_text(angle = 45, vjust = 0.5),
                            strip.text = element_text(size = 18),
                            strip.background = element_rect(color="black", linewidth = 1),
                            plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm")
  )

#export pdf
ggsave(ts.WY25.pl, path = "plots", file = "QWY25.pdf", width = 8, height = 5, units = "in")

#################
### Summarize ###
#################
## Mean & standard deviation ##
# Most recent 10 water years
Qdat.summ <- Qdat %>% filter(dateTime <= "2024-09-27" & dateTime >= "2014-10-01") %>%
                      group_by(site, site_no) %>%
                      summarize(Q_mn = mean(Q_cfs, na.rm = TRUE),
                                Q_SD = sd(Q_cfs, na.rm = TRUE))
                      
# Plot mean & SD
Qmn.pl <- Qdat.summ %>% ggplot(aes(x = site, y = Q_mn)) +
                          geom_pointrange(aes(ymin = Q_mn - Q_SD, ymax = Q_mn + Q_SD)) +
                          ylab("discharge (cfs)") +
                          theme_bw() +
                          theme(panel.grid = element_blank(),
                                panel.border = element_rect(color = "black", linewidth = 1),
                                axis.text = element_text(size = 18),
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                axis.title.y = element_text(size = 18),
                                axis.title.x = element_blank()
                          )

################
### Exercise ###
################
## Choose another river and analyze its flow regime. Find site codes here: https://waterdata.usgs.gov/nwis/rt

# 1) Describe three distinct features of the hydrograph. It will be helpful to look at a single water year in addition to the full record.

# 2) What are the relative magnitudes of the components of water balance in the catchment? Which features of the hydrograph provide evidence in support of your interpretation?

# 3) Describe how attributes of the climate or contributing area might have caused each of the three features of the hydrograph you identified.
