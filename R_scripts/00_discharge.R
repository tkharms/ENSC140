### Analysis of USGS discharge records ###
## T.K. Harms, 9/30/24

## Retrieve, summarize, and plot USGS river discharge monitoring data ##

# Load libraries
library(here)
library(tidyverse)
library(dataRetrieval)

############
### Data ###
############
## Use the dataRetrieval package to download river flow data from USGS
sites <- c( "05200510", #UMS
            "09510200", #SYC
            "12113346", #SPR
            "02225270") #OHO

pCode <- "00060"
# this is the parameter code for discharge, in cubic feet per second

# Download data from USGS.
Qdat <- readNWISuv(siteNumbers = sites,
                   parameterCd = pCode,
                   startDate = "2008-10-01",
                   endDate = "2025-09-27")

write.csv(Qdat, here("data", "Qdat.csv"), row.names = FALSE)

#missi <- Qdat %>% filter(site_no == 05200510)
#syc <- Qdat %>% filter(site_no == 09510200)
#spr <- Qdat %>% filter(site_no == 12113346)
#oho <- Qdat %>% filter(site_no == 02225270)

# Export smaller files for git upload
#write.csv(missi, here("data", "Qmiss.csv"), row.names = FALSE)
#write.csv(oho, here("data", "Qoh.csv"), row.names = FALSE)
#write.csv(spr, here("data", "Qspring.csv"), row.names = FALSE)
#write.csv(syc, here("data", "QSYC.csv"), row.names = FALSE)

## Read in downloaded data
# Qdat <- read.csv(here("data", "Qdat.csv"))
missi <- read.csv(here("data", "Qmiss.csv"))
syc <- read.csv(here("data", "QSYC.csv"))
spr <- read.csv(here("data", "Qspring.csv"))
oho <- read.csv(here("data", "Qoh.csv"))

# Merge sites into single dataframe
Qdat <- bind_rows(missi, syc, spr, oho)

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

## Exam figures ##
## Use the dataRetrieval package to download river flow data from USGS
exsites <- c( "09498500", #Salt
              "11078000", #Santa Ana
             
              "15514000") #Chena

pCode <- "00060"
# this is the parameter code for discharge, in cubic feet per second

# Download data from USGS.
Qdat <- readNWISuv(siteNumbers = exsites,
                   parameterCd = pCode,
                   startDate = "2025-10-01",
                   endDate = "2026-10-01")

sal <- readNWISuv(siteNumbers = 15484000,
                   parameterCd = pCode,
                   startDate = "2024-10-01",
                   endDate = "2025-10-01")

# change dates for Salcha to 2025 to allow plotting on same axis as other sites for exam purposes only
sal <- sal %>% mutate(dateTime = dateTime %m+% years(1))

# Merge sites into single dataframe
Qdatall <- bind_rows(Qdat, sal)

# Rename discharge columns
Qdatall <- Qdatall %>% rename(Q_cfs = X_00060_00000)

# Format datetime
Qdatall$dateTime <- as.POSIXct(Qdatall$dateTime, format = "%Y-%m-%d %H:%M:%S")

# Add site names
Qdatall <- Qdatall %>% mutate(site = ifelse(site_no == "09498500", "Salt",
                                        ifelse(site_no == "11078000", "Santa Ana",
                                            ifelse(site_no == "15514000", "Chena", "Salcha"
                                             ))))

Qdatall <- Qdatall %>% mutate(code = ifelse(site_no == "09498500", "River A",
                                            ifelse(site_no == "11078000", "River 1",
                                                   ifelse(site_no == "15514000", "River B", "River 2"
                                                   ))))

## Plots ##
set1.pl <- Qdatall %>% filter(site == "Chena" | site == "Salt") %>%
                    ggplot(aes(x = dateTime, y = Q_cfs)) +
                      geom_line() + 
                      ylab("discharge (cfs)") +
                      facet_wrap(vars(code), ncol = 1, scales = "free_y") +
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
ggsave(set1.pl, path = "plots", file = "Fig3A.pdf", width = 7, height = 8, units = "in")
ggsave(set1.pl, path = "plots", file = "Fig3A.jpeg", width = 7, height = 8, units = "in")

set2.pl <- Qdatall %>% filter(site == "Salcha" | site == "Santa Ana") %>%
  ggplot(aes(x = dateTime, y = Q_cfs)) +
  geom_line() + 
  ylab("discharge (cfs)") +
  facet_wrap(vars(code), ncol = 1, scales = "free") +
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
ggsave(set2.pl, path = "plots", file = "Fig3B.pdf", width = 7, height = 8, units = "in")
ggsave(set2.pl, path = "plots", file = "Fig3B.jpeg", width = 7, height = 8, units = "in")

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

head(Qdat)

################
### Exercise ###
################
## Choose another river and analyze its flow regime. Find site codes here: https://waterdata.usgs.gov/nwis/rt

# 1) Describe three distinct features of the hydrograph. It will be helpful to look at a single water year in addition to the full record.

# 2) What are the relative magnitudes of the components of water balance in the catchment? Which features of the hydrograph provide evidence in support of your interpretation?

# 3) Describe how attributes of the climate or contributing area might have caused each of the three features of the hydrograph you identified.
