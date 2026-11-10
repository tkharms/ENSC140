### Grouped data summary ###

library(tidyverse)
library(here)

############
### Data ###
############
dat <- read.csv(here("data", "LEvans_chem.csv"))

# Format dates
dat$date <- as.Date(dat$date, format = "%m/%d/%y")

#############                            
### Plots ###
#############
# depth profile: multiple years, single analyte
pl <- dat %>% ggplot(aes(x = NH4_mgNL, y = depth_m, groups = date)) +
                geom_hline(aes(yintercept = secchi_cm*0.01, color = as.factor(date)), linetype = "dashed") +
                geom_point(aes(color = as.factor(date))) +
                geom_path(aes(color = as.factor(date))) +
                scale_y_reverse() +
                xlab(expression(paste(NH[4]^"+"~ "(mg N/L)"))) +
                ylab("depth (m)") +
                theme_bw() +
                theme(panel.grid = element_blank(),
                      panel.border = element_rect(color = "black", linewidth = 1),
                      axis.text = element_text(size = 18),
                      axis.title = element_text(size = 18),
                      legend.text = element_text(size = 18),
                      legend.title = element_blank()
              )

ggsave(pl, path = "plots", file = "NH4.year.depth.pdf", width = 6, height = 6, units = "in")

# depth profile: single year, multiple analytes
N.pl <- dat %>% mutate(DON_mgL = TDN_mgL - (NH4_mgNL + NO3_mgNL)) %>%
                pivot_longer(cols = c(SRP_uM, T_C, SPC_uScm, DO_pctsat, DO_mgL, NH4_mgNL, NO3_mgNL, DOC_mgL, TDN_mgL, DON_mgL), names_to = "var", values_to = "val") %>%
                filter(var %in% c("NH4_mgNL", "NO3_mgNL", "DON_mgL")) %>%
                #filter(date > "2026-01-01") %>%
                ggplot(aes(x = val, y = depth_m, groups = var)) +
                  geom_point(aes(color = var)) +
                  geom_path(aes(color = var)) +
                  scale_y_reverse() +
                  xlab("N (mg/L)") +
                  ylab("depth (m)") +
                  scale_color_discrete(labels = c("DON", expression(NH[4]^"+"), expression(NO[3]^"-"))) +
                  #xlim(c(0, 2.5)) +
                  facet_wrap(~date) +
                  theme_bw() +
                  theme(panel.grid = element_blank(),
                        panel.border = element_rect(color = "black", linewidth = 1),
                        axis.text = element_text(size = 18),
                        axis.title = element_text(size = 18),
                        legend.text = element_text(size = 18),
                        legend.title = element_blank(),
                        strip.background = element_blank(),
                        strip.text = element_text(size = 18)
  )
