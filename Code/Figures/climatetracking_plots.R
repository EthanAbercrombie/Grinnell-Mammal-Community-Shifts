#Author: Ethan Abercrombie

#Packages
require(tidyverse)

####
#Load Data
####

site_data <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/site_data_clean.csv')

#General Graph Attributes

font_family = 'Helvetica'
text_size = 12
text_color = 'black'


###
#Lassen
###



#Plot CTI/CPI against respective climate axes. 
#Evaluate residuals between groups to see if climate tracking is occurring.

###
#Lassen
###
lassen_data <- filter(site_data,
                      Region == 'LA') %>% 
  mutate(tracking_residuals = NA)

lassen_data_H <- filter(lassen_data,
                        Era == 'H')
lassen_data_HM <- filter(lassen_data,
                         Era == 'HM')
#Save residuals of 1:1 line
lassen_residuals_H <- resid(lm(lassen_data_H$temp ~ lassen_data_H$CTI))
lassen_residuals_HM <- resid(lm(lassen_data_HM$temp ~ lassen_data_HM$CTI))

plot(lassen_data_H$CTI ~ lassen_data_H$temp)

ggplot() +
  geom_boxplot(aes(x = "Historical Residuals", y = lassen_residuals_H)) +
  geom_boxplot(aes(x = "Contemporary Residuals", y = lassen_residuals_HM))

lassen_residuals_H <- resid(lm(lassen_data_H$precip ~ lassen_data_H$CPI))
lassen_residuals_HM <- resid(lm(lassen_data_HM$precip ~ lassen_data_HM$CPI))

ggplot() +
  geom_boxplot(aes(x = "Historical Residuals", y = lassen_residuals_H)) +
  geom_boxplot(aes(x = "Contemporary Residuals", y = lassen_residuals_HM))

hist(lassen_residuals_H - lassen_residuals_HM)

####
#yosemite
###
yosemite_data <- filter(site_data,
                      Region == 'YO') %>% 
  mutate(tracking_residuals = NA)

yosemite_data_H <- filter(yosemite_data,
                        Era == 'H')
yosemite_data_HM <- filter(yosemite_data,
                         Era == 'HM')
#Save residuals of 1:1 line
yosemite_residuals_H <- resid(lm(yosemite_data_H$temp ~ yosemite_data_H$CTI))
yosemite_residuals_HM <- resid(lm(yosemite_data_HM$temp ~ yosemite_data_HM$CTI))

plot(yosemite_data_H$CTI ~ yosemite_data_H$temp)

ggplot() +
  geom_boxplot(aes(x = "Historical Residuals", y = yosemite_residuals_H)) +
  geom_boxplot(aes(x = "Contemporary Residuals", y = yosemite_residuals_HM))

yosemite_residuals_H <- resid(lm(yosemite_data_H$precip ~ yosemite_data_H$CPI))
yosemite_residuals_HM <- resid(lm(yosemite_data_HM$precip ~ yosemite_data_HM$CPI))

ggplot() +
  geom_boxplot(aes(x = "Historical Residuals", y = yosemite_residuals_H)) +
  geom_boxplot(aes(x = "Contemporary Residuals", y = yosemite_residuals_HM))

hist(yosemite_residuals_H-yosemite_residuals_HM)
####
#sequoia
###
sequoia_data <- filter(site_data,
                        Region == 'SS') %>% 
  mutate(tracking_residuals = NA)

sequoia_data_H <- filter(sequoia_data,
                          Era == 'H')
sequoia_data_HM <- filter(sequoia_data,
                           Era == 'HM')
#Save residuals of 1:1 line
sequoia_residuals_H <- resid(lm(sequoia_data_H$temp ~ sequoia_data_H$CTI))
sequoia_residuals_HM <- resid(lm(sequoia_data_HM$temp ~ sequoia_data_HM$CTI))

plot(sequoia_data_H$CTI ~ sequoia_data_H$temp)

ggplot() +
  geom_boxplot(aes(x = "Historical Residuals", y = sequoia_residuals_H)) +
  geom_boxplot(aes(x = "Contemporary Residuals", y = sequoia_residuals_HM))

sequoia_residuals_H <- resid(lm(sequoia_data_H$precip ~ sequoia_data_H$CPI))
sequoia_residuals_HM <- resid(lm(sequoia_data_HM$precip ~ sequoia_data_HM$CPI))

ggplot() +
  geom_boxplot(aes(x = "Historical Residuals", y = sequoia_residuals_H)) +
  geom_boxplot(aes(x = "Contemporary Residuals", y = sequoia_residuals_HM))

hist(sequoia_residuals_H - sequoia_residuals_HM)

####
#Climate tracking scatter plots of contemporary rates of climate change against rates of change in CCIs.
####

#Temperature

## Models
cti_lm_LA <- lm(cti_rate ~ temp_rate,
                data = filter(
                  site_data,
                  Region == 'LA' & Era == 'HM' 
                ))

cti_lm_YO <- lm(cti_rate ~ temp_rate,
                data = filter(
                  site_data,
                  Region == 'YO' & Era == 'HM' 
                ))

cti_lm_SS <- lm(cti_rate ~ temp_rate,
                data = filter(
                  site_data,
                  Region == 'SS' & Era == 'HM' 
                ))

cti_tracking <- ggplot(data = filter(site_data,
                     Era == 'HM')) +
  geom_point(aes(x = temp_rate,
                 y = cti_rate,
                 color = Region),
             alpha = 0.6) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = 2) +
  geom_abline(intercept = cti_lm_LA$coefficients[1],
              slope = cti_lm_LA$coefficients[2],
              linetype = 1,
              color = '#E41A1C') +
  geom_abline(intercept = cti_lm_YO$coefficients[1],
              slope = cti_lm_YO$coefficients[2],
              linetype = 1,
              color = '#377EB8') +
  geom_abline(intercept = cti_lm_SS$coefficients[1],
              slope = cti_lm_SS$coefficients[2],
              linetype = 1,
              color = '#4DAF4A') +
  labs(x = "Site Warming Rate (°C yr-1)",
       y = "Thermophilization Rate (°C yr-1)") +
  expand_limits(x = 0)



#Precipitation
cpi_lm_LA <- lm(cpi_rate ~ temp_rate,
                data = filter(
                  site_data,
                  Region == 'LA' & Era == 'HM' 
                ))

cpi_lm_YO <- lm(cpi_rate ~ temp_rate,
                data = filter(
                  site_data,
                  Region == 'YO' & Era == 'HM' 
                ))

cpi_lm_SS <- lm(cpi_rate ~ temp_rate,
                data = filter(
                  site_data,
                  Region == 'SS' & Era == 'HM' 
                ))

