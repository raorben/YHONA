# Script for summarizing YHONA cormorant data
library(googlesheets4)
library(dplyr)

# Read in data from Google Drive
cormdat <- read_sheet("https://docs.google.com/spreadsheets/d/1VAYO1ymj4uZ5lZL-S9KEfMNoW7QdFUAq8AKaVgPIsNg/edit?usp=drive_link")
obseffort <- read_sheet("https://docs.google.com/spreadsheets/d/1x9dOuMY67d4WeSX4ElHhGONZoTDf9yLdZ-y-3-_OFrk/edit?usp=drive_link")

# View data
View(cormdat)
View(obseffort)

# Convert to date formats
cormdat$first_obs_date <- as.POSIXct(cormdat$first_obs_date, format = "%Y-%M-%D", 
                                     options(digits.secs = 3), tz = "America/New_York")
cormdat$first_egg_obs <- as.POSIXct(cormdat$first_egg_obs, format = "%Y-%M-%D", 
                                    options(digits.secs = 3), tz = "America/New_York")
cormdat$chick_first_obs <- as.POSIXct(cormdat$chick_first_obs, format = "%Y-%M-%D", 
                                      options(digits.secs = 3), tz = "America/New_York")
cormdat$last_obs_date <- as.POSIXct(cormdat$last_obs_date, format = "%Y-%M-%D", 
                                    options(digits.secs = 3), tz = "America/New_York")
class(cormdat$first_obs_date)


obseffort$date <- as.POSIXct(obseffort$date, format = "%Y-%M-%D", 
                             options(digits.secs = 3), tz = "America/New_York")
obseffort$start <- as.POSIXct(obseffort$start, format = "%H:%M:%S", 
                              options(digits.secs = 3), tz = "America/New_York")
obseffort$end <- as.POSIXct(obseffort$end, format = "%H:%M:%S", 
                            options(digits.secs = 3), tz = "America/New_York")
obseffort$obs_hrs <- round(((difftime(obseffort$end, obseffort$start, units = "hours")) * (obseffort$num_obs)), digits=2)

###################################################################
# Summary of Monitoring Effort

# Calculate number of visits
obseffort %>% 
  count(year)

# Calculate number of observer-hours
obseffort %>%
  group_by(year) %>%
  summarise(effort = sum(obs_hrs))
# can write an if statement to add totals by cormorants vs murres



###################################################################
# Cormorant Summary Data
# Calculate mean clutch size
cormdat %>%
  group_by(year,species) %>%
  summarise(clutchsize = mean(num_eggs, na.rm=T))


# Calculate median hatch date
cormdat %>%
  group_by(year,species) %>%
  summarise(med_hatchdate = median(chick_first_obs, na.rm=T))


# Calculate hatch success
cormdat %>% 
  group_by(year,species) %>% 
  summarise(hatchsuccess = (num_hatched/num_eggs))


# Calculate number fledged

# Calculate fledging success

# Calculate productivity

# Histogram of cormorant clutch sizes

# Relationship between # eggs and # fledged by species