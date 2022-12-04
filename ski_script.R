## ----LOAD ZONE---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(zoo)
library(stargazer)
library(lmtest)
library(olsrr)
library(car)
library(sandwich)


## ----GEOMETRY DATA-----------------------------------------------------------
# import data
loc <- read_csv("safegraph_geometry/your_data_apr_04_2022_0220pm.csv")

# filter to Vail and separate out lat and long
loc.vail <- loc %>%
  filter(location_name == "Vail Mountain Resort") %>%
  mutate(clean = str_sub(polygon_wkt, 11, -3)) %>%
  separate(clean, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                    "11","12", "13", "14"), ",") %>%
  pivot_longer(cols = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                        "11", "12", "13", "14"), 
               names_to = "index", 
               values_to = "points")
loc.vail$points <- str_trim(loc.vail$points)
loc.vail <- loc.vail %>%
  separate(points, c("long", "lat"), " ")

# export to csv
write_csv(loc.vail, "loc_vail.csv")


## ----SNOTEL DATA-------------------------------------------------------------
# import data and join
snow_2018 <- read_csv("SNOTEL/842_STAND_YEAR=2018.csv")
snow_2019 <- read_csv("SNOTEL/842_STAND_YEAR=2019.csv")
snow_2020 <- read_csv("SNOTEL/842_STAND_YEAR=2020.csv")

# merge data frames
snow <- rbind(snow_2018, snow_2019, snow_2020)

# rename columns
snow <- rename(snow, depth = `SNWD.I-1 (in)`)
snow <- rename(snow, temp = `TOBS.I-1 (degF)`)
snow <- rename(snow, date = Date)

# add "change" column
snow <- snow %>%
  mutate(change = depth - lag(depth))

# lubridate
snow$date <- mdy(snow$date)

# narrow to ski season ski seasons
snow <- filter(snow, date >= "2018-11-14" & date <= "2019-04-24" | date >= "2019-11-15" & date <= "2020-03-14")

# add depth squared column
snow <- mutate(snow, depth_sq = depth ^ 2)


## ----PATTERNS DATA-----------------------------------------------------------
# import data
visits_raw <- read_csv("safegraph4-6-22/colorado_2018-2022.csv")

# filter to vail ski resort and organize by time
visits <- visits_raw %>%
  filter(location_name == "Vail Mountain Resort") %>%
  arrange(date_range_start)

# expand to daily visits
visits <- visits %>%
  mutate(clean = str_sub(visits_by_day, 2, -2)) %>%
  separate(clean, c("1", "2", "3", "4", "5", "6", "7"), ",") %>%
  pivot_longer(cols = c("1", "2", "3", "4", "5", "6", "7"), 
               names_to = "day_index", 
               values_to = "visits") 

# convert day_index to numeric
visits$day_index <- as.numeric(visits$day_index)
visits$date <- visits$date_range_start + days(visits$day_index - 1)
visits$day_index <- factor(visits$day_index)

# take out time part of date
# OBSERVATIONS START AT 7 AM EACH DAY
visits$date <- as.Date(ymd_hms(visits$date))

# filter for open days and select needed columns
# 2018-2019: Nov. 14 - Apr. 24
# 2019-2020: Nov. 15 - Mar. 14

visits <- visits %>%
  filter(date >= "2018-11-14" & date <= "2019-04-24" | date >= "2019-11-15" & date <= "2020-03-14") %>%
  select(date, day_index, visits, normalized_visits_by_state_scaling) %>%
  arrange(date)

# add binary indicator for weekend
visits <- visits %>%
  mutate(weekend = ifelse(day_index == 6 | day_index == 7, TRUE, FALSE))

# Add variable for Denver public school calendar (holidays)
visits <- visits %>%
  mutate(holiday = ifelse((date >= "2018-11-19" & date <= "2018-11-23") | 
                          (date >= "2018-12-24" & date <= "2019-01-04") |
                          (date >= "2019-03-25" & date <= "2019-03-29") |
                          (date == "2019-01-21") |
                          (date == "2019-02-18") |
                          (date >= "2019-11-25" & date <= "2019-11-29") | 
                          (date >= "2019-12-23" & date <= "2020-01-03") |
                          (date >= "2020-03-30" & date <= "2020-04-03") |
                          (date == "2020-01-20") |
                          (date == "2020-02-17"), TRUE, FALSE))

# Add dummy variable for month
visits <- visits %>%
  mutate(month = 
          ifelse(date >= "2018-11-01" & date < "2018-11-30" | date >= "2019-11-01" & date < "2019-11-30", "November", 
            ifelse(date >= "2018-11-30" & date < "2018-12-31" | date >= "2019-11-30" & date < "2019-12-31", "December",
              ifelse(date >= "2018-12-31" & date < "2019-01-31" | date >= "2019-12-31" & date < "2020-01-31", "January",
                ifelse(date >= "2019-01-31" & date < "2019-02-28" | date >= "2020-01-31" & date < "2020-02-28", "February",
                  ifelse(date >= "2019-02-28" & date < "2019-03-31" | date >= "2020-02-28" & date < "2020-03-31", "March",
                    "April"))))))

# log of visits
visits$visits <- as.numeric(visits$visits)
visits$visits_log <- log(visits$visits)

# add indicator for season
visits <- visits %>%
  mutate(season = ifelse(date >= "2018-11-14" & date <= "2019-04-24", "2018/2019", "2019/2020"))


## ----PLOT VISITS-------------------------------------------------------------
# visits 2018/2019
visits %>%
  filter(date >= "2018-11-14" & date <= "2019-04-24") %>%
  ggplot(aes(x = date, y = visits)) +
    geom_point(aes(colour = weekend)) +
    geom_line() +
    scale_color_manual(values = c("black", "red")) +
    xlab("Date") +
    ylab("Safe Graph visits") +
    annotate("rect", xmin = as.Date("2018-11-19"), xmax = as.Date("2018-11-23"),
             ymin = 0, ymax = 8000, alpha = .5) +
    annotate("rect", xmin = as.Date("2018-12-24"), xmax = as.Date("2019-01-04"),
             ymin = 0, ymax = 8000, alpha = .5) +
    annotate("rect", xmin = as.Date("2019-03-25"), xmax = as.Date("2019-03-29"),
             ymin = 0, ymax = 8000, alpha = .5)

# visits 2019/2020
visits %>%
  filter(date >= "2019-11-15" & date <= "2020-03-14") %>%
  ggplot(aes(x = date, y = visits)) +
    geom_point(aes(colour = weekend)) +
    geom_line() +
    scale_color_manual(values = c("black", "red")) +
    xlab("Date") +
    ylab("Safe Graph visits") +
    annotate("rect", xmin = as.Date("2019-11-25"), xmax = as.Date("2019-11-29"),
             ymin = 0, ymax = 8000, alpha = .5) +
    annotate("rect", xmin = as.Date("2019-12-23"), xmax = as.Date("2020-01-03"),
             ymin = 0, ymax = 8000, alpha = .5)
  

## ----COMBINE DATASETS--------------------------------------------------------
# combine datasets and export as new file
df <- full_join(visits, snow, by = "date")
df <- mutate(df, depth_log = log(depth))
write_csv(df, "data_clean.csv")

# this is a good point to visually explore relationships
ggplot(data = df, aes(x = temp, y = visits)) +
  geom_point()

# Graph depth/temp


## ----MODEL SELECTION---------------------------------------------------------
# stepwise: first round
mod_1 <- lm(df$visits ~ df$change)
mod_2 <- lm(df$visits ~ df$depth)
mod_3 <- lm(df$visits ~ df$temp)
# depth was chosen

# stepwise: second round
mod_4 <- lm(df$visits ~ df$depth + df$temp)
mod_5 <- lm(df$visits ~ df$depth + df$change)
# temp was added

# stepwise: third round 
mod_6 <- lm(df$visits ~ df$depth + df$temp + df$change)
# change (temporarily) eliminated

# stepwise: fourth round
mod_7 <- lm(df$visits ~ df$depth + df$temp + df$month)
mod_8 <- lm(df$visits ~ df$depth + df$temp + df$day_index)
mod_9 <- lm(df$visits ~ df$depth + df$temp + df$holiday)
# inclusion of month lowered effect of temp

# two models - one with TEMP and one with MONTH
mod_10 <- lm(df$visits ~ df$depth + df$temp + df$day_index + df$holiday)
mod_11 <- lm(df$visits ~ df$depth + df$month + df$day_index + df$holiday)
# mod_11 has much higher r^2

# model without logged visits
mod_12 <- lm(df$visits ~ df$depth + df$month + df$day_index + df$holiday)

# model with depth and visits logged
mod_13 <- lm(df$visits_log ~ df$depth_log + df$month + df$day_index + df$holiday)

# add interaction term
mod_14 <- lm(df$visits_log ~ df$depth_log + df$month + df$day_index + df$holiday + df$day_index * df$holiday)

# add everything
mod_15 <- lm(df$visits_log ~ df$depth_log + df$temp + df$change + df$month + df$day_index + df$holiday + df$day_index * df$holiday)

# with change
mod_16 <- lm(df$visits_log ~ df$depth_log + df$change + df$month + df$day_index + df$holiday)
# add newey-west
coeftest(mod_16, vcov = NeweyWest)

# without change
mod_17 <- lm(df$visits_log ~ df$depth_log  + df$month + df$day_index + df$holiday)
# add newey-west
coeftest(mod_17, vcov = NeweyWest)


## ----TEST FOR OLS ASSUMPTIONS------------------------------------------------
# linearity
# general plot
ggplot(data = df, aes(x = depth, y = visits)) +
  geom_point(aes(color = holiday))

ggplot(data = df, aes(x = depth_log, y = visits_log)) +
  geom_point(aes(color = holiday))
# plot of residuals
plot("X")
# holiday observations skew it a bit, but generally depth has a positive linear relationship with visits

# Goldfeld-Quandt test for heteroskedasticity
gqtest("X", order.by = ~ depth + month + day_index + holiday, data = df, fraction = 50)
# no heteroskedasticity for logged model, yes heteroskedasticity for non-logged model

# Breusch-Godfrey test for autocorrelation
bgtest("X", order = 3, data = df)
# there is auto-correlation, as can be expected with a time series. Cannot rely on standard errors
# Obtain Newey-West corrected errors
coeftest("X", vcov = NeweyWest)

# test for normality of errors using shapiro-wilks
ols_test_normality("X")
# normal errors

# multi-colinearity

# exogeneity
# this is why I excluded "change": effect of "change" was probably an effect of road closures or something else.


## ----REGRESSION OUTPUT-------------------------------------------------------
# Add Newey West correction
coeftest(mod_13, vcov = NeweyWest)
summary(mod_13)

# Test for snowfall on weekday, non-holiday days
nohol_week <- df %>%
  filter(holiday == FALSE & weekend == FALSE)
mod_16 <- lm(nohol_week$visits_log ~ nohol_week$depth_log + nohol_week$change + nohol_week$month + nohol_week$day_index)
summary(mod_16)


## ----BIBLIOGRAPHY------------------------------------------------------------
# siting packages
citation("dplyr")


