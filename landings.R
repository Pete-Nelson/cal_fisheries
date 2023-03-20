# purpose: pull in potential data sources from shared drives
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

# data read -----
cland <- readRDS("data/cland.rds")

# grouping var -----
test <- cland[1:500,c(7, 19, 21)] %>% mutate(SpeciesName = as.factor(SpeciesName))
test %>% 
  group_by(SpeciesName) %>% 
  # group landings data by SpeciesName
  mutate(grouping_var = 
           # add a grouping variable to the df where...
           ifelse(SpeciesName == "Jacksmelt", 
                  # if it's a jacksmelt
                  "Jacksmelt", 
                  # enter jacksmelt
                  "other fishes")) %>% 
  # otherwise enter other fishes
  filter(grouping_var != "other fishes")

cland %>% 
  group_by(year = year(LandingDate), SpeciesGroup) %>% 
  summarise(n = n()) %>% 
  print(n=20)

## add & select useful variables ----
cland <- 
  cland %>% 
  group_by(year = year(LandingDate), 
           SpeciesGroup) %>% 
  mutate(fishery =
           ifelse(SpeciesGroup == "RockCrab",
                  "rock crab",
                  "other"), .after = SpeciesGroup)

# this would probably work too
cland %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T),
           SpeciesGroup == "RockCrab") %>% 
  summarise(landings = n())


# seasonality ----
## all fisheries combined ----
### number of landings ----

N_land <- # create base plot
  cland %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(landings = n()) %>% 
  ggplot(aes(x = month, y = landings)) +
  geom_col()

# number of landings, facet on year
(N_land_fplot <-
    N_land +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Combined Fisheries", "number of landings"))

ggsave("figures/N_land_fplot.jpg")

### pounds landed ----

P_land <- # create base plot
  cland %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(pounds = sum(Pounds)/1000000) %>% 
  ggplot(aes(x = month, y = pounds)) +
  geom_col()

# pounds landed, facet on year
(P_land_fplot <-
    P_land +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Combined Fisheries", "total weight of landings") +
    ylab("pounds (in millions)"))

ggsave("figures/P_land_fplot.jpg")

### value landed ----

D_land <- # create base plot
  cland %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(revenue = sum(TotalPrice)/1000000) %>% 
  ggplot(aes(x = month, y = revenue)) +
  geom_col()

# value (ex-vessel, USD, no corrections), facet on year
(D_land_fplot <-
    D_land +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Combined Fisheries", "ex-vessel value (USD, not adjusted)") +
    ylab("revenue (x $1 million)"))

ggsave("figures/D_land_fplot.jpg")

## rc v other ----
# create stacked bargraph with rock crab on the bottom and all other fisheries on top
### number of landings ----

(N_rco_land <- # create base plot
   cland %>% 
   drop_na(fishery) %>% 
   group_by(year = year(LandingDate),
            month = month(LandingDate, label = T),
            fishery = fishery) %>% 
   summarise(landings = n()) %>% 
   ggplot(aes(x = month, y = landings, fill = fishery)) +
   geom_col() +
   scale_fill_manual(values = c("#9E9AC8", "#6A51A3")))

# number of landings, facet on year
(N_rco_land_fplot <-
    N_rco_land +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Rock Crab & Other Fisheries", "number of landings"))

ggsave("figures/N_rco_land_fplot.jpg")

### pounds landed ----

(P_rco_land <- # create base plot
   cland %>% 
   drop_na(fishery) %>% 
   group_by(year = year(LandingDate),
            month = month(LandingDate, label = T),
            fishery = fishery) %>% 
   summarise(pounds = sum(Pounds)/1000000) %>% 
   ggplot(aes(x = month, y = pounds, fill = fishery)) +
   geom_col() +
   scale_fill_manual(values = c("aquamarine2", "aquamarine4")))

# pounds landed, facet on year
(P_rco_land_fplot <-
    P_rco_land +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Rock Crab & Other Fisheries", "total weight of landings") +
    ylab("pounds (in millions)"))

ggsave("figures/P_rco_land_fplot.jpg")

## rock crab -----
# filter for rock crab landings only
cland %>% 
  filter(grepl("Crab", SpeciesName) & grepl("rock", SpeciesName)) %>% 
  # SpeciesName that contain "Crab" and "rock"!
  select(SpeciesName) %>% 
  group_by(SpeciesName) %>% 
  summarise(n = n())

# probably better to filter by SpeciesGroup
rc <- 
  cland %>% 
  filter(SpeciesGroup == "RockCrab")

### number of landings ----

N_rc <- # create base plot
  rc %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(landings = n()) %>% 
  ggplot(aes(x = month, y = landings)) +
  geom_col(fill = "green") 

# number of landings, facet on year
(N_rc_fplot <-
    N_rc +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Rock Crab only", "number of landings"))

ggsave("figures/N_rc_fplot.jpg")

### pounds landed ----

P_rc <- # create base plot
  rc %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(pounds = sum(Pounds)/100000) %>% 
  ggplot(aes(x = month, y = pounds)) +
  geom_col(fill = "green")

# pounds landed, facet on year
(P_rc_fplot <-
    P_rc +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Rock Crab only", "total weight of landings") +
    ylab("pounds (x 100,000)"))

ggsave("figures/P_rc_fplot.jpg")

### value landed ----

D_rc <- # create base plot
  rc %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(revenue = sum(TotalPrice)/100000) %>% 
  ggplot(aes(x = month, y = revenue)) +
  geom_col(fill = "green")

# value (ex-vessel, USD, no corrections), facet on year
(D_rc_fplot <- 
    D_rc +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Rock Crab only", "ex-vessel value (USD, not adjusted)") +
    ylab("revenue (x $100,000)"))

ggsave("figures/D_rc_fplot.jpg")

## rc alternatives -----
# filter for rock crab landings only
cland %>% 
  filter(!grepl("Crab", SpeciesName) & !grepl("rock", SpeciesName)) %>% 
  # SpeciesName that do NOT contain "Crab" and "rock"!
  select(SpeciesName) %>% 
  group_by(SpeciesName) %>% 
  summarise(n = n())

# better to filter by SpeciesGroup
nrc <- 
  cland %>% 
  filter(SpeciesGroup != "RockCrab")

### number of landings ----

N_nrc <- # create base plot
  nrc %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(landings = n()) %>% 
  ggplot(aes(x = month, y = landings)) +
  geom_col(fill = "blue")

# number of landings, facet on year
(N_nrc_fplot <-
    N_nrc +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Non-Rock Crab Fisheries", "number of landings"))

ggsave("figures/N_nrc_fplot.jpg")

### pounds landed ----

P_nrc <- # create base plot
  nrc %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(pounds = sum(Pounds)/1000000) %>% 
  ggplot(aes(x = month, y = pounds)) +
  geom_col(fill = "blue")

# pounds landed, facet on year
(P_nrc_fplot <- 
    P_nrc +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Non-Rock Crab Fisheries", "total weight of landings") +
    ylab("pounds (in millions)"))

ggsave("figures/P_nrc_fplot.jpg")

### value landed ----

D_nrc <- # create base plot
  nrc %>% 
  group_by(year = year(LandingDate),
           month = month(LandingDate, label = T)) %>% 
  summarise(revenue = sum(TotalPrice)/1000000) %>% 
  ggplot(aes(x = month, y = revenue)) +
  geom_col(fill = "blue")

# value (ex-vessel, USD, no corrections), facet on year
(D_nrc_fplot <- 
    D_nrc +
    facet_wrap( ~ year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggtitle("Non-Rock Crab Fisheries", "ex-vessel value (USD, not adjusted)") +
    ylab("revenue (x $1 million)"))

ggsave("figures/D_nrc_fplot.jpg")

# data find -----
data <- read_csv("G:/Shared drives/CrabHAB SE Project 2021-/Data/Quantitative/CDFW Cleaned/cland_RockCrab/Landings_RCPermits.csv")
# data description from Matt's ReadMe, "“Landings_RCPermits.csv”: Contains the landings of all fisherfolk that had Rock Crab permits in the same year as the landing. Please note that this includes catch that are not rock crabs. We can use this to look at what proportion of their catch sold/profit is from rock crab. This csv is derived from “Landing_all.csv” and “cfpermRCrab_workingfile_CrabHAB.csv” using “Landings_RockCrab.R”" GREAT!
names(data)
glimpse(data)
range(data$Year) # 2005-2021; for comparison's sake, would be nice to have from 1998
range(data$CFLYear) # 2005-2021

# looks like some duplicative variables (eg LandingDate; Year/year; etc), some useless/extraneous/unknown purpose (eg V1, CDFWBlockID), many better in other format (eg GearGroup as factor)

# QAQC -----
## date ----
# LandingDate2 is suspect (no landings Jan-Mar 2005); try formatting LandingDate
test <- data[1:500,] %>% 
  mutate(LandingDate = mdy(LandingDate))
data <- data %>% 
  mutate(LandingDate = mdy(LandingDate)) # 2005 seems to be a partial year; commercial fish licensing year?!

## revenue ----
# Weird data gaps aren't always (?) consistent. See for example Jan-Mar 2007 non-rock crab fisheries: Number of landings each month is ca 1000 and weight is >1 million lbs, but revenue appears to be zero. Scale issue?
data %>% 
  filter(year(LandingDate) == "2007" & 
           month(LandingDate) < 4) %>% 
  group_by(month = month(LandingDate, label = T)) %>% 
  summarise(landings = n(),
            pounds = sum(Pounds),
            value = sum(TotalPrice)) %>% 
  write_csv("figures/missing 2007 data.csv")
# Not a scale issue and not limited to non-rock crab fisheries. Seems to be problems with how (or if) unit price was included

data %>% 
  filter(year(LandingDate) == "2007" & 
           month(LandingDate) < 4) %>% 
  group_by(month = month(LandingDate, label = T)) %>% 
  summarise(landings = n(),
            pounds = sum(Pounds),
            price = mean(UnitPrice, na.rm = T),
            value = sum(TotalPrice))
### NOTE ----
# revenue data are too wonky to use or trust!

## whitespace ----
# remove whitespace in SpeciesName ( same issue with others, no doubt, but not tackled here)
test$SpeciesName <- str_squish(test$SpeciesName)
levels(as.factor(test$SpeciesName))
test %>% filter(SpeciesName == "Anchovy northern") %>% select(SpeciesName)
data$SpeciesName <- str_squish(data$SpeciesName)

saveRDS(data, file = "data/data.rds")

# backup df ----
# use 'data' as the backup dataframe!
cland <- data

saveRDS(cland, file = "data/cland.rds")

# SCRATCH #########################################################
levels(as.factor(cland$SpeciesName))
cland %>% 
  filter(SpeciesName == "Crab brown rock") # req str_squish(), removes whitespace

# From CP: "For all fish bus id with (spp group) rock crab (gear group) trap receipts from permitted rock crab fishermen in year x, how many landings & pounds of all other spp (including rock crab caught incidentally by others) did they receive at CA ports  in year x?"
cland %>% 
  filter(SpeciesGroup != "RockCrab" & GearGroup == "Trap") %>% 
  group_by(year(LandingDate), month(LandingDate),
           BusinessID, SpeciesGroup) %>% 
  summarise(pounds = sum(Pounds),
            n = n()) %>% 
  print(n = 30)


