#### Make Over Monday 
## Load Libraries ####
library(dplyr)
library(ggplot2)

## Read Data ####
fp = "/mnt/2016 Apr 03/Data/Urban Diversity in America v6.csv"
df.raw = read.csv(file=fp, stringsAsFactors = FALSE)

## Data Manipulations ####
names(df.raw)[3] = "CityI"
names(df.raw)[4] = "NeighborI"
names(df.raw)[5] = "IntegrationI"

df.raw %>%
  summarize(minC = min(CityI), maxC = max(CityI), rngC = maxC-minC,
            minN = min(NeighborI), maxN = max(NeighborI), rngN = maxN-minN,
            minI = min(IntegrationI), maxI = max(IntegrationI), rngI = maxI-minI,
            minRngAll = min(rngC,rngN,rngI)) 

## Visulization ####