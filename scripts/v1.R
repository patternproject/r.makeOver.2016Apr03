#### Make Over Monday 
## Load Libraries ####
library(dplyr)
library(ggplot2)
library(tidyr)

## Read Data ####
fp = "/mnt/r.makeOver.2016Apr03/data/Urban Diversity in America v6.csv"
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

df.1 = df.raw
# as Integration has the lowest range (0.3), use it to segment the other two 
# divide this range into 5 chunks

# intI has the chunk value, ranging from 1 to 5
df.1$intervalI = cut(df.1$IntegrationI, breaks=5, include.lowest=TRUE, labels=c(1,2,3,4,5))

df.2 <- df.1 %>% gather(metric, value, 3:6)

## Visulization ####
# draw a bar plot for CityI and NeighborhoodI, faceting on IntegrationI
