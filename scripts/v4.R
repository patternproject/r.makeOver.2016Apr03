#### Make Over Monday 

## `````````````````````````````````````````````
## Load Libraries ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
## `````````````````````````````````````````````

## `````````````````````````````````````````````
## Read Data ####
fp = "/mnt/r.makeOver.2016Apr03/data/Urban Diversity in America v6.csv"
df.raw = read.csv(file=fp, stringsAsFactors = FALSE)
## `````````````````````````````````````````````

## `````````````````````````````````````````````
## Data Manipulations ####
## `````````````````````````````````````````````

### Fixing Names ####
names(df.raw)[3] = "CityI"
names(df.raw)[4] = "NeighborI"
names(df.raw)[5] = "IntegrationI"

# Out of the three 
# "Integration" has the smallest range
df.raw %>%
  summarize(minC = min(CityI), maxC = max(CityI), rngC = maxC-minC,
            minN = min(NeighborI), maxN = max(NeighborI), rngN = maxN-minN,
            minI = min(IntegrationI), maxI = max(IntegrationI), rngI = maxI-minI,
            minRngAll = min(rngC,rngN,rngI)) 

# Approach 1 ####
# Using "City" (Approach 2 uses States)

### divide this range into 5 chunks ####
# as Integration has the lowest range (0.3), use it to segment the other two 
# intervalI has the chunk value, ranging from 1 to 5, set by labels
df.2 = df.raw
df.2$intervalI = cut(df.2$IntegrationI, breaks=5, include.lowest=TRUE, labels=c(1,2,3,4,5))

### for ggplot ####
# converting from wide to long format
df.3 = df.2 %>% gather(metric,value,3:4) #not 3:5 to exclude Integration which has been used in cut

### setting the labels for intervalI ####
# current value
levels(df.2$intervalI)

# the various ranges created by cut 
unique(cut(df.2$IntegrationI, breaks=5, include.lowest=TRUE))

# setting the new value only for printing
my.labels = c("-19% to -13%", "-13% to -7%", "-7% to -1%",  "-1% to 5%",  "5% to 11%")

df.2$intervalI <- factor(
  df.2$intervalI, 
  labels=my.labels)

# Approach 2 ####
# Using "State"
# (refer to v2 for details)

## `````````````````````````````````````````````
## Visulization ####
## `````````````````````````````````````````````

# Approach 1b (1) ####
### City Data, Bar Plot

# draw a bar plot for CityI and NeighborhoodI, faceting on IntegrationI
p1 <- ggplot(df.3, aes(x=City,y=value,fill=metric)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip()

p2 <- p1 + 
  facet_wrap(~ intervalI)

p3 <- p2 + 
  labs(x=NULL, y=NULL, title="Temp")

# Approach 1b (2) ####
### City Data, Scatter Plot

g1 <- ggplot(df.2, aes(x=CityI, y=NeighborI)) + 
  geom_point(color="#218C8D", shape=19, size=3, position=position_jitter(w=0.2, h=0.2)) +    # 1/4 opacity
  facet_wrap(~ intervalI)

g2 <- g1 +  
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Remove shaded confidence region
              fullrange=TRUE, #fill="antiquewhite2", alpha=1/3,
              linetype="dashed",color="darkred") + # Extend regression lines 
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent) #+
  scale_color_manual(labels=my.labels) # does not work

### Panel / Facet Settings
g3 <- g2 + 
  theme(panel.background=element_rect(fill="#efefef", color=NA)) + 
  theme(strip.background=element_rect(fill="#858585", color=NA)) + 
  theme(strip.text=element_text(family="OpenSans-CondensedBold", size=12, color="white", hjust=0.5)) + 
  theme(panel.margin.x=unit(1, "cm")) + 
  theme(panel.margin.y=unit(0.5, "cm")) + 
  theme(panel.grid.major.y=element_line(color="#b2b2b2")) +
  theme(axis.ticks = element_blank()) 

### Hide all the vertical gridlines
g4 <- g3 + theme(panel.grid.minor.x=element_blank(),
                 panel.grid.major.x=element_blank())

g4 <- g4 + theme(panel.grid.minor.y=element_blank())

g4 <- g4 + theme(plot.title = element_text(face="bold", color = "black", size=18))

### Setting the Labels, sub labels
g4 <- g4 + 
  ylab("Neighborhood Index")  +  # Set y-axis label
  xlab("City Index") +   # Set y-axis label
  ggtitle(expression(atop(bold("Diversity Index"), atop(italic("Each Facet contains a subset of Integration Index"), "")))) +
  #ggtitle("Diversity Index") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

### Adding some text to the graphs
# to get the counts of values in each interval
df.temp = as.data.frame(table(df.2$intervalI))

df.text = data.frame(x=0.1, y=0.6, textLabel=df.temp$Freq, intervalI=my.labels)

# printing the count of values in each interval
g5 <- g4 + 
  geom_text(data=df.text, aes(x,y,label=textLabel), inherit.aes=FALSE)

# setting common text
g5 + annotate("text", label = "Data Points: ", size = 4, x = 0.2, y = 0.75)


### Plot Done ####
g5
