#### Make Over Monday 
## Load Libraries ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)

## Read Data ####
fp = "/mnt/r.makeOver.2016Apr03/data/Urban Diversity in America v6.csv"
df.raw = read.csv(file=fp, stringsAsFactors = FALSE)

## Data Manipulations ####

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
# Using "City"

### divide this range into 5 chunks ####
# as Integration has the lowest range (0.3), use it to segment the other two 
# intervalI has the chunk value, ranging from 1 to 5, set by labels
df.2 = df.raw
df.2$intervalI = cut(df.2$IntegrationI, breaks=5, include.lowest=TRUE, labels=c(1,2,3,4,5))

### for ggplot ####
# converting from wide to long format
df.3 = df.2 %>% gather(metric,value,3:4) #not 3:5 to exclude Integration which has been used in cut

### setting the labels for intervalI
# current value
levels(df.2$intervalI)

# the various ranges created by cut -- TOOTOO
unique(cut(df.2$IntegrationI, breaks=5, include.lowest=TRUE))

# setting the new value only for printing
my.labels = c("-19% to -13%", "-13% to -7%", "-7% to -1%",  "-1% to 5%",  "5% to 11%")

df.2$intervalI <- factor(
  df.2$intervalI, 
  labels=my.labels)


# Approach 2 ####
# Using "State"
# (refer to v2 for details)

## Visulization ####

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
  #geom_point(shape=19, color="#218C8D") +    # 1/4 opacity
  #geom_point(aes(color=intervalI), shape=19, size=3, position=position_jitter(w=0.2, h=0.2)) +    # 1/4 opacity
  geom_point(color="#218C8D", shape=19, size=3, position=position_jitter(w=0.2, h=0.2)) +    # 1/4 opacity
  #geom_jitter(width = 0.5, height = 0.5, size=0.5) +
  facet_wrap(~ intervalI)

g2 <- g1 +  
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Remove shaded confidence region
              fullrange=TRUE, #fill="antiquewhite2", alpha=1/3,
              linetype="dashed",color="darkred") + # Extend regression lines 
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent) #+
  scale_color_manual(labels=my.labels) # does not work

g3 <- g2 + 
  theme(panel.background=element_rect(fill="#efefef", color=NA)) + 
  theme(strip.background=element_rect(fill="#858585", color=NA)) + 
  theme(strip.text=element_text(family="OpenSans-CondensedBold", size=12, color="white", hjust=0.5)) + 
  theme(panel.margin.x=unit(1, "cm")) + 
  theme(panel.margin.y=unit(0.5, "cm")) + 
  theme(panel.grid.major.y=element_line(color="#b2b2b2")) +
  theme(axis.ticks = element_blank()) 


# Hide all the vertical gridlines
g4 <- g3 + theme(panel.grid.minor.x=element_blank(),
                 panel.grid.major.x=element_blank())

g4 <- g4 + theme(panel.grid.minor.y=element_blank())

g4 <- g4 + theme(plot.title = element_text(face="bold", color = "black", size=18))

g4 <- g4 + 
  ylab("Neighborhood Index")  +  # Set y-axis label
  xlab("City Index") +   # Set y-axis label
  ggtitle("Diversity Index") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

g5 <- g4 + 
  geom_text(data=df.cor, aes(x=0, y=3, label=V1)


# Approach 2b ####

# Good pointers about scatter plots
# http://www.sthda.com/english/wiki/print.php?id=188

p4 <- ggplot(df.4, aes(x=m.C, y=m.N)) + 
  #geom_point(shape=19, color="#218C8D") +    # 1/4 opacity
  geom_point(aes(color=intervalI), shape=19, size=3) +    # 1/4 opacity
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Add shaded confidence region
              fullrange=TRUE, 
              linetype="dashed",
              color="darkred", fill="antiquewhite2", alpha=1/3) # Extend regression lines
  
p5 <- p4 +  
  #facet_wrap(~ intervalI) + 
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent) +
  scale_color_manual(name="Integration Index", values=c("#F9E559", "#218C8D", "#6CCECB", "#EF7126", "#8EDC9D"))

p6 <- p5 + 
  theme(panel.background=element_rect(fill="#efefef", color=NA)) + 
  theme(strip.background=element_rect(fill="#858585", color=NA)) + 
  theme(strip.text=element_text(family="OpenSans-CondensedBold", size=12, color="white", hjust=0.5)) + 
  theme(panel.margin.x=unit(1, "cm")) + 
  theme(panel.margin.y=unit(0.5, "cm")) + 
  theme(panel.grid.major.y=element_line(color="#b2b2b2")) +
  theme(axis.ticks = element_blank()) 
  

# Hide all the vertical gridlines
p7 <- p6 + theme(panel.grid.minor.x=element_blank(),
                 panel.grid.major.x=element_blank())

p8 <- p7 + theme(panel.grid.minor.y=element_blank())

p9 <- p8 + theme(plot.title = element_text(face="bold", color = "black", size=18))

  
#p10 <- p9 + geom_rug()

p10 <- p9 + 
  ylab("Neighborhood Index")  +  # Set y-axis label
  xlab("City Index") +   # Set y-axis label
  ggtitle("Diversity Index") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  
# Set the "anchoring point" of the legend (bottom-left is 0,0; top-right is 1,1)
# Put bottom-left corner of legend box in bottom-left corner of graph
p11 <- p10 + theme(legend.justification=c(0,0), legend.position=c(0.1,0.003))
p11 <- p11 + theme(legend.background = element_rect(fill="#efefef", size=.5, linetype="dotted"))# Modifying the legend box
p11 <- p11 + theme(legend.direction = "horizontal")

p11