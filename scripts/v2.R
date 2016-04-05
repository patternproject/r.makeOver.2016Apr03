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
names(df.raw)[3] = "CityI"
names(df.raw)[4] = "NeighborI"
names(df.raw)[5] = "IntegrationI"

df.raw %>%
  summarize(minC = min(CityI), maxC = max(CityI), rngC = maxC-minC,
            minN = min(NeighborI), maxN = max(NeighborI), rngN = maxN-minN,
            minI = min(IntegrationI), maxI = max(IntegrationI), rngI = maxI-minI,
            minRngAll = min(rngC,rngN,rngI)) 

# Approach 1 ####

# Using "City"

# for ggplot
# df.1 <- df.raw %>% gather(metric, value, 3:5)

# as Integration has the lowest range (0.3), use it to segment the other two 
# divide this range into 5 chunks
# intI has the chunk value, ranging from 1 to 5
df.2 = df.raw
df.2$intervalI = cut(df.2$IntegrationI, breaks=5, include.lowest=TRUE, labels=c(1,2,3,4,5))

# for ggplot
df.3 = df.2 %>% gather(metric,value,3:4) #not 3:5 to exclude Integration which has been used in cut

# Approach 2 ####
# Using "State"

df.4 <- df.raw %>% 
  group_by(State) %>%
  summarize(m.C = mean(CityI), m.N = mean(NeighborI), m.I = mean(IntegrationI)) 

# Integration has the min range
df.5 <- df.4 %>%
  summarize(rng.C = max(m.C) - min(m.C),
         rng.N = max(m.N) - min(m.N),
         rng.I = max(m.I) - min(m.I))

df.4$intervalI = cut(df.4$m.I, breaks=5, include.lowest=TRUE, labels=c(1,2,3,4,5))

levels(df.4$intervalI) = unique(cut(df.4$m.I, breaks=5, include.lowest=TRUE))


## Visulization ####

# Approach 1b ####

# draw a bar plot for CityI and NeighborhoodI, faceting on IntegrationI
p1 <- ggplot(df.3, aes(x=City,y=value,fill=metric)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip()

p2 <- p1 + 
  facet_wrap(~ intervalI)


p3 <- p2 + 
  labs(x=NULL, y=NULL, title="Temp")

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