### Save the code and the data files to a local directory in your computer.
### Set the working directory to the directory where the code and data files are located.
setwd("~/Rejoice/Academics/PhD/Terms/Spring 2021/Advanced Data Visualization/Final Project/Arrests")

### Loading necessary libraries

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(ggalt)

### Reading data. Ensure the files are in the same working directory as the code.

d.raw<-read.csv("Use_Of_Force.csv")
pop<-read.csv("pop.csv")
colnames(pop)[1]="Race"

#Extracting year
d.raw$date_time <- as.POSIXct(d.raw$Occured_date_time, format = "%m/%d/%Y %H:%M:%S")
d.raw$year<-format(d.raw$date_time, format="%Y")
d.raw$year.num<-as.numeric(format(d.raw$date_time, format="%Y"))

#Extracting date
d.raw$date<-format(d.raw$date_time, format="%m/%d/%Y")
d.raw$date.sub<-as.numeric(format(d.raw$date_time, format="%d"))

#Extracting hour
d.raw$hour<-as.numeric(format(as.POSIXct(d.raw$Occured_date_time, format = "%m/%d/%Y %I:%M:%S %p"), format="%H"))

#Extracting month
d.raw$month<-as.numeric(format(d.raw$date_time, format="%m"))

#Checking if subject id's are unique
nrow(d.raw)==length(unique(d.raw$Subject_ID))

#Subsetting the dataset to remove duplicate inidents for the same person on the same day
d<-d.raw%>% distinct(Subject_ID,date, .keep_all= TRUE)

d$year.f<-factor(d$year)

d.count.yr<-count(d, year.f, Subject_Race)

yr<-seq(2014,2021,1)
ttl<-c()

for (x in 1:length(yr)) {
  ttl[x]<-sum(d.count.yr$n[d.count.yr$year.f==as.character(yr[x])])
}

d.sub<-data.frame(yr,ttl)
d.sub$yr.f<-factor(d.sub$yr)
d.count.yr<-cbind(d.count.yr,d.sub[match(d.count.yr$year.f, d.sub$yr.f),2])
names(d.count.yr)[4] <- "total"

pop$racyr<-with(pop, paste0(Race, Year))
d.count.yr$racyr<-with(d.count.yr, paste0(Subject_Race, year.f))
d.count.yr<-cbind(d.count.yr,pop[match(d.count.yr$racyr, pop$racyr),3])
colnames(d.count.yr)[6] = "Population"

d.count.yr.sub <- d.count.yr %>% drop_na()

### Normalizing count based on population

d.count.yr.sub$n.norm <- c()
for (i in 1:nrow(d.count.yr.sub)) {
  d.count.yr.sub$n.norm[i] <- round((500000 * d.count.yr.sub$n[i])/d.count.yr.sub$Population[i])
}

d.count.yr.sub$racyr<-NULL

### Prepping for the first graph

prop2019<-d.count.yr.sub[d.count.yr.sub$year.f=="2019",]
rownames(prop2019)<-NULL
prop2019$n.pct<-round((prop2019$n/sum(prop2019$n))*100,0)
prop2019$n.pct[3]=prop2019$n.pct[3]+1
prop2019$n.pct[6]=prop2019$n.pct[6]+1
prop2019$pop.pct<-round((prop2019$Population/sum(prop2019$Population))*100,0)

prop2019.sub <- data.frame("race" = c(prop2019$Subject_Race, prop2019$Subject_Race),
                           "pct" = c(prop2019$pop.pct, prop2019$n.pct))
prop2019.sub$x<-c(rep(1,6), rep(12,6))

### Creating the first graph

ggplot(prop2019.sub) +
  geom_area(aes(x=x, y=pct, fill=race),
            size=1, colour="black") +
  scale_fill_manual(values = brewer.pal(6, "BrBG"),
                    guide = guide_legend(title = "Race", reverse = TRUE)) +
  coord_flip() +
  xlim(-0.25,12.5) +
  geom_text(x = 0.75, y = 33, label = "66%") +
  geom_text(x = 0.75, y = 67, label = "0%") +
  geom_text(x = 0.75, y = 71, label = "7%") +
  geom_text(x = 0.75, y = 78, label = "8%") +
  geom_text(x = 0.75, y = 91, label = "18%") +
  geom_text(x = 0.75, y = 101, label = "1%") +
  geom_text(x = 0.25, y = 51, label = "% share in population", size = 5) +
  geom_text(x = 12.75, y = 51, label = "% share in use of force", size = 5) +
  geom_text(x = 12.25, y = 26, label = "53%") +
  geom_text(x = 12.25, y = 54.5, label = "1%") +
  geom_text(x = 12.25, y = 58.5, label = "5%") +
  geom_text(x = 12.25, y = 78, label = "36%") +
  geom_text(x = 12.25, y = 98.5, label = "4%") +
  geom_text(x = 12.25, y = 101, label = "1%") +
  labs(title = "Comparison of use of force by Seattle police with population share in 2019") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=15, hjust = 0.5, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

###################################################################

### Prepping for the second graph

chng <- spread(subset(d.count.yr.sub, select=c(year.f,Subject_Race, n.norm)), year.f, n.norm)
colnames(chng) <- c("Race", "y2015", "y2016", "y2017", "y2018", "y2019")

chng$diff<-chng$y2015-chng$y2019
chng$pct<-round((chng$diff/chng$y2015)*100,0)

box<-data.frame('x'=c(2950, 5250, 5250, 2950),
                'xend'=c(5250, 5250, 2950, 2950),
                'y'=c('White', 'White', 'Hispanic or Latino', 'Hispanic or Latino'),
                'yend'=c('White','Hispanic or Latino', 'Hispanic or Latino', 'White'))

### Creating the second graph

ggplot() +
  geom_segment(data=chng, aes(y = reorder(Race, y2019),
                         x = y2015,
                         xend = y2019,
                         yend = reorder(Race, y2015)),
               arrow = arrow(length = unit(0.5, "cm")),
               size = 2, color = "grey") +
  geom_dumbbell(data=chng, 
                aes(y = reorder(Race, y2019),
                    x = y2015,
                    xend = y2019),
                size = 2,
                size_x = 4,
                size_xend = 4,
                colour = "grey",
                colour_x = "blue",
                colour_xend = "darkred") +
  geom_text(aes(x=2025, y= "Black or African American", label="2019"),
            color="darkred", hjust=1, size=4) +
  geom_text(aes(x=5100, y= "Black or African American", label="2015"),
            color="blue", hjust=1, size=4) +
  geom_text(aes(x=3600, y= "Black or African American", label="2819 cases (- 58%)"),
            color="black", vjust=1, nudge_y = 0.20, size=4) +
  geom_text(aes(x=1750, y= "American Indian/Alaska Native", label="1703 cases (- 65%)"),
            color="black", vjust=1, nudge_y = 0.20, size=4) +
  geom_text(aes(x=1800, y= "Nat Hawaiian/Oth Pac Islander", label="1845 cases (- 68%)"),
            color="black", vjust=1, nudge_y = 0.20, size=4) +
  geom_text(aes(x=500, y= "White", label="233 cases (- 39%)"),
            color="black", vjust=1, nudge_y = 0.25, size=4) +
  geom_text(aes(x=700, y= "Hispanic or Latino", label="602 cases (- 63%)"),
            color="black", vjust=1, nudge_y = 0.25, size=4) +
  geom_text(aes(x=250, y= "Asian", label="168 cases (- 60%)"),
            color="black", vjust=1, nudge_y = 0.25, size=4) +
  geom_text(aes(x=4000, y= "Hispanic or Latino", label="They don't represent the actual number of cases."),
            color="black", vjust=1, nudge_y = 0.35, nudge_x = -25, size=4.5) +
  geom_text(aes(x=4000, y= "Hispanic or Latino", label="The number of cases has been normalized with population."),
            color="black", vjust=-1, nudge_y = 0.35, nudge_x = 150, size=4.5) +
  geom_text(aes(x=4000, y= "Hispanic or Latino", label="Note:"),
            color="black", vjust=-2, nudge_y = 0.43, nudge_x = -900, size=4.5) +
  geom_path(data = box, aes(x = x, y = y), color = "black") +
  geom_segment(data = box, aes(x = x, xend = x, y = y, yend = yend), color = "black") +
  labs(title = "5-year difference in use of force cases by Seattle police",
       subtitle = "2015 to 2019",
       x = "Number of Use of force cases",
       y = "") +
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size=12, color = "black"),
        axis.title.x = element_text(size=12, color = "black"))


######################################################################