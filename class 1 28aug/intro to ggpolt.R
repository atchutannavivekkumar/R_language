# loading packages

library(tidyverse)

library(MASS)
source('hw.R')

data("faithful")
head(faithful)
str(faithful)

#A scatter plot (also called a scatterplot, scatter graph, scatter chart, scattergram, 
#or scatter diagram) is a type of plot or mathematical diagram using Cartesian
#coordinates to display values for typically two variables for a set of data. 
#If the points are coded (color/shape/size), additional variables can be displayed. 
#The data are displayed as a collection of points, each having the value of one 
#variable determining the position on the horizontal axis and the value of the 
#other variable determining the position on the vertical axis.
library(tidyverse) 

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point()

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape =20,size =5,colour ="black")

ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape =21 ,fill ="red",size =5,colour ="black")


ggplot(faithful,aes(x=waiting,y=eruptions)) +
  geom_point(shape=21, size=2, fill="green", color="black")+
  geom_smooth(method=loess, linewidth=1.2)  +
  labs(title = "Old Faithful Geyser Eruptions",
       x="Waiting Time Between Eruptions in Minutes",
       y="Eruption Duration in Minutes")+hw_theme

#HISTROGRAM
ggplot(faithful,aes(x=waiting)) +
  geom_histogram()+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Count",
       title="Old Faithful Geyser Eruptions") + hw_theme

ggplot(faithful,aes(x=waiting)) +
  geom_histogram(binwidth=4,
                 fill="cornsilk",color="black")+
  labs(x="Waiting Time Between Eruptions In Minutes",
       y="Counts",
       title="Old Faithful Geyser Eruptions")+hw_theme
histplot <- ggplot(faithful,aes(x=waiting, y = after_stat(count)/sum(after_stat(count)))) +
  geom_histogram(binwidth=4,
                 fill="cornsilk",color="black")+
      labs(x="Waiting Time Between Eruptions In Minutes",
       y="Relative Frequencies",
       title="Old Faithful Geyser Eruptions")+hw_theme

histplot

histplot+ geom_freqpoly(binwidth=2,color="red",linewidth=1.2)


histplot+ geom_line(stat="density",color="blue",linewidth=1.2)+
  xlim(33,104)

histplot +
  geom_density(adjust= 0.8 ,fill="cyan",color="black",alpha=0.4)+
  xlim(35,102)
#---------

source('hw.r')
library(tidyverse)

data("diamonds")
str(diamonds)

ggplot(diamonds , aes( x = color) ) +
  geom_bar() +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw
ggplot(diamonds , aes( x = color) ) +
  geom_bar(fill="cornsilk",color='black') +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw

ggplot(diamonds , aes( x = color, fill = color) ) +
  geom_bar() +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw
#or
ggplot(diamonds , aes( x = color) ) +
  geom_bar(fill = color) +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw


ggplot(diamonds, aes(x=color, fill=color))+
  geom_bar(color="black") +        # black outline
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set")+ hw +
  theme(legend.position="none")
ggplot(diamonds, aes( x=color, fill=cut) )+
  geom_bar(color="black") +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set",
       fill="Cut") + hw

ggplot(diamonds, aes( x=cut, fill=color) )+
  geom_bar(color="black") +
  labs(x="Diamond Cut Classes",
       y="Count",
       title="Diamond Data Set",
       fill="Color") + hw

ggplot(diamonds,aes(x=color,fill=cut))+
  geom_bar(color=gray(.55)) +
  labs(x="Diamond Color Classes",
       y="Count",
       title="Diamond Data Set",
       fill="Cut")+
  scale_fill_manual(
    values=c("red","orange",rgb(0,.8,0),'cyan','violet'))+ hw


ggplot(data=diamonds, aes(x=carat))+
  geom_density(fill="cyan",color="black")+ hw

ggplot(data=diamonds, aes(x=carat))+
  geom_density(fill="cyan",color="black",adjust=2) + hw



ggplot(data=diamonds, aes( x=carat, group=cut, fill=cut) )+
  geom_density( color="black", adjust=2, alpha= .2) +
  scale_fill_manual(
    values=c("red", "yellow", "green","cyan", "violet")) + hw

scatt <- ggplot(diamonds, aes( x=carat, y=price) )
scatt + geom_point()  + hw
scatt + geom_point( aes( color = color) ) + hw
scatt + geom_point(aes(color=color))+ facet_wrap(~color) +hw















