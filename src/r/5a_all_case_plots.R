#=====================================
#Goal: Compare how market reaction has changed over time from 1999 to 2015.
#=====================================

#Imports
library(dplyr)
library(ggthemes)
library(gridExtra)

#Prevent scientific notation and show 6 digits right of the decimal point
options("scipen"=100, "digits"=6)

#Add text "Case" before case number in column: case.
#This is because we will use facet_wrap which uses the case number to name each grid, but we can't
#customize it within ggplot

#Adding "Case" infront of the case # in  facet titles
#all_Stats$case <- paste("Case", all_Stats$case)


#Imports
library(ggplot2)

#DEPENDENCIES: 
#Reproduction Script 1 
#Reproduction Script 2
#Script: "Compile Data.R" In Folder "LOTM_Visuals"




#================================================
#Store each case as individual dataframes
#===============================================

case2<-all_Stats%>%filter(case==2)
case5<-all_Stats%>%filter(case==5)
case6<-all_Stats%>%filter(case==6)
case7<-all_Stats%>%filter(case==7)
case9<-all_Stats%>%filter(case==9)
case12<-all_Stats%>%filter(case==12)
case14<-all_Stats%>%filter(case==14)
case15<-all_Stats%>%filter(case==15)
case17<-all_Stats%>%filter(case==17)
case19<-all_Stats%>%filter(case==19)
case23<-all_Stats%>%filter(case==23)
case26<-all_Stats%>%filter(case==26)
case28<-all_Stats%>%filter(case==28)
case29<-all_Stats%>%filter(case==29)
case35<-all_Stats%>%filter(case==35)
case36<-all_Stats%>%filter(case==36)
case44<-all_Stats%>%filter(case==44)
case47<-all_Stats%>%filter(case==47)
case48<-all_Stats%>%filter(case==48)
case51<-all_Stats%>%filter(case==51)
case56<-all_Stats%>%filter(case==56)
case58<-all_Stats%>%filter(case==58)
case60<-all_Stats%>%filter(case==60)
case61<-all_Stats%>%filter(case==61)
case64<-all_Stats%>%filter(case==64)
case66<-all_Stats%>%filter(case==66)
case67<-all_Stats%>%filter(case==67)
case69<-all_Stats%>%filter(case==69)
case71<-all_Stats%>%filter(case==71)
case74<-all_Stats%>%filter(case==74)
case77<-all_Stats%>%filter(case==77)
case78<-all_Stats%>%filter(case==78)
case86<-all_Stats%>%filter(case==86)
case87<-all_Stats%>%filter(case==87)
case88<-all_Stats%>%filter(case==88)
case93<-all_Stats%>%filter(case==93)
case99<-all_Stats%>%filter(case==99)
case100<-all_Stats%>%filter(case==100)
case111<-all_Stats%>%filter(case==111)
case120<-all_Stats%>%filter(case==120)
case123<-all_Stats%>%filter(case==123)
case124<-all_Stats%>%filter(case==124)
case126<-all_Stats%>%filter(case==126)
case131<-all_Stats%>%filter(case==131)
case132<-all_Stats%>%filter(case==132)
case144<-all_Stats%>%filter(case==144)
case149<-all_Stats%>%filter(case==149)
case152<-all_Stats%>%filter(case==152)
case153<-all_Stats%>%filter(case==153)
case154<-all_Stats%>%filter(case==154)
case155<-all_Stats%>%filter(case==155)
case156<-all_Stats%>%filter(case==156)
case158<-all_Stats%>%filter(case==158)
case159<-all_Stats%>%filter(case==159)
case163<-all_Stats%>%filter(case==163)
case166<-all_Stats%>%filter(case==166)
case171<-all_Stats%>%filter(case==171)
case172<-all_Stats%>%filter(case==172)
case173<-all_Stats%>%filter(case==173)
case174<-all_Stats%>%filter(case==174)
case175<-all_Stats%>%filter(case==175)
case177<-all_Stats%>%filter(case==177)
case180<-all_Stats%>%filter(case==180)
case184<-all_Stats%>%filter(case==184)
case185<-all_Stats%>%filter(case==185)
case187<-all_Stats%>%filter(case==187)
case188<-all_Stats%>%filter(case==188)
case189<-all_Stats%>%filter(case==189)
case191<-all_Stats%>%filter(case==191)
case192<-all_Stats%>%filter(case==192)
case194<-all_Stats%>%filter(case==194)
case200<-all_Stats%>%filter(case==200)
case202<-all_Stats%>%filter(case==202)
case206<-all_Stats%>%filter(case==206)
case207<-all_Stats%>%filter(case==207)
case208<-all_Stats%>%filter(case==208)
case209<-all_Stats%>%filter(case==209)
case211<-all_Stats%>%filter(case==211)


#=========================================================
#GGPlot ToC:
#=========================================================
  #x: Value to plot on the x-axis
  
  #y: Value to plot on the y-axis
  
  #group: how to group x,y data. We use "ticker" to have one line per ticker
  
  #color: use "ticker" to assign a unique line color for each ticker.
  
  #scale_x_discrete: add custom x-axis marks. 
  
  #breaks: what x values (rows in dataframe) to set custom ticks at
  
  #labels: The text or labels to appear at ticks specified in "breaks"
  
  #theme_fivethirtyeith: Preset Aesthetics Theme from package "ggThemes"
  
  #theme:
  
  #axis.text.x: adjust the angle and positioning of the x-axis labels
  
  #legend.title: remove the legend title
  
  #legend.positoin/direction: adjust legend location and position
  
  #scale_y_continuous: Same as "scale_x_discrete" above, but for y-axis
  
  #Note: we use "continuous" rather than "discrete" to decribe the type of data
  
  #scale_color_manual: use the colors we create below ("palle") rather than default colors
  
  #ggtitle: insert custom title. Rather than enter explicit text, we pull data from the "Case_Name" column
#====================================================

#Custom color pallete
palle <- c("#008FD5", "#FF2700", "#77AB43", "#FFCC33", "#CC79A7", "purple", "#999999")
#F0E442 = Yellow
#CC79A7 = Pink
#999999 = Grey

case2_gg <- ggplot(case2,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) +
  geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle) + ggtitle(case2$Case_Name)

case5_gg <- ggplot(case5,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) +
  geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle) + ggtitle(case5$Case_Name)

case6_gg <- ggplot(case6,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) +
  geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case6$Case_Name)

case7_gg <- ggplot(case7,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) +
  geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case7$Case_Name)

case9_gg <- ggplot(case9,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case9$Case_Name)

case12_gg <- ggplot(case12,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case12$Case_Name)

case14_gg <- ggplot(case14,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case14$Case_Name)

case15_gg <- ggplot(case15,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case15$Case_Name)

case17_gg <- ggplot(case17,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case17$Case_Name)

case19_gg <- ggplot(case19,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case19$Case_Name)

case23_gg <- ggplot(case23,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case23$Case_Name)

case26_gg <- ggplot(case26,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case26$Case_Name)

case28_gg <- ggplot(case28,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case28$Case_Name)

case29_gg <- ggplot(case29,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case29$Case_Name)

case35_gg <- ggplot(case35,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case35$Case_Name)

case36_gg <- ggplot(case36,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case36$Case_Name)

case44_gg <- ggplot(case44,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case44$Case_Name)

case47_gg <- ggplot(case47,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case47$Case_Name)

case48_gg <- ggplot(case48,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case48$Case_Name)

case51_gg <- ggplot(case51,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case51$Case_Name)

case56_gg <- ggplot(case56,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case56$Case_Name)

case58_gg <- ggplot(case58,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case58$Case_Name)

case60_gg <- ggplot(case60,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case60$Case_Name)

case61_gg <- ggplot(case61,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case61$Case_Name)

case64_gg <- ggplot(case64,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case64$Case_Name)

case66_gg <- ggplot(case66,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case66$Case_Name)

case67_gg <- ggplot(case67,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case67$Case_Name)

case69_gg <- ggplot(case69,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case69$Case_Name)

case71_gg <- ggplot(case71,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case71$Case_Name)

case74_gg <- ggplot(case74,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)

case77_gg <- ggplot(case77,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case77$Case_Name)

case78_gg <- ggplot(case78,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case78$Case_Name)

case86_gg <- ggplot(case86,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case86$Case_Name)

case87_gg <- ggplot(case87,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case87$Case_Name)

case88_gg <- ggplot(case88,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case88$Case_Name)

case93_gg <- ggplot(case93,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case93$Case_Name)

case99_gg <- ggplot(case99,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case99$Case_Name)

case100_gg <- ggplot(case100,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case100$Case_Name)

case111_gg <- ggplot(case111,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case111$Case_Name)

case120_gg <- ggplot(case120,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case120$Case_Name)

case123_gg <- ggplot(case123,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case123$Case_Name)

case124_gg <- ggplot(case124,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case124$Case_Name)

case126_gg <- ggplot(case126,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case126$Case_Name)

case131_gg <- ggplot(case131,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case131$Case_Name)

case132_gg <- ggplot(case132,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case132$Case_Name)

case144_gg <- ggplot(case144,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case144$Case_Name)

case149_gg <- ggplot(case149,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case149$Case_Name)

case152_gg <- ggplot(case152,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case152$Case_Name)

case153_gg <- ggplot(case153,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case153$Case_Name)

case154_gg <- ggplot(case154,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case154$Case_Name)

case155_gg <- ggplot(case155,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case155$Case_Name)

case156_gg <- ggplot(case156,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case156$Case_Name)

case158_gg <- ggplot(case158,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case158$Case_Name)

case159_gg <- ggplot(case159,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case159$Case_Name)

case163_gg <- ggplot(case163,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case163$Case_Name)

case166_gg <- ggplot(case166,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case166$Case_Name)

case171_gg <- ggplot(case171,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case171$Case_Name)

case172_gg <- ggplot(case172,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case172$Case_Name)

case173_gg <- ggplot(case173,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case173$Case_Name)

case174_gg <- ggplot(case174,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case174$Case_Name)

case175_gg <- ggplot(case175,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case175$Case_Name)

case177_gg <- ggplot(case177,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case177$Case_Name)

case180_gg <- ggplot(case180,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case180$Case_Name)

case184_gg <- ggplot(case184,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case184$Case_Name)

case185_gg <- ggplot(case185,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case185$Case_Name)

case187_gg <- ggplot(case187,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case187$Case_Name)

case188_gg <- ggplot(case188,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case188$Case_Name)

case189_gg <- ggplot(case189,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case189$Case_Name)

case191_gg <- ggplot(case191,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case191$Case_Name)

case192_gg <- ggplot(case192,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case192$Case_Name)

case194_gg <- ggplot(case194,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case194$Case_Name)

case200_gg <- ggplot(case200,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case200$Case_Name)

case202_gg <- ggplot(case202,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case202$Case_Name)

case206_gg <- ggplot(case206,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case206$Case_Name)

case207_gg <- ggplot(case207,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case207$Case_Name)

case208_gg <- ggplot(case208,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case208$Case_Name)

case209_gg <- ggplot(case209,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case209$Case_Name)

case211_gg <- ggplot(case211,aes(x=event_period,y=cum_abr_perc, group = ticker, color = ticker)) + geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)+ ggtitle(case211$Case_Name)

#=========================================
#Set case graphs at two cases per page
#========================================
#Overview:
  #arrangeGrob: use to save the results with "ggsave"
  #grid.arrange: use to display the results

#Examples:
  #Display 2 cases on a grid/page
case_pg1<-grid.arrange(case2_gg,case5_gg)
  #Display 3 cases
cases256<- grid.arrange(case2_gg, case5_gg, case6_gg)

#Save the graphs to a pdf
case_pg1<-arrangeGrob(case2_gg,case5_gg)
ggsave(case_pg1, file ="figures/case_pg1.pdf", width=297,height=210,units="mm")

  #width and height numbers = landscape (wide) page layout
  #flip width/height values for portrait (standard) page layout

#Save all Case plots to pages with 2 cases per page
case_pg1<-arrangeGrob(case2_gg,case5_gg)
case_pg2<-arrangeGrob(case6_gg,case7_gg)
case_pg3<-arrangeGrob(case9_gg,case12_gg)
case_pg4<-arrangeGrob(case14_gg,case15_gg)
case_pg5<-arrangeGrob(case17_gg,case19_gg)
case_pg6<-arrangeGrob(case23_gg,case26_gg)
case_pg7<-arrangeGrob(case28_gg,case29_gg)
case_pg8<-arrangeGrob(case35_gg,case36_gg)
case_pg9<-arrangeGrob(case44_gg,case47_gg)
case_pg10<-arrangeGrob(case48_gg,case51_gg)
case_pg11<-arrangeGrob(case56_gg,case58_gg)
case_pg12<-arrangeGrob(case60_gg,case61_gg)
case_pg13<-arrangeGrob(case64_gg,case66_gg)
case_pg14<-arrangeGrob(case67_gg,case69_gg)
case_pg15<-arrangeGrob(case71_gg,case74_gg)
case_pg16<-arrangeGrob(case77_gg,case78_gg)
case_pg17<-arrangeGrob(case86_gg,case87_gg)
case_pg18<-arrangeGrob(case88_gg,case93_gg)
case_pg19<-arrangeGrob(case99_gg,case100_gg)
case_pg20<-arrangeGrob(case111_gg,case120_gg)
case_pg21<-arrangeGrob(case123_gg,case124_gg)
case_pg22<-arrangeGrob(case126_gg,case131_gg)
case_pg23<-arrangeGrob(case132_gg,case144_gg)
case_pg24<-arrangeGrob(case149_gg,case152_gg)
case_pg25<-arrangeGrob(case153_gg,case154_gg)
case_pg26<-arrangeGrob(case155_gg,case156_gg)
case_pg27<-arrangeGrob(case158_gg,case159_gg)
case_pg28<-arrangeGrob(case163_gg,case166_gg)
case_pg29<-arrangeGrob(case171_gg,case172_gg)
case_pg30<-arrangeGrob(case173_gg,case174_gg)
case_pg31<-arrangeGrob(case175_gg,case177_gg)
case_pg32<-arrangeGrob(case180_gg,case184_gg)
case_pg33<-arrangeGrob(case185_gg,case187_gg)
case_pg34<-arrangeGrob(case188_gg,case189_gg)
case_pg35<-arrangeGrob(case191_gg,case192_gg)
case_pg36<-arrangeGrob(case194_gg,case200_gg)
case_pg37<-arrangeGrob(case202_gg,case206_gg)
case_pg38<-arrangeGrob(case207_gg,case208_gg)
case_pg39<-arrangeGrob(case209_gg,case211_gg)


