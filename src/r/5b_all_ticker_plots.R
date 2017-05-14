#=======================
#Create ggplot objects for each case/ticker combo.
#This is the ticker/case version of the case ggplot list
#========================

#Calling Plot Instructions (After script has been executed)
  #Call an indiivudal case/ticker plot
    #plot(case[n]_[TICKER]_gg)
    #Example: plot(case2_CVX_gg)

  #View multiple plots in the same window
    #grid.arrange(case[n]_[TICKER]_gg, case[n]_[TICKER]_gg, ...etc.)
    #Example: grid.arrange(case2_CVX_gg, case2_XOM_gg)

  #Save multiple plots to file instead of displaying them in window
    #(1) [my_object_name] <- arrangeGrob(case[n]_[TICKER]_gg, case[n]_[TICKER]_gg, ...etc.)
    #(2) ggsave(my_object_name, file ="my_object_name.pdf", width = 297, height = 210, units="mm)

#========================================================
#Get each case/ticker combo
ticker_case_df <- all_Stats %>%
  group_by(case, ticker) %>%
  summarise()

#Store dataframes of each case/ticker combo

case2_CVX<-all_Stats%>%filter(case==2,ticker=="CVX")
case2_XOM<-all_Stats%>%filter(case==2,ticker=="XOM")
case5_ABT<-all_Stats%>%filter(case==5,ticker=="ABT")
case5_BMY<-all_Stats%>%filter(case==5,ticker=="BMY")
case5_JNJ<-all_Stats%>%filter(case==5,ticker=="JNJ")
case6_KSU<-all_Stats%>%filter(case==6,ticker=="KSU")
case7_PLA<-all_Stats%>%filter(case==7,ticker=="PLA")
case9_XLF<-all_Stats%>%filter(case==9,ticker=="XLF")
case12_XLE<-all_Stats%>%filter(case==12,ticker=="XLE")
case14_XLV<-all_Stats%>%filter(case==14,ticker=="XLV")
case15_GM<-all_Stats%>%filter(case==15,ticker=="GM")
case15_HMC<-all_Stats%>%filter(case==15,ticker=="HMC")
case17_CC<-all_Stats%>%filter(case==17,ticker=="CC")
case19_XLI<-all_Stats%>%filter(case==19,ticker=="XLI")
case23_VGR<-all_Stats%>%filter(case==23,ticker=="VGR")
case26_XLI<-all_Stats%>%filter(case==26,ticker=="XLI")
case26_XLP<-all_Stats%>%filter(case==26,ticker=="XLP")
case28_GWO<-all_Stats%>%filter(case==28,ticker=="GWO")
case29_XLE<-all_Stats%>%filter(case==29,ticker=="XLE")
case35_T<-all_Stats%>%filter(case==35,ticker=="T")
case36_T<-all_Stats%>%filter(case==36,ticker=="T")
case36_XLY<-all_Stats%>%filter(case==36,ticker=="XLY")
case44_WLB<-all_Stats%>%filter(case==44,ticker=="WLB")
case47_NSC<-all_Stats%>%filter(case==47,ticker=="NSC")
case47_UNP<-all_Stats%>%filter(case==47,ticker=="UNP")
case47_XLI<-all_Stats%>%filter(case==47,ticker=="XLI")
case48_PHS<-all_Stats%>%filter(case==48,ticker=="PHS")
case51_AZN<-all_Stats%>%filter(case==51,ticker=="AZN")
case51_BMY<-all_Stats%>%filter(case==51,ticker=="BMY")
case51_PFE<-all_Stats%>%filter(case==51,ticker=="PFE")
case51_XLV<-all_Stats%>%filter(case==51,ticker=="XLV")
case56_XLP<-all_Stats%>%filter(case==56,ticker=="XLP")
case58_TRV<-all_Stats%>%filter(case==58,ticker=="TRV")
case60_XLY<-all_Stats%>%filter(case==60,ticker=="XLY")
case61_XLF<-all_Stats%>%filter(case==61,ticker=="XLF")
case64_V<-all_Stats%>%filter(case==64,ticker=="V")
case66_XLI<-all_Stats%>%filter(case==66,ticker=="XLI")
case67_UNH<-all_Stats%>%filter(case==67,ticker=="UNH")
case69_CVX<-all_Stats%>%filter(case==69,ticker=="CVX")
case69_HES<-all_Stats%>%filter(case==69,ticker=="HES")
case69_XOM<-all_Stats%>%filter(case==69,ticker=="XOM")
case71_XLI<-all_Stats%>%filter(case==71,ticker=="XLI")
case74_XLP<-all_Stats%>%filter(case==74,ticker=="XLP")
case77_MRK<-all_Stats%>%filter(case==77,ticker=="MRK")
case78_YRCW<-all_Stats%>%filter(case==78,ticker=="YRCW")
case86_ITW<-all_Stats%>%filter(case==86,ticker=="ITW")
case87_MER<-all_Stats%>%filter(case==87,ticker=="MER")
case88_EBAY<-all_Stats%>%filter(case==88,ticker=="EBAY")
case93_MO<-all_Stats%>%filter(case==93,ticker=="MO")
case99_GLBC<-all_Stats%>%filter(case==99,ticker=="GLBC")
case99_VOX<-all_Stats%>%filter(case==99,ticker=="VOX")
case100_TFX<-all_Stats%>%filter(case==100,ticker=="TFX")
case100_VOX<-all_Stats%>%filter(case==100,ticker=="VOX")
case111_XLV<-all_Stats%>%filter(case==111,ticker=="XLV")
case120_S<-all_Stats%>%filter(case==120,ticker=="S")
case120_XLY<-all_Stats%>%filter(case==120,ticker=="XLY")
case123_MO<-all_Stats%>%filter(case==123,ticker=="MO")
case124_DD<-all_Stats%>%filter(case==124,ticker=="DD")
case126_PFE<-all_Stats%>%filter(case==126,ticker=="PFE")
case131_CNA<-all_Stats%>%filter(case==131,ticker=="CNA")
case132_CDE<-all_Stats%>%filter(case==132,ticker=="CDE")
case144_XLI<-all_Stats%>%filter(case==144,ticker=="XLI")
case149_AXP<-all_Stats%>%filter(case==149,ticker=="AXP")
case149_WEX<-all_Stats%>%filter(case==149,ticker=="WEX")
case152_GM<-all_Stats%>%filter(case==152,ticker=="GM")
case153_VOX<-all_Stats%>%filter(case==153,ticker=="VOX")
case153_XTL<-all_Stats%>%filter(case==153,ticker=="XTL")
case154_XPH<-all_Stats%>%filter(case==154,ticker=="XPH")
case155_XTL<-all_Stats%>%filter(case==155,ticker=="XTL")
case156_XLF<-all_Stats%>%filter(case==156,ticker=="XLF")
case158_JAH<-all_Stats%>%filter(case==158,ticker=="JAH")
case158_XLY<-all_Stats%>%filter(case==158,ticker=="XLY")
case159_XES<-all_Stats%>%filter(case==159,ticker=="XES")
case163_XLY<-all_Stats%>%filter(case==163,ticker=="XLY")
case166_GT<-all_Stats%>%filter(case==166,ticker=="GT")
case171_CS<-all_Stats%>%filter(case==171,ticker=="CS")
case171_DB<-all_Stats%>%filter(case==171,ticker=="DB")
case171_UBS<-all_Stats%>%filter(case==171,ticker=="UBS")
case172_XLV<-all_Stats%>%filter(case==172,ticker=="XLV")
case173_STN<-all_Stats%>%filter(case==173,ticker=="STN")
case174_XHE<-all_Stats%>%filter(case==174,ticker=="XHE")
case175_KMI<-all_Stats%>%filter(case==175,ticker=="KMI")
case175_XLE<-all_Stats%>%filter(case==175,ticker=="XLE")
case177_AET<-all_Stats%>%filter(case==177,ticker=="AET")
case177_CI<-all_Stats%>%filter(case==177,ticker=="CI")
case177_HCA<-all_Stats%>%filter(case==177,ticker=="HCA")
case177_HNT<-all_Stats%>%filter(case==177,ticker=="HNT")
case177_HUM<-all_Stats%>%filter(case==177,ticker=="HUM")
case177_MGLN<-all_Stats%>%filter(case==177,ticker=="MGLN")
case177_WLP<-all_Stats%>%filter(case==177,ticker=="WLP")
case180_PSO<-all_Stats%>%filter(case==180,ticker=="PSO")
case184_XLE<-all_Stats%>%filter(case==184,ticker=="XLE")
case184_XOP<-all_Stats%>%filter(case==184,ticker=="XOP")
case185_XLP<-all_Stats%>%filter(case==185,ticker=="XLP")
case187_MYGN<-all_Stats%>%filter(case==187,ticker=="MYGN")
case188_ACT<-all_Stats%>%filter(case==188,ticker=="ACT")
case188_XPH<-all_Stats%>%filter(case==188,ticker=="XPH")
case189_MA<-all_Stats%>%filter(case==189,ticker=="MA")
case191_F<-all_Stats%>%filter(case==191,ticker=="F")
case192_VOX<-all_Stats%>%filter(case==192,ticker=="VOX")
case192_XTL<-all_Stats%>%filter(case==192,ticker=="XTL")
case194_XLK<-all_Stats%>%filter(case==194,ticker=="XLK")
case200_LLNW<-all_Stats%>%filter(case==200,ticker=="LLNW")
case202_CTS<-all_Stats%>%filter(case==202,ticker=="CTS")
case202_SANM<-all_Stats%>%filter(case==202,ticker=="SANM")
case206_C<-all_Stats%>%filter(case==206,ticker=="C")
case207_XES<-all_Stats%>%filter(case==207,ticker=="XES")
case207_XLE<-all_Stats%>%filter(case==207,ticker=="XLE")
case207_XOP<-all_Stats%>%filter(case==207,ticker=="XOP")
case208_BHI<-all_Stats%>%filter(case==208,ticker=="BHI")
case208_HAL<-all_Stats%>%filter(case==208,ticker=="HAL")
case208_XES<-all_Stats%>%filter(case==208,ticker=="XES")
case208_XLE<-all_Stats%>%filter(case==208,ticker=="XLE")
case208_XOP<-all_Stats%>%filter(case==208,ticker=="XOP")
case209_CBS<-all_Stats%>%filter(case==209,ticker=="CBS")
case209_FOXA<-all_Stats%>%filter(case==209,ticker=="FOXA")
case209_SBGI<-all_Stats%>%filter(case==209,ticker=="SBGI")
case211_S<-all_Stats%>%filter(case==211,ticker=="S")


#======================================
#Store each case/ticker combo as a ggplot object
#======================================


case2_CVX_gg<-ggplot(case2_CVX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case2_CVX$Case_Name,"\n",case2_CVX$ticker))+
  case2_XOM_gg<-ggplot(case2_XOM,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case2_XOM$Case_Name,"\n",case2_XOM$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)

case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)

case5_BMY_gg<-ggplot(case5_BMY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_BMY$Case_Name,"\n",case5_BMY$ticker))+ case2_XOM_gg<-ggplot(case2_XOM,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case2_XOM$Case_Name,"\n",case2_XOM$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)

case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case5_JNJ_gg<-ggplot(case5_JNJ,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_JNJ$Case_Name,"\n",case5_JNJ$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case6_KSU_gg<-ggplot(case6_KSU,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case6_KSU$Case_Name,"\n",case6_KSU$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case7_PLA_gg<-ggplot(case7_PLA,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case7_PLA$Case_Name,"\n",case7_PLA$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case9_XLF_gg<-ggplot(case9_XLF,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case9_XLF$Case_Name,"\n",case9_XLF$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case12_XLE_gg<-ggplot(case12_XLE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case12_XLE$Case_Name,"\n",case12_XLE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case14_XLV_gg<-ggplot(case14_XLV,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case14_XLV$Case_Name,"\n",case14_XLV$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case15_GM_gg<-ggplot(case15_GM,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case15_GM$Case_Name,"\n",case15_GM$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case15_HMC_gg<-ggplot(case15_HMC,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case15_HMC$Case_Name,"\n",case15_HMC$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case17_CC_gg<-ggplot(case17_CC,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case17_CC$Case_Name,"\n",case17_CC$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case19_XLI_gg<-ggplot(case19_XLI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case19_XLI$Case_Name,"\n",case19_XLI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case23_VGR_gg<-ggplot(case23_VGR,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case23_VGR$Case_Name,"\n",case23_VGR$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case26_XLI_gg<-ggplot(case26_XLI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case26_XLI$Case_Name,"\n",case26_XLI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case26_XLP_gg<-ggplot(case26_XLP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case26_XLP$Case_Name,"\n",case26_XLP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case28_GWO_gg<-ggplot(case28_GWO,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case28_GWO$Case_Name,"\n",case28_GWO$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case29_XLE_gg<-ggplot(case29_XLE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case29_XLE$Case_Name,"\n",case29_XLE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case35_T_gg<-ggplot(case35_T,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case35_T$Case_Name,"\n",case35_T$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case36_T_gg<-ggplot(case36_T,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case36_T$Case_Name,"\n",case36_T$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case36_XLY_gg<-ggplot(case36_XLY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case36_XLY$Case_Name,"\n",case36_XLY$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case44_WLB_gg<-ggplot(case44_WLB,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case44_WLB$Case_Name,"\n",case44_WLB$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case47_NSC_gg<-ggplot(case47_NSC,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case47_NSC$Case_Name,"\n",case47_NSC$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case47_UNP_gg<-ggplot(case47_UNP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case47_UNP$Case_Name,"\n",case47_UNP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case47_XLI_gg<-ggplot(case47_XLI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case47_XLI$Case_Name,"\n",case47_XLI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case48_PHS_gg<-ggplot(case48_PHS,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case48_PHS$Case_Name,"\n",case48_PHS$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case51_AZN_gg<-ggplot(case51_AZN,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case51_AZN$Case_Name,"\n",case51_AZN$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case51_BMY_gg<-ggplot(case51_BMY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case51_BMY$Case_Name,"\n",case51_BMY$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case51_PFE_gg<-ggplot(case51_PFE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case51_PFE$Case_Name,"\n",case51_PFE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case51_XLV_gg<-ggplot(case51_XLV,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case51_XLV$Case_Name,"\n",case51_XLV$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case56_XLP_gg<-ggplot(case56_XLP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case56_XLP$Case_Name,"\n",case56_XLP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case58_TRV_gg<-ggplot(case58_TRV,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case58_TRV$Case_Name,"\n",case58_TRV$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case60_XLY_gg<-ggplot(case60_XLY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case60_XLY$Case_Name,"\n",case60_XLY$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case61_XLF_gg<-ggplot(case61_XLF,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case61_XLF$Case_Name,"\n",case61_XLF$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case64_V_gg<-ggplot(case64_V,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case64_V$Case_Name,"\n",case64_V$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case66_XLI_gg<-ggplot(case66_XLI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case66_XLI$Case_Name,"\n",case66_XLI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case67_UNH_gg<-ggplot(case67_UNH,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case67_UNH$Case_Name,"\n",case67_UNH$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case69_CVX_gg<-ggplot(case69_CVX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case69_CVX$Case_Name,"\n",case69_CVX$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case69_HES_gg<-ggplot(case69_HES,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case69_HES$Case_Name,"\n",case69_HES$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case69_XOM_gg<-ggplot(case69_XOM,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case69_XOM$Case_Name,"\n",case69_XOM$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case71_XLI_gg<-ggplot(case71_XLI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case71_XLI$Case_Name,"\n",case71_XLI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case74_XLP_gg<-ggplot(case74_XLP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case74_XLP$Case_Name,"\n",case74_XLP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case77_MRK_gg<-ggplot(case77_MRK,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case77_MRK$Case_Name,"\n",case77_MRK$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case78_YRCW_gg<-ggplot(case78_YRCW,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case78_YRCW$Case_Name,"\n",case78_YRCW$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case86_ITW_gg<-ggplot(case86_ITW,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case86_ITW$Case_Name,"\n",case86_ITW$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case87_MER_gg<-ggplot(case87_MER,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case87_MER$Case_Name,"\n",case87_MER$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case88_EBAY_gg<-ggplot(case88_EBAY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case88_EBAY$Case_Name,"\n",case88_EBAY$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case93_MO_gg<-ggplot(case93_MO,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case93_MO$Case_Name,"\n",case93_MO$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case99_GLBC_gg<-ggplot(case99_GLBC,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case99_GLBC$Case_Name,"\n",case99_GLBC$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case99_VOX_gg<-ggplot(case99_VOX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case99_VOX$Case_Name,"\n",case99_VOX$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case100_TFX_gg<-ggplot(case100_TFX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case100_TFX$Case_Name,"\n",case100_TFX$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case100_VOX_gg<-ggplot(case100_VOX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case100_VOX$Case_Name,"\n",case100_VOX$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case111_XLV_gg<-ggplot(case111_XLV,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case111_XLV$Case_Name,"\n",case111_XLV$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case120_S_gg<-ggplot(case120_S,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case120_S$Case_Name,"\n",case120_S$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case120_XLY_gg<-ggplot(case120_XLY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case120_XLY$Case_Name,"\n",case120_XLY$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case123_MO_gg<-ggplot(case123_MO,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case123_MO$Case_Name,"\n",case123_MO$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case124_DD_gg<-ggplot(case124_DD,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case124_DD$Case_Name,"\n",case124_DD$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case126_PFE_gg<-ggplot(case126_PFE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case126_PFE$Case_Name,"\n",case126_PFE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case131_CNA_gg<-ggplot(case131_CNA,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case131_CNA$Case_Name,"\n",case131_CNA$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case132_CDE_gg<-ggplot(case132_CDE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case132_CDE$Case_Name,"\n",case132_CDE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case144_XLI_gg<-ggplot(case144_XLI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case144_XLI$Case_Name,"\n",case144_XLI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case149_AXP_gg<-ggplot(case149_AXP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case149_AXP$Case_Name,"\n",case149_AXP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case149_WEX_gg<-ggplot(case149_WEX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case149_WEX$Case_Name,"\n",case149_WEX$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case152_GM_gg<-ggplot(case152_GM,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case152_GM$Case_Name,"\n",case152_GM$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case153_VOX_gg<-ggplot(case153_VOX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case153_VOX$Case_Name,"\n",case153_VOX$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case153_XTL_gg<-ggplot(case153_XTL,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case153_XTL$Case_Name,"\n",case153_XTL$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case154_XPH_gg<-ggplot(case154_XPH,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case154_XPH$Case_Name,"\n",case154_XPH$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case155_XTL_gg<-ggplot(case155_XTL,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case155_XTL$Case_Name,"\n",case155_XTL$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case156_XLF_gg<-ggplot(case156_XLF,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case156_XLF$Case_Name,"\n",case156_XLF$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case158_JAH_gg<-ggplot(case158_JAH,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case158_JAH$Case_Name,"\n",case158_JAH$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case158_XLY_gg<-ggplot(case158_XLY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case158_XLY$Case_Name,"\n",case158_XLY$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case159_XES_gg<-ggplot(case159_XES,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case159_XES$Case_Name,"\n",case159_XES$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case163_XLY_gg<-ggplot(case163_XLY,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case163_XLY$Case_Name,"\n",case163_XLY$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case166_GT_gg<-ggplot(case166_GT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case166_GT$Case_Name,"\n",case166_GT$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case171_CS_gg<-ggplot(case171_CS,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case171_CS$Case_Name,"\n",case171_CS$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case171_DB_gg<-ggplot(case171_DB,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case171_DB$Case_Name,"\n",case171_DB$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case171_UBS_gg<-ggplot(case171_UBS,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case171_UBS$Case_Name,"\n",case171_UBS$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case172_XLV_gg<-ggplot(case172_XLV,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case172_XLV$Case_Name,"\n",case172_XLV$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case173_STN_gg<-ggplot(case173_STN,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case173_STN$Case_Name,"\n",case173_STN$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case174_XHE_gg<-ggplot(case174_XHE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case174_XHE$Case_Name,"\n",case174_XHE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case175_KMI_gg<-ggplot(case175_KMI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case175_KMI$Case_Name,"\n",case175_KMI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case175_XLE_gg<-ggplot(case175_XLE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case175_XLE$Case_Name,"\n",case175_XLE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case177_AET_gg<-ggplot(case177_AET,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case177_AET$Case_Name,"\n",case177_AET$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case177_CI_gg<-ggplot(case177_CI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case177_CI$Case_Name,"\n",case177_CI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case177_HCA_gg<-ggplot(case177_HCA,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case177_HCA$Case_Name,"\n",case177_HCA$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case177_HNT_gg<-ggplot(case177_HNT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case177_HNT$Case_Name,"\n",case177_HNT$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case177_HUM_gg<-ggplot(case177_HUM,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case177_HUM$Case_Name,"\n",case177_HUM$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case177_MGLN_gg<-ggplot(case177_MGLN,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case177_MGLN$Case_Name,"\n",case177_MGLN$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case177_WLP_gg<-ggplot(case177_WLP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case177_WLP$Case_Name,"\n",case177_WLP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case180_PSO_gg<-ggplot(case180_PSO,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case180_PSO$Case_Name,"\n",case180_PSO$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case184_XLE_gg<-ggplot(case184_XLE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case184_XLE$Case_Name,"\n",case184_XLE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case184_XOP_gg<-ggplot(case184_XOP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case184_XOP$Case_Name,"\n",case184_XOP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case185_XLP_gg<-ggplot(case185_XLP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case185_XLP$Case_Name,"\n",case185_XLP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case187_MYGN_gg<-ggplot(case187_MYGN,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case187_MYGN$Case_Name,"\n",case187_MYGN$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case188_ACT_gg<-ggplot(case188_ACT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case188_ACT$Case_Name,"\n",case188_ACT$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case188_XPH_gg<-ggplot(case188_XPH,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case188_XPH$Case_Name,"\n",case188_XPH$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case189_MA_gg<-ggplot(case189_MA,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case189_MA$Case_Name,"\n",case189_MA$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case191_F_gg<-ggplot(case191_F,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case191_F$Case_Name,"\n",case191_F$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case192_VOX_gg<-ggplot(case192_VOX,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case192_VOX$Case_Name,"\n",case192_VOX$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case192_XTL_gg<-ggplot(case192_XTL,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case192_XTL$Case_Name,"\n",case192_XTL$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case194_XLK_gg<-ggplot(case194_XLK,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case194_XLK$Case_Name,"\n",case194_XLK$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case200_LLNW_gg<-ggplot(case200_LLNW,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case200_LLNW$Case_Name,"\n",case200_LLNW$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case202_CTS_gg<-ggplot(case202_CTS,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case202_CTS$Case_Name,"\n",case202_CTS$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case202_SANM_gg<-ggplot(case202_SANM,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case202_SANM$Case_Name,"\n",case202_SANM$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case206_C_gg<-ggplot(case206_C,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case206_C$Case_Name,"\n",case206_C$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case207_XES_gg<-ggplot(case207_XES,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case207_XES$Case_Name,"\n",case207_XES$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case207_XLE_gg<-ggplot(case207_XLE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case207_XLE$Case_Name,"\n",case207_XLE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case207_XOP_gg<-ggplot(case207_XOP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case207_XOP$Case_Name,"\n",case207_XOP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case208_BHI_gg<-ggplot(case208_BHI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case208_BHI$Case_Name,"\n",case208_BHI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case208_HAL_gg<-ggplot(case208_HAL,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case208_HAL$Case_Name,"\n",case208_HAL$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case208_XES_gg<-ggplot(case208_XES,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case208_XES$Case_Name,"\n",case208_XES$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case208_XLE_gg<-ggplot(case208_XLE,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case208_XLE$Case_Name,"\n",case208_XLE$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case208_XOP_gg<-ggplot(case208_XOP,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case208_XOP$Case_Name,"\n",case208_XOP$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case209_CBS_gg<-ggplot(case209_CBS,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case209_CBS$Case_Name,"\n",case209_CBS$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case209_FOXA_gg<-ggplot(case209_FOXA,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case209_FOXA$Case_Name,"\n",case209_FOXA$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case209_SBGI_gg<-ggplot(case209_SBGI,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case209_SBGI$Case_Name,"\n",case209_SBGI$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)


case211_S_gg<-ggplot(case211_S,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case211_S$Case_Name,"\n",case211_S$ticker))+ case5_ABT_gg<-ggplot(case5_ABT,aes(x=event_period,y=cum_abr_perc,group=1,color=ticker))+
  ggtitle(paste0(case5_ABT$Case_Name,"\n",case5_ABT$ticker))+ geom_line() +
  scale_x_discrete(breaks=c(1, 7, 79, 157), labels = c("9:30am", "10:00am", "Day 1 Close", "Day 2 Close")) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  theme(axis.text.x=element_text(angle = 50, vjust=1.2, hjust = 1.2),
        legend.title=element_blank(),
        legend.position="right",
        legend.direction="vertical") +
  scale_y_continuous(breaks =c(-.5, 0, .5, .75, 1), labels = c("-50%", "0%", "50%", "75%", "100%")) +
  scale_color_manual(values = palle)








#=============================
#Graphs with only one ticker each
#=============================



#ggsave(ticker_gg_pg1,file="ticker_gg_pg1.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg2,file="ticker_gg_pg2.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg3,file="ticker_gg_pg3.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg4,file="ticker_gg_pg4.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg5,file="ticker_gg_pg5.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg6,file="ticker_gg_pg6.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg7,file="ticker_gg_pg7.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg8,file="ticker_gg_pg8.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg9,file="ticker_gg_pg9.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg10,file="ticker_gg_pg10.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg11,file="ticker_gg_pg11.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg12,file="ticker_gg_pg12.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg13,file="ticker_gg_pg13.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg14,file="ticker_gg_pg14.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg15,file="ticker_gg_pg15.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg16,file="ticker_gg_pg16.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg17,file="ticker_gg_pg17.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg18,file="ticker_gg_pg18.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg19,file="ticker_gg_pg19.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg20,file="ticker_gg_pg20.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg21,file="ticker_gg_pg21.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg22,file="ticker_gg_pg22.pdf",width=297,height=210,units="mm")
#gsave(ticker_gg_pg23,file="ticker_gg_pg23.pdf",width=297,height=210,units="mm")
##ggsave(ticker_gg_pg24,file="ticker_gg_pg24.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg25,file="ticker_gg_pg25.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg26,file="ticker_gg_pg26.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg27,file="ticker_gg_pg27.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg28,file="ticker_gg_pg28.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg29,file="ticker_gg_pg29.pdf",width=297,height=210,units="mm")
#ggsave(ticker_gg_pg30,file="ticker_gg_pg30.pdf",width=297,height=210,units="mm")