#Imports
library(dplyr)
library(ggplot2)
library(reshape2)

#Set options to not display scientific notation and 6 figures behind the decimal point
options("scipen"=100, "digits"=6)

#DEPENDENCIES: Previoulsy Completed Reproduction Scripts 1 and 2

#Dependencies:
#LOTM_Stats from Reproduction Scripts 1 and 2
#To save memory/time, read in LOTM_Stats.csv from selected output folder as "LOTM_Stats"

#==================
#Desired Dataframes
#==================
#We want to create dataframes for each case containing: 
#Timestamp
#Source: $daEve from event study object of each case. E.g., CaseX$daEve
#Abnormal Returns of Relevant Symbols
#Source: $abr from the event study object for each case. 
#Actual Returns of Relevant Symbols and Index (SP or SPY)
#Source: The all_return_xts object we created at the beginning of the first reproduction script

#==================================
#Identify Relevant Symbols and Periods Using LOTM_Stats
#==================================

#In LOTM_Stats, create new columns for the first (d1) and second (d2) day of our event window with class = POSIXct
#then filter for rows with symbols that were significant under the SPY index for establishing normal returns
#then select columns: Case, Symbol, d1, d2 (other columns dropped).

Sig_Stats <- LOTM_Stats %>%
  mutate(d1 = mdy(LOTM_Stats$dateDecision),
         d2 = d1 + days(1)) %>%
  filter(Index == "SPY", sig =="**" | sig == "***") %>%
  select(Case, term, Symbol, d1, d2)


#The event studies used trade data from symbol SPY for the first 107 cases
#and SP for the remaining cases. Create separate objects based on SP or SPY as the index.
#We don't want the dataframes for each case to have a column for SP and SPY, one of which would contain NAs.
SPY_Stats <- Sig_Stats %>%
  filter(Case < 108)

SP_Stats <- Sig_Stats %>%
  filter(Case > 107)


#======================
#Generate code to create dataframes with actual and abnormal returns for each 5 minute period during the event window
#for all cases and symbols that were significant under the SP/SPY Index.
#=====================
#We use the "paste" function to help minimize errors that could result from manual coding.

#The following code for SPY_code and SP_code is OPTIONAL. 
#The output is included below and is what we actually use to generate the dataframes.

SPY_code <- data.frame(paste0("case", SPY_Stats$Case, "_Stats_", SPY_Stats$Symbol, 
                              "<-data.frame(timestamp = ymd_hms(row.names(Case", SPY_Stats$Case, "$daEve)),",
                              "event_period = 1:156,",
                              "term =", SPY_Stats$term, ",",
                              SPY_Stats$Symbol,"_abr=Case", SPY_Stats$Case, "$abr$Ait.", SPY_Stats$Symbol, ",", 
                              SPY_Stats$Symbol, "_act=all_return_xts$", SPY_Stats$Symbol, "['", SPY_Stats$d1, "::", SPY_Stats$d2, "'],",
                              "SPY_act = all_return_xts$SPY['", SPY_Stats$d1, "::", SPY_Stats$d2, "'], Case =", SPY_Stats$Case,",row.names = NULL)"))

SP_code <- data.frame(paste0("case", SP_Stats$Case, "_Stats_", SP_Stats$Symbol, 
                             "<-data.frame(timestamp = ymd_hms(row.names(Case", SP_Stats$Case, "$daEve)),",
                             "event_period = 1:156,",
                             "term =", SP_Stats$term, ",",
                             SP_Stats$Symbol,"_abr=Case", SP_Stats$Case, "$abr$Ait.", SP_Stats$Symbol, ",", 
                             SP_Stats$Symbol, "_act=all_return_xts$", SP_Stats$Symbol, "['", SP_Stats$d1, "::", SP_Stats$d2, "'],",
                             "SP500_act = all_return_xts$SP['", SP_Stats$d1, "::", SP_Stats$d2, "'], Case =", SP_Stats$Case,", row.names = NULL)"))

#A sample output in a more readable format:
#SP/SPY_code generates a dataframe with one columns whose rows contain 
#text which we will use as the code to getnerate the dataframes for each case/symbol

#case6_Stats<-data.frame(timestamp=ymd_hms(row.names(Case6$daEve)),
#                        event_period = 1:156,
#                        KSU_abr=Case6$abr$Ait.KSU,
#                        KSU_act=all_return_xts$KSU['2000-04-17::2000-04-18'],
#                       SPY_act=all_return_xts$SPY['2000-04-17::2000-04-18'],
#                       Case=6,
#                        row.names=NULL),



#Output from SPY/SP_code with manual edits including:
#(1) Row numbers removed by copy and pasting the output into excel, then parsed with "text to columns" and removed whitespace with Word.
#(2) Removed Symbol suffix object names for cases without multiple symbols
#(3) For case 206, Calls/objects with Symbol "C" need to have a period at the end of it ("C.") Then removed the "." from the column name
#(4) For Case58, replaced Symbol calls to TRV with TRV.7041 to align with underlying dataframes.

dummy <- data.frame(timestamp = "09:30:00", event_period = 0, 0, 0, 0, 0, 0) 

#SPY_code output
case_list <- list(
  case2_Stats_CVX<-data.frame(timestamp=ymd_hms(row.names(Case2$daEve)),event_period=1:156,term=1999,CVX_abr=Case2$abr$Ait.CVX,CVX_act=all_return_xts$CVX['2000-03-06::2000-03-07'],SPY_act=all_return_xts$SPY['2000-03-06::2000-03-07'],Case=2,row.names=NULL),
  case2_Stats_XOM<-data.frame(timestamp=ymd_hms(row.names(Case2$daEve)),event_period=1:156,term=1999,XOM_abr=Case2$abr$Ait.XOM,XOM_act=all_return_xts$XOM['2000-03-06::2000-03-07'],SPY_act=all_return_xts$SPY['2000-03-06::2000-03-07'],Case=2,row.names=NULL),
  case5_Stats_ABT<-data.frame(timestamp=ymd_hms(row.names(Case5$daEve)),event_period=1:156,term=1999,ABT_abr=Case5$abr$Ait.ABT,ABT_act=all_return_xts$ABT['2000-04-03::2000-04-04'],SPY_act=all_return_xts$SPY['2000-04-03::2000-04-04'],Case=5,row.names=NULL),
  case5_Stats_BMY<-data.frame(timestamp=ymd_hms(row.names(Case5$daEve)),event_period=1:156,term=1999,BMY_abr=Case5$abr$Ait.BMY,BMY_act=all_return_xts$BMY['2000-04-03::2000-04-04'],SPY_act=all_return_xts$SPY['2000-04-03::2000-04-04'],Case=5,row.names=NULL),
  case5_Stats_JNJ<-data.frame(timestamp=ymd_hms(row.names(Case5$daEve)),event_period=1:156,term=1999,JNJ_abr=Case5$abr$Ait.JNJ,JNJ_act=all_return_xts$JNJ['2000-04-03::2000-04-04'],SPY_act=all_return_xts$SPY['2000-04-03::2000-04-04'],Case=5,row.names=NULL),
  case6_Stats_KSU<-data.frame(timestamp=ymd_hms(row.names(Case6$daEve)),event_period=1:156,term=1999,KSU_abr=Case6$abr$Ait.KSU,KSU_act=all_return_xts$KSU['2000-04-17::2000-04-18'],SPY_act=all_return_xts$SPY['2000-04-17::2000-04-18'],Case=6,row.names=NULL),
  case7_Stats_PLA<-data.frame(timestamp=ymd_hms(row.names(Case7$daEve)),event_period=1:156,term=1999,PLA_abr=Case7$abr$Ait.PLA,PLA_act=all_return_xts$PLA['2000-05-22::2000-05-23'],SPY_act=all_return_xts$SPY['2000-05-22::2000-05-23'],Case=7,row.names=NULL),
  case9_Stats_XLF<-data.frame(timestamp=ymd_hms(row.names(Case9$daEve)),event_period=1:156,term=1999,XLF_abr=Case9$abr$Ait.XLF,XLF_act=all_return_xts$XLF['2000-06-12::2000-06-13'],SPY_act=all_return_xts$SPY['2000-06-12::2000-06-13'],Case=9,row.names=NULL),
  case12_Stats_XLE<-data.frame(timestamp=ymd_hms(row.names(Case12$daEve)),event_period=1:156,term=2000,XLE_abr=Case12$abr$Ait.XLE,XLE_act=all_return_xts$XLE['2000-12-04::2000-12-05'],SPY_act=all_return_xts$SPY['2000-12-04::2000-12-05'],Case=12,row.names=NULL),
  case14_Stats_XLV<-data.frame(timestamp=ymd_hms(row.names(Case14$daEve)),event_period=1:156,term=2000,XLV_abr=Case14$abr$Ait.XLV,XLV_act=all_return_xts$XLV['2001-02-21::2001-02-22'],SPY_act=all_return_xts$SPY['2001-02-21::2001-02-22'],Case=14,row.names=NULL),
  case15_Stats_GM<-data.frame(timestamp=ymd_hms(row.names(Case15$daEve)),event_period=1:156,term=2000,GM_abr=Case15$abr$Ait.GM,GM_act=all_return_xts$GM['2001-02-27::2001-02-28'],SPY_act=all_return_xts$SPY['2001-02-27::2001-02-28'],Case=15,row.names=NULL),
  case15_Stats_HMC<-data.frame(timestamp=ymd_hms(row.names(Case15$daEve)),event_period=1:156,term=2000,HMC_abr=Case15$abr$Ait.HMC,HMC_act=all_return_xts$HMC['2001-02-27::2001-02-28'],SPY_act=all_return_xts$SPY['2001-02-27::2001-02-28'],Case=15,row.names=NULL),
  case17_Stats_CC<-data.frame(timestamp=ymd_hms(row.names(Case17$daEve)),event_period=1:156,term=2000,CC_abr=Case17$abr$Ait.CC,CC_act=all_return_xts$CC['2001-03-21::2001-03-22'],SPY_act=all_return_xts$SPY['2001-03-21::2001-03-22'],Case=17,row.names=NULL),
  case19_Stats_XLI<-data.frame(timestamp=ymd_hms(row.names(Case19$daEve)),event_period=1:156,term=2000,XLI_abr=Case19$abr$Ait.XLI,XLI_act=all_return_xts$XLI['2001-06-04::2001-06-05'],SPY_act=all_return_xts$SPY['2001-06-04::2001-06-05'],Case=19,row.names=NULL),
  case23_Stats_VGR<-data.frame(timestamp=ymd_hms(row.names(Case23$daEve)),event_period=1:156,term=2000,VGR_abr=Case23$abr$Ait.VGR,VGR_act=all_return_xts$VGR['2001-06-28::2001-06-29'],SPY_act=all_return_xts$SPY['2001-06-28::2001-06-29'],Case=23,row.names=NULL),
  case26_Stats_XLI<-data.frame(timestamp=ymd_hms(row.names(Case26$daEve)),event_period=1:156,term=2001,XLI_abr=Case26$abr$Ait.XLI,XLI_act=all_return_xts$XLI['2001-12-10::2001-12-11'],SPY_act=all_return_xts$SPY['2001-12-10::2001-12-11'],Case=26,row.names=NULL),
  case26_Stats_XLP<-data.frame(timestamp=ymd_hms(row.names(Case26$daEve)),event_period=1:156,term=2001,XLP_abr=Case26$abr$Ait.XLP,XLP_act=all_return_xts$XLP['2001-12-10::2001-12-11'],SPY_act=all_return_xts$SPY['2001-12-10::2001-12-11'],Case=26,row.names=NULL),
  case28_Stats_GWO<-data.frame(timestamp=ymd_hms(row.names(Case28$daEve)),event_period=1:156,term=2001,GWO_abr=Case28$abr$Ait.GWO,GWO_act=all_return_xts$GWO['2002-01-08::2002-01-09'],SPY_act=all_return_xts$SPY['2002-01-08::2002-01-09'],Case=28,row.names=NULL),
  case29_Stats_XLE<-data.frame(timestamp=ymd_hms(row.names(Case29$daEve)),event_period=1:156,term=2001,XLE_abr=Case29$abr$Ait.XLE,XLE_act=all_return_xts$XLE['2002-01-09::2002-01-10'],SPY_act=all_return_xts$SPY['2002-01-09::2002-01-10'],Case=29,row.names=NULL),
  case35_Stats_T<-data.frame(timestamp=ymd_hms(row.names(Case35$daEve)),event_period=1:156,term=2001,T_abr=Case35$abr$Ait.T,T_act=all_return_xts$T['2002-05-13::2002-05-14'],SPY_act=all_return_xts$SPY['2002-05-13::2002-05-14'],Case=35,row.names=NULL),
  case36_Stats_T<-data.frame(timestamp=ymd_hms(row.names(Case36$daEve)),event_period=1:156,term=2001,T_abr=Case36$abr$Ait.T,T_act=all_return_xts$T['2002-05-20::2002-05-21'],SPY_act=all_return_xts$SPY['2002-05-20::2002-05-21'],Case=36,row.names=NULL),
  case36_Stats_XLY<-data.frame(timestamp=ymd_hms(row.names(Case36$daEve)),event_period=1:156,term=2001,XLY_abr=Case36$abr$Ait.XLY,XLY_act=all_return_xts$XLY['2002-05-20::2002-05-21'],SPY_act=all_return_xts$SPY['2002-05-20::2002-05-21'],Case=36,row.names=NULL),
  case44_Stats_WLB<-data.frame(timestamp=ymd_hms(row.names(Case44$daEve)),event_period=1:156,term=2002,WLB_abr=Case44$abr$Ait.WLB,WLB_act=all_return_xts$WLB['2003-01-15::2003-01-16'],SPY_act=all_return_xts$SPY['2003-01-15::2003-01-16'],Case=44,row.names=NULL),
  case47_Stats_NSC<-data.frame(timestamp=ymd_hms(row.names(Case47$daEve)),event_period=1:156,term=2002,NSC_abr=Case47$abr$Ait.NSC,NSC_act=all_return_xts$NSC['2003-03-10::2003-03-11'],SPY_act=all_return_xts$SPY['2003-03-10::2003-03-11'],Case=47,row.names=NULL),
  case47_Stats_UNP<-data.frame(timestamp=ymd_hms(row.names(Case47$daEve)),event_period=1:156,term=2002,UNP_abr=Case47$abr$Ait.UNP,UNP_act=all_return_xts$UNP['2003-03-10::2003-03-11'],SPY_act=all_return_xts$SPY['2003-03-10::2003-03-11'],Case=47,row.names=NULL),
  case47_Stats_XLI<-data.frame(timestamp=ymd_hms(row.names(Case47$daEve)),event_period=1:156,term=2002,XLI_abr=Case47$abr$Ait.XLI,XLI_act=all_return_xts$XLI['2003-03-10::2003-03-11'],SPY_act=all_return_xts$SPY['2003-03-10::2003-03-11'],Case=47,row.names=NULL),
  case48_Stats_PHS<-data.frame(timestamp=ymd_hms(row.names(Case48$daEve)),event_period=1:156,term=2002,PHS_abr=Case48$abr$Ait.PHS,PHS_act=all_return_xts$PHS['2003-04-07::2003-04-08'],SPY_act=all_return_xts$SPY['2003-04-07::2003-04-08'],Case=48,row.names=NULL),
  case51_Stats_AZN<-data.frame(timestamp=ymd_hms(row.names(Case51$daEve)),event_period=1:156,term=2002,AZN_abr=Case51$abr$Ait.AZN,AZN_act=all_return_xts$AZN['2003-05-19::2003-05-20'],SPY_act=all_return_xts$SPY['2003-05-19::2003-05-20'],Case=51,row.names=NULL),
  case51_Stats_BMY<-data.frame(timestamp=ymd_hms(row.names(Case51$daEve)),event_period=1:156,term=2002,BMY_abr=Case51$abr$Ait.BMY,BMY_act=all_return_xts$BMY['2003-05-19::2003-05-20'],SPY_act=all_return_xts$SPY['2003-05-19::2003-05-20'],Case=51,row.names=NULL),
  case51_Stats_PFE<-data.frame(timestamp=ymd_hms(row.names(Case51$daEve)),event_period=1:156,term=2002,PFE_abr=Case51$abr$Ait.PFE,PFE_act=all_return_xts$PFE['2003-05-19::2003-05-20'],SPY_act=all_return_xts$SPY['2003-05-19::2003-05-20'],Case=51,row.names=NULL),
  case51_Stats_XLV<-data.frame(timestamp=ymd_hms(row.names(Case51$daEve)),event_period=1:156,term=2002,XLV_abr=Case51$abr$Ait.XLV,XLV_act=all_return_xts$XLV['2003-05-19::2003-05-20'],SPY_act=all_return_xts$SPY['2003-05-19::2003-05-20'],Case=51,row.names=NULL),
  case56_Stats_XLP<-data.frame(timestamp=ymd_hms(row.names(Case56$daEve)),event_period=1:156,term=2002,XLP_abr=Case56$abr$Ait.XLP,XLP_act=all_return_xts$XLP['2003-06-09::2003-06-10'],SPY_act=all_return_xts$SPY['2003-06-09::2003-06-10'],Case=56,row.names=NULL),
  case58_Stats_TRV<-data.frame(timestamp=ymd_hms(row.names(Case58$daEve)),event_period=1:156,term=2002,TRV_abr=Case58$abr$Ait.TRV.7041,TRV_act=all_return_xts$TRV.7041['2003-06-23::2003-06-24'],SPY_act=all_return_xts$SPY['2003-06-23::2003-06-24'],Case=58,row.names=NULL),
  case60_Stats_XLY<-data.frame(timestamp=ymd_hms(row.names(Case60$daEve)),event_period=1:156,term=2002,XLY_abr=Case60$abr$Ait.XLY,XLY_act=all_return_xts$XLY['2003-06-26::2003-06-27'],SPY_act=all_return_xts$SPY['2003-06-26::2003-06-27'],Case=60,row.names=NULL),
  case61_Stats_XLF<-data.frame(timestamp=ymd_hms(row.names(Case61$daEve)),event_period=1:156,term=2003,XLF_abr=Case61$abr$Ait.XLF,XLF_act=all_return_xts$XLF['2003-12-02::2003-12-03'],SPY_act=all_return_xts$SPY['2003-12-02::2003-12-03'],Case=61,row.names=NULL),
  case64_Stats_V<-data.frame(timestamp=ymd_hms(row.names(Case64$daEve)),event_period=1:156,term=2003,V_abr=Case64$abr$Ait.V,V_act=all_return_xts$V['2004-04-21::2004-04-22'],SPY_act=all_return_xts$SPY['2004-04-21::2004-04-22'],Case=64,row.names=NULL),
  case66_Stats_XLI<-data.frame(timestamp=ymd_hms(row.names(Case66$daEve)),event_period=1:156,term=2003,XLI_abr=Case66$abr$Ait.XLI,XLI_act=all_return_xts$XLI['2004-05-03::2004-05-04'],SPY_act=all_return_xts$SPY['2004-05-03::2004-05-04'],Case=66,row.names=NULL),
  case67_Stats_UNH<-data.frame(timestamp=ymd_hms(row.names(Case67$daEve)),event_period=1:156,term=2003,UNH_abr=Case67$abr$Ait.UNH,UNH_act=all_return_xts$UNH['2004-06-21::2004-06-22'],SPY_act=all_return_xts$SPY['2004-06-21::2004-06-22'],Case=67,row.names=NULL),
  case69_Stats_CVX<-data.frame(timestamp=ymd_hms(row.names(Case69$daEve)),event_period=1:156,term=2003,CVX_abr=Case69$abr$Ait.CVX,CVX_act=all_return_xts$CVX['2004-06-24::2004-06-25'],SPY_act=all_return_xts$SPY['2004-06-24::2004-06-25'],Case=69,row.names=NULL),
  case69_Stats_HES<-data.frame(timestamp=ymd_hms(row.names(Case69$daEve)),event_period=1:156,term=2003,HES_abr=Case69$abr$Ait.HES,HES_act=all_return_xts$HES['2004-06-24::2004-06-25'],SPY_act=all_return_xts$SPY['2004-06-24::2004-06-25'],Case=69,row.names=NULL),
  case69_Stats_XOM<-data.frame(timestamp=ymd_hms(row.names(Case69$daEve)),event_period=1:156,term=2003,XOM_abr=Case69$abr$Ait.XOM,XOM_act=all_return_xts$XOM['2004-06-24::2004-06-25'],SPY_act=all_return_xts$SPY['2004-06-24::2004-06-25'],Case=69,row.names=NULL),
  case71_Stats_XLI<-data.frame(timestamp=ymd_hms(row.names(Case71$daEve)),event_period=1:156,term=2004,XLI_abr=Case71$abr$Ait.XLI,XLI_act=all_return_xts$XLI['2004-12-13::2004-12-14'],SPY_act=all_return_xts$SPY['2004-12-13::2004-12-14'],Case=71,row.names=NULL),
  case74_Stats_XLP<-data.frame(timestamp=ymd_hms(row.names(Case74$daEve)),event_period=1:156,term=2004,XLP_abr=Case74$abr$Ait.XLP,XLP_act=all_return_xts$XLP['2005-05-16::2005-05-17'],SPY_act=all_return_xts$SPY['2005-05-16::2005-05-17'],Case=74,row.names=NULL),
  case77_Stats_MRK<-data.frame(timestamp=ymd_hms(row.names(Case77$daEve)),event_period=1:156,term=2004,MRK_abr=Case77$abr$Ait.MRK,MRK_act=all_return_xts$MRK['2005-06-13::2005-06-14'],SPY_act=all_return_xts$SPY['2005-06-13::2005-06-14'],Case=77,row.names=NULL),
  case78_Stats_YRCW<-data.frame(timestamp=ymd_hms(row.names(Case78$daEve)),event_period=1:156,term=2004,YRCW_abr=Case78$abr$Ait.YRCW,YRCW_act=all_return_xts$YRCW['2005-06-20::2005-06-21'],SPY_act=all_return_xts$SPY['2005-06-20::2005-06-21'],Case=78,row.names=NULL),
  case86_Stats_ITW<-data.frame(timestamp=ymd_hms(row.names(Case86$daEve)),event_period=1:156,term=2005,ITW_abr=Case86$abr$Ait.ITW,ITW_act=all_return_xts$ITW['2006-03-01::2006-03-02'],SPY_act=all_return_xts$SPY['2006-03-01::2006-03-02'],Case=86,row.names=NULL),
  case87_Stats_MER<-data.frame(timestamp=ymd_hms(row.names(Case87$daEve)),event_period=1:156,term=2005,MER_abr=Case87$abr$Ait.MER,MER_act=all_return_xts$MER['2006-03-21::2006-03-22'],SPY_act=all_return_xts$SPY['2006-03-21::2006-03-22'],Case=87,row.names=NULL),
  case88_Stats_EBAY<-data.frame(timestamp=ymd_hms(row.names(Case88$daEve)),event_period=1:156,term=2005,EBAY_abr=Case88$abr$Ait.EBAY,EBAY_act=all_return_xts$EBAY['2006-05-15::2006-05-16'],SPY_act=all_return_xts$SPY['2006-05-15::2006-05-16'],Case=88,row.names=NULL),
  case93_Stats_MO<-data.frame(timestamp=ymd_hms(row.names(Case93$daEve)),event_period=1:156,term=2006,MO_abr=Case93$abr$Ait.MO,MO_act=all_return_xts$MO['2007-02-20::2007-02-21'],SPY_act=all_return_xts$SPY['2007-02-20::2007-02-21'],Case=93,row.names=NULL),
  case99_Stats_GLBC<-data.frame(timestamp=ymd_hms(row.names(Case99$daEve)),event_period=1:156,term=2006,GLBC_abr=Case99$abr$Ait.GLBC,GLBC_act=all_return_xts$GLBC['2007-04-17::2007-04-18'],SPY_act=all_return_xts$SPY['2007-04-17::2007-04-18'],Case=99,row.names=NULL),
  case99_Stats_VOX<-data.frame(timestamp=ymd_hms(row.names(Case99$daEve)),event_period=1:156,term=2006,VOX_abr=Case99$abr$Ait.VOX,VOX_act=all_return_xts$VOX['2007-04-17::2007-04-18'],SPY_act=all_return_xts$SPY['2007-04-17::2007-04-18'],Case=99,row.names=NULL),
  case100_Stats_TFX<-data.frame(timestamp=ymd_hms(row.names(Case100$daEve)),event_period=1:156,term=2006,TFX_abr=Case100$abr$Ait.TFX,TFX_act=all_return_xts$TFX['2007-04-30::2007-05-01'],SPY_act=all_return_xts$SPY['2007-04-30::2007-05-01'],Case=100,row.names=NULL),
  case100_Stats_VOX<-data.frame(timestamp=ymd_hms(row.names(Case100$daEve)),event_period=1:156,term=2006,VOX_abr=Case100$abr$Ait.VOX,VOX_act=all_return_xts$VOX['2007-04-30::2007-05-01'],SPY_act=all_return_xts$SPY['2007-04-30::2007-05-01'],Case=100,row.names=NULL),
  case111_Stats_XLV<-data.frame(timestamp=ymd_hms(row.names(Case111$daEve)),event_period=1:156,term=2007,XLV_abr=Case111$abr$Ait.XLV,XLV_act=all_return_xts$XLV['2008-02-20::2008-02-21'],SP500_act=all_return_xts$SP['2008-02-20::2008-02-21'],Case=111,row.names=NULL),
  case120_Stats_S<-data.frame(timestamp=ymd_hms(row.names(Case120$daEve)),event_period=1:156,term=2007,S_abr=Case120$abr$Ait.S,S_act=all_return_xts$S['2008-06-23::2008-06-24'],SP500_act=all_return_xts$SP['2008-06-23::2008-06-24'],Case=120,row.names=NULL),
  case120_Stats_XLY<-data.frame(timestamp=ymd_hms(row.names(Case120$daEve)),event_period=1:156,term=2007,XLY_abr=Case120$abr$Ait.XLY,XLY_act=all_return_xts$XLY['2008-06-23::2008-06-24'],SP500_act=all_return_xts$SP['2008-06-23::2008-06-24'],Case=120,row.names=NULL),
  case123_Stats_MO<-data.frame(timestamp=ymd_hms(row.names(Case123$daEve)),event_period=1:156,term=2008,MO_abr=Case123$abr$Ait.MO,MO_act=all_return_xts$MO['2008-12-15::2008-12-16'],SP500_act=all_return_xts$SP['2008-12-15::2008-12-16'],Case=123,row.names=NULL),
  case124_Stats_DD<-data.frame(timestamp=ymd_hms(row.names(Case124$daEve)),event_period=1:156,term=2008,DD_abr=Case124$abr$Ait.DD,DD_act=all_return_xts$DD['2009-01-26::2009-01-27'],SP500_act=all_return_xts$SP['2009-01-26::2009-01-27'],Case=124,row.names=NULL),
  case126_Stats_PFE<-data.frame(timestamp=ymd_hms(row.names(Case126$daEve)),event_period=1:156,term=2008,PFE_abr=Case126$abr$Ait.PFE,PFE_act=all_return_xts$PFE['2009-03-04::2009-03-05'],SP500_act=all_return_xts$SP['2009-03-04::2009-03-05'],Case=126,row.names=NULL),
  case131_Stats_CNA<-data.frame(timestamp=ymd_hms(row.names(Case131$daEve)),event_period=1:156,term=2008,CNA_abr=Case131$abr$Ait.CNA,CNA_act=all_return_xts$CNA['2009-06-18::2009-06-19'],SP500_act=all_return_xts$SP['2009-06-18::2009-06-19'],Case=131,row.names=NULL),
  case132_Stats_CDE<-data.frame(timestamp=ymd_hms(row.names(Case132$daEve)),event_period=1:156,term=2008,CDE_abr=Case132$abr$Ait.CDE,CDE_act=all_return_xts$CDE['2009-06-22::2009-06-23'],SP500_act=all_return_xts$SP['2009-06-22::2009-06-23'],Case=132,row.names=NULL),
  case132_Stats_CDM<-data.frame(timestamp=ymd_hms(row.names(Case132$daEve)),event_period=1:156,term=2008,CDM_abr=Case132$abr$Ait.CDM,CDM_act=all_return_xts$CDM['2009-06-22::2009-06-23'],SP500_act=all_return_xts$SP['2009-06-22::2009-06-23'],Case=132,row.names=NULL),
  case144_Stats_XLI<-data.frame(timestamp=ymd_hms(row.names(Case144$daEve)),event_period=1:156,term=2009,XLI_abr=Case144$abr$Ait.XLI,XLI_act=all_return_xts$XLI['2010-04-21::2010-04-22'],SP500_act=all_return_xts$SP['2010-04-21::2010-04-22'],Case=144,row.names=NULL),
  case149_Stats_AXP<-data.frame(timestamp=ymd_hms(row.names(Case149$daEve)),event_period=1:156,term=2010,AXP_abr=Case149$abr$Ait.AXP,AXP_act=all_return_xts$AXP['2011-01-24::2011-01-25'],SP500_act=all_return_xts$SP['2011-01-24::2011-01-25'],Case=149,row.names=NULL),
  case149_Stats_WEX<-data.frame(timestamp=ymd_hms(row.names(Case149$daEve)),event_period=1:156,term=2010,WEX_abr=Case149$abr$Ait.WEX,WEX_act=all_return_xts$WEX['2011-01-24::2011-01-25'],SP500_act=all_return_xts$SP['2011-01-24::2011-01-25'],Case=149,row.names=NULL),
  case152_Stats_GM<-data.frame(timestamp=ymd_hms(row.names(Case152$daEve)),event_period=1:156,term=2010,GM_abr=Case152$abr$Ait.GM,GM_act=all_return_xts$GM['2011-02-23::2011-02-24'],SP500_act=all_return_xts$SP['2011-02-23::2011-02-24'],Case=152,row.names=NULL),
  case153_Stats_VOX<-data.frame(timestamp=ymd_hms(row.names(Case153$daEve)),event_period=1:156,term=2010,VOX_abr=Case153$abr$Ait.VOX,VOX_act=all_return_xts$VOX['2011-03-01::2011-03-02'],SP500_act=all_return_xts$SP['2011-03-01::2011-03-02'],Case=153,row.names=NULL),
  case153_Stats_XTL<-data.frame(timestamp=ymd_hms(row.names(Case153$daEve)),event_period=1:156,term=2010,XTL_abr=Case153$abr$Ait.XTL,XTL_act=all_return_xts$XTL['2011-03-01::2011-03-02'],SP500_act=all_return_xts$SP['2011-03-01::2011-03-02'],Case=153,row.names=NULL),
  case154_Stats_XPH<-data.frame(timestamp=ymd_hms(row.names(Case154$daEve)),event_period=1:156,term=2010,XPH_abr=Case154$abr$Ait.XPH,XPH_act=all_return_xts$XPH['2011-03-29::2011-03-30'],SP500_act=all_return_xts$SP['2011-03-29::2011-03-30'],Case=154,row.names=NULL),
  case155_Stats_XTL<-data.frame(timestamp=ymd_hms(row.names(Case155$daEve)),event_period=1:156,term=2010,XTL_abr=Case155$abr$Ait.XTL,XTL_act=all_return_xts$XTL['2011-04-27::2011-04-28'],SP500_act=all_return_xts$SP['2011-04-27::2011-04-28'],Case=155,row.names=NULL),
  case156_Stats_XLF<-data.frame(timestamp=ymd_hms(row.names(Case156$daEve)),event_period=1:156,term=2010,XLF_abr=Case156$abr$Ait.XLF,XLF_act=all_return_xts$XLF['2011-05-16::2011-05-17'],SP500_act=all_return_xts$SP['2011-05-16::2011-05-17'],Case=156,row.names=NULL),
  case158_Stats_JAH<-data.frame(timestamp=ymd_hms(row.names(Case158$daEve)),event_period=1:156,term=2010,JAH_abr=Case158$abr$Ait.JAH,JAH_act=all_return_xts$JAH['2011-05-31::2011-06-01'],SP500_act=all_return_xts$SP['2011-05-31::2011-06-01'],Case=158,row.names=NULL),
  case158_Stats_XLY<-data.frame(timestamp=ymd_hms(row.names(Case158$daEve)),event_period=1:156,term=2010,XLY_abr=Case158$abr$Ait.XLY,XLY_act=all_return_xts$XLY['2011-05-31::2011-06-01'],SP500_act=all_return_xts$SP['2011-05-31::2011-06-01'],Case=158,row.names=NULL),
  case159_Stats_XES<-data.frame(timestamp=ymd_hms(row.names(Case159$daEve)),event_period=1:156,term=2010,XES_abr=Case159$abr$Ait.XES,XES_act=all_return_xts$XES['2011-06-06::2011-06-07'],SP500_act=all_return_xts$SP['2011-06-06::2011-06-07'],Case=159,row.names=NULL),
  case163_Stats_XLY<-data.frame(timestamp=ymd_hms(row.names(Case163$daEve)),event_period=1:156,term=2010,XLY_abr=Case163$abr$Ait.XLY,XLY_act=all_return_xts$XLY['2011-06-20::2011-06-21'],SP500_act=all_return_xts$SP['2011-06-20::2011-06-21'],Case=163,row.names=NULL),
  case166_Stats_GT<-data.frame(timestamp=ymd_hms(row.names(Case166$daEve)),event_period=1:156,term=2010,GT_abr=Case166$abr$Ait.GT,GT_act=all_return_xts$GT['2011-06-27::2011-06-28'],SP500_act=all_return_xts$SP['2011-06-27::2011-06-28'],Case=166,row.names=NULL),
  case171_Stats_CS<-data.frame(timestamp=ymd_hms(row.names(Case171$daEve)),event_period=1:156,term=2011,CS_abr=Case171$abr$Ait.CS,CS_act=all_return_xts$CS['2012-03-26::2012-03-27'],SP500_act=all_return_xts$SP['2012-03-26::2012-03-27'],Case=171,row.names=NULL),
  case171_Stats_DB<-data.frame(timestamp=ymd_hms(row.names(Case171$daEve)),event_period=1:156,term=2011,DB_abr=Case171$abr$Ait.DB,DB_act=all_return_xts$DB['2012-03-26::2012-03-27'],SP500_act=all_return_xts$SP['2012-03-26::2012-03-27'],Case=171,row.names=NULL),
  case171_Stats_UBS<-data.frame(timestamp=ymd_hms(row.names(Case171$daEve)),event_period=1:156,term=2011,UBS_abr=Case171$abr$Ait.UBS,UBS_act=all_return_xts$UBS['2012-03-26::2012-03-27'],SP500_act=all_return_xts$SP['2012-03-26::2012-03-27'],Case=171,row.names=NULL),
  case172_Stats_XLV<-data.frame(timestamp=ymd_hms(row.names(Case172$daEve)),event_period=1:156,term=2011,XLV_abr=Case172$abr$Ait.XLV,XLV_act=all_return_xts$XLV['2012-04-17::2012-04-18'],SP500_act=all_return_xts$SP['2012-04-17::2012-04-18'],Case=172,row.names=NULL),
  case173_Stats_STN<-data.frame(timestamp=ymd_hms(row.names(Case173$daEve)),event_period=1:156,term=2011,STN_abr=Case173$abr$Ait.STN,STN_act=all_return_xts$STN['2012-06-18::2012-06-19'],SP500_act=all_return_xts$SP['2012-06-18::2012-06-19'],Case=173,row.names=NULL),
  case174_Stats_XHE<-data.frame(timestamp=ymd_hms(row.names(Case174$daEve)),event_period=1:156,term=2011,XHE_abr=Case174$abr$Ait.XHE,XHE_act=all_return_xts$XHE['2012-06-18::2012-06-19'],SP500_act=all_return_xts$SP['2012-06-18::2012-06-19'],Case=174,row.names=NULL),
  case175_Stats_KMI<-data.frame(timestamp=ymd_hms(row.names(Case175$daEve)),event_period=1:156,term=2011,KMI_abr=Case175$abr$Ait.KMI,KMI_act=all_return_xts$KMI['2012-06-21::2012-06-22'],SP500_act=all_return_xts$SP['2012-06-21::2012-06-22'],Case=175,row.names=NULL),
  case175_Stats_XLE<-data.frame(timestamp=ymd_hms(row.names(Case175$daEve)),event_period=1:156,term=2011,XLE_abr=Case175$abr$Ait.XLE,XLE_act=all_return_xts$XLE['2012-06-21::2012-06-22'],SP500_act=all_return_xts$SP['2012-06-21::2012-06-22'],Case=175,row.names=NULL),
  case177_Stats_AET<-data.frame(timestamp=ymd_hms(row.names(Case177$daEve)),event_period=1:156,term=2011,AET_abr=Case177$abr$Ait.AET,AET_act=all_return_xts$AET['2012-06-28::2012-06-29'],SP500_act=all_return_xts$SP['2012-06-28::2012-06-29'],Case=177,row.names=NULL),
  case177_Stats_CI<-data.frame(timestamp=ymd_hms(row.names(Case177$daEve)),event_period=1:156,term=2011,CI_abr=Case177$abr$Ait.CI,CI_act=all_return_xts$CI['2012-06-28::2012-06-29'],SP500_act=all_return_xts$SP['2012-06-28::2012-06-29'],Case=177,row.names=NULL),
  case177_Stats_WLP<-data.frame(timestamp=ymd_hms(row.names(Case177$daEve)),event_period=1:156,term=2011,WLP_abr=Case177$abr$Ait.WLP,WLP_act=all_return_xts$WLP['2012-06-28::2012-06-29'],SP500_act=all_return_xts$SP['2012-06-28::2012-06-29'],Case=177,row.names=NULL),
  case177_Stats_HCA<-data.frame(timestamp=ymd_hms(row.names(Case177$daEve)),event_period=1:156,term=2011,HCA_abr=Case177$abr$Ait.HCA,HCA_act=all_return_xts$HCA['2012-06-28::2012-06-29'],SP500_act=all_return_xts$SP['2012-06-28::2012-06-29'],Case=177,row.names=NULL),
  case177_Stats_HNT<-data.frame(timestamp=ymd_hms(row.names(Case177$daEve)),event_period=1:156,term=2011,HNT_abr=Case177$abr$Ait.HNT,HNT_act=all_return_xts$HNT['2012-06-28::2012-06-29'],SP500_act=all_return_xts$SP['2012-06-28::2012-06-29'],Case=177,row.names=NULL),
  case177_Stats_HUM<-data.frame(timestamp=ymd_hms(row.names(Case177$daEve)),event_period=1:156,term=2011,HUM_abr=Case177$abr$Ait.HUM,HUM_act=all_return_xts$HUM['2012-06-28::2012-06-29'],SP500_act=all_return_xts$SP['2012-06-28::2012-06-29'],Case=177,row.names=NULL),
  case177_Stats_MGLN<-data.frame(timestamp=ymd_hms(row.names(Case177$daEve)),event_period=1:156,term=2011,MGLN_abr=Case177$abr$Ait.MGLN,MGLN_act=all_return_xts$MGLN['2012-06-28::2012-06-29'],SP500_act=all_return_xts$SP['2012-06-28::2012-06-29'],Case=177,row.names=NULL),
  case180_Stats_PSO<-data.frame(timestamp=ymd_hms(row.names(Case180$daEve)),event_period=1:156,term=2012,PSO_abr=Case180$abr$Ait.PSO,PSO_act=all_return_xts$PSO['2013-03-19::2013-03-20'],SP500_act=all_return_xts$SP['2013-03-19::2013-03-20'],Case=180,row.names=NULL),
  case184_Stats_XLE<-data.frame(timestamp=ymd_hms(row.names(Case184$daEve)),event_period=1:156,term=2012,XLE_abr=Case184$abr$Ait.XLE,XLE_act=all_return_xts$XLE['2013-04-17::2013-04-18'],SP500_act=all_return_xts$SP['2013-04-17::2013-04-18'],Case=184,row.names=NULL),
  case184_Stats_XOP<-data.frame(timestamp=ymd_hms(row.names(Case184$daEve)),event_period=1:156,term=2012,XOP_abr=Case184$abr$Ait.XOP,XOP_act=all_return_xts$XOP['2013-04-17::2013-04-18'],SP500_act=all_return_xts$SP['2013-04-17::2013-04-18'],Case=184,row.names=NULL),
  case185_Stats_XLP<-data.frame(timestamp=ymd_hms(row.names(Case185$daEve)),event_period=1:156,term=2012,XLP_abr=Case185$abr$Ait.XLP,XLP_act=all_return_xts$XLP['2013-05-13::2013-05-14'],SP500_act=all_return_xts$SP['2013-05-13::2013-05-14'],Case=185,row.names=NULL),
  case187_Stats_MYGN<-data.frame(timestamp=ymd_hms(row.names(Case187$daEve)),event_period=1:156,term=2012,MYGN_abr=Case187$abr$Ait.MYGN,MYGN_act=all_return_xts$MYGN['2013-06-13::2013-06-14'],SP500_act=all_return_xts$SP['2013-06-13::2013-06-14'],Case=187,row.names=NULL),
  case188_Stats_ACT<-data.frame(timestamp=ymd_hms(row.names(Case188$daEve)),event_period=1:156,term=2012,ACT_abr=Case188$abr$Ait.ACT,ACT_act=all_return_xts$ACT['2013-06-17::2013-06-18'],SP500_act=all_return_xts$SP['2013-06-17::2013-06-18'],Case=188,row.names=NULL),
  case188_Stats_XPH<-data.frame(timestamp=ymd_hms(row.names(Case188$daEve)),event_period=1:156,term=2012,XPH_abr=Case188$abr$Ait.XPH,XPH_act=all_return_xts$XPH['2013-06-17::2013-06-18'],SP500_act=all_return_xts$SP['2013-06-17::2013-06-18'],Case=188,row.names=NULL),
  case189_Stats_MA<-data.frame(timestamp=ymd_hms(row.names(Case189$daEve)),event_period=1:156,term=2012,MA_abr=Case189$abr$Ait.MA,MA_act=all_return_xts$MA['2013-06-20::2013-06-21'],SP500_act=all_return_xts$SP['2013-06-20::2013-06-21'],Case=189,row.names=NULL),
  case191_Stats_F<-data.frame(timestamp=ymd_hms(row.names(Case191$daEve)),event_period=1:156,term=2013,F_abr=Case191$abr$Ait.F,F_act=all_return_xts$F['2013-12-02::2013-12-03'],SP500_act=all_return_xts$SP['2013-12-02::2013-12-03'],Case=191,row.names=NULL),
  case192_Stats_VOX<-data.frame(timestamp=ymd_hms(row.names(Case192$daEve)),event_period=1:156,term=2013,VOX_abr=Case192$abr$Ait.VOX,VOX_act=all_return_xts$VOX['2013-12-10::2013-12-11'],SP500_act=all_return_xts$SP['2013-12-10::2013-12-11'],Case=192,row.names=NULL),
  case192_Stats_XTL<-data.frame(timestamp=ymd_hms(row.names(Case192$daEve)),event_period=1:156,term=2013,XTL_abr=Case192$abr$Ait.XTL,XTL_act=all_return_xts$XTL['2013-12-10::2013-12-11'],SP500_act=all_return_xts$SP['2013-12-10::2013-12-11'],Case=192,row.names=NULL),
  case194_Stats_XLK<-data.frame(timestamp=ymd_hms(row.names(Case194$daEve)),event_period=1:156,term=2013,XLK_abr=Case194$abr$Ait.XLK,XLK_act=all_return_xts$XLK['2014-01-14::2014-01-15'],SP500_act=all_return_xts$SP['2014-01-14::2014-01-15'],Case=194,row.names=NULL),
  case200_Stats_LLNW<-data.frame(timestamp=ymd_hms(row.names(Case200$daEve)),event_period=1:156,term=2013,LLNW_abr=Case200$abr$Ait.LLNW,LLNW_act=all_return_xts$LLNW['2014-06-02::2014-06-03'],SP500_act=all_return_xts$SP['2014-06-02::2014-06-03'],Case=200,row.names=NULL),
  case202_Stats_CTS<-data.frame(timestamp=ymd_hms(row.names(Case202$daEve)),event_period=1:156,term=2013,CTS_abr=Case202$abr$Ait.CTS,CTS_act=all_return_xts$CTS['2014-06-09::2014-06-10'],SP500_act=all_return_xts$SP['2014-06-09::2014-06-10'],Case=202,row.names=NULL),
  case202_Stats_SANM<-data.frame(timestamp=ymd_hms(row.names(Case202$daEve)),event_period=1:156,term=2013,SANM_abr=Case202$abr$Ait.SANM,SANM_act=all_return_xts$SANM['2014-06-09::2014-06-10'],SP500_act=all_return_xts$SP['2014-06-09::2014-06-10'],Case=202,row.names=NULL),
  case206_Stats_C<-data.frame(timestamp=ymd_hms(row.names(Case206$daEve)),event_period=1:156,term=2013,C_abr=Case206$abr$Ait.C.,C_act=all_return_xts$C.['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=206,row.names=NULL),
  case207_Stats_XES<-data.frame(timestamp=ymd_hms(row.names(Case207$daEve)),event_period=1:156,term=2013,XES_abr=Case207$abr$Ait.XES,XES_act=all_return_xts$XES['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=207,row.names=NULL),
  case207_Stats_XLE<-data.frame(timestamp=ymd_hms(row.names(Case207$daEve)),event_period=1:156,term=2013,XLE_abr=Case207$abr$Ait.XLE,XLE_act=all_return_xts$XLE['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=207,row.names=NULL),
  case207_Stats_XOP<-data.frame(timestamp=ymd_hms(row.names(Case207$daEve)),event_period=1:156,term=2013,XOP_abr=Case207$abr$Ait.XOP,XOP_act=all_return_xts$XOP['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=207,row.names=NULL),
  case208_Stats_HAL<-data.frame(timestamp=ymd_hms(row.names(Case208$daEve)),event_period=1:156,term=2013,HAL_abr=Case208$abr$Ait.HAL,HAL_act=all_return_xts$HAL['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=208,row.names=NULL),
  case208_Stats_BHI<-data.frame(timestamp=ymd_hms(row.names(Case208$daEve)),event_period=1:156,term=2013,BHI_abr=Case208$abr$Ait.BHI,BHI_act=all_return_xts$BHI['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=208,row.names=NULL),
  case208_Stats_XES<-data.frame(timestamp=ymd_hms(row.names(Case208$daEve)),event_period=1:156,term=2013,XES_abr=Case208$abr$Ait.XES,XES_act=all_return_xts$XES['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=208,row.names=NULL),
  case208_Stats_XLE<-data.frame(timestamp=ymd_hms(row.names(Case208$daEve)),event_period=1:156,term=2013,XLE_abr=Case208$abr$Ait.XLE,XLE_act=all_return_xts$XLE['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=208,row.names=NULL),
  case208_Stats_XOP<-data.frame(timestamp=ymd_hms(row.names(Case208$daEve)),event_period=1:156,term=2013,XOP_abr=Case208$abr$Ait.XOP,XOP_act=all_return_xts$XOP['2014-06-23::2014-06-24'],SP500_act=all_return_xts$SP['2014-06-23::2014-06-24'],Case=208,row.names=NULL),
  case209_Stats_CBS<-data.frame(timestamp=ymd_hms(row.names(Case209$daEve)),event_period=1:156,term=2013,CBS_abr=Case209$abr$Ait.CBS,CBS_act=all_return_xts$CBS['2014-06-25::2014-06-26'],SP500_act=all_return_xts$SP['2014-06-25::2014-06-26'],Case=209,row.names=NULL),
  case209_Stats_SBGI<-data.frame(timestamp=ymd_hms(row.names(Case209$daEve)),event_period=1:156,term=2013,SBGI_abr=Case209$abr$Ait.SBGI,SBGI_act=all_return_xts$SBGI['2014-06-25::2014-06-26'],SP500_act=all_return_xts$SP['2014-06-25::2014-06-26'],Case=209,row.names=NULL),
  case209_Stats_FOXA<-data.frame(timestamp=ymd_hms(row.names(Case209$daEve)),event_period=1:156,term=2013,FOXA_abr=Case209$abr$Ait.FOXA,FOXA_act=all_return_xts$FOXA['2014-06-25::2014-06-26'],SP500_act=all_return_xts$SP['2014-06-25::2014-06-26'],Case=209,row.names=NULL),
  case211_Stats_S<-data.frame(timestamp=ymd_hms(row.names(Case211$daEve)),event_period=1:156,term=2013,S_abr=Case211$abr$Ait.S,S_act=all_return_xts$S['2014-06-25::2014-06-26'],SP500_act=all_return_xts$SP['2014-06-25::2014-06-26'],Case=211,row.names=NULL))

#Rename TRV in case 58 to remove suffix ".7041" 
names(case_list[[33]]) <- c("timestamp", "event_period", "term", "TRV_abr", "TRV", "SPY", "Case")

#Rename C. in case 206 to remove suffix "."
names(case_list[[107]]) <- c("timestamp", "event_period", "term", "C_abr", "C", "SPY", "Case")


all_Stats <- melt(case_list, id.vars = c("timestamp", "Case", "term", "event_period"))
all_Stats <- all_Stats %>%
  select(-L1) #drop the column that identifies which dataframe in the list the row came from.

#Remove duplicate rows. We created multiple dataframes for cases with more than one stock,
#both of which contained SP/SPY data resulting in duplicates of the SP/SPY data.
all_Stats <- unique(all_Stats)

#update column names
names(all_Stats) <- c("timestamp", "case", "term", "event_period", "ticker", "value")

#Separate abnormal returns (suffix "_abr") from actual returns (no suffix).
#Convert the ticker symbol to character so we can use the "_abr" suffix to tell R which rows to pull out
all_Stats$ticker <- as.character(all_Stats$ticker)

#============================
#Clean and prep "abr_returns"
#============================
#Store rows with _abr suffix
abr_returns <- filter(all_Stats, grepl('_abr', ticker))

#Drop the "_abr" suffix from tickers
abr_returns <- abr_returns %>%
  separate(ticker, into = c("ticker", "suffix"), extra="drop") %>%
  select(-suffix)

#============================
#Clean and prep "act_returns"
#============================
#Store rows without _abr suffix (the "!" means "not")
act_returns <- filter(all_Stats, !grepl('_abr', ticker))

#Remove Index returns (SP and SPY)
act_returns <- filter(act_returns, !grepl('SPY', ticker))
act_returns <- filter(act_returns, !grepl('SP', ticker))


#For both act_returns and abr_returns, rename "value" column to the type of returns
names(act_returns) <- c("timestamp", "case", "term", "event_period", "ticker", "act_return")
names(abr_returns) <- c("timestamp", "case", "term", "event_period", "ticker", "abr_return")

#Combine act_returns and abr_returns into a single dataframe with a column, 
#rather than a row, for each type of return.
all_Stats <- left_join(act_returns, abr_returns)

#Convert the ticker symbol back to Factor. Convert case and term to class factor
#to better reflect the data type.
all_Stats$ticker <- as.factor(all_Stats$ticker)
all_Stats$case <- as.factor(all_Stats$case)
all_Stats$term <- as.factor(all_Stats$term)

#======================
#Add case names to rows
#======================
#Remove ticker "CDM" since we don't have data
all_Stats <- filter(all_Stats, !grepl('CDM', ticker))

#Pull case names from SPY_Sigs
SPY_Sigs_names <- SPY_Sigs %>%
  select(Case, Symbol, Case_Name)

#rename columns to match all_Stats column names 
names(SPY_Sigs_names) <- c("case", "ticker", "Case_Name")

#Add case names to all_Abr
#R* will default to matching by "case" and "ticker" since the columns match which is what we want
all_Stats <- left_join(all_Stats, SPY_Sigs_names)

#Update column classes again
all_Stats$ticker <- as.factor(all_Stats$ticker)
all_Stats$case <- as.factor(all_Stats$case)
all_Stats$term <- as.factor(all_Stats$term)

#=========================
#Calculate New Return Stats
#========================
all_Stats <- all_Stats %>%
  group_by(case, ticker) %>%
  mutate(cum_abr = cumsum(abr_return),
         abr_perc = abr_return/sum(abr_return),
         abs_abr_perc = abs(abr_return)/sum(abs(abr_return)),
         abs_cum_abr = cumsum(abs(abr_return)),
         cum_abr_perc = cum_abr/sum(abr_return),
         abs_cum_abr_perc = abs_cum_abr/sum(abs(abr_return)))

#================
#Add in dummy row with 0 return values for graph aesthetics
#Important to do this after new stats above so that the 0 values don't impact actual stats
#==============

#For each case/ticker combo, create a row for t0 (9:30:00am) with 0 returns for graph aesthetics.
#We use "summarise" because we want to maintain the key id's (case, term, ticker, and Case_Name).
dummy <- all_Stats %>%
  group_by(case, ticker) %>%
  summarise(timestamp = timestamp[1] - minutes(5),
            term = term[1],
            event_period = 0,
            act_return = 0,
            abr_return = 0,
            Case_Name = Case_Name[1],
            cum_abr = 0,
            abr_perc = 0,
            abs_abr_perc = 0,
            abs_cum_abr = 0,
            cum_abr_perc = 0,
            abs_cum_abr_perc = 0)

#Add the 9:30:00am rows we just created to all_Abr
#We are safe using rbind here since the data is in long format and 
#each row contains key id's to arrange by.
all_Stats <- rbind(all_Stats, dummy)

#Re-arrange the row order
all_Stats <- all_Stats %>%
  arrange(case, ticker, event_period)

#Increase all event period figures by 1 since rows in R* start at 1, not 0.
#This is to avoid error/confusion
all_Stats <- all_Stats %>%
  mutate(event_period = event_period + 1)

#===============================================================
#group_A: 4 Groups (3 hours 15 minutes each). 2 groups per day
#==============================================================


#Split rows for each case/ticker
#Each day = 156 rows + 1 row for 9:30am on D1

#Note: The first event period group for each case/ticker will have 1 more row
#than all others because we appended the starting point 9:30:00 where returns are 0.

event_quarter_1 <- all_Stats %>%
  group_by(case, ticker) %>%
  slice(1:40)

event_quarter_2 <- all_Stats %>%
  group_by(case, ticker) %>%
  slice(41:79)

event_quarter_3 <- all_Stats %>%
  group_by(case, ticker) %>%
  slice(80:118)

event_quarter_4 <- all_Stats %>%
  group_by(case, ticker) %>%
  slice(119:157)

#Create column id showing which group the row belongs to 
event_quarter_1$event_quarter = 1 #D1 9:30 to D1 12:45
event_quarter_2$event_quarter = 2 #D1 12:50 to D1 Close (4:00)
event_quarter_3$event_quarter = 3 #D2 9:35 to D2 12:45
event_quarter_4$event_quarter = 4 #D2 12:50 to D2 Close (4:00)

#Recombine into one large dataframe
#rbind is safe to use since our data is in long form and the rows contain all key/id information.
event_quarter <- rbind(event_quarter_1, event_quarter_2, event_quarter_3, event_quarter_4)

#Add the event_quarter id to all_Stats
all_Stats <- left_join(all_Stats, event_quarter)


#=========================
#Calculate summary statistics of event_quarters
#=========================

event_quarter_stats <- all_Stats %>%
  group_by(case, ticker, event_quarter) %>%
  summarise(cum_abr_diff = max(cum_abr) - min(cum_abr),
            quarter_end_cum_abr = last(cum_abr)) %>%
  mutate(perc_of_terminal_cum_abr = cum_abr_diff / last(quarter_end_cum_abr))




