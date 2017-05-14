#Ver: 1.1 - 
# Replication Materials- "Law on the Market"
# Authors: Daniel Martin Katz, Jim Chen, Tyler Sollinger & Michael J Bommarito II
# Updated: 6/20/2015

library(dplyr)
library(reshape2)
library(tidyr)
options("scipen"=100, "digits"=6)

#=======================================================
#Generate DVT and Market cap figures for symbols signficiant under the SPY Index
#=======================================================
Eval.Data <- read.csv("./data/wrds/WRDS SHROUT SPY Index.csv")

#Rename columns to match SPY_Sigs where applicable
names(Eval.Data) <- c("Case", "Symbol", "PE", "DATE", 
                      "PERMNO", "SHROUT_thous", "SHROUT", 
                      "event_date", "BIDLO", "ASKHI",
                      "PRC", "VOL", "td_count", "term")


#Calculate DVT and MC Figures using WRDS data (Eval.Data)
#To get the DVT and MC figures, we need to manipulate the data in very different ways. 
#For simplicity, lets store one clean copy for DVT and one for MC calculations before we begin.
DVT_df <- Eval.Data
MC_df <- Eval.Data

#=============
#Calculate DVT
#=============
#Create column that identifies whether the row is part of the 
#Estimation Period or the Event Window. 
DVT_df$Period <- cut(DVT_df$td_count, breaks = c(-31, -1, Inf),
                     labels = c("Est", "Event"))

#Calculate daily DVT (Each row = one day) - Mutate will run that mulitplication on each row and populate a new column called DVT
#Learn more about mutate here: http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
DVT_df <- DVT_df %>%
  mutate(DVT = VOL * PRC)

#Calcualte mean DVT for the Estimation Period and do the same for the Event Window.
DVT_df <- DVT_df %>%
  group_by(Case, Symbol, Period) %>%
  summarise(DVT_mean = mean(DVT))

#Separate the DVT_mean values we just created so that the estimation period and event
#window figures are in two columns rather than rows stacked on top of each other.
DVT_df <- DVT_df %>%
  spread(Period, DVT_mean)

#Calculate difference between the mean DVT of our estimation period and the event window. 
#"Diff" is the net difference. "Delta" is the absolute value (distance from 0)
DVT_df <- DVT_df %>%
  mutate(Abnormal_DVT = Event - Est)

#============================
#Calculate Market Cap Figures
#============================
#Formula = P0 * AR * S

#PO = Close price the day before the decision date (PRC). 
#"PO" Stands for "Price Zero"

#AR = Abnormal returns over the 2-day event window 
#Corresponds with the point estimate from the event study results

#S = Shares Outstanding
#Obtained from WRDS

#We need data for "P0" and Shares Outstanding on the defore the decision date.
MC_df <- MC_df %>%
  filter(td_count =="-1")

#Create a new column for the market cap figures based on the formula above. 
#Corresponding Column Names in WRDS dataset: PO=PRC, AR=PE, S=SHROUT
MC_df <- MC_df %>%
  mutate(Market_Cap_Impact = PRC * SHROUT * PE)



#=====================================
#Cleanup DVT_df and MC_df
#=====================================
MC_df <- MC_df %>%
  select(-DATE, -PERMNO, -SHROUT_thous, -SHROUT, -BIDLO, -ASKHI, -PRC, -VOL, -td_count, -term, -event_date)

names(MC_df) <- c("Case", "Symbol", "Point_Estimate", "Market_Cap_Impact")

MC_df$Case <- as.factor(MC_df$Case)

DVT_df <- DVT_df %>%
  select(-Est, -Event)

DVT_df$Case <- as.factor(DVT_df$Case)

# ==================================================================
#Prep LOTM_Stats Data For SPY Index Significant Stats Table 
# ==================================================================

#Filter for symbols that were statistically significant at the 95% or greater level.
SPY_Sigs <- LOTM_Stats %>%
  filter(sig == "**" | sig == "***", Index == "SPY")

#Remove the symbol "CDM" (From Case 132). Shares outstanding data was not available on WRDS. 
#"CDM" is Toronto Exchange symbol for Coeur Mining Inc. We still have the NYSE data (CDE).
SPY_Sigs <- filter(SPY_Sigs, !grepl('CDM', Symbol))

#Drop unnecessary columns
SPY_Sigs <- SPY_Sigs %>%
  select(-Index, -error, -t.value, -p.value, -docket)

names(SPY_Sigs) <- c("Case", "Symbol", "Point_Estimate", "sig", "Decision", "Cite", "term", "Case_Name")

SPY_Sigs$Case <- as.factor(SPY_Sigs$Case)
SPY_Sigs$Symbol <- as.factor(SPY_Sigs$Symbol)
SPY_Sigs$Point_Estimate <- as.numeric(SPY_Sigs$Point_Estimate)
SPY_Sigs$term <- as.factor(SPY_Sigs$term)


#Join SPY_Sigs with MC_df and DVT_df
SPY_Sigs <- left_join(SPY_Sigs, MC_df)
SPY_Sigs <- left_join(SPY_Sigs, DVT_df)

#Combine various columns for readability
SPY_Sigs$Case_Name <- paste(SPY_Sigs$Case_Name, SPY_Sigs$Cite, sep = ", ")
SPY_Sigs$Point_Estimate <- paste(SPY_Sigs$Point_Estimate, SPY_Sigs$sig)

#Drop duplicative columns
SPY_Sigs <- SPY_Sigs %>%
  select(-sig, -Cite)

#Rearrange column order
SPY_Sigs <- SPY_Sigs %>%
  select(Case, Case_Name, Symbol, Abnormal_DVT, Market_Cap_Impact, Point_Estimate, Decision, term)



