#Filter for Obamacare Case, all tickers.
Obamacare <- all_Stats %>%
  filter(case == 177)

#================================
#Plot All tickers on one graph
#================================
#Cumulative Abnormal Returns
Obamacare_cum_abr <- ggplot(Obamacare, aes(x = event_period, y = cum_abr, group = ticker, color = ticker)) +
  geom_line(size = 1.0) + 
  scale_x_discrete(breaks = c(1, 7, 79, 157),
                   labels = c("9:30", "10:00", "D1 Close", "D2 Close")) + 
  ggtitle("Obamacare Cumulative ABR")

ggsave(Obamacare_cum_abr, file = "figures/Obamacare_cum_abr2.pdf", width = 297, height = 210, units = "mm")

#Cumulative Abnormal Returns Percent of Point Estimate
Obamacare_cum_abr_perc <- ggplot(Obamacare, aes(x = event_period, y = cum_abr_perc, group = ticker, color = ticker)) +
  geom_line(size = 1.0) +
  scale_x_discrete(breaks = c(7, 79, 157),
                   labels = c("10:00", "D1 Close", "D2 Close")) + 
  ggtitle("Obamacare Cumulative ABR Percent of Point Estimate")


#===============================
#Each Ticker Gets Its Own Graph
#===============================
#Split data so it fits on two pages as 1x4. There are 7 total tickers
Obamacare_1 <- Obamacare[1:628,] #Data for 4 tickers = 157 (per ticker) * 4 = 628
Obamacare_2 <- Obamacare[629:1099,] 
#Add the first obamacase case to the end of Obamacare_2. It is a repeat, but this
#will make sure graphs print out as same size since Obamacare_2 only has 3 cases in it.
Obamacare_2 <- rbind(Obamacare_2, Obamacare[1:157,])

#3 remaining tickers * 156 (per ticker) = 468 + 624 = 1092

#Cumulative Abnormal Returns
#Page 1
Obamacare_cum_abr_facet_1 <- ggplot(Obamacare_1, aes(x = event_period, y = cum_abr, group =1)) +
  geom_line(size = 1.0) +
  facet_wrap(~ticker, ncol = 1) +
  scale_x_discrete(breaks = c(7, 79, 157),
                   labels = c("10:00", "D1 Close", "D2 Close")) + 
  ggtitle("Obamacare Cumulative ABR Page 1 of 2")


#Page 2
Obamacare_cum_abr_facet_2 <- ggplot(Obamacare_2, aes(x = event_period, y = cum_abr, group =1)) +
  geom_line(size = 1.0) +
  facet_wrap(~ticker, ncol = 1) +
  scale_x_discrete(breaks = c(1, 7, 79, 157),
                   labels = c("9:35", "10:00", "D1 Close", "D2 Close")) +
  ggtitle("Obamacare Cumulative ABR Page 2 of 2")

ggsave(Obamacare_cum_abr_facet_2, file = "figures/Sample.pdf",  width = 297, height = 210, units = "mm")


#Cumulative Abnormal Returns Percent of Point Estimate
#Page 1
Obamacare_cum_abr_perc_facet_1 <- ggplot(Obamacare_1, aes(x = event_period, y = cum_abr_perc, group =1)) +
  geom_line(size = 1.0)+
  facet_wrap(~ticker, ncol = 1) +
  scale_x_discrete(breaks = c(7, 79, 157),
                   labels = c("10:00", "D1 Close", "D2 Close")) +
  ggtitle("Obamacare Cumulative ABR Percent of Point Estimate Page 1 of 2")

#Page 2
Obamacare_cum_abr_perc_facet_2 <- ggplot(Obamacare_2, aes(x = event_period, y = cum_abr_perc, group =1)) +
  geom_line(size = 1.0)+
  facet_wrap(~ticker, ncol = 1) +
  scale_x_discrete(breaks = c(7, 79, 157),
                   labels = c("10:00", "D1 Close", "D2 Close")) +
  ggtitle("Obamacare Cumulative ABR Percent of Point Estimate Page 1 of 2")

ggsave(Obamacare_cum_abr, file = "figures/Obamacare_cum_abr.pdf", width = 210, height = 297, units = "mm")
ggsave(Obamacare_cum_abr_perc, file = "figures/Obamacare_cum_abr_perc.pdf", width = 210, height = 297, units = "mm")

ggsave(Obamacare_cum_abr_facet_1 , file = "figures/Obamacare_cum_abr_facet_1 .pdf", width = 210, height = 297, units = "mm")
ggsave(Obamacare_cum_abr_facet_2 , file = "figures/Obamacare_cum_abr_facet_2 .pdf", width = 210, height = 297, units = "mm")

ggsave(Obamacare_cum_abr_perc_facet_1 , file = "figures/Obamacare_cum_abr_perc_facet_1 .pdf", width = 210, height = 297, units = "mm")
ggsave(Obamacare_cum_abr_perc_facet_2 , file = "figures/Obamacare_cum_abr_perc_facet_2 .pdf", width = 210, height = 297, units = "mm")