#Ver: 1.3 - 
# Replication Materials- "Law on the Market"
# Authors: Daniel Martin Katz, Jim Chen, Tyler Sollinger & Michael J Bommarito II
# Updated: 7/17/2015

# Set path
# IMPORTANT: You must change this to the path where you placed the replication materials.
setwd('/data/workspace/law-on-the-market/')

# Source each step in the process
source("src/r/1_event_studies.R")
source("src/r/2_master_stat_table.R")
source("src/r/3_dvt_market_cap.R")
source("src/r/4_generate_figures.R")

# Output data
write.csv(LOTM_Stats, file="output/lotm_event_stats.csv")
write.csv(all_Stats, file="output/lotm_timeseries_stats.csv")
