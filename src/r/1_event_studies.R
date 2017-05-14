#Ver: 1.1 - 
# Replication Materials- "Law on the Market"
# Authors: Daniel Martin Katz, Jim Chen, Tyler Sollinger & Michael J Bommarito II
# Updated: 7/12/2015

#Imports -remember to use - install.packages("package_name") if do not have them already 
library(erer)
library(lubridate)
library(plyr)
library(xts)
library(erer)

#Set Time Zone To UTC (Required for proper event study date conversion to numeric)
Sys.setenv(TZ="UTC")

# ================================================================
# Calculate the period-to-period return from raw OHLC data.
# ================================================================

#Need to set the path to wherever the files are located 

base_ohlc_path <- file.path("./data/equity-5min/")

# Start by getting the file list
file_list <- list.files(base_ohlc_path, "*.csv")

# Now iterate over all files and merge them against the 
# initial XTS object
all_return_xts <- NULL

for (file_name in file_list) {
  # Open file
  series_data <- read.csv(file.path(base_ohlc_path, file_name))
  
  tryCatch({
    # Try to map the date and time using lubridate
    series_data$timestamp <- mdy_hm(paste(series_data$Date, series_data$Time))
    
    # Calculate return and create XTS object
    series_data$oc_return <- log(series_data$Close) - log(series_data$Open)
    series_return <- xts(series_data$oc_return, series_data$timestamp)
    names(series_return) <- gsub(".csv", "", file_name)
    
    # Merge into main XTS object
    if (!is.null(all_return_xts)) {
      all_return_xts <- merge.xts(all_return_xts, series_return)
    } else {
      all_return_xts <- series_return
    }
    
    # Output status
    print(sprintf("Successfully merged %s.  Resulting dimensions are %d x %d, %s - %s.", file_name, nrow(all_return_xts),
                  ncol(all_return_xts), min(index(all_return_xts)), max(index(all_return_xts))))
  }, error = function(e) {
    # Handle errors
    print(sprintf("Error in file %s: %s", file_name, e))
  })
}


returns_df <- data.frame(DateTime_POSIX = as.POSIXlt(index(all_return_xts), tz="UTC"),
                         DateTime_numeric = as.numeric(as.POSIXlt(index(all_return_xts), tz="UTC")), all_return_xts)

#Now lets run our individual event models

#evReturn:
  # This is the core function for event analysis. It estimates a market model by firm and then calculate
  # abnormal returns by firm and over time. The time series of stock returns have irregular time
  # frequency because of varying trading days. Thus, the time dimension is explicitly specified as a
  # y.date column in the data of y.

Case1 <- evReturn(y = returns_df, firm = c("CAG", "XLP"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 388, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-02-23 09:35:00")), event.win = 77.5)

Case1.XLP <- evReturn(y = returns_df, firm = c("CAG"),
                      y.date = "DateTime_numeric", index = "XLP", est.win = 385, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-02-23 09:35:00")), event.win = 77.5)

Case2 <- evReturn(y = returns_df, firm = c("BP", "CVX", "XOM", "XLE"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-03-07 09:35:00")), event.win = 77.5)

Case2.XLE <- evReturn(y = returns_df, firm = c("BP", "CVX", "XOM"),
                      y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-03-07 09:35:00")), event.win = 77.5)

Case3 <- evReturn(y = returns_df, firm = c("MO", "RAI", "XLP"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-03-22 09:35:00")), event.win = 77.5)

Case3.XLP <- evReturn(y = returns_df, firm = c("MO", "RAI"),
                      y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-03-22 09:35:00")), event.win = 77.5)

Case4 <- evReturn(y = returns_df, firm = c("WMT", "XLY"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-03-23 09:35:00")), event.win = 77.5)

Case4.XLY <- evReturn(y = returns_df, firm = c("WMT"),
                      y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-03-23 09:35:00")), event.win = 77.5)

Case5 <- evReturn(y = returns_df, firm = c("ABT", "BMY", "JNJ", "XLV"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-04-04 09:35:00")), event.win = 77.5)

Case5.XLV <- evReturn(y = returns_df, firm = c("ABT", "BMY", "JNJ"),
                      y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-04-04 09:35:00")), event.win = 77.5)

Case6 <- evReturn(y = returns_df, firm = c("CSX", "KSU", "NSC", "UNP", "XLI"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-04-18 09:35:00")), event.win = 77.5)

Case6.XLI <- evReturn(y = returns_df, firm = c("CSX", "KSU", "NSC", "UNP"),
                      y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-04-18 09:35:00")), event.win = 77.5)

Case7 <- evReturn(y = returns_df, firm = c("PLA", "PLAA", "XLY"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-05-23 09:35:00")), event.win = 77.5)

Case7.XLY <- evReturn(y = returns_df, firm = c("PLA", "PLAA"),
                      y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-05-23 09:35:00")), event.win = 77.5)

Case8 <- evReturn(y = returns_df, firm = c("GM", "HMC", "TM", "XLY"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-05-23 09:35:00")), event.win = 77.5)

Case8.XLY <- evReturn(y = returns_df, firm = c("GM", "HMC", "TM"),
                      y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-05-23 09:35:00")), event.win = 77.5)

Case9 <- evReturn(y = returns_df, firm = c("ASB", "XLF"),
                  y.date = "DateTime_numeric", index = "SPY", est.win = 300, digits = 6,
                  event.date = as.numeric(as.POSIXct("2000-06-13 09:35:00")), event.win = 77.5)

Case9.XLF <- evReturn(y = returns_df, firm = c("ASB"),
                      y.date = "DateTime_numeric", index = "XLF", est.win = 300, digits = 6,
                      event.date = as.numeric(as.POSIXct("2000-06-13 09:35:00")), event.win = 77.5)

Case10 <- evReturn(y = returns_df, firm = c("MRO", "XOM", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2000-06-27 09:35:00")), event.win = 77.5)

Case10.XLE <- evReturn(y = returns_df, firm = c("MRO", "XOM"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2000-06-27 09:35:00")), event.win = 77.5)

Case11 <- evReturn(y = returns_df, firm = c("XLF"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2000-11-29 09:35:00")), event.win = 77.5)

Case12 <- evReturn(y = returns_df, firm = c("BP", "CVX", "XOM", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 312, digits = 6,
                   event.date = as.numeric(as.POSIXct("2000-12-05 09:35:00")), event.win = 77.5)

Case12.XLE <- evReturn(y = returns_df, firm = c("BP", "CVX", "XOM"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 312, digits = 6,
                       event.date = as.numeric(as.POSIXct("2000-12-05 09:35:00")), event.win = 77.5)

Case13 <- evReturn(y = returns_df, firm = c("BP", "CVX", "XOM", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2000-12-13 09:35:00")), event.win = 77.5)

Case13.XLE <- evReturn(y = returns_df, firm = c("BP", "CVX", "XOM"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2000-12-13 09:35:00")), event.win = 77.5)

Case14 <- evReturn(y = returns_df, firm = c("JNJ", "XLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-02-22 09:35:00")), event.win = 77.5)

Case14.XLV <- evReturn(y = returns_df, firm = c("JNJ"),
                       y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-02-22 09:35:00")), event.win = 77.5)

Case15 <- evReturn(y = returns_df, firm = c("F", "GM", "HMC", "TM", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-02-28 09:35:00")), event.win = 77.5)

Case15.XLY <- evReturn(y = returns_df, firm = c("F", "GM", "HMC", "TM"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-02-28 09:35:00")), event.win = 77.5)

Case16 <- evReturn(y = returns_df, firm = c("LMT", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-02-28 09:35:00")), event.win = 77.5)

Case16.XLI <- evReturn(y = returns_df, firm = c("LMT"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-02-28 09:35:00")), event.win = 77.5)

Case17 <- evReturn(y = returns_df, firm = c("CC", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-03-22 09:35:00")), event.win = 77.5)

Case17.XLY <- evReturn(y = returns_df, firm = c("CC"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-03-22 09:35:00")), event.win = 77.5)

Case18 <- evReturn(y = returns_df, firm = c("CBE", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-05-15 09:35:00")), event.win = 77.5)

Case18.XLI <- evReturn(y = returns_df, firm = c("CBE"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-05-15 09:35:00")), event.win = 77.5)

Case19 <- evReturn(y = returns_df, firm = c("DD", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-06-05 09:35:00")), event.win = 77.5)

Case19.XLI <- evReturn(y = returns_df, firm = c("DD"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-06-05 09:35:00")), event.win = 77.5)

Case20 <- evReturn(y = returns_df, firm = c("MEA", "XLB"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-06-19 09:35:00")), event.win = 77.5)

Case20.XLB <- evReturn(y = returns_df, firm = c("MEA"),
                       y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-06-19 09:35:00")), event.win = 77.5)

Case21 <- evReturn(y = returns_df, firm = c("ENL", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-06-26 09:35:00")), event.win = 77.5)

Case21.XLI <- evReturn(y = returns_df, firm = c("ENL"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-06-26 09:35:00")), event.win = 77.5)

Case22 <- evReturn(y = returns_df, firm = c("UNFI", "XLP"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-06-26 09:35:00")), event.win = 77.5)

Case22.XLP <- evReturn(y = returns_df, firm = c("UNFI"),
                       y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-06-26 09:35:00")), event.win = 77.5)

Case23 <- evReturn(y = returns_df, firm = c("BTI", "MO", "RAI", "UVV", "VGR", "XLP"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-06-29 09:35:00")), event.win = 77.5)

Case23.XLP <- evReturn(y = returns_df, firm = c("BTI", "MO", "RAI", "UVV", "VGR"),
                       y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-06-29 09:35:00")), event.win = 77.5)

Case24 <- evReturn(y = returns_df, firm = c("TRW", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-11-14 09:35:00")), event.win = 77.5)

Case24.XLY <- evReturn(y = returns_df, firm = c("TRW"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-11-14 09:35:00")), event.win = 77.5)

Case25 <- evReturn(y = returns_df, firm = c("CZR", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-11-28 09:35:00")), event.win = 77.5)

Case25.XLY <- evReturn(y = returns_df, firm = c("CZR"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-11-28 09:35:00")), event.win = 77.5)

Case26 <- evReturn(y = returns_df, firm = c("DD", "DOW", "MON", "SYT", "XLB", "XLI", "XLP", "XLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 389, digits = 6,
                   event.date = as.numeric(as.POSIXct("2001-12-11 09:35:00")), event.win = 77.5)

Case26.XLB <- evReturn(y = returns_df, firm = c("DD", "DOW", "MON", "SYT", "XLI", "XLP", "XLV"),
                       y.date = "DateTime_numeric", index = "XLB", est.win = 389, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-12-11 09:35:00")), event.win = 77.5)

Case26.XLI <- evReturn(y = returns_df, firm =c("DD", "DOW", "MON", "SYT", "XLB", "XLP", "XLV"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 389, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-12-11 09:35:00")), event.win = 77.5)

Case26.XLP <- evReturn(y = returns_df, firm = c("DD", "DOW", "MON", "SYT", "XLB", "XLI", "XLV"),
                       y.date = "DateTime_numeric", index = "XLP", est.win = 389, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-12-11 09:35:00")), event.win = 77.5)

Case26.XLV <- evReturn(y = returns_df, firm = c("DD", "DOW", "MON", "SYT", "XLB", "XLI", "XLP"),
                       y.date = "DateTime_numeric", index = "XLV", est.win = 389, digits = 6,
                       event.date = as.numeric(as.POSIXct("2001-12-11 09:35:00")), event.win = 77.5)

Case27 <- evReturn(y = returns_df, firm = c("TM", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-01-09 09:35:00")), event.win = 77.5)

Case27.XLY <- evReturn(y = returns_df, firm = c("TM"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-01-09 09:35:00")), event.win = 77.5)

Case28 <- evReturn(y = returns_df, firm = c("GWO"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 380, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-01-09 09:35:00")), event.win = 77.5)

Case29 <- evReturn(y = returns_df, firm = c("RIG", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-01-10 09:35:00")), event.win = 77.5)

Case29.XLE <- evReturn(y = returns_df, firm = c("RIG"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-01-10 09:35:00")), event.win = 77.5)

Case30 <- evReturn(y = returns_df, firm = c("CMCSA", "DISH", "T", "VZ", "IYZ", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-01-17 09:35:00")), event.win = 77.5)

Case30.IYZ <- evReturn(y = returns_df, firm = c("CMCSA", "DISH", "T", "VZ", "XLY"),
                       y.date = "DateTime_numeric", index = "IYZ", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-01-17 09:35:00")), event.win = 77.5)

Case30.XLY <- evReturn(y = returns_df, firm = c("CMCSA", "DISH", "T", "VZ", "IYZ"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-01-17 09:35:00")), event.win = 77.5)

Case31 <- evReturn(y = returns_df, firm = c("ACI", "BTU", "CLD", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 389, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-02-20 09:35:00")), event.win = 77.5)

Case31.XLE <- evReturn(y = returns_df, firm = c("ACI", "BTU", "CLD"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 389, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-02-20 09:35:00")), event.win = 77.5)

#Separate Study For WLB because the first 21 periods of data is unavailable
Case31.WLB <- evReturn(y = returns_df, firm = c("ACI", "BTU", "CLD", "WLB", "XLE"),
                       y.date = "DateTime_numeric", index = "SPY", est.win = 369, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-02-20 09:35:00")), event.win = 77.5)

Case31.WLB.XLE <- evReturn(y = returns_df, firm = c("ACI", "BTU", "CLD", "WLB"),
                           y.date = "DateTime_numeric", index = "XLE", est.win = 369, digits = 6,
                           event.date = as.numeric(as.POSIXct("2002-02-20 09:35:00")), event.win = 77.5)

Case32 <- evReturn(y = returns_df, firm = c("AT", "CMS", "LNT", "XLU"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-03-05 09:35:00")), event.win = 77.5)

Case32.XLU <- evReturn(y = returns_df, firm = c("AT", "CMS", "LNT"),
                       y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-03-05 09:35:00")), event.win = 77.5)

Case33 <- evReturn(y = returns_df, firm = c("WWW", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-03-20 09:35:00")), event.win = 77.5)

Case33.XLY <- evReturn(y = returns_df, firm = c("WWW"),
                       y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-03-20 09:35:00")), event.win = 77.5)

Case34 <- evReturn(y = returns_df, firm = c("XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-04-30 09:35:00")), event.win = 77.5)

Case35 <- evReturn(y = returns_df, firm = c("T", "IYZ", "XLY", "VZ"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-05-14 09:35:00")), event.win = 77.5)

Case35.IYZ <- evReturn(y = returns_df, firm = c("T", "XLY", "VZ"),
                       y.date = "DateTime_numeric", index = "IYZ", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-05-14 09:35:00")), event.win = 77.5)

Case35.XLY <- evReturn(y = returns_df, firm = c("T", "IYZ", "VZ"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-05-14 09:35:00")), event.win = 77.5)

Case36 <- evReturn(y = returns_df, firm = c("T", "VZ", "XLY", "IYZ"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-05-21 09:35:00")), event.win = 77.5)

Case36.XLY <- evReturn(y = returns_df, firm = c("T", "VZ", "IYZ"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-05-21 09:35:00")), event.win = 77.5)

Case36.IYZ <- evReturn(y = returns_df, firm = c("T", "VZ", "XLY"),
                       y.date = "DateTime_numeric", index = "IYZ", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-05-21 09:35:00")), event.win = 77.5)

Case37 <- evReturn(y = returns_df, firm = c("CVX", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-06-11 09:35:00")), event.win = 77.5)

Case37.XLE <- evReturn(y = returns_df, firm = c("CVX"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-06-11 09:35:00")), event.win = 77.5)

Case38 <- evReturn(y = returns_df, firm = c("JPM", "XLF"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-06-11 09:35:00")), event.win = 77.5)

Case38.XLF <- evReturn(y = returns_df, firm = c("JPM"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-06-11 09:35:00")), event.win = 77.5)

Case39 <- evReturn(y = returns_df, firm = c("AET", "CI", "CNC", "HUM", "UNH", "WLP", "XLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-06-21 09:35:00")), event.win = 77.5)

Case39.XLV <- evReturn(y = returns_df, firm = c("AET", "CI", "CNC", "HUM", "UNH", "WLP"),
                       y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-06-21 09:35:00")), event.win = 77.5)

#Data was not available for SPY during this date range, not sure why.
Case40.XLF <- evReturn(y = returns_df, firm = c("C.", "F", "XLY"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-10-16 09:35:00")), event.win = 77.5)

Case40.XLY <- evReturn(y = returns_df, firm = c("C.", "F", "XLF"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-10-16 09:35:00")), event.win = 77.5)

Case41 <- evReturn(y = returns_df, firm = c("SYT", "XLB"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 155, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-11-06 09:35:00")), event.win = 77.5)

Case41.XLB <- evReturn(y = returns_df, firm = c("SYT"),
                       y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-11-06 09:35:00")), event.win = 77.5)

Case42 <- evReturn(y = returns_df, firm = c("YRCW", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 155, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-11-06 09:35:00")), event.win = 77.5)

Case42.XLI <- evReturn(y = returns_df, firm = c("YRCW"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-11-06 09:35:00")), event.win = 77.5)

Case43 <- evReturn(y = returns_df, firm = c("BC", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2002-12-04 09:35:00")), event.win = 77.5)

Case43.XLI <- evReturn(y = returns_df, firm = c("BC"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2002-12-04 09:35:00")), event.win = 77.5)

Case44 <- evReturn(y = returns_df, firm = c("ACI", "BTU", "CLD", "WLB", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-01-16 09:35:00")), event.win = 77.5)

Case44.XLE <- evReturn(y = returns_df, firm = c("ACI", "BTU", "CLD", "WLB"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-01-16 09:35:00")), event.win = 77.5)

Case45 <- evReturn(y = returns_df, firm = c("BA", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-03-05 09:35:00")), event.win = 77.5)

Case45.XLI <- evReturn(y = returns_df, firm = c("BA"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-03-05 09:35:00")), event.win = 77.5)

Case46 <- evReturn(y = returns_df, firm = c("LB", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 389, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-03-05 09:35:00")), event.win = 77.5)

Case46.XLY <- evReturn(y = returns_df, firm = c("LB"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 389, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-03-05 09:35:00")), event.win = 77.5)

Case47 <- evReturn(y = returns_df, firm = c("CSX", "KSU", "NSC", "UNP", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-03-11 09:35:00")), event.win = 77.5)

Case47.XLI <- evReturn(y = returns_df, firm = c("CSX", "KSU", "NSC", "UNP"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-03-11 09:35:00")), event.win = 77.5)

Case48 <- evReturn(y = returns_df, firm = c("PHS", "XLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-04-08 09:35:00")), event.win = 77.5)

Case48.XLV <- evReturn(y = returns_df, firm = c("PHS"),
                       y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-04-08 09:35:00")), event.win = 77.5)

Case49 <- evReturn(y = returns_df, firm = c("ALL", "PGR", "SAFT", "TRV.7041", "XLF"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-04-08 09:35:00")), event.win = 77.5)

Case49.XLF <- evReturn(y = returns_df, firm = c("ALL", "PGR", "SAFT", "TRV.7041"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-04-08 09:35:00")), event.win = 77.5)

Case50 <- evReturn(y = returns_df, firm = c("XLP"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-04-23 09:35:00")), event.win = 77.5)

Case51 <- evReturn(y = returns_df, firm = c("AMGN", "AZN", "BMY", "JNJ", "LLY", "PFE", "XLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-05-20 09:35:00")), event.win = 77.5)

Case51.XLV <- evReturn(y = returns_df, firm = c("AMGN", "AZN", "BMY", "JNJ", "LLY", "PFE"),
                       y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-05-20 09:35:00")), event.win = 77.5)

Case52 <- evReturn(y = returns_df, firm = c("SWK", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-05-28 09:35:00")), event.win = 77.5)

Case52.XLI <- evReturn(y = returns_df, firm = c("SWK"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-05-28 09:35:00")), event.win = 77.5)

Case53 <- evReturn(y = returns_df, firm = c("HRB", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-03 09:35:00")), event.win = 77.5)

Case53.XLE <- evReturn(y = returns_df, firm = c("HRB"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-03 09:35:00")), event.win = 77.5)

Case54 <- evReturn(y = returns_df, firm = c("FOXA", "XLP", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-03 09:35:00")), event.win = 77.5)

Case54.XLP <- evReturn(y = returns_df, firm = c("FOXA", "XLY"),
                       y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-03 09:35:00")), event.win = 77.5)

Case54.XLY <- evReturn(y = returns_df, firm = c("FOXA", "XLP"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-03 09:35:00")), event.win = 77.5)

Case55 <- evReturn(y = returns_df, firm = c("CZR", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-10 09:35:00")), event.win = 77.5)

Case55.XLY <- evReturn(y = returns_df, firm = c("CZR"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-10 09:35:00")), event.win = 77.5)

Case56 <- evReturn(y = returns_df, firm = c("DF", "LWAY", "XLP"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-10 09:35:00")), event.win = 77.5)

Case56.XLP <- evReturn(y = returns_df, firm = c("DF", "LWAY"),
                       y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-10 09:35:00")), event.win = 77.5)

Case57 <- evReturn(y = returns_df, firm = c("DOW", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-10 09:35:00")), event.win = 77.5)

Case57.XLI <- evReturn(y = returns_df, firm = c("DOW"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-10 09:35:00")), event.win = 77.5)


Case58 <- evReturn(y = returns_df, firm = c("CB", "CNA", "TRV.7041", "XLF"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-24 09:35:00")), event.win = 77.5)

Case58.XLF <- evReturn(y = returns_df, firm = c("CB", "CNA", "TRV.7041"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-24 09:35:00")), event.win = 77.5)

Case59 <- evReturn(y = returns_df, firm = c("WAC", "XLF"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-24 09:35:00")), event.win = 77.5)

Case59.XLF <- evReturn(y = returns_df, firm = c("WAC"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-24 09:35:00")), event.win = 77.5)

Case60 <- evReturn(y = returns_df, firm = c("NKE", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-06-27 09:35:00")), event.win = 77.5)

Case60.XLY <- evReturn(y = returns_df, firm = c("NKE"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-06-27 09:35:00")), event.win = 77.5)

Case61 <- evReturn(y = returns_df, firm = c("RTN", "XLF", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2003-12-03 09:35:00")), event.win = 77.5)

Case61.XLF <- evReturn(y = returns_df, firm = c("RTN", "XLI"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-12-03 09:35:00")), event.win = 77.5)

Case61.XLI <- evReturn(y = returns_df, firm = c("RTN", "XLF"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2003-12-03 09:35:00")), event.win = 77.5)

Case62 <- evReturn(y = returns_df, firm = c("T", "VZ", "IYZ", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-01-14 09:35:00")), event.win = 77.5)

Case62.IYZ <- evReturn(y = returns_df, firm = c("T", "VZ", "XLY"),
                       y.date = "DateTime_numeric", index = "IYZ", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-01-14 09:35:00")), event.win = 77.5)

Case62.XLY <- evReturn(y = returns_df, firm = c("T", "VZ", "IYZ"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-01-14 09:35:00")), event.win = 77.5)

Case63 <- evReturn(y = returns_df, firm = c("GD", "XLF", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-02-25 09:35:00")), event.win = 77.5)

Case63.XLF <- evReturn(y = returns_df, firm = c("GD", "XLI"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-02-25 09:35:00")), event.win = 77.5)

Case63.XLI <- evReturn(y = returns_df, firm = c("GD", "XLF"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-02-25 09:35:00")), event.win = 77.5)

Case64 <- evReturn(y = returns_df, firm = c("AXP", "V", "XLF", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-04-22 09:35:00")), event.win = 77.5)

Case64.XLF <- evReturn(y = returns_df, firm = c("AXP", "V", "XLY"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-04-22 09:35:00")), event.win = 77.5)

Case64.XLY <- evReturn(y = returns_df, firm = c("AXP", "V", "XLF"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-04-22 09:35:00")), event.win = 77.5)

Case65 <- evReturn(y = returns_df, firm = c("F", "GM", "HMC", "TM", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-04-29 09:35:00")), event.win = 77.5)

Case65.XLY <- evReturn(y = returns_df, firm = c("F", "GM", "HMC", "TM"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-04-29 09:35:00")), event.win = 77.5)

Case66 <- evReturn(y = returns_df, firm = c("RRD", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-05-04 09:35:00")), event.win = 77.5)

Case66.XLI <- evReturn(y = returns_df, firm = c("RRD"),
                       y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-05-04 09:35:00")), event.win = 77.5)

Case67 <- evReturn(y = returns_df, firm = c("AET", "HNT", "UNH", "CI", "HUM", "XLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-06-22 09:35:00")), event.win = 77.5)

Case67.XLV <- evReturn(y = returns_df, firm = c("AET", "HNT", "UNH", "CI", "HUM"),
                       y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-06-22 09:35:00")), event.win = 77.5)

Case68 <- evReturn(y = returns_df, firm = c("AMD", "INTC"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-06-22 09:35:00")), event.win = 77.5)

Case68.XLK <- evReturn(y = returns_df, firm = c("AMD", "INTC"),
                       y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-06-22 09:35:00")), event.win = 77.5)

Case69 <- evReturn(y = returns_df, firm = c("CVX", "HES", "XLE", "XOM"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-06-25 09:35:00")), event.win = 77.5)

Case69.XLE <- evReturn(y = returns_df, firm = c("CVX", "HES"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-06-25 09:35:00")), event.win = 77.5)

Case69.XOM <- evReturn(y = returns_df, firm = c("CVX", "HES"),
                       y.date = "DateTime_numeric", index = "XOM", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-06-25 09:35:00")), event.win = 77.5)

Case70 <- evReturn(y = returns_df, firm = c("NSC", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-11-10 09:35:00")), event.win = 77.5)

Case70.XLI <- evReturn(y = returns_df, firm = c("NSC"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-11-10 09:35:00")), event.win = 77.5)

Case71 <- evReturn(y = returns_df, firm = c("AVL", "CBE", "XLB", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2004-12-14 09:35:00")), event.win = 77.5)

Case71.XLB <- evReturn(y = returns_df, firm = c("AVL", "CBE"),
                       y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-12-14 09:35:00")), event.win = 77.5)

Case71.XLI <- evReturn(y = returns_df, firm = c("AVL", "CBE"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2004-12-14 09:35:00")), event.win = 77.5)

Case72 <- evReturn(y = returns_df, firm = c("XOM", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-03-31 09:35:00")), event.win = 77.5)

Case72.XLE <- evReturn(y = returns_df, firm = c("XOM"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2005-03-31 09:35:00")), event.win = 77.5)

Case73 <- evReturn(y = returns_df, firm = c("DOW", "XLB"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-04-28 09:35:00")), event.win = 77.5)

Case73.XLB <- evReturn(y = returns_df, firm = c("DOW"),
                       y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2005-04-28 09:35:00")), event.win = 77.5)

Case74 <- evReturn(y = returns_df, firm = c("SAM", "STZ", "XLP"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-05-17 09:35:00")), event.win = 77.5)

Case74.XLP <- evReturn(y = returns_df, firm = c("SAM", "STZ"),
                       y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2005-05-17 09:35:00")), event.win = 77.5)

Case75 <- evReturn(y = returns_df, firm = c("CVX", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-05-24 09:35:00")), event.win = 77.5)

Case75.XLE <- evReturn(y = returns_df, firm = c("CVX"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2005-05-24 09:35:00")), event.win = 77.5)

Case76 <- evReturn(y = returns_df, firm = c("XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-06-07 09:35:00")), event.win = 77.5)

Case77 <- evReturn(y = returns_df, firm = c("IART", "MRK", "XLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-06-14 09:35:00")), event.win = 77.5)

Case77.XLV <- evReturn(y = returns_df, firm = c("IART", "MRK"),
                       y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2005-06-14 09:35:00")), event.win = 77.5)

Case78 <- evReturn(y = returns_df, firm = c("YRCW", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-06-21 09:35:00")), event.win = 77.5)

Case78.XLI <- evReturn(y = returns_df, firm = c("YRCW"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2005-06-21 09:35:00")), event.win = 77.5)

Case79 <- evReturn(y = returns_df, firm = c("XOM", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-06-24 09:35:00")), event.win = 77.5)

Case79.XLE <- evReturn(y = returns_df, firm = c("XOM"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2005-06-24 09:35:00")), event.win = 77.5)

Case80 <- evReturn(y = returns_df, firm = c("DF", "PBJ", "TSN"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2005-11-09 09:35:00")), event.win = 77.5)

Case81 <- evReturn(y = returns_df, firm = c("VOLV"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-01-11 09:35:00")), event.win = 77.5)

Case82 <- evReturn(y = returns_df, firm = c("CAG", "PBJ"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-01-24 09:35:00")), event.win = 77.5)

Case83 <- evReturn(y = returns_df, firm = c("PBJ", "TSN"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-02-22 09:35:00")), event.win = 77.5)

Case84 <- evReturn(y = returns_df, firm = c("DPZ", "DRI", "YUM"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-02-23 09:35:00")), event.win = 77.5)

Case85 <- evReturn(y = returns_df, firm = c("CVX"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-03-01 09:35:00")), event.win = 77.5)

Case86 <- evReturn(y = returns_df, firm = c("ITW"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-03-02 09:35:00")), event.win = 77.5)

Case86.XLI <- evReturn(y = returns_df, firm = c("ITW"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2006-03-02 09:35:00")), event.win = 77.5)

Case87 <- evReturn(y = returns_df, firm = c("MER", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-03-22 09:35:00")), event.win = 77.5)

Case87.XLE <- evReturn(y = returns_df, firm = c("MER"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2006-03-22 09:35:00")), event.win = 77.5)

Case88 <- evReturn(y = returns_df, firm = c("EBAY", "XLK"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-05-16 09:35:00")), event.win = 77.5)

Case88.XLK <- evReturn(y = returns_df, firm = c("EBAY"),
                       y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2006-05-16 09:35:00")), event.win = 77.5)

Case89 <- evReturn(y = returns_df, firm = c("MHK", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-06-06 09:35:00")), event.win = 77.5)

Case89.XLY <- evReturn(y = returns_df, firm = c("MHK"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2006-06-06 09:35:00")), event.win = 77.5)


Case90 <- evReturn(y = returns_df, firm = c("BP", "XES", "XLE"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2006-12-12 09:35:00")), event.win = 77.5)

Case90.XES <- evReturn(y = returns_df, firm = c("BP", "XLE"),
                       y.date = "DateTime_numeric", index = "XES", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2006-12-12 09:35:00")), event.win = 77.5)

Case90.XLE <- evReturn(y = returns_df, firm = c("BP", "XES"),
                       y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2006-12-12 09:35:00")), event.win = 77.5)

Case91 <- evReturn(y = returns_df, firm = c("AZN", "XPH"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-01-10 09:35:00")), event.win = 77.5)

Case91.XPH <- evReturn(y = returns_df, firm = c("AZN"),
                       y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-01-10 09:35:00")), event.win = 77.5)

Case92 <- evReturn(y = returns_df, firm = c("NSC", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-01-11 09:35:00")), event.win = 77.5)

Case92.XLI <- evReturn(y = returns_df, firm = c("NSC"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-01-11 09:35:00")), event.win = 77.5)

Case93 <- evReturn(y = returns_df, firm = c("MO", "XLP"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-02-21 09:35:00")), event.win = 77.5)

Case93.XLP <- evReturn(y = returns_df, firm = c("MO"),
                       y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-02-21 09:35:00")), event.win = 77.5)

Case94 <- evReturn(y = returns_df, firm = c("WY", "XLF"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-02-21 09:35:00")), event.win = 77.5)

Case94.XLF <- evReturn(y = returns_df, firm = c("WY"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-02-21 09:35:00")), event.win = 77.5)

Case95 <- evReturn(y = returns_df, firm = c("PCG", "TRV.7041", "XLU"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-03-21 09:35:00")), event.win = 77.5)

Case95.XLU <- evReturn(y = returns_df, firm = c("PCG", "TRV.7041"),
                       y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-03-21 09:35:00")), event.win = 77.5)

Case96 <- evReturn(y = returns_df, firm = c("ROK", "XLI"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-03-28 09:35:00")), event.win = 77.5)

Case96.XLI <- evReturn(y = returns_df, firm = c("ROK"),
                       y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-03-28 09:35:00")), event.win = 77.5)

Case97 <- evReturn(y = returns_df, firm = c("DUK", "XLU"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-04-03 09:35:00")), event.win = 77.5)

Case97.XLU <- evReturn(y = returns_df, firm = c("DUK"),
                       y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-04-03 09:35:00")), event.win = 77.5)

Case98 <- evReturn(y = returns_df, firm = c("F", "GM", "TM", "XLY"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-04-03 09:35:00")), event.win = 77.5)

Case98.XLY <- evReturn(y = returns_df, firm = c("F", "GM", "TM"),
                       y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-04-03 09:35:00")), event.win = 77.5)

Case99 <- evReturn(y = returns_df, firm = c("GLBC", "VOX", "IYZ", "XLF"),
                   y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                   event.date = as.numeric(as.POSIXct("2007-04-18 09:35:00")), event.win = 77.5)

Case99.IYZ <- evReturn(y = returns_df, firm = c("GLBC", "VOX", "XLF"),
                       y.date = "DateTime_numeric", index = "IYZ", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-04-18 09:35:00")), event.win = 77.5)

Case99.XLF <- evReturn(y = returns_df, firm = c("GLBC", "VOX", "IYZ"),
                       y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                       event.date = as.numeric(as.POSIXct("2007-04-18 09:35:00")), event.win = 77.5)

Case100 <- evReturn(y = returns_df, firm = c("MSFT", "TFX", "VOX", "IYZ", "XLF", "XLK", "XLV", "XLY"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-05-01 09:35:00")), event.win = 77.5)

Case100.IYZ <- evReturn(y = returns_df, firm = c("MSFT", "TFX", "VOX", "XLF", "XLK", "XLV", "XLY"),
                        y.date = "DateTime_numeric", index = "IYZ", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-01 09:35:00")), event.win = 77.5)

Case100.XLF <- evReturn(y = returns_df, firm = c("MSFT", "TFX", "VOX", "IYZ", "XLK", "XLV", "XLY"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-01 09:35:00")), event.win = 77.5)

Case100.XLK <- evReturn(y = returns_df, firm = c("MSFT", "TFX", "VOX", "IYZ", "XLF", "XLV", "XLY"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-01 09:35:00")), event.win = 77.5)

Case100.XLV <- evReturn(y = returns_df, firm = c("MSFT", "TFX", "VOX", "IYZ", "XLF", "XLK", "XLY"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-01 09:35:00")), event.win = 77.5)

Case100.XLY <- evReturn(y = returns_df, firm = c("MSFT", "TFX", "VOX", "IYZ", "XLF", "XLK", "XLV"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-01 09:35:00")), event.win = 77.5)

Case101 <- evReturn(y = returns_df, firm = c("VZ", "VOX", "IYZ"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-05-22 09:35:00")), event.win = 77.5)

Case101.VOX <- evReturn(y = returns_df, firm = c("VZ", "IYZ"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-22 09:35:00")), event.win = 77.5)

Case101.IYZ <- evReturn(y = returns_df, firm = c("VZ", "VOX"),
                        y.date = "DateTime_numeric", index = "IYZ", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-22 09:35:00")), event.win = 77.5)

Case102 <- evReturn(y = returns_df, firm = c("GT", "XLY"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-05-30 09:35:00")), event.win = 77.5)

Case102.XLY <- evReturn(y = returns_df, firm = c("GT"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-05-30 09:35:00")), event.win = 77.5)

Case103 <- evReturn(y = returns_df, firm = c("SAF", "KIE"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-06-05 09:35:00")), event.win = 77.5)

Case103.KIE <- evReturn(y = returns_df, firm = c("SAF"),
                        y.date = "DateTime_numeric", index = "KIE", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-06-05 09:35:00")), event.win = 77.5)

Case104 <- evReturn(y = returns_df, firm = c("MO", "XLP"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-06-12 09:35:00")), event.win = 77.5)

Case104.XLP <- evReturn(y = returns_df, firm = c("MO"),
                        y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-06-12 09:35:00")), event.win = 77.5)

Case105 <- evReturn(y = returns_df, firm = c("CLF", "NRG", "CNP", "XLE", "XLU"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-06-19 09:35:00")), event.win = 77.5)

Case105.XLE <- evReturn(y = returns_df, firm = c("CLF", "NRG", "CNP", "XLU"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-06-19 09:35:00")), event.win = 77.5)

Case105.XLU <- evReturn(y = returns_df, firm = c("CLF", "NRG", "CNP", "XLE"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-06-19 09:35:00")), event.win = 77.5)

Case106 <- evReturn(y = returns_df, firm = c("CS", "XLP"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-06-19 09:35:00")), event.win = 77.5)

Case106.XLF <- evReturn(y = returns_df, firm = c("CS"),
                        y.date = "DateTime_numeric", index = "XLP", est.win = 389, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-06-19 09:35:00")), event.win = 77.5)

Case107 <- evReturn(y = returns_df, firm = c("TLAB", "XLK"),
                    y.date = "DateTime_numeric", index = "SPY", est.win = 390, digits = 6,
                    event.date = as.numeric(as.POSIXct("2007-06-22 09:35:00")), event.win = 77.5)

Case107.XLK <- evReturn(y = returns_df, firm = c("TLAB"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6,
                        event.date = as.numeric(as.POSIXct("2007-06-22 09:35:00")), event.win = 77.5)

Case108 <- evReturn(y = returns_df, firm = c("CSX", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2007-12-05 09:35:00")), event.win = 77.5)

Case108.XLI <- evReturn(y = returns_df, firm = c("CSX"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2007-12-05 09:35:00")), event.win = 77.5)

Case109 <- evReturn(y = returns_df, firm = c("CSCO", "XLK"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-01-16 09:35:00")), event.win = 77.5)

Case109.XLK <- evReturn(y = returns_df, firm = c("CSCO"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-01-16 09:35:00")), event.win = 77.5)

Case110 <- evReturn(y = returns_df, firm = c("CB", "TRV", "CNA", "RLI", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 312, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-02-21 09:35:00")), event.win = 77.5)

Case110.XLF <- evReturn(y = returns_df, firm = c("CB", "TRV", "CNA", "RLI"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 312, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-02-21 09:35:00")), event.win = 77.5)

Case111 <- evReturn(y = returns_df, firm = c("MDT", "XLV"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 312, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-02-21 09:35:00")), event.win = 77.5)

Case111.XLV <- evReturn(y = returns_df, firm = c("MDT"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 312, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-02-21 09:35:00")), event.win = 77.5)

Case112 <- evReturn(y = returns_df, firm = c("S"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-02-27 09:35:00")), event.win = 77.5)

Case113 <- evReturn(y = returns_df, firm = c("FDX"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-02-28 09:35:00")), event.win = 77.5)

Case114 <- evReturn(y = returns_df, firm = c("MAT"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 312, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-03-26 09:35:00")), event.win = 77.5)

Case115 <- evReturn(y = returns_df, firm = c("TE"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-04-16 09:35:00")), event.win = 77.5)

Case116 <- evReturn(y = returns_df, firm = c("MWV"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-04-16 09:35:00")), event.win = 77.5)

Case117 <- evReturn(y = returns_df, firm = c("CBRL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-05-28 09:35:00")), event.win = 77.5) 

Case118 <- evReturn(y = returns_df, firm = c("XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-06-10 09:35:00")), event.win = 77.5)

Case119 <- evReturn(y = returns_df, firm = c("MET"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-06-20 09:35:00")), event.win = 77.5)

Case119.XLY <- evReturn(y = returns_df, firm = c("MET"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-06-20 09:35:00")), event.win = 77.5)

Case120 <- evReturn(y = returns_df, firm = c("S", "VZ", "T", "TMUS", "TDS", "XLY", "VOX"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-06-24 09:35:00")), event.win = 77.5)

Case120.XLY <- evReturn(y = returns_df, firm = c("S", "VZ", "T", "TMUS", "TDS", "VOX"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-06-24 09:35:00")), event.win = 77.5)

Case120.VOX <- evReturn(y = returns_df, firm = c("S", "VZ", "T", "TMUS", "TDS", "XLY"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-06-24 09:35:00")), event.win = 77.5)

Case121 <- evReturn(y = returns_df, firm = c("XLE", "XES", "XOM", "XOP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-06-27 09:35:00")), event.win = 77.5)

Case121.XLE <- evReturn(y = returns_df, firm = c("XOM", "XES", "XOP"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-06-27 09:35:00")), event.win = 77.5)

Case121.XES <- evReturn(y = returns_df, firm = c("XOM", "XLE", "XOP"),
                        y.date = "DateTime_numeric", index = "XES", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-06-27 09:35:00")), event.win = 77.5)

Case121.XOP <- evReturn(y = returns_df, firm = c("XOM", "XLE", "XES"),
                        y.date = "DateTime_numeric", index = "XOP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-06-27 09:35:00")), event.win = 77.5)

Case122 <- evReturn(y = returns_df, firm = c("MS", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-06-27 09:35:00")), event.win = 77.5)

Case122.XLF <- evReturn(y = returns_df, firm = c("MS"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-06-27 09:35:00")), event.win = 77.5)


Case123 <- evReturn(y = returns_df, firm = c("MO", "PM", "LO", "RAI", "XLP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2008-12-16 09:35:00")), event.win = 77.5)

Case123.XLP <- evReturn(y = returns_df, firm = c("MO", "PM", "LO", "RAI"),
                        y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2008-12-16 09:35:00")), event.win = 77.5)

Case124 <- evReturn(y = returns_df, firm = c("DD", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-01-27 09:35:00")), event.win = 77.5)

Case124.XLI <- evReturn(y = returns_df, firm = c("DD"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-01-27 09:35:00")), event.win = 77.5)

Case125 <- evReturn(y = returns_df, firm = c("T"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-02-26 09:35:00")), event.win = 77.5)

Case126 <- evReturn(y = returns_df, firm = c("PFE"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-03-05 09:35:00")), event.win = 77.5)


Case127 <- evReturn(y = returns_df, firm = c("DFS"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-03-10 09:35:00")), event.win = 77.5)

Case128 <- evReturn(y = returns_df, firm = c("MO"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-04-01 09:35:00")), event.win = 77.5)

Case129 <- evReturn(y = returns_df, firm = c("T", "XLY", "VOX"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-05-19 09:35:00")), event.win = 77.5)

Case129.XLY <- evReturn(y = returns_df, firm = c("T", "VOX"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-05-19 09:35:00")), event.win = 77.5)

Case129.VOX <- evReturn(y = returns_df, firm = c("T", "XLY"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-05-19 09:35:00")), event.win = 77.5)

Case130 <- evReturn(y = returns_df, firm = c("CSX", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-06-02 09:35:00")), event.win = 77.5)

Case130.XLI <- evReturn(y = returns_df, firm = c("CSX"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-06-02 09:35:00")), event.win = 77.5)

Case131 <- evReturn(y = returns_df, firm = c("TRV", "RLI", "CNA", "CB", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-06-19 09:35:00")), event.win = 77.5)

Case131.XLF <- evReturn(y = returns_df, firm = c("TRV", "RLI", "CNA", "CB"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-06-19 09:35:00")), event.win = 77.5)

Case132 <- evReturn(y = returns_df, firm = c("CDE", "CDM", "XLB"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-06-23 09:35:00")), event.win = 77.5)

Case132.XLB <- evReturn(y = returns_df, firm = c("CDE", "CDM"),
                        y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-06-23 09:35:00")), event.win = 77.5)

Case133 <- evReturn(y = returns_df, firm = c("JPM", "PNC", "AXP", "C.",
                                             "V", "DFS","WFC", "FMER", "FITB", "WTFC", "USB", "MA", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-06-30 09:35:00")), event.win = 77.5)

Case133.XLF <- evReturn(y = returns_df, firm = c("JPM", "PNC", "AXP", "C.",
                                                 "V", "DFS","WFC", "FMER", "FITB", "WTFC", "USB", "MA"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-06-30 09:35:00")), event.win = 77.5)

Case134 <- evReturn(y = returns_df, firm = c("MHK", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-12-09 09:35:00")), event.win = 77.5)

Case134.XLY <- evReturn(y = returns_df, firm = c("MHK"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-12-09 09:35:00")), event.win = 77.5)

Case135 <- evReturn(y = returns_df, firm = c("UNP", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2009-12-09 09:35:00")), event.win = 77.5)

Case135.XLI <- evReturn(y = returns_df, firm = c("UNP"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2009-12-09 09:35:00")), event.win = 77.5)

Case136 <- evReturn(y = returns_df, firm = c("NRG", "AEP", "XLU"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 389, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-01-14 09:35:00")), event.win = 77.5)

Case136.XLU <- evReturn(y = returns_df, firm = c("NRG", "AEP"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-01-14 09:35:00")), event.win = 77.5)

Case137 <- evReturn(y = returns_df, firm = c("DUK", "XLU"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-01-21 09:35:00")), event.win = 77.5)

Case137.XLU <- evReturn(y = returns_df, firm = c("DUK"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-01-21 09:35:00")), event.win = 77.5)

Case138 <- evReturn(y = returns_df, firm = c("MO", "PM", "LO", "RAI", "XLP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-01-26 09:35:00")), event.win = 77.5)

Case138.XLP <- evReturn(y = returns_df, firm = c("MO", "PM", "LO", "RAI"),
                        y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-01-26 09:35:00")), event.win = 77.5)

Case139 <- evReturn(y = returns_df, firm = c("XOM", "BP", "XLE", "XES", "XOP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-03-03 09:35:00")), event.win = 77.5) 

Case139.XLE <- evReturn(y = returns_df, firm = c("XOM", "BP", "XES", "XOP"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-03-03 09:35:00")), event.win = 77.5) 

Case139.XES <- evReturn(y = returns_df, firm = c("XOM", "BP", "XLE", "XOP"),
                        y.date = "DateTime_numeric", index = "XES", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-03-03 09:35:00")), event.win = 77.5) 

Case139.XOP <- evReturn(y = returns_df, firm = c("XOM", "BP", "XLE", "XES"),
                        y.date = "DateTime_numeric", index = "XOP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-03-03 09:35:00")), event.win = 77.5)

Case140 <- evReturn(y = returns_df, firm = c("ENL", "MHFI", "TRI", "RUK", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-03-03 09:35:00")), event.win = 77.5)

Case140.XLI <- evReturn(y = returns_df, firm = c("ENL", "MHFI", "TRI", "RUK"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-03-03 09:35:00")), event.win = 77.5)

Case141 <- evReturn(y = returns_df, firm = c("GS", "UBS", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-03-09 09:35:00")), event.win = 77.5)

Case141.XLF <- evReturn(y = returns_df, firm = c("GS", "UBS"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-03-09 09:35:00")), event.win = 77.5)

Case142 <- evReturn(y = returns_df, firm = c("EVR", "MS", "UBS"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-03-31 09:35:00")), event.win = 77.5)

Case143 <- evReturn(y = returns_df, firm = c("ALL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-04-01 09:35:00")), event.win = 77.5)

Case144 <- evReturn(y = returns_df, firm = c("XRX", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-04-22 09:35:00")), event.win = 77.5) 

Case144.XLI <- evReturn(y = returns_df, firm = c("XRX"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-04-22 09:35:00")), event.win = 77.5) 

Case145 <- evReturn(y = returns_df, firm = c("MRK", "XLV", "XPH"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-04-28 09:35:00")), event.win = 77.5)

Case145.XLV <- evReturn(y = returns_df, firm = c("MRK", "XPH"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-04-28 09:35:00")), event.win = 77.5)

Case145.XPH <- evReturn(y = returns_df, firm = c("MRK", "XLV"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-04-28 09:35:00")), event.win = 77.5)

Case146 <- evReturn(y = returns_df, firm = c("RBC", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-06-22 09:35:00")), event.win = 77.5)

Case146.XLF <- evReturn(y = returns_df, firm = c("RBC"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-06-22 09:35:00")), event.win = 77.5)

Case147 <- evReturn(y = returns_df, firm = c("MON", "XLP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-06-22 09:35:00")), event.win = 77.5)

Case147.XLP <- evReturn(y = returns_df, firm = c("MON"),
                        y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-06-22 09:35:00")), event.win = 77.5)


Case148 <- evReturn(y = returns_df, firm = c("COST", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2010-12-14 09:35:00")), event.win = 77.5)

Case148.XLY <- evReturn(y = returns_df, firm = c("COST"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2010-12-14 09:35:00")), event.win = 77.5)

Case149 <- evReturn(y = returns_df, firm = c("PNC", "AXP", "C.", "V", "DFS", 
                                             "WFC", "FMER", "FITB", "HBAN", 
                                             "WTFC", "USB", "MA", "WEX", "COF", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-01-25 09:35:00")), event.win = 77.5)

Case149.XLF <- evReturn(y = returns_df, firm = c("PNC", "AXP", "C.", "V", "DFS", 
                                                 "WFC", "FMER", "FITB", "HBAN", 
                                                 "WTFC", "USB", "MA", "WEX", "COF"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-01-25 09:35:00")), event.win = 77.5)

Case150 <- evReturn(y = returns_df, firm = c("PFE", "XLV", "XPH"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-02-23 09:35:00")), event.win = 77.5)

Case150.XLV <- evReturn(y = returns_df, firm = c("PFE", "XPH"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-02-23 09:35:00")), event.win = 77.5)

Case150.XPH <- evReturn(y = returns_df, firm = c("PFE", "XLV"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-02-23 09:35:00")), event.win = 77.5)

Case151 <- evReturn(y = returns_df, firm = c("CSX", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-02-23 09:35:00")), event.win = 77.5)

Case151.XLI <- evReturn(y = returns_df, firm = c("CSX"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-02-23 09:35:00")), event.win = 77.5)

Case152 <- evReturn(y = returns_df, firm = c("F", "GM", "TM", "HMC", "TSLA", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-02-24 09:35:00")), event.win = 77.5)

Case152.XLY <- evReturn(y = returns_df, firm = c("F", "GM", "TM", "HMC", "TSLA"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-02-24 09:35:00")), event.win = 77.5)

Case153 <- evReturn(y = returns_df, firm = c("T", "XLY", "VOX", "XTL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-03-02 09:35:00")), event.win = 77.5)

Case153.XLY <- evReturn(y = returns_df, firm = c("T", "VOX", "XTL"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-03-02 09:35:00")), event.win = 77.5)

Case153.VOX <- evReturn(y = returns_df, firm = c("T", "XLY", "XTL"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-03-02 09:35:00")), event.win = 77.5)

Case153.XTL <- evReturn(y = returns_df, firm = c("T", "XLY", "VOX"),
                        y.date = "DateTime_numeric", index = "XTL", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-03-02 09:35:00")), event.win = 77.5)

Case154 <- evReturn(y = returns_df, firm = c("XLV", "XPH"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-03-30 09:35:00")), event.win = 77.5)

Case154.XLV <- evReturn(y = returns_df, firm = c("XPH"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-03-30 09:35:00")), event.win = 77.5)

Case154.XPH <- evReturn(y = returns_df, firm = c("XLV"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-03-30 09:35:00")), event.win = 77.5)

Case155 <- evReturn(y = returns_df, firm = c("T", "VOX", "XTL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 312, digits = 6,
                    event.date = as.numeric(as.POSIXct("2011-04-28 09:35:00")), event.win = 77.5)

Case155.VOX <- evReturn(y = returns_df, firm = c("T", "XTL"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 312, digits = 6,
                        event.date = as.numeric(as.POSIXct("2011-04-28 09:35:00")), event.win = 77.5)

Case155.XTL <- evReturn(y = returns_df, firm = c("T", "VOX"),
                        y.date = "DateTime_numeric", index = "XTL", est.win = 312, digits = 6,
                        event.date = as.numeric(as.POSIXct("2011-04-28 09:35:00")), event.win = 77.5)

Case156 <- evReturn(y = returns_df, firm = c("CI", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-05-17 09:35:00")), event.win = 77.5) 

Case156.XLF <- evReturn(y = returns_df, firm = c("CI"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-05-17 09:35:00")), event.win = 77.5)

Case157 <- evReturn(y = returns_df, firm = c("GD", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-05-24 09:35:00")), event.win = 77.5)

Case157.XLI <- evReturn(y = returns_df, firm = c("GD"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-05-24 09:35:00")), event.win = 77.5)

Case158 <- evReturn(y = returns_df, firm = c("JAH", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-01 09:35:00")), event.win = 77.5)

Case158.XLY <- evReturn(y = returns_df, firm = c("JAH"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-01 09:35:00")), event.win = 77.5)

Case159 <- evReturn(y = returns_df, firm = c("HAL", "XLE", "XES", "XOP", "GNR"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 389, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-07 09:35:00")), event.win = 77.5)

Case159.XLE <- evReturn(y = returns_df, firm = c("HAL", "XES", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-07 09:35:00")), event.win = 77.5)

Case159.XES <- evReturn(y = returns_df, firm = c("HAL", "XLE", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XES", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-07 09:35:00")), event.win = 77.5)

Case159.XOP <- evReturn(y = returns_df, firm = c("HAL", "XLE", "XES", "GNR"),
                        y.date = "DateTime_numeric", index = "XOP", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-07 09:35:00")), event.win = 77.5)

Case159.GNR <- evReturn(y = returns_df, firm = c("HAL", "XLE", "XES", "XOP"),
                        y.date = "DateTime_numeric", index = "GNR", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-07 09:35:00")), event.win = 77.5)

Case160 <- evReturn(y = returns_df, firm = c("MSFT", "XLK"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-10 09:35:00")), event.win = 77.5)

Case160.XLK <- evReturn(y = returns_df, firm = c("MSFT"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-10 09:35:00")), event.win = 77.5)

Case161 <- evReturn(y = returns_df, firm = c("WIN", "XLK"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-10 09:35:00")), event.win = 77.5)

Case161.XLK <- evReturn(y = returns_df, firm = c("WIN"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-10 09:35:00")), event.win = 77.5)

Case162 <- evReturn(y = returns_df, firm = c("JNS", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-14 09:35:00")), event.win = 77.5)

Case162.XLF <- evReturn(y = returns_df, firm = c("JNS"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-14 09:35:00")), event.win = 77.5)

Case163 <- evReturn(y = returns_df, firm = c("WMT", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-21 09:35:00")), event.win = 77.5)

Case163.XLY <- evReturn(y = returns_df, firm = c("WMT"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-21 09:35:00")), event.win = 77.5)

Case164 <- evReturn(y = returns_df, firm = c("AEP", "XLU"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-21 09:35:00")), event.win = 77.5)

Case164.XLU <- evReturn(y = returns_df, firm = c("AEP"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-21 09:35:00")), event.win = 77.5)

Case165 <- evReturn(y = returns_df, firm = c("CSX", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-24 09:35:00")), event.win = 77.5)

Case165.XLI <- evReturn(y = returns_df, firm = c("CSX"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-24 09:35:00")), event.win = 77.5)

Case166 <- evReturn(y = returns_df, firm = c("GT", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-28 09:35:00")), event.win = 77.5)

Case166.XLY <- evReturn(y = returns_df, firm = c("GT"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-28 09:35:00")), event.win = 77.5)

Case167 <- evReturn(y = returns_df, firm = c("EA", "ATVI", "KNM", "TTWO", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2011-06-28 09:35:00")), event.win = 77.5)

Case167.XLY <- evReturn(y = returns_df, firm = c("EA", "ATVI", "KNM", "TTWO"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2011-06-28 09:35:00")), event.win = 77.5)


Case168 <- evReturn(y = returns_df, firm = c("ATLC", "BAC", "COF", "C.", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-01-11 09:35:00")), event.win = 77.5)

Case168.XLF <- evReturn(y = returns_df, firm = c("ATLC", "BAC", "COF", "C."),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-01-11 09:35:00")), event.win = 77.5)

Case169 <- evReturn(y = returns_df, firm = c("GOOGL", "XLK"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 312, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-01-19 09:35:00")), event.win = 77.5)

Case169.XLK <- evReturn(y = returns_df, firm = c("GOOGL"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 312, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-01-19 09:35:00")), event.win = 77.5)

Case170 <- evReturn(y = returns_df, firm = c("TSN", "XLP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-01-24 09:35:00")), event.win = 77.5)

Case170.XLP <- evReturn(y = returns_df, firm = c("TSN"),
                        y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-01-24 09:35:00")), event.win = 77.5)

Case171 <- evReturn(y = returns_df, firm = c("CS", "C.", "DB", "UBS", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-03-27 09:35:00")), event.win = 77.5)

Case171.XLF <- evReturn(y = returns_df, firm = c("CS", "C.", "DB", "UBS"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-03-27 09:35:00")), event.win = 77.5)

Case172 <- evReturn(y = returns_df, firm = c("TEVA", "ACT", "MYL", "XLV", "XPH", "XBI", "XHE"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 389, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-04-18 09:35:00")), event.win = 77.5)

Case172.XLV <- evReturn(y = returns_df, firm = c("TEVA", "ACT", "MYL", "XPH", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-04-18 09:35:00")), event.win = 77.5)

Case172.XPH <- evReturn(y = returns_df, firm = c("TEVA", "ACT", "MYL", "XLV", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-04-18 09:35:00")), event.win = 77.5)

Case172.XBI <- evReturn(y = returns_df, firm = c("TEVA", "ACT", "MYL", "XLV", "XPH", "XHE"),
                        y.date = "DateTime_numeric", index = "XBI", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-04-18 09:35:00")), event.win = 77.5)

Case172.XHE <- evReturn(y = returns_df, firm = c("TEVA", "ACT", "MYL", "XLV", "XPH", "XBI"),
                        y.date = "DateTime_numeric", index = "XHE", est.win = 389, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-04-18 09:35:00")), event.win = 77.5)

Case173 <- evReturn(y = returns_df, firm = c("STN", "BYI", "IGT", "XLI", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case173.XLI <- evReturn(y = returns_df, firm = c("STN", "BYI", "IGT", "XLY"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case173.XLY <- evReturn(y = returns_df, firm = c("STN", "BYI", "IGT", "XLI"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case174 <- evReturn(y = returns_df, firm = c("GSK", "PFE", "JNJ", "XLV", "XPH", "XBI", "XHE"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case174.XLV <- evReturn(y = returns_df, firm = c("GSK", "PFE", "JNJ", "XPH", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case174.XPH <- evReturn(y = returns_df, firm = c("GSK", "PFE", "JNJ", "XLV", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case174.XBI <- evReturn(y = returns_df, firm = c("GSK", "PFE", "JNJ", "XLV", "XPH", "XHE"),
                        y.date = "DateTime_numeric", index = "XBI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case174.XHE <- evReturn(y = returns_df, firm = c("GSK", "PFE", "JNJ", "XLV", "XPH", "XBI"),
                        y.date = "DateTime_numeric", index = "XHE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-19 09:35:00")), event.win = 77.5)

Case175 <- evReturn(y = returns_df, firm = c("ETE", "ETP", "DPM", "ENB", "KMI", "APU",
                                             "FGP", "XLE", "XES", "XLU", "XOP", "GNR"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case175.XLE <- evReturn(y = returns_df, firm = c("ETE", "ETP", "DPM", "ENB", "KMI", "APU",
                                                 "FGP", "XES", "XLU", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case175.XES <- evReturn(y = returns_df, firm = c("ETE", "ETP", "DPM", "ENB", "KMI", "APU",
                                                 "FGP", "XLE", "XLU", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XES", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case175.XLU <- evReturn(y = returns_df, firm = c("ETE", "ETP", "DPM", "ENB", "KMI", "APU",
                                                 "FGP", "XLE", "XES", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case175.XOP <- evReturn(y = returns_df, firm = c("ETE", "ETP", "DPM", "ENB", "KMI", "APU",
                                                 "FGP", "XLE", "XES", "XLU", "GNR"),
                        y.date = "DateTime_numeric", index = "XOP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case175.GNR <- evReturn(y = returns_df, firm = c("ETE", "ETP", "DPM", "ENB", "KMI", "APU",
                                                 "FGP", "XLE", "XES", "XLU", "XOP"),
                        y.date = "DateTime_numeric", index = "GNR", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case176 <- evReturn(y = returns_df, firm = c("FOXA", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case176.XLY <- evReturn(y = returns_df, firm = c("FOXA"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-22 09:35:00")), event.win = 77.5)

Case177 <- evReturn(y = returns_df, firm = c("AET", "CI", "WLP", "HCA", "HNT",
                                             "HUM", "SFG", "CNC", "MGLN", "WCG", "XLV"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2012-06-29 09:35:00")), event.win = 77.5)

Case177.XLV <- evReturn(y = returns_df, firm = c("AET", "CI", "WLP", "HCA", "HNT",
                                                 "HUM", "SFG", "CNC", "MGLN", "WCG"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2012-06-29 09:35:00")), event.win = 77.5)

Case178 <- evReturn(y = returns_df, firm = c("NKE", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-01-10 09:35:00")), event.win = 77.5)

Case178.XLY <- evReturn(y = returns_df, firm = c("NKE"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-01-10 09:35:00")), event.win = 77.5)


Case179 <- evReturn(y = returns_df, firm = c("AMGN", "XLV"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-02-28 09:35:00")), event.win = 77.5)

Case179.XLV <- evReturn(y = returns_df, firm = c("AMGN"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-02-28 09:35:00")), event.win = 77.5)

Case180 <- evReturn(y = returns_df, firm = c("JWA", "PSO", "TRI", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-03-20 09:35:00")), event.win = 77.5)

Case180.XLY <- evReturn(y = returns_df, firm = c("JWA", "PSO", "TRI"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-03-20 09:35:00")), event.win = 77.5)

Case181 <- evReturn(y = returns_df, firm = c("TRV", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-03-20 09:35:00")), event.win = 77.5)

Case181.XLF <- evReturn(y = returns_df, firm = c("TRV"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-03-20 09:35:00")), event.win = 77.5)

Case182 <- evReturn(y = returns_df, firm = c("IP", "XLB"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-03-21 09:35:00")), event.win = 77.5)

Case182.XLB <- evReturn(y = returns_df, firm = c("IP"),
                        y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-03-21 09:35:00")), event.win = 77.5)

Case183 <- evReturn(y = returns_df, firm = c("CMCSA", "CMCSK", "DTV", "DISH", 
                                             "VZ", "NFLX", "XLY", "VOX", "XTL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-03-28 09:35:00")), event.win = 77.5)

Case183.XLY <- evReturn(y = returns_df, firm = c("CMCSA", "CMCSK", "DTV", "DISH",
                                                 "VZ", "NFLX", "VOX", "XTL"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-03-28 09:35:00")), event.win = 77.5)

Case183.VOX <- evReturn(y = returns_df, firm = c("CMCSA", "CMCSK", "DTV", "DISH",
                                                 "VZ", "NFLX", "XLY", "XTL"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-03-28 09:35:00")), event.win = 77.5)

Case183.XTL <- evReturn(y = returns_df, firm = c("CMCSA", "CMCSK", "DTV", "DISH",
                                                 "VZ", "NFLX", "XLY", "VOX"),
                        y.date = "DateTime_numeric", index = "XTL", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-03-28 09:35:00")), event.win = 77.5)

Case184 <- evReturn(y = returns_df, firm = c("RDSA", "XLE", "XES", "XOP", "GNR"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-04-18 09:35:00")), event.win = 77.5)

Case184.XLE <- evReturn(y = returns_df, firm = c("RDSA", "XES", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-04-18 09:35:00")), event.win = 77.5)

Case184.XES <- evReturn(y = returns_df, firm = c("RDSA", "XLE", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XES", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-04-18 09:35:00")), event.win = 77.5)

Case184.XOP <- evReturn(y = returns_df, firm = c("RDSA", "XLE", "XES", "GNR"),
                        y.date = "DateTime_numeric", index = "XOP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-04-18 09:35:00")), event.win = 77.5)

Case184.GNR <- evReturn(y = returns_df, firm = c("RDSA", "XLE", "XES", "XOP"),
                        y.date = "DateTime_numeric", index = "GNR", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-04-18 09:35:00")), event.win = 77.5)

Case185 <- evReturn(y = returns_df, firm = c("MON", "SYT", "XLB", "XLP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-05-14 09:35:00")),event.win = 77.5)

Case185.XLB <- evReturn(y = returns_df, firm = c("MON", "SYT", "XLP"),
                        y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-05-14 09:35:00")),event.win = 77.5)

Case185.XLP <- evReturn(y = returns_df, firm = c("MON", "SYT", "XLP"),
                        y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-05-14 09:35:00")),event.win = 77.5)

Case186 <- evReturn(y = returns_df, firm = c("PPL", "XLU"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-05-21 09:35:00")), event.win = 77.5)

Case186.XLU <- evReturn(y = returns_df, firm = c("PPL"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-05-21 09:35:00")), event.win = 77.5)

Case187 <- evReturn(y = returns_df, firm = c("MYGN", "ABT", "AMGN", "XLV", "XPH", "XBI", "XHE"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-06-14 09:35:00")), event.win = 77.5)

Case187.XLV <- evReturn(y = returns_df, firm = c("MYGN", "ABT", "AMGN", "XPH", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-14 09:35:00")), event.win = 77.5)

Case187.XPH <- evReturn(y = returns_df, firm = c("MYGN", "ABT", "AMGN", "XLV", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-14 09:35:00")), event.win = 77.5)

Case187.XBI <- evReturn(y = returns_df, firm = c("MYGN", "ABT", "AMGN", "XLV", "XPH", "XHE"),
                        y.date = "DateTime_numeric", index = "XBI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-14 09:35:00")), event.win = 77.5)

Case187.XHE <- evReturn(y = returns_df, firm = c("MYGN", "ABT", "AMGN", "XLV", "XPH", "XBI"),
                        y.date = "DateTime_numeric", index = "XHE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-14 09:35:00")), event.win = 77.5)

Case188 <- evReturn(y = returns_df, firm = c("ACT", "MYL", "NVS", "TEVA", "XLV", "XPH", "XBI", "XHE"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-06-18 09:35:00")), event.win = 77.5)

Case188.XLV <- evReturn(y = returns_df, firm = c("ACT", "MYL", "NVS", "TEVA", "XPH", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-18 09:35:00")), event.win = 77.5)

Case188.XPH <- evReturn(y = returns_df, firm = c("ACT", "MYL", "NVS", "TEVA", "XLV", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-18 09:35:00")), event.win = 77.5)

Case188.XBI <- evReturn(y = returns_df, firm = c("ACT", "MYL", "NVS", "TEVA", "XLV", "XPH", "XHE"),
                        y.date = "DateTime_numeric", index = "XBI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-18 09:35:00")), event.win = 77.5)

Case188.XHE <- evReturn(y = returns_df, firm = c("ACT", "MYL", "NVS", "TEVA", "XLV", "XPH", "XBI"),
                        y.date = "DateTime_numeric", index = "XHE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-18 09:35:00")), event.win = 77.5)

Case189 <- evReturn(y = returns_df, firm = c("AXP", "DFS", "MA", "V", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-06-21 09:35:00")), event.win = 77.5)

Case189.XLF <- evReturn(y = returns_df, firm = c("AXP", "DFS", "MA", "V"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-21 09:35:00")), event.win = 77.5)

Case190 <- evReturn(y = returns_df, firm = c("PRGO", "XLV", "XPH", "XBI", "XHE"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-06-25 09:35:00")), event.win = 77.5)

Case190.XLV <- evReturn(y = returns_df, firm = c("PRGO", "XPH", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XLV", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-25 09:35:00")), event.win = 77.5)

Case190.XPH <- evReturn(y = returns_df, firm = c("PRGO", "XLV", "XBI", "XHE"),
                        y.date = "DateTime_numeric", index = "XPH", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-25 09:35:00")), event.win = 77.5)

Case190.XBI <- evReturn(y = returns_df, firm = c("PRGO", "XLV", "XPH", "XHE"),
                        y.date = "DateTime_numeric", index = "XBI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-25 09:35:00")), event.win = 77.5)

Case190.XHE <- evReturn(y = returns_df, firm = c("PRGO", "XLV", "XPH", "XBI"),
                        y.date = "DateTime_numeric", index = "XHE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-06-25 09:35:00")), event.win = 77.5)

Case191 <- evReturn(y = returns_df, firm = c("F"),
                    y.date = "DateTime_numeric", index = "XLY", est.win = 312, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-12-03 09:35:00")), event.win = 77.5)

Case192 <- evReturn(y = returns_df, firm = c("S", "XLY", "VOX", "XTL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-12-11 09:35:00")), event.win = 77.5)

Case192.XLY <- evReturn(y = returns_df, firm = c("S", "VOX", "XTL"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-12-11 09:35:00")), event.win = 77.5)

Case192.VOX <- evReturn(y = returns_df, firm = c("S", "XLY", "XTL"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-12-11 09:35:00")), event.win = 77.5)

Case192.XTL <- evReturn(y = returns_df, firm = c("S", "XLY", "VOX"),
                        y.date = "DateTime_numeric", index = "XTL", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-12-11 09:35:00")), event.win = 77.5)

Case193 <- evReturn(y = returns_df, firm = c("WMT", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2013-12-17 09:35:00")), event.win = 77.5)

Case193.XLY <- evReturn(y = returns_df, firm = c("WMT"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2013-12-17 09:35:00")), event.win = 77.5)

Case194 <- evReturn(y = returns_df, firm = c("AUO", "XLK"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 382, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-01-15 09:35:00")), event.win = 77.5)

Case194.XLK <- evReturn(y = returns_df, firm = c("AUO"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-01-15 09:35:00")), event.win = 77.5)

Case195 <- evReturn(y = returns_df, firm = c("MDT"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 312, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-01-23 09:35:00")), event.win = 77.5)

Case196 <- evReturn(y = returns_df, firm = c("X", "XLB"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-01-28 09:35:00")), event.win = 77.5)

Case196.XLB <- evReturn(y = returns_df, firm = c("X"),
                        y.date = "DateTime_numeric", index = "XLB", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-01-28 09:35:00")), event.win = 77.5)

Case197 <- evReturn(y = returns_df, firm = c("LXK", "MMM", "XLK", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-03-26 09:35:00")), event.win = 77.5)

Case197.XLK <- evReturn(y = returns_df, firm = c("LXK", "MMM", "XLI"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-03-26 09:35:00")), event.win = 77.5)

Case197.XLI <- evReturn(y = returns_df, firm = c("LXK", "MMM", "XLK"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-03-26 09:35:00")), event.win = 77.5)

Case198 <- evReturn(y = returns_df, firm = c("DAL", "ALK", "UAL", "LUV", 
                                             "ALGT", "JBLU", "SAVE", "AAL", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-04-03 09:35:00")), event.win = 77.5)

Case198.XLI <- evReturn(y = returns_df, firm = c("DAL", "ALK", "UAL", "LUV", 
                                                 "ALGT", "JBLU", "SAVE", "AAL"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-04-03 09:35:00")), event.win = 77.5)

Case199 <- evReturn(y = returns_df, firm = c("CBS", "DIS", "TWX", "LGF", "FOXA", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-05-20 09:35:00")), event.win = 77.5)

Case199.XLY <- evReturn(y = returns_df, firm = c("CBS", "DIS", "TWX", "LGF", "FOXA"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-05-20 09:35:00")), event.win = 77.5)

Case200 <- evReturn(y = returns_df, firm = c("LLNW", "AKAM", "XLK"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-03 09:35:00")), event.win = 77.5)

Case200.XLK <- evReturn(y = returns_df, firm = c("LLNW", "AKAM"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-03 09:35:00")), event.win = 77.5)
Case201 <- evReturn(y = returns_df, firm = c("NLS", "XLY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-03 09:35:00")), event.win = 77.5)

Case201.XLY <- evReturn(y = returns_df, firm = c("NLS"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-03 09:35:00")), event.win = 77.5)

Case202 <- evReturn(y = returns_df, firm = c("CTS", "SANM", "XLI", "XLK"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-10 09:35:00")), event.win = 77.5)

Case202.XLI <- evReturn(y = returns_df, firm = c("CTS", "SANM", "XLK"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-10 09:35:00")), event.win = 77.5)

Case202.XLK <- evReturn(y = returns_df, firm = c("CTS", "SANM", "XLI"),
                        y.date = "DateTime_numeric", index = "XLK", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-10 09:35:00")), event.win = 77.5)

Case203 <- evReturn(y = returns_df, firm = c("KO", "DPS", "PEP", "XLP"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-13 09:35:00")), event.win = 77.5)

Case203.XLP <- evReturn(y = returns_df, firm = c("KO", "DPS", "PEP"),
                        y.date = "DateTime_numeric", index = "XLP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-13 09:35:00")), event.win = 77.5)

Case204 <- evReturn(y = returns_df, firm = c("SWHC", "RGR", "XLI"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-17 09:35:00")), event.win = 77.5)

Case204.XLI <- evReturn(y = returns_df, firm = c("SWHC", "RGR"),
                        y.date = "DateTime_numeric", index = "XLI", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-17 09:35:00")), event.win = 77.5)

Case205 <- evReturn(y = returns_df, firm = c("PCY"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-17 09:35:00")), event.win = 77.5)

Case206 <- evReturn(y = returns_df, firm = c("JPM", "WFC", "PNC", "C.", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case206.XLF <- evReturn(y = returns_df, firm = c("JPM", "WFC", "PNC", "C."),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case207 <- evReturn(y = returns_df, firm = c("DUK", "AEP", "CNP", "ETR", "XLU", "XES", "XLE", "XOP", "GNR"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case207.XLU <- evReturn(y = returns_df, firm = c("DUK", "AEP", "CNP", "ETR", "XES", "XLE", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case207.XES <- evReturn(y = returns_df, firm = c("DUK", "AEP", "CNP", "ETR", "XLU", "XLE", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XES", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case207.XLE <- evReturn(y = returns_df, firm = c("DUK", "AEP", "CNP", "ETR", "XLU", "XES", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case207.XOP <- evReturn(y = returns_df, firm = c("DUK", "AEP", "CNP", "ETR", "XLU", "XES", "XLE", "GNR"),
                        y.date = "DateTime_numeric", index = "XOP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case207.GNR <- evReturn(y = returns_df, firm = c("DUK", "AEP", "CNP", "ETR", "XLU", "XES", "XLE", "XOP"),
                        y.date = "DateTime_numeric", index = "GNR", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case208 <- evReturn(y = returns_df, firm = c("HAL", "BHI", "SLP", "XLU", "XES", "XLE", "XOP", "GNR"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case208.XLU <- evReturn(y = returns_df, firm = c("HAL", "BHI", "SLP", "XES", "XLE", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLU", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case208.XES <- evReturn(y = returns_df, firm = c("HAL", "BHI", "SLP", "XLU", "XLE", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XES", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case208.XLE <- evReturn(y = returns_df, firm = c("HAL", "BHI", "SLP", "XLU", "XES", "XOP", "GNR"),
                        y.date = "DateTime_numeric", index = "XLE", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case208.XOP <- evReturn(y = returns_df, firm = c("HAL", "BHI", "SLP", "XLU", "XES", "XLE", "GNR"),
                        y.date = "DateTime_numeric", index = "XOP", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case208.GNR <- evReturn(y = returns_df, firm = c("HAL", "BHI", "SLP", "XLU", "XES", "XLE", "XOP"),
                        y.date = "DateTime_numeric", index = "GNR", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-24 09:35:00")), event.win = 77.5)

Case209 <- evReturn(y = returns_df, firm = c("CBS", "SBGI", "MEG", "FOXA", 
                                             "VIAB", "CMCSA", "XLY", "VOX", "XTL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case209.XLY <- evReturn(y = returns_df, firm = c("CBS", "SBGI", "MEG", "FOXA", 
                                                 "VIAB", "CMCSA", "VOX", "XTL"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case209.VOX <- evReturn(y = returns_df, firm = c("CBS", "SBGI", "MEG", "FOXA", 
                                                 "VIAB", "CMCSA", "XLY", "XTL"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case209.XTL <- evReturn(y = returns_df, firm = c("CBS", "SBGI", "MEG", "FOXA", 
                                                 "VIAB", "CMCSA", "XLY", "VOX"),
                        y.date = "DateTime_numeric", index = "XTL", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case210 <- evReturn(y = returns_df, firm = c("FITB", "PNC", "BAC", "USB", "XLF"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case210.XLF <- evReturn(y = returns_df, firm = c("FITB", "PNC", "BAC", "USB"),
                        y.date = "DateTime_numeric", index = "XLF", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case211 <- evReturn(y = returns_df, firm = c("VZ", "T", "S", "AAPL", "XLY", "VOX", "XTL"),
                    y.date = "DateTime_numeric", index = "SP", est.win = 390, digits = 6, 
                    event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case211.XLY <- evReturn(y = returns_df, firm = c("VZ", "T", "S", "AAPL", "VOX", "XTL"),
                        y.date = "DateTime_numeric", index = "XLY", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case211.VOX <- evReturn(y = returns_df, firm = c("VZ", "T", "S", "AAPL", "XLY", "XTL"),
                        y.date = "DateTime_numeric", index = "VOX", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)

Case211.XTL <- evReturn(y = returns_df, firm = c("VZ", "T", "S", "AAPL", "XLY", "VOX"),
                        y.date = "DateTime_numeric", index = "XTL", est.win = 390, digits = 6, 
                        event.date = as.numeric(as.POSIXct("2014-06-26 09:35:00")), event.win = 77.5)


#==================================
#Displaying Individual Case Results
#==================================
#Example method for displaying results using Case 1:
# Case1 #Display all three: $reg, $abc, $abr (see below)
# Case1$reg regression coefficients by firm
# Case1$abc #abnormal returns by day over the event window and by firm
# Case1$abr #average abnormal returns across firms
# Case1$daEst #data used to estimate the market model for the last firm as specified in codefirm
# Case1$daEve #data over the event window for the last firm
# Case1$ra #fitted market model for the last firm

#==================================
#Plotting Individual Case Results
#==================================
# plot average cumulative abnormal returns from event analysis versus days in event window.
# plot(Case1) 








