# Required packages
library(tidyverse)
library(lubridate)

setwd("SPY_Investigation")

# Asset and Date information
asset <- "SPY"
level <- 10

existing_files <- dir(pattern=paste0(asset, "_(.*)_",level), path="data/lobster", full.names = TRUE)
dates <- gsub(paste0(".*/",asset, "_(.*)_3420.*"), "\\1", existing_files)
dates <- unique(dates)

n <- as.integer(Sys.getenv("SGE_TASK_ID"))

date <- dates[n]

# Read in Data
messages_filename <- paste0("data/lobster/",asset, "_", date,"_34200000_57600000_message_", level, ".csv")
orderbook_filename <- paste0("data/lobster/",asset, "_", date,"_34200000_57600000_orderbook_", level, ".csv")

store_output <- paste0("output/summary_files/LOBSTER_", asset, "_", date,"_summary.csv")

if (all(file.exists(c(orderbook_filename, messages_filename)))) {
    
  messages_raw <- read_csv(messages_filename, 
                           col_names = c("ts", "type", "order_id", "m_size", "m_price", "direction", "null"), 
                           col_types = cols(
                             ts = col_double(),
                             type = col_integer(),
                             order_id = col_integer(),
                             m_size = col_double(),
                             m_price = col_double(),
                             direction = col_integer(),
                             null = col_skip())) %>% 
    mutate(ts = as.POSIXct(ts, origin=date, tz="GMT"), 
           m_price = m_price / 10000)
  
  
  orderbook_raw <- read_csv(orderbook_filename,
                            col_names = paste(rep(c("ask_price", "ask_size", "bid_price", "bid_size"), level), rep(1:level, each=4), sep="_"),
                            cols(.default = col_double())) %>% 
    mutate_at(vars(contains("price")), ~./10000)
  
  orderbook <- bind_cols(messages_raw, orderbook_raw) 
  orderbook <- orderbook %>% filter(type!=6)

  compute_depth <- function(df, side = "bid", bp = 0){
    if(side =="bid"){
      value_bid <- (1-bp/10000)*df %>% select("bid_price_1") 
      index_bid <- df %>% select(contains("bid_price")) %>% mutate_all(function(x) {x >= value_bid})
      sum_vector <- (df %>% select(contains("bid_size"))*index_bid) %>% rowSums()
    }else{
      value_ask <- (1+bp/10000)*df %>% select("ask_price_1")
      index_ask <- df %>% select(contains("ask_price")) %>% mutate_all(function(x) {x <= value_ask})
      sum_vector <- (df %>% select(contains("ask_size"))*index_ask) %>% rowSums()
      
    }
    return(sum_vector)
  }
  
  orderbook <- orderbook %>% mutate(midquote = ask_price_1/2 + bid_price_1/2, 
                                    spread = (ask_price_1 - bid_price_1)/midquote * 10000,
                                    volume = if_else(type ==4|type ==5, m_size, 0),
                                    hidden_volume = if_else(type ==5, m_size, 0),
                                    depth_bid = compute_depth(orderbook),
                                    depth_ask = compute_depth(orderbook, side="ask"),
                                    depth_bid_5 = compute_depth(orderbook, bp = 5),
                                    depth_ask_5 = compute_depth(orderbook, bp =5, side="ask"))
  
  orderbook_nested <- orderbook %>%
    mutate(ts_minute = floor_date(ts, "1 minute")) %>% 
    select(midquote:ts_minute) %>% 
    group_by(ts_minute) %>% 
    mutate(messages = n(),
           volume = sum(volume),
           hidden_volume = sum(hidden_volume)) %>% 
    summarise_all(median)
  
  # Store file for evaluation
    
    print(summary(orderbook_nested))
    write_csv(orderbook_nested, store_output)

}
