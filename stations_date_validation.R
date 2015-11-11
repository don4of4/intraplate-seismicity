if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr")

# Formatting
data.arrivals$datetime <- as.POSIXct(data.arrivals$datetime)
stations.iris$start <- as.POSIXct(stations.iris$start)
stations.iris$end <- as.POSIXct(stations.iris$end)


first_arrival <- data.arrivals %>% group_by(STA) %>% arrange(STA)  %>% 
  summarise(first_sta_arrival = min(datetime))
first_arrival$sta <- first_arrival$STA
first_arrival$STA <- NULL

last_arrival <- data.arrivals %>% group_by(STA) %>% arrange(STA)  %>% 
  summarise(last_sta_arrival = max(datetime))
last_arrival$sta <- last_arrival$STA
last_arrival$STA <- NULL

result <- stations.iris %>% left_join(first_arrival, by="sta") %>% 
  left_join(first_arrival, by="sta")

# Find all stations where the start date must be incorrect
badstarts <- subset(result, result$first_sta_arrival > result$start)

# Find all stations where the end date must be incorrect
badstarts <- subset(result, result$last_sta_arrival > result$end)


