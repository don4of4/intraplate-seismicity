plotstations <- stations.iris
#-> creates a dataframe with formatted station, start and end, without duplicates


# (1) Place the data into a dataframe and convert the date
plotstations <- stations.iris
plotstations$start <- as.Date(plotstations$start)
plotstations$end <- as.Date(plotstations$end)

# (2) Add additional col's simplifying the start and end year to only the year
# (3) Group the stations by station name and start/end date (year only)
# (4) Remove duplicates. Now we are left with duplicate rows for each station due to different channels OR gaps
# (5) Select only the variables we want.

station_data <- plotstations %>% mutate(start_year=year(start), end_year=ifelse(year(end) > year(Sys.Date()), year(Sys.Date()), year(end)) ) %>% 
                group_by(sta, start_year, end_year) %>% distinct() %>% select(sta, start_year, end_year)
                
# (6) Iterate the station_data, expanding the date sequence, merging it row-wise on station name (to handle gaps and network changes), exploding it, 
#     then summarizing it to a freq. table.

station_summary <- station_data %>% mutate(years = list(seq(start_year, end_year, by = 1))) %>% group_by(sta) %>% select(years)  %>% 
  do(year = unique(unlist(.$years))) %>% ungroup() %>% unnest(year) %>% group_by(year) %>% summarise(freq=n())
  
            
# OLD METHOD  (No merging for gaps)
#station_summary <- sapply(1:nrow(station_data), function(x){year=seq(station_data$start_year[x], station_data$end_year[x], by = 1)}) %>%
#                   unlist %>% data.frame(year=.) %>% group_by(year) %>% summarise(freq=n())


#cleanup
plotstations <- NULL