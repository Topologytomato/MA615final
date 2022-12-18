# The script is written to clean and save the data for EDA and shiny app

library(lubridate)
library(tidyverse)
path_data <- "/Users/willowwu/Downloads/data"
# get dir for all data
list_folder <- list.dirs(path_data, recursive = FALSE)
data_rbind <- data.frame()

for (i in 1:29) {
  example_folder <- list_folder[i]
  
  # read the data
  list_file <- list.files(example_folder)
  stop_times <- read.csv(paste0(example_folder, "/stop_times.txt"))
  stops <- read.csv(paste0(example_folder, "/stops.txt"))
  trips <- read.csv(paste0(example_folder, "/trips.txt"))
  routes <- read.csv(paste0(example_folder, "/routes.txt"))
  check_points <- read.csv(paste0(example_folder, "/checkpoints.txt"))
  feed_info <- read.csv(paste0(example_folder, "/feed_info.txt"))

  # get start date and end date of the data
  start_date <- as.Date(as.character(feed_info$feed_start_date[1]), format = "%Y%m%d")
  end_date <- as.Date(as.character(feed_info$feed_end_date[1]), format = "%Y%m%d")

  # join all the data in 1 dataframe
  col_trips <- c("route_id", "trip_id", "service_id")
  col_routes <- c("route_id", "route_desc")
  
  # combine the data sets
  stop_times <- stop_times %>% filter(checkpoint_id != "")
  data_full <- stop_times %>% left_join(trips[, col_trips], by = "trip_id")
  data_full <- data_full %>% left_join(routes[, col_routes], by = "route_id")
  data_full <- data_full %>% left_join(stops, by = "stop_id")

  # organize the data
  data_full$departure_time <- as.POSIXct(as.character(data_full$departure_time), format = "%H:%M:%S")
  data_full$start_point <- data_full$stop_name
  data_full$stop_point <- c(data_full$start_point[2:dim(data_full)[1]], NA)
  data_full$arrival_time <- c(data_full$departure_time[2:dim(data_full)[1]], NA)

  # delete the last rows for every trip_id
  data_full <- data_full %>%
    group_by(trip_id) %>%
    arrange(desc(departure_time)) %>%
    slice(-1) %>%
    ungroup()

  # convert the time into datetime type and calculate travel time
  data_full$travel_time <- mapply(difftime, data_full$arrival_time, data_full$departure_time, units = "sec")
  data_full <- data_full %>% filter(travel_time > 0)

  # get columns need
  colname_all <- c("route_id", "route_desc","start_point", "stop_point", "travel_time")
  data_need <- data_full[, colname_all]
  data_need <- unique(data_need)
  
  # attach the month and date range for the data
  data_need$month <- month(start_date)
  
  # combine the data
  data_rbind <- rbind(data_rbind, data_need)
  print(i)
}

# save the data
write.csv(data_rbind, "data_combined_traveltime.txt",row.names = FALSE)

# get ferry data
data_rbind <- data.frame()
for (i in 1:29) {
  example_folder <- list_folder[i]
  
  # read the data
  list_file <- list.files(example_folder)
  stop_times <- read.csv(paste0(example_folder, "/stop_times.txt"))
  trips <- read.csv(paste0(example_folder, "/trips.txt"))
  routes <- read.csv(paste0(example_folder, "/routes.txt"))
  check_points <- read.csv(paste0(example_folder, "/checkpoints.txt"))
  feed_info <- read.csv(paste0(example_folder, "/feed_info.txt"))
  
  # get start date and end date of the data
  start_date <- as.Date(as.character(feed_info$feed_start_date[1]), format = "%Y%m%d")
  end_date <- as.Date(as.character(feed_info$feed_end_date[1]), format = "%Y%m%d")
  
  # filter the ferry data
  data_full <- stop_times[grep("boat", stop_times$trip_id, ignore.case = TRUE),]
  
  # organize the data
  data_full$departure_time <- as.POSIXct(as.character(data_full$departure_time), format = "%H:%M:%S")
  data_full$start_point <- data_full$stop_id
  data_full$stop_point <- c(data_full$start_point[2:dim(data_full)[1]], NA)
  data_full$arrival_time <- c(data_full$departure_time[2:dim(data_full)[1]], NA)
  
  # convert the time into datetime type and calculate travel time
  data_full$travel_time <- mapply(difftime, data_full$arrival_time, data_full$departure_time, units = "sec")
  data_full <- data_full %>% filter(travel_time > 0)
  data_full$route_desc <- "Ferry"
  data_need <- data_full
  
  # attach the month and date range for the data
  data_need$month <- month(start_date)
  
  # combine the data
  data_rbind <- rbind(data_rbind, data_need)
  print(i)
  print(dim(data_rbind))
}

data_rbind$route_id <- 0
data_rbind[grep("Boat-F1", data_rbind$trip_id), "route_id"] <- "Boat-F1"
data_rbind[grep("Boat-F4", data_rbind$trip_id), "route_id"] <- "Boat-F4"

# columns need
col_all <- c("route_id", "route_desc", "start_point",
             "stop_point","travel_time", "month")

data_rbind <- unique(data_rbind)

# save the data
write.csv(data_rbind[,col_all], "data_ferry.txt",row.names = FALSE)

# We can also combine the data of calendar, calendar_date and routes,
# to figure out other information about reliability of the mbta service
calendar_full <- data.frame()
for (i in 1:29){
  example_folder <- list_folder[i]
  calendar <- read.csv(paste0(example_folder, "/calendar.txt"))
  calendar_full <- rbind(calendar_full, calendar)
}
calendar_full <- unique(calendar_full)
write.csv(calendar_full, "calendar_full.txt",row.names = FALSE)

calendar_dates_full <- data.frame()
for (i in 1:29){
  example_folder <- list_folder[i]
  calendar_dates <- read.csv(paste0(example_folder, "/calendar_dates.txt"))
  calendar_dates_full <- rbind(calendar_dates_full, calendar_dates)
}
calendar_dates_full <- unique(calendar_dates_full)
write.csv(calendar_dates_full, "calendar_dates_full.txt",row.names = FALSE)

routes_full <- data.frame()
for (i in 1:29){
  example_folder <- list_folder[i]
  routes <- read.csv(paste0(example_folder, "/routes.txt"))
  routes_full <- rbind(routes_full, routes)
}
routes_full <- unique(routes_full)
write.csv(routes_full, "routes_full.txt",row.names = FALSE)

checkpoints_full <- data.frame()
for (i in 1:29){
  example_folder <- list_folder[i]
  checkpoints <- read.csv(paste0(example_folder, "/checkpoints.txt"))
  checkpoints_full <- rbind(checkpoints_full, checkpoints)
}
checkpoints_full <- unique(checkpoints_full)
write.csv(checkpoints_full, "checkpoints_full.txt",row.names = FALSE)

stops_full <- data.frame()
for (i in 1:29){
  example_folder <- list_folder[i]
  stops <- read.csv(paste0(example_folder, "/stops.txt"))
  stops_full <- rbind(stops_full, stops)
}
stops_full <- unique(stops_full)
write.csv(stops_full, "stops_full.txt",row.names = FALSE)

trips_full <- data.frame()
for (i in 1:29){
  example_folder <- list_folder[i]
  trips <- read.csv(paste0(example_folder, "/trips.txt"))
  trips <- unique(trips[,c("route_id","service_id")])
  trips_full <- rbind(trips_full, trips)
}
trips_full <- unique(trips_full)
write.csv(trips_full, "trips_full.txt",row.names = FALSE)


# Then we deal with real-time data
# prepare for the real travel time for LR and HR
path_data <- "/Users/willowwu/Downloads/TravelTimes_2021～2022"
# get dir for all data
list_file <- list.files(path_data, recursive = FALSE)

# define the first date of selected week
d1 <- ymd("2022-01-06")
d2 <- ymd("2022-02-17")
d3 <- ymd("2022-03-02")
d4 <- ymd("2022-04-19")
d5 <- ymd("2022-05-24")
d6 <- ymd("2022-06-09")
d7 <- ymd("2022-07-20")
d8 <- ymd("2022-08-16")
d9 <- ymd("2022-09-01")
d10 <- ymd("2022-10-13")
d11 <- ymd("2021-11-24")
d12 <- ymd("2021-12-22")

data_rbind <- data.frame()
for (i in 1:length(list_file)) {
  example_file <- list_file[i]
  
  # read the data
  travel_times <- read.csv(paste0(path_data,"/", example_file))
  
  # get start date and end date of the data
  travel_times$service_date <- as.Date(travel_times$service_date, format = "%Y-%m-%d")
  travel_times$month <- month(travel_times$service_date)
  
  m <- min(month(travel_times$service_date))
  d <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
  
  # filter the time we want
  travel_times_1 <- travel_times %>% filter(service_date >= d[m] & service_date <= (d[m] + days(6)))
  travel_times_2 <- travel_times %>% filter(service_date >= d[m+1] & service_date <= (d[m+1] + days(6)))
  travel_times_3 <- travel_times %>% filter(service_date >= d[m+2] & service_date <= (d[m+2] + days(6)))
  travel_times <- bind_rows(travel_times_1, travel_times_2, travel_times_3)

  col_travel <- c("month", "service_date", "from_stop_id", "to_stop_id",
                  "route_id", "direction_id", "travel_time_sec")
  
  # combine the data
  data_rbind <- bind_rows(data_rbind, travel_times[,col_travel])
  print(i)
}
write.csv(data_rbind, "travel_times_full.txt",row.names = FALSE)

# prepare for the real travel time for Bus
path_data <- "/Users/willowwu/Downloads/Bus_time_2021~2022"
# get dir for all data
list_file <- list.files(path_data, recursive = FALSE)
data_rbind <- data.frame()

for (i in 1:length(list_file)) {
  example_file <- list_file[i]
  
  # read the data
  bus_times <- read.csv(paste0(path_data,"/", example_file))
  
  # filter the data
  # get start date and end date of the data
  bus_times$service_date <- as.Date(bus_times$service_date, format = "%Y-%m-%d")
  bus_times$month <- month(bus_times$service_date)
  
  m <- min(month(bus_times$service_date), na.rm = TRUE)
  d <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
  
  # filter the time we want
  bus_times <- bus_times %>% filter(service_date >= d[m] & service_date <= (d[m] + days(6)))
  
  bus_times$scheduled <- as.POSIXct(bus_times$scheduled, format = "%Y-%M-%d %H:%M:%S")
  bus_times$actual <- as.POSIXct(bus_times$actual, format = "%Y-%M-%d %H:%M:%S")
  bus_times$time_difference <- mapply(difftime, bus_times$scheduled, bus_times$actual, units = "sec")
  col_bus <- c("month", "service_date", "time_point_id", "time_difference")
  
  # combine the data
  data_rbind <- bind_rows(data_rbind, bus_times[,col_bus])
  print(i)
}

data_rbind <- data_rbind %>% left_join(checkpoints_full, by = c("time_point_id" = "checkpoint_id"))
write.csv(data_rbind, "bus_times_full.txt",row.names = FALSE)

