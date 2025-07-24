#### Environmental data conversion
env <- read.csv("./project_data/data_lamberts.csv")

n_months <- nrow(env)
start_date <- as.Date("1958-01-01")
dates <- seq.Date(start_date, by = "month", length.out = n_months)

# Add month column as the first column
env <- data.frame(Month = format(dates, "%Y-%m"), env)
env$Month <- as.POSIXct(env$Month, format = "%Y-%m", tz="Africa/Johannesburg")
env$Date <- env$Month

gannet$StartDate <- as.POSIXct(gannet$StartDate, format = "%Y-%m-%d")

gannet$Date <- format(gannet$StartDate, "%Y-%m")

dates_counts <- unique(gannet$Date) %>% as.character()

new_env <- gannet %>% 
  select(Date) %>% 
  left_join(env, by = "Date")
  

write.csv(new_env, "./project_data/new_env.csv")
