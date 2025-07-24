#### Environmental data conversion
env <- read.csv("data_lamberts.csv")

n_months <- nrow(env)
start_date <- as.Date("1958-01-01")
dates <- seq.Date(start_date, by = "month", length.out = n_months)

# Add month column as the first column
env <- data.frame(Month = format(dates, "%Y-%m"), env)
