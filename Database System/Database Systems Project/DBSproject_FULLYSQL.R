# Library the package

# Database Connection
library('DBI')
library('RMySQL')

# General Visualisation
library('ggplot2') #1111
library('scales')
library('grid')
library('gridExtra')
library('RColorBrewer')
library('corrplot')

source("http://peterhaschke.com/Code/multiplot.R") # 111

# general data manipulation
library('dplyr') # data manipulation 111
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation111
library('forcats') # factor manipulation

# specific visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation  11
library('ggExtra') # visualisation
library('ggforce') # visualisation
library('viridis') # visualisation

# specific data manipulation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation

# Date plus forecast
library('lubridate') # date and time 111
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis
library('timetk') # time series analysis

# Maps / geospatial
library('geosphere') # geospatial locations
library('leaflet') # maps 1111
library('leaflet.extras') # maps
library('maps') # maps

####################################################################################################################################


# Connecting the MySQL Database Server
db = dbConnect(
  MySQL(),
  user = 'myuser',
  password = '123456',
  dbname = 'database_system_1',
  host = '119.91.113.204'
)


# View the Connection
summary(db)
dbGetInfo(db)
dbListTables(db)


# Get the Data
air_reserve = fetch(dbSendQuery(db, "SELECT * FROM air_reserve"), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

air_store = fetch(dbSendQuery(db, "SELECT * FROM air_store_info"), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

air_visits = fetch(dbSendQuery(db, "SELECT * FROM air_visit_data"), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

holidays = fetch(dbSendQuery(db, "SELECT * FROM date_info"), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.


# View the Data
# Using glimpse() in the 'dplyr' package instead of summary(), more elegant.
summary(air_reserve)
glimpse(air_reserve)

summary(air_store)
glimpse(air_store)

summary(air_visits)
glimpse(air_visits)

summary(holidays)
glimpse(holidays)


# Change the data type of date and time ( Changing from 'character' data type into 'date' data type )

air_reserve = air_reserve %>%
  mutate(
    visit_date = ymd(visit_date),
    visit_time = hms(visit_time),
    reserve_date = ymd(reserve_date),
    reserve_time = hms(reserve_time)
  )

air_store = air_store %>%
  mutate(
    air_genre_name = as.factor(air_genre_name),
    air_area_name = as.factor(air_area_name)
  )

air_visits = air_visits %>%
  mutate(visit_date = ymd(visit_date))

holidays = holidays %>%
  mutate(
    holiday_flg = as.logical(holiday_flg),
    date = ymd(calendar_date),
    calendar_date = as.character(calendar_date)
  )


# After Changing the data type, view the Data again
summary(air_reserve)
glimpse(air_reserve)

summary(air_store)
glimpse(air_store)

summary(air_visits)
glimpse(air_visits)

summary(holidays)
glimpse(holidays)


####################################################################################################################################


SQL_Statement = "SELECT visit_date, SUM(visitors) AS all_visitors
                 FROM air_visit_data GROUP BY visit_date"
sum_air_visits = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.


p1 = sum_air_visits %>%
  mutate(visit_date = ymd(visit_date)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(col = "blue") +
  labs(x = "Date" , y = "All Visitors")

SQL_Statement = "SELECT DAYNAME(visit_date) AS dayname, SUM(visitors) AS all_visitors
                 FROM air_visit_data GROUP BY dayname"
dayname_all_air_visits = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.


p2 = dayname_all_air_visits %>%
  ggplot(aes(reorder(dayname, all_visitors), all_visitors, fill = dayname)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 0.9)) +
  labs(x = "Day of the week", y = "All Visitors") +
  scale_fill_hue()

SQL_Statement = "SELECT DAYNAME(visit_date) AS dayname, AVG(visitors) AS median_visitors
                 FROM air_visit_data GROUP BY dayname"
dayname_avg_air_visits = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p3 = dayname_avg_air_visits %>%
  ggplot(aes(reorder(dayname, median_visitors), median_visitors, fill = dayname)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 0.9)) +
  labs(x = "Day of the week", y = "Median Visitors") +
  scale_fill_hue()

SQL_Statement = "SELECT DATE_FORMAT(visit_date,'%b') AS month, SUM(visitors) AS all_visitors
                 FROM air_visit_data GROUP BY month"
month_all_air_visits = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p4 <- month_all_air_visits %>%
  ggplot(aes(reorder(month, all_visitors), all_visitors, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "All Visitors")

SQL_Statement = "SELECT DATE_FORMAT(visit_date,'%b') AS month, AVG(visitors) AS median_visitors
                 FROM air_visit_data GROUP BY month"
month_avg_air_visits = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p5 <- month_avg_air_visits %>%
  ggplot(aes(reorder(month, median_visitors), median_visitors, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median Visitors")

layout <- matrix(c(1,1,1,1,2,3,4,5),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)
####################################################################################################################################

SQL_Statement = "SELECT visit_date, SUM(reserve_visitors) AS all_visitors
                 FROM air_reserve GROUP BY visit_date"
sumvisit_air_reserve = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

sumvisit_air_reserve = sumvisit_air_reserve %>%
  mutate(
    visit_date = ymd(visit_date)
  )

p1 = sumvisit_air_reserve %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(col = "blue") +
  labs(x = "Date" , y = "All Visitors")

SQL_Statement = "SELECT DATE_FORMAT(visit_time,'%H') AS visit_time, SUM(reserve_visitors) AS all_visitors
                 FROM air_reserve GROUP BY visit_time"
sumvisit_time_air_reserve = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p2 <- sumvisit_time_air_reserve %>%
  ggplot(aes(visit_time, all_visitors, fill = visit_time)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Visit Time", y = "All Visitors")

SQL_Statement = "SELECT (visit_date-reserve_date)*24+(visit_time-reserve_time)/10000 AS diff_hour, SUM(reserve_visitors) AS all_visitors
                 FROM air_reserve GROUP BY diff_hour HAVING diff_hour<=360"
delay_air_reserve1 = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p3 <- delay_air_reserve1 %>%
  ggplot(aes(diff_hour, all_visitors, fill = diff_hour)) +
  geom_col() +
  geom_vline(xintercept = 24, color = 'red') +
  geom_vline(xintercept = 48, color = 'orange') +
  geom_vline(xintercept = 72, color = 'green') +
  geom_vline(xintercept = 120, color = 'blue') +
  geom_vline(xintercept = 240, color = 'yellow') +
  theme(legend.position = "none") +
  labs(x = "Time from reservation to visit")

SQL_Statement = "SELECT (visit_date-reserve_date)*24+(visit_time-reserve_time)/10000 AS diff_hour, SUM(reserve_visitors) AS all_visitors
                 FROM air_reserve GROUP BY diff_hour HAVING diff_hour>360 AND diff_hour<=1440"
delay_air_reserve2 = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p4 <- delay_air_reserve2 %>%
  ggplot(aes(diff_hour, all_visitors, fill = diff_hour)) +
  geom_col() +
  geom_vline(xintercept = 480, color = 'red') +
  geom_vline(xintercept = 720, color = 'orange') +
  theme(legend.position = "none") +
  labs(x = "Time from reservation to visit")

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

####################################################################################################################################

leaflet(air_store) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(
    ~ longitude,
    ~ latitude,
    popup = ~ air_store_id,
    label = ~ air_genre_name,
    clusterOptions = markerClusterOptions()
  )

####################################################################################################################################

SQL_Statement = "SELECT air_genre_name, COUNT(*) AS num FROM air_store_info GROUP BY air_genre_name"
type_air_store=fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p1 = type_air_store %>%
  ggplot(aes(reorder(air_genre_name, num), num, fill = air_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (air_genre_name)", y = "Number of air restaurants")

SQL_Statement = "SELECT air_area_name, COUNT(*) AS num FROM air_store_info GROUP BY air_area_name ORDER BY num DESC LIMIT 0,15"
type_air_store=fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p2 = type_air_store %>%
  ggplot(aes(reorder(air_area_name, num) , num, fill = air_area_name)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (air_area_name)", y = "Number of air restaurants")

layout <- matrix(c(1,1,1,1,2,2,2,2),2,4,byrow=TRUE)
multiplot(p1, p2, layout=layout)

##########################################################

p1 <- foo %>%
  ggplot(aes(holiday_flg, fill = holiday_flg)) +
  geom_bar(color = 'blue') +
  theme(legend.position = "none")

SQL_Statement = "SELECT calendar_date, holiday_flg FROM date_info"
holiday1 = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p2 = holiday1 %>%
  mutate(calendar_date = ymd(calendar_date)) %>%
  mutate(holiday_flg = as.logical(holiday_flg)) %>%
  ggplot(aes(calendar_date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "date")

SQL_Statement = "SELECT calendar_date, holiday_flg FROM date_info WHERE calendar_date>'2016-1-1' AND calendar_date<='2016-5-31'"
holiday2 = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p3 = holiday2 %>%
  mutate(calendar_date = ymd(calendar_date)) %>%
  mutate(holiday_flg = as.logical(holiday_flg)) %>%
  ggplot(aes(calendar_date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "date")

SQL_Statement = "SELECT calendar_date, holiday_flg FROM date_info WHERE calendar_date>'2017-1-1' AND calendar_date<='2017-5-31'"
holiday3 = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.


p4 = holiday3 %>%
  mutate(calendar_date = ymd(calendar_date)) %>%
  mutate(holiday_flg = as.logical(holiday_flg)) %>%
  ggplot(aes(calendar_date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "date")

layout <- matrix(c(1,1,2,2,3,3,4,4),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
##############################
SQL_Statement = "SELECT visit_date, air_genre_name, AVG(visitors) AS mean_visitors
                 FROM air_visit_data AS ad
                 JOIN air_store_info AS ai ON ad.air_store_id = ai.air_store_id
                 GROUP BY visit_date, air_genre_name"

air_visit_store = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

air_visit_store  %>%
  mutate(air_genre_name = as.factor(air_genre_name))  %>%
  mutate(visit_date = ymd(visit_date))  %>%
  ungroup() %>%
  ggplot(aes(visit_date, mean_visitors, color = air_genre_name)) +
  geom_line() +
  labs(y = "Average number of visitors to 'air' restaurants", x = "Date") +
  theme(legend.position = "none") +
  scale_y_log10() +
  facet_wrap(~ air_genre_name)


##############################


SQL_Statement = "SELECT DAYNAME(visit_date) AS dayname, air_genre_name, AVG(visitors) AS mean_visitors
                 FROM air_visit_data AS ad
                 JOIN air_store_info AS ai ON ad.air_store_id = ai.air_store_id
                 GROUP BY dayname, air_genre_name"
daytime_air_visit_store = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.


p1 <- daytime_air_visit_store %>%
  ggplot(aes(air_genre_name, mean_visitors, color = dayname)) +
  geom_point(size = 4) +
  theme(legend.position = "left",
        plot.title = element_text(size = 14)) +
  coord_flip() +
  labs(x = "") +
  scale_x_discrete(position = "top") +
  ggtitle("air_genre_name") +
  scale_color_hue()

SQL_Statement = "SELECT air_genre_name, visitors
                 FROM air_visit_data AS ad
                 JOIN air_store_info AS ai ON ad.air_store_id = ai.air_store_id"
air_visit_store = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p2 <- air_visit_store %>%
  ggplot(aes(visitors, air_genre_name, fill = air_genre_name)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(y = "") +
  scale_fill_cyclical(values = c("blue", "red"))

layout <- matrix(c(1,1,1,1,2,2,2,2),2,4,byrow=TRUE)
multiplot(p1, p2, layout=layout)


##############################

SQL_Statement = "SELECT holiday_flg, visitors FROM air_visit_data AS ad JOIN date_info AS h ON ad.visit_date = h.calendar_date"
air_visit_holiday = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.


p1 = air_visit_holiday %>%
  ggplot(aes(holiday_flg, visitors, color = holiday_flg, group=holiday_flg)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none")

SQL_Statement = "SELECT holiday_flg, DAYNAME(visit_date) AS dayname, AVG(visitors) AS mean_visitors
                 FROM air_visit_data AS ad JOIN date_info AS h ON ad.visit_date = h.calendar_date
                 GROUP BY holiday_flg, dayname"
avg_air_visit_holiday = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

p2 = avg_air_visit_holiday %>%
  ggplot(aes(dayname, mean_visitors, color = holiday_flg)) +
  geom_point(size = 4) +
  theme(legend.position = "none") +
  labs(y = "Average number of visitors")

layout <- matrix(c(1,1,1,1,2,2,2,2),2,4,byrow=TRUE)
multiplot(p1, p2, layout=layout)

##############################

SQL_Statement = "SELECT air_genre_name, COUNT(*) AS num FROM air_store_info GROUP BY air_genre_name, air_area_name"
cnt_air_store = fetch(dbSendQuery(db, SQL_Statement), n = -1)
dbClearResult(dbListResults(db)[[1]]) # Connection with pending rows. Close the Resultset before continuing.

cnt_air_store %>%
  ggplot(aes(reorder(air_genre_name, num), num)) +
  geom_boxplot() +
  geom_jitter(color = "blue") +
  scale_y_log10() +
  coord_flip() +
  labs(x = "Air genre", y = "Occurences per air area")

##############################
## time-seriees parameter
foo <- air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(air_store_id, air_genre_name) %>%
  summarise(mean_log_visits = mean(log1p(visitors)),
            mean_log_visits = mean(log1p(visitors)),
            sd_log_visits = sd(log1p(visitors))) %>%
  ungroup()

params_ts1 <- function(rownr){
  bar <- air_visits %>%
    filter(air_store_id == foo$air_store_id[rownr])
  slope <- summary(lm(visitors ~ visit_date, data = bar))$coef[2]
  slope_err <- summary(lm(visitors ~ visit_date, data = bar))$coef[4]
  
  foobar <- tibble(
    air_store_id = foo$air_store_id[rownr],
    slope = slope,
    slope_err = slope_err
  )
  
  return(foobar)
}

params <- params_ts1(1)
for (i in seq(2,nrow(foo))){
  params <- bind_rows(params, params_ts1(i))
}

ts_params <- foo %>%
  left_join(params, by = "air_store_id")
# 1-d 
p1 <- ts_params %>%
  ggplot(aes(mean_log_visits)) +
  geom_histogram(bins = 50, fill = "blue")

p2 <- ts_params %>%
  ggplot(aes(sd_log_visits)) +
  geom_histogram(bins = 50, fill = "blue")

p3 <- ts_params %>%
  filter(slope < 0.5) %>%
  ggplot(aes(slope)) +
  geom_histogram(bins = 50, fill = "blue") +
  labs(x = "Slope < 0.5")

p4 <- ts_params %>%
  ggplot((aes(mean_log_visits, sd_log_visits))) +
  geom_point(size = 2, color = "blue")

p5 <- ts_params %>%
  ggplot((aes(slope, slope_err))) +
  geom_point(size = 2, color = "blue")

layout <- matrix(c(1,1,2,2,3,3,4,4,4,5,5,5),2,6,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)
ts_params %>%
  filter(abs(slope) > 0.25) %>%
  select(air_store_id, air_genre_name, slope, slope_err)
# 
ts_params %>%
  ggplot(aes(mean_log_visits, slope, color = air_genre_name)) +
  geom_errorbarh(aes(xmin = mean_log_visits - sd_log_visits,
                     xmax = mean_log_visits + sd_log_visits),
                 color = "grey70", size = 0.7) +
  geom_errorbar(aes(ymin = slope - slope_err,
                    ymax = slope + slope_err),
                color = "grey70", size = 0.7) +
  geom_point() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3, override.aes = list(size = 4))) +
  labs(color = "") +
  facet_zoom(y = (slope < 0.05 & slope > -0.1))
## arima 
air_id = "air_ba937bf13d40fb24"
pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()
max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
  rownames_to_column()
visits_train <- visits %>% filter(visit_date <= split_date)
visits_valid <- visits %>% filter(visit_date > split_date)
arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                        stepwise = FALSE, approximation = FALSE)
arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50,95))
## vis
arima_visits %>%
  autoplot +
  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
  labs(x = "Time [weeks]", y = "log1p visitors vs auto.arima predictions")
## ploting 
plot_auto_arima_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    rownames_to_column()
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                          stepwise = FALSE, approximation = FALSE)
  
  arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50,95))
  
  arima_visits %>%
    autoplot +
    geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
    labs(x = "Time [weeks]", y = "log1p visitors vs forecast")
}
#
test <-read_csv(str_c('sample_submission.csv'), col_types = cols())   
p1 <- plot_auto_arima_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_auto_arima_air_id("air_8e4360a64dbd4c50")
p3 <- plot_auto_arima_air_id("air_1c0b150f9e696a5f")
p4 <- plot_auto_arima_air_id("air_900d755ebd2f7bbd")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
#propeht
air_id = "air_ba937bf13d40fb24"

pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  rownames_to_column() %>%
  select(y = visitors,
         ds = visit_date)

visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)

proph <- prophet(visits_train, changepoint.prior.scale=0.5, yearly.seasonality=FALSE, daily.seasonality = FALSE)
future <- make_future_dataframe(proph, periods = pred_len)
fcast <- predict(proph, future)

plot(proph, fcast)

prophet_plot_components(proph, fcast)
##
fcast %>%
  as.tibble() %>%
  mutate(ds = date(ds)) %>%
  ggplot(aes(ds, yhat)) + 
  geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
  geom_line(colour = "blue") +
  geom_line(data = visits_train, aes(ds, y), colour = "black") +
  geom_line(data = visits_valid, aes(ds, y), colour = "grey50")
# another function 
plot_prophet_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors,
           ds = visit_date)
  
  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  
  proph <- prophet(visits_train, changepoint.prior.scale=0.5,
                   yearly.seasonality=FALSE, daily.seasonality = FALSE)
  future <- make_future_dataframe(proph, periods = pred_len)
  fcast <- predict(proph, future)
  
  p <- fcast %>%
    as.tibble() %>%
    mutate(ds = date(ds)) %>%
    ggplot(aes(ds, yhat)) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
    geom_line(colour = "blue") +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    labs(title = str_c("Prophet for ", air_id))
  
  return(p)
}  

p1 <- plot_prophet_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_prophet_air_id("air_8e4360a64dbd4c50")
p3 <- plot_prophet_air_id("air_1c0b150f9e696a5f")

p4 <- plot_prophet_air_id("air_820d1919cbecaa0a")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

plot_prophet_air_id_holiday <- function(air_id, use_hday){
  
  air_visits_cut <- air_visits %>%
    filter(visit_date <= ymd("20160531"))
  
  hday <- holidays %>%
    filter(holiday_flg == TRUE) %>%
    mutate(holiday = "holiday") %>%
    select(ds = date, holiday)
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits_cut$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits_cut$visit_date), max(air_visits_cut$visit_date), 1))
  
  foo <- air_visits_cut %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors,
           ds = visit_date)
  
  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  
  if (use_hday == TRUE){
    proph <- prophet(visits_train,
                     changepoint.prior.scale=0.5,
                     yearly.seasonality=FALSE,
                     daily.seasonality=FALSE,
                     holidays = hday)
    ptitle = "Prophet (w/ holidays) for "
  } else {
    proph <- prophet(visits_train,
                     changepoint.prior.scale=0.5,
                     yearly.seasonality=FALSE,
                     daily.seasonality = FALSE)
    ptitle = "Prophet for "
  }
  
  future <- make_future_dataframe(proph, periods = pred_len)
  fcast <- predict(proph, future)
  
  p <- fcast %>%
    as.tibble() %>%
    mutate(ds = date(ds)) %>%
    ggplot(aes(ds, yhat)) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
    geom_line(colour = "blue") +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    labs(title = str_c(ptitle, air_id))
  
  return(p)
}  

p1 <- plot_prophet_air_id_holiday("air_5c817ef28f236bdf", TRUE)
p2 <- plot_prophet_air_id_holiday("air_5c817ef28f236bdf", FALSE)

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)

## holt winters 
plot_hw_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    rownames_to_column()
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  hw.fit <- HoltWinters(tsclean(ts(visits_train$visitors, frequency = 7)))
  
  hw_visits <- predict(hw.fit, n.ahead = pred_len, prediction.interval = T, level = 0.95) %>%
    as.tibble() %>%
    bind_cols(visits_valid)
  
  visits_train %>%
    ggplot(aes(visit_date, visitors)) +
    geom_line() +
    geom_ribbon(data = hw_visits, aes(x = visit_date, ymin = lwr, ymax = upr), fill = "light blue") +
    geom_line(data = hw_visits, aes(visit_date, visitors), color = "grey60") +
    geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
    geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
    labs(x = "Time [weeks]", y = "log1p visitors vs predictions") +
    ggtitle("HoltWinters")
}
##
plot_hw_air_id("air_ba937bf13d40fb24")
p1 <- plot_hw_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_hw_air_id("air_8e4360a64dbd4c50")
p3 <- plot_hw_air_id("air_1c0b150f9e696a5f")
p4 <- plot_hw_air_id("air_820d1919cbecaa0a")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)