install.packages("devtools")
devtools::install_github("rstudio/keras")
library(keras)
library(tensorflow)
install_keras()
install_tensorflow()

## wd etc ----
library(data.table)
library(stringr)
library(lubridate)
library(zoo)
library(dplyr)
library(RMySQL)
library(DBI)
library(keras)
library(tensorflow)
library(ggplot2)
library(tibble)

setwd('C:/Users/Clause/iCloudDrive/Documents/Rprogram')
# DB connection
db <- dbConnect(MySQL(), user = 'myuser', password = '123456', dbname = 'database_system_1',host = '119.91.113.204')
# View Data
summary(db)  
dbGetInfo(db)  
dbListTables(db)
# Get Data
reserve_air <- fetch(dbSendQuery(db, "select * from air_reserve"))
dbClearResult(dbListResults(db)[[1]])
air_store <- fetch(dbSendQuery(db, "select * from air_store_info"))
dbClearResult(dbListResults(db)[[1]])
air_visits <- fetch(result <- dbSendQuery(db, "select * from air_visit_data"))
dbClearResult(dbListResults(db)[[1]])
holidays <- fetch(result <- dbSendQuery(db, "select * from date_info"))
dbClearResult(dbListResults(db)[[1]])

## data: train and test ----
xtrain <- air_visits
xtest <- fread('sample_submission.csv')

# align the columns (test has the store id and date concatenated)
xtest$air_store_id <- str_sub(xtest$id, 1,-12)
xtest$visit_date <- str_sub(xtest$id, -10)
xtest$id <- NULL

# format
xtrain$visit_date <- as.Date(xtrain$visit_date)
xtest$visit_date <- as.Date(xtest$visit_date)

# combine 
xtrain <- rbind(xtrain, xtest)

## reservations: air
# convert to datetime (from lubridate)
reserve_air$visit_datetime <- parse_date_time(reserve_air$visit_datetime, orders = '%Y-%m-%d H:M:S' )
reserve_air$reserve_datetime <- parse_date_time(reserve_air$reserve_datetime, orders = '%Y-%m-%d H:M:S' )
# waiting time
reserve_air$time_ahead <- as.double(reserve_air$visit_datetime - reserve_air$reserve_datetime)/3600
# round to day
reserve_air$visit_date <- as.Date(reserve_air$visit_datetime)
reserve_air$reserve_datetime <- as.Date(reserve_air$visit_datetime)
# aggregate to id x date combo
res_air_agg <- reserve_air[ j = list(air_res_visitors = sum(reserve_visitors),
                                     air_mean_time_ahead = round(mean(time_ahead),2)) ,
                            by = list(air_store_id, visit_date)]
rm(reserve_air)

## store info: air
xstore <- fread('air_store_info.csv')
xstore$air_genre_name <- factor(xstore$air_genre_name)
levels(xstore$air_genre_name) <- 1:nlevels(xstore$air_genre_name)
xstore$air_genre_name <- as.integer(xstore$air_genre_name)

xstore$air_area_name <- factor(xstore$air_area_name)
levels(xstore$air_area_name) <- 1:nlevels(xstore$air_area_name)
xstore$air_area_name <- as.integer(xstore$air_area_name)

# HHI
HHI <- function(x){
  x <- as.numeric(x)
  c<- 0
  for (i in 1:30) {
    a <- sum(x==i)/length(x)
    b <- a**2
    c <- c + b
  }
  c<-1/c
  return(c)
}
HHI_air <- tapply(xstore$air_genre_name, xstore$air_area_name, HHI)
HHI_air <- as.data.frame(HHI_air)
HHI_air <- cbind(HHI_air,1:103)
names(HHI_air) <- c('HHI','air_area_name')

xstore <- merge(xstore,HHI_air, by='air_area_name')

## date info
xdate <- fread('date_info.csv')
xdate$day_of_week <- NULL
xdate$calendar_date <- as.Date(xdate$calendar_date)

## data aggregation
xtrain <- merge(xtrain, res_air_agg, all.x = T)
xtrain <- merge(xtrain, xstore, all.x = T, by = 'air_store_id' )
xtrain <- merge(xtrain, xdate, by.x = 'visit_date', by.y = 'calendar_date')
rm(res_air_agg, xstore, xdate, HHI_air)

xtrain[is.na(xtrain)] <- 0

# Feature Engineering
# holiday in the last 3 days
xtrain[ , `:=`(h3a = rollapply(holiday_flg, width = 3, FUN = function(s) sign(sum(s, na.rm = T)),
                               partial = TRUE, fill = 0, align = 'right') ),
        by = c('air_store_id')]

# visits
xtrain[ , `:=`(vis14 = rollapply(log1p(visitors), width = 39, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(vis21 = rollapply(log1p(visitors), width = 46, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(vis28 = rollapply(log1p(visitors), width = 60, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(vis35 = rollapply(log1p(visitors), width = 74, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]

xtrain[ , `:=`(vLag1 = round((vis21 - vis14)/7,2))]
xtrain[ , `:=`(vLag2 = round((vis28 - vis14)/21,2))]
xtrain[ , `:=`(vLag3 = round((vis35 - vis14)/35,2))]
xtrain[ , vis14 := NULL, with = TRUE]
xtrain[ , vis21 := NULL, with = TRUE]
xtrain[ , vis28 := NULL, with = TRUE]
xtrain[ , vis35 := NULL, with = TRUE]

# reservations 
xtrain[ , `:=`(res7 = rollapply(log1p(air_res_visitors), width = 7, FUN = function(s) sum(s, na.rm = T),
                                partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(res14 = rollapply(log1p(air_res_visitors), width = 14, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(res21 = rollapply(log1p(air_res_visitors), width = 21, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]
xtrain[ , `:=`(res28 = rollapply(log1p(air_res_visitors), width = 28, FUN = function(s) sum(s, na.rm = T),
                                 partial = TRUE, fill = 0, align = 'right') ) ,
        by = c('air_store_id')]

# separate 
xtest <- xtrain[visitors == 0]
xtrain <- xtrain[visitors > 0]

## set label

x0 <- xtrain[visit_date <= '2017-01-09' & visit_date > '2016-04-01']
x1 <- xtrain[visit_date <= '2017-03-09' & visit_date > '2017-01-09']
x2 <- xtrain[visit_date > '2017-03-09']

y0 <- log1p(x0$visitors)
y1 <- log1p(x1$visitors)
y2 <- log1p(x2$visitors)

mx1 <- as.integer(max(x0$visit_date) -min(x0$visit_date) )
mx2 <- as.integer(x0$visit_date -min(x0$visit_date))


x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

train_data <- x0
train_labels <- y0
test_data <- x1
test_labels <- y1

paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))
train_data[1, ]

# normalization
train_df <- train_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  mutate(label = train_labels)

test_df <- test_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  mutate(label = test_labels)

test_df <- x2 %>% 
  as_tibble(.name_repair = "minimal") %>% 
  mutate(label = y2)
library(tfdatasets)
spec <- tfdatasets::feature_spec(train_df, label ~ . ) %>% 
  tfdatasets::step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

# model
layer <- layer_dense_features(
  feature_columns = dense_features(spec), 
  dtype = tf$float32
)
layer(train_df)

input <- layer_input_from_dataset(train_df %>% select(-label))

output <- input %>% 
  layer_dense_features(dense_features(spec)) %>% 
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1) 

model <- keras_model(input, output)

summary(model)

model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list('mean_squared_error')
  )

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

history <- model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 100,
  validation_split = 0.2,
  verbose = 2,
  callbacks = list(print_dot_callback)
)


plot(history)

# l2 regularization
output <- input %>% 
  layer_dense_features(dense_features(spec)) %>% 
  layer_dense(units = 512, activation = "relu",kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dense(units = 512, activation = "relu",kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dense(units = 1) 

model <- keras_model(input, output)
model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list('mean_squared_error')
  )
# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

history <- model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 200,
  validation_split = 0.2,
  verbose = 2,
  callbacks = list(early_stop))

plot(history)
c(loss, mae) %<-% (model %>% evaluate(test_df %>% select(-label), test_df$label, verbose = 0))

scores <- model %>% 
  evaluate(test_df, test_df$label, verbose = 0)
plot(scores)
scores

test_predictions <- model %>% predict(x2 %>% select(-label))
test_predictions[ , 1]
print( paste('validation error:', round(sd(test_predictions[ , 1] - y1),4), sep = ' ' ))
