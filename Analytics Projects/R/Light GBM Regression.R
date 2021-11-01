## wd etc ----
getwd()
setwd('/Users/clause/documents/Rprogram')
require(data.table)
require(stringr)
require(lubridate)
require(zoo)
require(lightgbm)

## data: train and test ----
xtrain <- fread('air_visit_data.csv')
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
reserve_air <- fread('air_reserve.csv')
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

# visits processed group by number of 39,39-46,46-60,60-74,
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

## lgbm - validation

x0 <- xtrain[visit_date <= '2017-03-09' & visit_date > '2016-04-01']
x1 <- xtrain[visit_date > '2017-03-09']
y0 <- log1p(x0$visitors)
y1 <- log1p(x1$visitors)

mx1 <- as.integer(max(x0$visit_date) -min(x0$visit_date) )
mx2 <- as.integer(x0$visit_date -min(x0$visit_date))


x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

cat_features <- c('air_genre_name', 'air_area_name')

# LightGBM can use categorical features directly (without one-hot encoding). 
# The experiment on Expo data shows about 8x speed-up compared with one-hot encoding.
d0 <- lgb.Dataset(as.matrix(x0), label = y0, 
                  categorical_feature = cat_features, 
                  free_raw_data = TRUE)
d1 <- lgb.Dataset(as.matrix(x1), label = y1, 
                  categorical_feature = cat_features, 
                  free_raw_data = TRUE)

params <- list(objective = 'regression', metric = 'mse', max_depth = 7,  
               feature_fraction = 0.7,  
               bagging_fraction = 0.8, 
               min_data_in_leaf = 30,
               learning_rate = 0.02, 
               num_threads = 4,
               weight = 'wgt')

ntrx <- 1000
valids <- list(valid = d1)
model <- lgb.train(params = params,  data = d0, valids = valids, nrounds = ntrx,
                   early_stopping_rounds = 10)

pred_val <- predict(model, as.matrix(x1))
print( paste('validation error:', round(sd(pred_val - y1),4), sep = ' ' ))

# 0.5863 0.5869

ntrx <- model$best_iter

## lgbm - full

x0 <- xtrain
x1 <- xtest
y0 <- log1p(x0$visitors)

x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

cat_features <- c('air_genre_name', 'air_area_name')
d0 <- lgb.Dataset(as.matrix(x0), label = y0, 
                  categorical_feature = cat_features, 
                  free_raw_data = FALSE)

params <- list(objective = 'regression', metric = 'mse', max_depth = 7,  
               feature_fraction = 0.7,  
               bagging_fraction = 0.8, 
               min_data_in_leaf = 30,
               learning_rate = 0.02, 
               num_threads = 4, 
               weight = 'wgt')

model <- lgb.train(params = params,  data = d0, nrounds = ntrx)

importance <- lgb.importance(model)
lgb.plot.importance(importance,measure = 'Gain')


# predict
pred_full <- predict(model, as.matrix(x1))

prx <- data.frame(id = paste(xtest$air_store_id, xtest$visit_date , sep = '_')  ,
                  visitors = expm1(pred_full))
prx
#write.csv(prx, 'xgb_3011.csv', row.names = F, quote = F)