setwd("E:\\4thyear\\Bigdata")
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('alluvial') # visualisation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
library('xgboost') # modelling
library('caret') # modelling
train <- as.tibble(fread('train.csv'))
test <- as.tibble(fread('test.csv'))
sample_submit <- as.tibble(fread('sample_submission.csv'))
sum(is.na(train))

sum(is.na(test))
#combine data
combine <- bind_rows(train %>% mutate(dset = "train"), 
                     test %>% mutate(dset = "test",
                                     dropoff_datetime = NA,
                                     trip_duration = NA))
#reformat data
combine <- combine %>% mutate(dset = factor(dset))
train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))
#check consistent
train %>%
  mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime)) + trip_duration) > 0) %>%
  select(check, pickup_datetime, dropoff_datetime, trip_duration) %>%
  group_by(check) %>%
  count()

jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)
train$distance <- distCosine(pick_coord, drop_coord)
train$bearing = bearing(pick_coord, drop_coord)


train$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
train$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
train$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
train$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

train <- train %>%
  mutate(speed = distance/trip_duration,
         date = date(pickup_datetime),
         month = lubridate::month(pickup_datetime,label = TRUE),
         wday = lubridate::wday(pickup_datetime,label = TRUE),
         wday = forcats::fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         hour = hour(pickup_datetime),
         work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
         jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3)
        )
train <- train %>%
  filter(trip_duration < 22*3600,
         distance > 0 | (near(distance, 0) & trip_duration < 60),
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5,
         trip_duration > 10,
         speed < 100)
pick_coord <- test %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- test %>%
  select(dropoff_longitude, dropoff_latitude)
test$distance <- distCosine(pick_coord, drop_coord)
test$bearing = bearing(pick_coord, drop_coord)


test$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
test$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
test$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
test$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)
test <- test %>%
  mutate(date = date(pickup_datetime),
         hour = hour(pickup_datetime),
         month = lubridate::month(pickup_datetime,label = TRUE),
         wday = lubridate::wday(pickup_datetime,label = TRUE),
         wday = forcats::fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
         jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3)
  )
foo <- combine %>%
  mutate(date = date(ymd_hms(pickup_datetime))) %>%
  group_by(date, dset) %>%
  count()

# airport coordinates again, just to be sure
jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

# derive distances
pick_coord <- combine %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- combine %>%
  select(dropoff_longitude, dropoff_latitude)

combine$distance <- distCosine(pick_coord, drop_coord)
combine$bearing = bearing(pick_coord, drop_coord)

combine$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
combine$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
combine$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
combine$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

# add dates
combine <- combine %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         date = date(pickup_datetime)
  )

combine <- combine %>%
  mutate(store_and_fwd_flag = as.integer(factor(store_and_fwd_flag)),
         speed = distance/trip_duration,
         vendor_id = as.integer(vendor_id),
         month = as.integer(month(pickup_datetime)),
         wday = wday(pickup_datetime, label = TRUE),
         wday = as.integer(fct_relevel(wday, c("Sun", "Sat", "Mon", "Tue", "Wed", "Thu", "Fri"))),
         hour = hour(pickup_datetime),
         work = as.integer( (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Fri","Wed","Thurs")) ),
         jfk_trip = as.integer( (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3) ),
         lg_trip = as.integer( (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3) )
  )
glimpse(combine)

# Specific definitions:
#---------------------------------
# predictor features
train_cols <- c("hour", "distance", "wday","month", "vendor_id", "bearing","jfk_trip",'lg_trip','passenger_count',
                "pickup_longitude", "pickup_latitude", "dropoff_latitude", "dropoff_longitude")
# target feature
y_col <- c("trip_duration")
# identification feature
id_col <- c("id") 
# auxilliary features
aux_cols <- c("dset")
# cleaning features
clean_cols <- c("jfk_dist_drop", "jfk_dist_pick")
#---------------------------------

# General extraction
#---------------------------------
# extract test id column
test_id <- combine %>%
  filter(dset == "test") %>%
  select_(.dots = id_col)

# all relevant columns for train/test
cols <- c(train_cols, y_col, aux_cols, clean_cols)
combine <- combine %>%
  select_(.dots = cols)

# split train/test
train <- combine %>%
  filter(dset == "train") %>%
  select_(.dots = str_c("-",c(aux_cols)))
test <- combine %>%
  filter(dset == "test") %>%
  select_(.dots = str_c("-",c(aux_cols, clean_cols, y_col)))
#---------------------------------
train <- train %>%
  mutate(trip_duration = log(trip_duration + 1))
train <- train %>%
  filter(trip_duration < 24*3600,
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5
  ) %>%
  select_(.dots = str_c("-",c(clean_cols)))
set.seed(4321)
trainIndex <- createDataPartition(train$trip_duration, p = 0.8, list = FALSE, times = 1)

train <- train[trainIndex,]
valid <- train[-trainIndex,]
valid <- valid %>%
  select_(.dots = str_c("-",c(clean_cols)))

train <- train %>%
  filter(trip_duration < 24*3600,
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5
  ) %>%
  select_(.dots = str_c("-",c(clean_cols)))
#convert to XGB matrix
foo <- train %>% select(-trip_duration)
bar <- valid %>% select(-trip_duration)

dtrain <- xgb.DMatrix(as.matrix(foo),label = train$trip_duration)
dvalid <- xgb.DMatrix(as.matrix(bar),label = valid$trip_duration)
dtest <- xgb.DMatrix(as.matrix(test))

xgb_params <- list(colsample_bytree = 0.7, #variables per tree 
                   subsample = 0.7, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 5, #tree levels
                   eta = 0.3, #shrinkage
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   seed = 4321
)

watchlist <- list(train=dtrain, valid=dvalid)
set.seed(4321)
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   print_every_n = 5,
                   watchlist = watchlist,
                   nrounds = 300)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 5, nrounds=200)

imp_matrix <- as.tibble(xgb.importance(feature_names = colnames(train %>% select(-trip_duration)), model = gb_dt))

imp_matrix %>%
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Features", y = "Importance")
test_preds <- predict(gb_dt,dtest)
pred <- test_id %>%
  mutate(trip_duration = exp(test_preds) - 1)

pred %>% write_csv('submit2.csv')
identical(dim(sample_submit),dim(pred))
glimpse(sample_submit)
glimpse(pred)
bind_cols(sample_submit, pred) %>% head(5)
dt <- read.csv('submission-dt.csv')
rf <- read.csv('submission-rf.csv')
foo <- train %>%
  select(trip_duration) %>%
  mutate(dset = "train",
         trip_duration = exp(trip_duration) - 1)
bar <-pred%>%
  mutate(dset = "predict")

bind_rows(foo, bar) %>%
  ggplot(aes(trip_duration, fill = dset)) +
  geom_density(alpha = 0.5) +
  scale_x_log10()

