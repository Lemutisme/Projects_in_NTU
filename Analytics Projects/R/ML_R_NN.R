## chapter7. neural network
## refer to p231 in ML with R

#########################
####DNN in concrete######
#########################
# data URL:  https://archive.ics.uci.edu/ml/datasets/concrete+compressive+strength
concrete <- read.csv("Concrete.csv")
str(concrete)

# normalizing
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm[[9]])
summary(concrete[[9]])

# dividing into train & test
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# neuralnet modeling, hidden layers = 5
library('neuralnet')
concrete_model <- neuralnet(strength ~ .,
                            data = concrete_train, hidden = 5)
# visualization
plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

##################################
####DNN in qsar_fish_toxicity#####
##################################
# data URL: https://archive.ics.uci.edu/ml/datasets/QSAR+fish+toxicity
QSAR <- read.csv("qsar_fish_toxicity.csv", header = F, sep = ';')
str(QSAR)

# normalizing
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

QSAR_norm <- as.data.frame(lapply(QSAR, normalize))
summary(QSAR_norm[[7]])
summary(QSAR[[7]])

# dividing into train & test
QSAR_train <- QSAR_norm[1:606, ]
QSAR_test <- QSAR_norm[607:908, ]

# neuralnet modeling, try hidden layers = 1-5
library('neuralnet')
for (i in c(1,2,3,4,5)) {
  hidden <- i
  QSAR_model <- neuralnet(V7 ~ V1 + V2 + V3 + V4 +V5 + V6,
                          data = QSAR_train, hidden = hidden)
  # visualization
  plot(QSAR_model)
  model_results <- neuralnet::compute(QSAR_model, QSAR_test[1:6])
  predicted_strength <- model_results$net.result
  output <- cor(predicted_strength, QSAR_test$V7)
  print(output)
}



#########################
####keras in concrete####
#########################
library(keras)

concrete <- read.csv("Concrete.csv")
concrete_train <- concrete[1:773, ]
concrete_test <- concrete[774:1030, ]

train_data <- concrete_train[,-9]
train_labels <- concrete_train[,9]
test_data <- concrete_test[,-9]
test_labels <- concrete_test[,9]

paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))
train_data[1, ]

library(dplyr)

train_df <- train_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  mutate(label = train_labels)

test_df <- test_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  mutate(label = test_labels)

spec <- feature_spec(train_df, label ~ . ) %>% 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

spec

layer <- layer_dense_features(
  feature_columns = dense_features(spec), 
  dtype = tf$float32
)
layer(train_df)

input <- layer_input_from_dataset(train_df %>% select(-label))

output <- input %>% 
  layer_dense_features(dense_features(spec)) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) 

model <- keras_model(input, output)

summary(model)

model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
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
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

library(ggplot2)
plot(history)

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

history <- model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop)
)

plot(history)

scores <- model %>% 
  evaluate(test_df, test_df$label, verbose = 0)
scores







