data=read.csv(data)
# LOAD PACKAGES 
library(reshape);library(dplyr);library(tidyverse);library(ggplot2);library(dlnm) ;library(sf); library(mgcv) ; library(splines) ;library(spdep)
# Missing Imputation
library(caret)
dataPM=dataPM[,-1]
train_size <- 0.75
imp_int_full <- na.interpolation(dataPM)
# Create an empty dataframe to store the results
results_df <- data.frame(
  Column_Name = character(),
  Model = character(),
  MAE = numeric(),
  RMSE = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each column
for (col_name in names(dataPM)) {
  
  # Set the current column as the target variable Y
  NA_indices <- is.na(dataPM[[col_name]]) 
  Stat_full <- imp_int_full[!NA_indices,]
  # Train-test split
  train_index <- sample(1:nrow(Stat_full), train_size * nrow(Stat_full))
  train_data <- Stat_full[train_index, ]
  test_data <- Stat_full[-train_index, ]
  
  # Construct formula
  formula_str <- paste(col_name, "~ .")
  
  # Train KNN regression model
  knn_model <- train(
    as.formula(formula_str),
    data = train_data,
    method = "knn",
    preProcess = c("center", "scale"),  # Center and scale the data
    tuneGrid = expand.grid(k = 3:10),   # Grid search for optimal K
    trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
  )
  
  # Train Random Forest regression model
  ctrl <- trainControl(method="repeatedcv",repeats = 3)
  # Random forrest

  rf_model <- train(
    as.formula(formula_str),
    data = train_data,
    method = "rf",
    trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)  # 5-fold cross-validation
  
  
  
  # Make predictions with both models on the test dataset
  knn_predictions <- predict(knn_model, newdata = test_data)
  rf_predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate MAE, RMSE, and R-squared for KNN
  knn_mae <- mean(abs(test_data[[col_name]] - knn_predictions))
  knn_rmse <- sqrt(mean((test_data[[col_name]] - knn_predictions)^2))
  knn_r2 <- cor(test_data[[col_name]], knn_predictions)^2
  
  # Calculate MAE, RMSE, and R-squared for Random Forest
  rf_mae <- mean(abs(test_data[[col_name]] - rf_predictions))
  rf_rmse <- sqrt(mean((test_data[[col_name]] - rf_predictions)^2))
  rf_r2 <- cor(test_data[[col_name]], rf_predictions)^2
  
  # Add results to the dataframe
  results_df <- rbind(results_df, data.frame(
    Column_Name = rep(col_name, 2),
    Model = c("KNN", "Random Forest"),
    MAE = c(knn_mae, rf_mae),
    RMSE = c(knn_rmse, rf_rmse),
    R_squared = c(knn_r2, rf_r2)
  ))
}

# fit the best model ---------------------------------------------

# Step 1: linear interpolation
imp_int_full <- na_interpolation(dataPM)
for (col_name in names(dataPM)) {
  # Set the current column as the target variable Y
  NA_indices <-is.na(dataPM[[col_name]]) 
  Stat_full <- imp_int_full[!NA_indices,]
  # Construct formula
  formula_str <- paste(col_name, "~ .")
  # Train KNN regression model
  rf_model <- train(
    as.formula(formula_str),
    data = Stat_full,
    method = "rf",
    preProcess = c("center", "scale"),  # Center and scale the data
    tuneGrid = expand.grid(mtry  = 3:10),   # Grid search for optimal K
    trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
  )
  predictions <- predict(rf_model, newdata = imp_int_full[NA_indices,])
  
  # RF-imputed data is used for the respective column
  imp_int_full[NA_indices,col_name] <- predictions 
}

# change to long format and add coordinates
imp_int_full_long <- imp_int_full %>%
  pivot_longer(
    cols = starts_with("Station"), 
    names_to = "stationID",
    values_to = "PM2.5", 
    names_prefix = "Station_"
  ) %>%
  mutate(stationID = as.numeric(stationID))

# save imputed data
write.csv(imp_int_full_long, file = "pm_per_station_imputed.csv",
          row.names = FALSE)