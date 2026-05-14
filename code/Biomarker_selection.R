# Define resampling method (e.g., 10-fold cross-validation)
train_control <- trainControl(method = "cv", number = 10)

# Train the model using stepwise AIC
step_model_lm <- train(Tissue.Type ~ ., 
                    data = read_counts_table, 
                    method = "lmStepAIC", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output

# Train the model using stepwise AIC
decision_tre <- train(Tissue.Type ~ ., 
                    data = read_counts_table, 
                    method = "rpart", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output

# Train the model using stepwise AIC
decision_tre <- train(Tissue.Type ~ ., 
                    data = read_counts_table, 
                    method = "rf", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output
