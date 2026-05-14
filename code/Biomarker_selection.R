# Define resampling method (e.g., 10-fold cross-validation)
train_control <- trainControl(method = "cv", number = 10)

# Train the model using stepwise AIC
step_model_lm <- train(Tissue.Type ~ ., 
                    data = read_counts_table, 
                    method = "lmStepAIC", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output

# Train the model using stepwise AIC
decision_tree <- train(Tissue.Type ~ ., 
                    data = read_counts_table, 
                    method = "rpart", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output



# bwplot               
png(filename=paste(output_dir,"Tissue_Type_rpart.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  # Plot the bayesian network graph
  fancyRpartPlot(Head_rpart, caption = NULL, sub=NULL)  
dev.off()

# Train the model using stepwise AIC
random_forest <- train(Tissue.Type ~ ., 
                    data = read_counts_table, 
                    method = "rf", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output

# Get Variable Importance
importance <- varImp(random_forest)

# bwplot               
png(filename=paste(output_dir,"Tissue_Type_varIMP.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  # Plot Variable Importance
  plot(importance, main="")
dev.off()
