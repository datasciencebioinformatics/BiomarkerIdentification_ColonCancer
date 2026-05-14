# Define resampling method (e.g., 10-fold cross-validation)
train_control <- trainControl(method = "cv", number = 10)

# Add Tissue_Type collunn sample_sheet_data
df_read_counts_table<-data.frame(t(read_counts_table[rownames(res_tumor_normal),]),Tissue_Type=sample_sheet_data[colnames(read_counts_table),"Tissue.Type"])

# Cobvert to factor
df_read_counts_table$Tissue_Type<-factor(df_read_counts_table$Tissue_Type)


# Train the model using stepwise AIC
random_forest <- train(Tissue_Type ~ ., 
                    data = df_read_counts_table, 
                    method = "rf", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output

# Train the model using stepwise AIC
decision_tree <- train(Tissue_Type ~ ., 
                    data = df_read_counts_table, 
                    method = "rpart", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output



# bwplot               
png(filename=paste(output_dir,"Tissue_Type_rpart.png",sep=""), width = 10, height = 10, res=600, units = "cm")  
  # Plot the bayesian network graph
  fancyRpartPlot(decision_tree, caption = NULL, sub=NULL)  
dev.off()

# Train the model using stepwise AIC
random_forest <- train(Tissue.Type ~ ., 
                    data = read_counts_table, 
                    method = "rf", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = FALSE to suppress iteration output

# Get Variable Importance
importance <- varImp(random_forest)

# Save data.frame
df_importance<-data.frame(data.frame(importance$importance),gene_name= correspondence_table[rownames(importance$importance),"gene_name"])

# Sorts 'df' by the 'age' column in ascending order
df_importance <- df_importance[order(-df_importance$Overall), ] 

# Select top 20 biomarkers
df_importance_top <-  head(df_importance,n=20)

# Save the top selected biomarkers
write_xlsx(df_mean[rownames(df_importance_top),], paste(output_dir,"df_importance_top.xlsx",sep=""))

