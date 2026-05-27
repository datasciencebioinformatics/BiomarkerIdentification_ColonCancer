# Define resampling method (e.g., 10-fold cross-validation)
train_control <- trainControl(method = "repeatedcv", number = 10)

# Add Tissue_Type collunn sample_sheet_data
df_counts_table_tpm_cp<-data.frame(t(df_counts_table_tpm[rownames(res_tumor_normal),]),Tissue_Type=sample_sheet_data[colnames(df_counts_table_tpm),"Tissue.Type"])

# Cobvert to factor
df_counts_table_tpm_cp$Tissue_Type<-factor(df_counts_table_tpm_cp$Tissue_Type)

# decision tree
decision_tree <-rpart(Tissue_Type ~ ., data = df_counts_table_tpm_cp, method = "class")

# bwplot               
png(filename=paste(output_dir,"Tissue_Type_rpart.png",sep=""), width = 10, height = 10, res=600, units = "cm")  
  # Plot the decision tree
  fancyRpartPlot(decision_tree, caption = NULL, sub=NULL)  
dev.off()

# Train the model using stepwise AIC
random_forest <- train(Tissue_Type ~ ., 
                    data = df_counts_table_tpm_cp, 
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
df_importance_top <-  head(df_importance,n=10)

# Save the top selected biomarkers
write_xlsx(df_importance_top[rownames(df_importance_top),], paste(output_dir,"df_importance_top.xlsx",sep=""))

