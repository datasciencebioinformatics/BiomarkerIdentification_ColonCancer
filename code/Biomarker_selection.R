# 1. Configurar o método de reamostragem (Validação Cruzada)
# define trainControl
# Define train control parameters (e.g., 10-fold cross-validation)
# Custom summary function for MRE 
# Custom summary function for MRE
mreSummary <- function(data, lev = NULL, model = NULL) {
	rmse=rmse(data$obs, data$pred) 
	mae=mae(data$obs, data$pred)
	mre=mre(data$obs, data$pred) 
	cor=cor(data$obs, data$pred)

	# If NA, correct
	if (is.null(rmse)) {rmse=NA}
	if (is.null(mae)) {mae=NA}
	if (is.null(mre)) {mre=NA}
	
  c(MRE = mre, RMSE=rmse, MAE = mae, Cor=cor)
}

# Set the trainning function
train_control <- trainControl(method = "cv", number = 10, verboseIter = TRUE, summaryFunction = mreSummary)
########################################################################################################################################################################

# Add Tissue_Type collunn sample_sheet_data
df_counts_table_tpm_cp<-data.frame(t(read_counts_table_tpm[rownames(res_tumor_normal),]),Tissue_Type=sample_sheet_data[colnames(read_counts_table_tpm),"Tissue.Type"])

# Cobvert to factor
df_counts_table_tpm_cp$Tissue_Type<-factor(df_counts_table_tpm_cp$Tissue_Type)

# decision tree
decision_tree <-rpart(Tissue_Type ~ ., data = df_counts_table_tpm_cp, method = "class")

# bwplot               
png(filename=paste(output_dir,"Tissue_Type_rpart.png",sep=""), width = 10, height = 10, res=600, units = "cm")  
  # Plot the decision tree
  fancyRpartPlot(decision_tree, caption = NULL, sub=NULL)  
dev.off()

# Make a copy
df_counts_table_tpm_cp_2<-df_counts_table_tpm_cp

# Convert to numeric
df_counts_table_tpm_cp_2$Tissue_Type<-as.numeric(df_counts_table_tpm_cp_2$Tissue_Type)

# 6. Model for combination of parameter
model_comb <- caret::train(Tissue_Type ~ ., data = df_counts_table_tpm_cp_2, method = "rpart", trControl = train_control)


rmse=round(model_comb$results$RMSE,2)
mae=round(model_comb$results$MAE,2)
mre=round(model_comb$results$MRE,2) 
cor=round(model_comb$results$Cor,2)
################################################################################################################################################################################3

# Train the model using stepwise AIC
random_forest <- train(Tissue_Type ~ ., 
                    data = df_counts_table_tpm_cp_2, 
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
write_xlsx(df_mean[rownames(df_importance_top),], paste(output_dir,"df_importance_top.xlsx",sep=""))

# 6. Model for combination of parameter
model_comb <- caret::train(Tissue_Type ~ ., data = df_counts_table_tpm_cp_2, method = "rpart", trControl = train_control)

rmse=round(model_comb$results$RMSE,2)
mae=round(model_comb$results$MAE,2)
mre=round(model_comb$results$MRE,2) 
cor=round(model_comb$results$Cor,2)
