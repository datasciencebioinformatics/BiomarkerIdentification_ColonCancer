# Define resampling method (e.g., 10-fold cross-validation)
train_control <- trainControl(method = "repeatedcv", number = 10)

# Add Tissue_Type collunn sample_sheet_data
df_read_counts_table<-data.frame(t(read_counts_table[rownames(res_tumor_normal),]),Tissue_Type=sample_sheet_data[colnames(read_counts_table),"Tissue.Type"])

# Cobvert to factor
df_read_counts_table$Tissue_Type<-factor(df_read_counts_table$Tissue_Type)

# decision tree
decision_tree <-rpart(Tissue_Type ~ ., data = df_read_counts_table, method = "class")

# bwplot               
png(filename=paste(output_dir,"Tissue_Type_rpart.png",sep=""), width = 10, height = 10, res=600, units = "cm")  
  # Plot the bayesian network graph
  fancyRpartPlot(decision_tree, caption = NULL, sub=NULL)  
dev.off()

# Train the model using stepwise AIC
random_forest <- train(Tissue_Type ~ ., 
                    data = df_read_counts_table, 
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

###############################################################
# 1. Regressão linear múltipla (com seleção stepwise por AIC)
# A seleção de modelo deve ser realizada utilizando o método stepwise regression com abordagem backward elimination, tendo como critério único a minimização do Akaike Information Criterion (AIC) para cada experimento e para cada variável de saída. A seleção deve ser realizada exclusivamente com dados de train. Nenhuma informação de test pode ser utilizada na seleção.
# For the multiple linear regression, lm method was used to fit the trainning set on the surrogate models. StepAIC function was called to select the best model based on AIC. The trainning set was again used to fit on the selected model and obtain the prediction statistitics (RMSE, Rsquared, MAE with SD).

# lm method was used to fit the trainning set on the surrogate models
multiple_linear_regression_lm <- lm(Tissue_Type ~ ., data = df_read_counts_table)

# Perform stepwise selection (direction "both", "backward", "forward")
surrogate_model_final_lm <- stepAIC(multiple_linear_regression_lm, direction = "backward", trace = 0)

# Train the linear model
multiple_linear_regression_lm_selected <- caret::train(formula(surrogate_model_final_lm), data = training_set, method = "lm", trControl = train_control)

#########################################################################################################
df_read_counts_table$Tissue_Type<-as.numeric(df_read_counts_table$Tissue_Type)

# Create bayesian networks
bn_viscour <- hc(df_read_counts_table)

# bwplot               
png(filename=paste(output_dir,"Bayesian_Network_structure.png",sep=""), width = 17, height = 17, res=600, units = "cm")  
  # Plot the bayesian network graph
  plot(as.igraph(tb_viscour), vertex.color="black",vertex.size=25,vertex.label.color="orange",layout=layout_with_kk)
dev.off()
