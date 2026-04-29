########################################################################
# First phase, Tissue.Type
# Create DESeqDataSet from your prepared matrix
dds_tumor_normal <- DESeqDataSetFromMatrix(countData = read_counts_table,
                              colData = sample_sheet_data,
                              design = ~ Tissue.Type)
# Run DeSeq2
dds_tumor_normal <- DESeq(dds_tumor_normal)

# Obtain the results
res_tumor_normal <- results(dds_tumor_normal, contrast=c("Tissue.Type","Tumor","Normal"))

# Take 
res_tumor_normal<-data.frame(res_tumor_normal[which(res_tumor_normal$padj<0.01 & abs(res_tumor_normal$log2FoldChange)>3),])
########################################################################
# 2. Save the object to an .rds file
saveRDS(dds_tumor_normal, file = file.path(project_folder, "/rsd","/dds_tumor_normal.rsd" ))
saveRDS(res_tumor_normal, file = file.path(project_folder, "/rsd","/res_tumor_normal.rsd" ))



########################################################################
# Second, Metastatic-Primary versus Not Applicable
# Add diagnosis collumn
sample_sheet_data$diagnosis <- sample_sheet_data$Tumor.Descriptor

# Set the normal samples
sample_sheet_data[which(sample_sheet_data$Tissue.Type == "Normal"),"diagnosis"]<-"Normal"

# Create DESeqDataSet from your prepared matrix
dds_diagnosis <- DESeqDataSetFromMatrix(countData = read_counts_table,
                              colData = sample_sheet_data,
                              design = ~ diagnosis)



# Run DeSeq2
dds_diagnosis <- DESeq(dds_diagnosis)

# Obtain the results
res_diagnosis_Primary       <- results(dds_diagnosis, contrast=c("diagnosis","Primary","Normal"))
res_diagnosis_Metastatic    <- results(dds_diagnosis, contrast=c("diagnosis","Metastatic","Normal"))
res_diagnosis_Premalignant  <- results(dds_diagnosis, contrast=c("diagnosis","Premalignant","Normal"))

# Take 
res_diagnosis_Primary     <-data.frame(res_diagnosis_Primary[which(res_diagnosis_Primary$padj<0.01 & abs(res_diagnosis_Primary$log2FoldChange)>3),])
res_diagnosis_Metastatic  <-data.frame(res_diagnosis_Metastatic[which(res_diagnosis_Metastatic$padj<0.01 & abs(res_diagnosis_Metastatic$log2FoldChange)>3),])
res_diagnosis_Premalignant<-data.frame(res_diagnosis_Premalignant[which(res_diagnosis_Premalignant$padj<0.01 & abs(res_diagnosis_Premalignant$log2FoldChange)>3),])
########################################################################
# 2. Save the object to an .rds file
saveRDS(dds_diagnosis, file = file.path(project_folder, "/rsd","/dds_diagnosis.rsd" ))


# 2. Save the object to an .rds file
saveRDS(res_diagnosis_Primary, file = file.path(project_folder, "/rsd","/res_diagnosis_Primary.rsd" ))
saveRDS(res_diagnosis_Metastatic, file = file.path(project_folder, "/rsd","/res_diagnosis_Metastatic.rsd" ))
saveRDS(res_diagnosis_Premalignant, file = file.path(project_folder, "/rsd","/res_diagnosis_Premalignant.rsd" ))



