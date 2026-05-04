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
res_tumor_normal<-data.frame(res_tumor_normal[which(res_tumor_normal$padj<0.05 & abs(res_tumor_normal$log2FoldChange)>3.0),])

# Add the gene names and gene symbols


# From the File.ID, only the ID is kept in the variable sample_id
write_xlsx(cbind(Gene_symbol=correspondence_table[rownames(res_tumor_normal),2],res_tumor_normal),paste(output_dir,paste("SupplementalTableS1",".xlsx",sep=""),sep=""))
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

# Add the gene names and gene symbols
res_diagnosis_Primary<-cbind(Gene_symbol=correspondence_table[rownames(res_diagnosis_Primary),2],res_diagnosis_Primary)
res_diagnosis_Metastatic<-cbind(Gene_symbol=correspondence_table[rownames(res_diagnosis_Metastatic),2],res_diagnosis_Metastatic)
res_diagnosis_Premalignant<-cbind(Gene_symbol=correspondence_table[rownames(res_diagnosis_Premalignant),2],res_diagnosis_Premalignant)

# Take 
res_diagnosis_Primary     <-data.frame(res_diagnosis_Primary[which(res_diagnosis_Primary$padj<0.05 & abs(res_diagnosis_Primary$log2FoldChange)>3.0),])
res_diagnosis_Metastatic  <-data.frame(res_diagnosis_Metastatic[which(res_diagnosis_Metastatic$padj<0.05 & abs(res_diagnosis_Metastatic$log2FoldChange)>3.0),])
res_diagnosis_Premalignant<-data.frame(res_diagnosis_Premalignant[which(res_diagnosis_Premalignant$padj<0.05 & abs(res_diagnosis_Premalignant$log2FoldChange)>3.0),])

sheets <- list("Primary" = res_diagnosis_Primary, "Metastatic" = res_diagnosis_Metastatic, "Premalignant"=res_diagnosis_Premalignant )
write_xlsx(sheets,,paste(output_dir,paste("SupplementalTableS2",".xlsx",sep=""),sep=""))
########################################################################

########################################################################
# Third, sex-specific genes
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

# Add the gene names and gene symbols

# Take 
res_diagnosis_Primary     <-data.frame(res_diagnosis_Primary[which(res_diagnosis_Primary$padj<0.05 & abs(res_diagnosis_Primary$log2FoldChange)>3.0),])
res_diagnosis_Metastatic  <-data.frame(res_diagnosis_Metastatic[which(res_diagnosis_Metastatic$padj<0.05 & abs(res_diagnosis_Metastatic$log2FoldChange)>3.0),])
res_diagnosis_Premalignant<-data.frame(res_diagnosis_Premalignant[which(res_diagnosis_Premalignant$padj<0.05 & abs(res_diagnosis_Premalignant$log2FoldChange)>3.0),])

sheets <- list("Primary" = res_diagnosis_Primary, "Metastatic" = res_diagnosis_Metastatic, "Premalignant"=res_diagnosis_Premalignant )
write_xlsx(sheets,,paste(output_dir,paste("SupplementalTableS2",".xlsx",sep=""),sep=""))
########################################################################
