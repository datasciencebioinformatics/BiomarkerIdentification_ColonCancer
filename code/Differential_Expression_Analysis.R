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
res_tumor_normal<-cbind(Gene_symbol=correspondence_table[rownames(res_tumor_normal),2],res_tumor_normal)


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

cbind()

sheets <- list("Primary" = res_diagnosis_Primary, "Metastatic" = res_diagnosis_Metastatic, "Premalignant"=res_diagnosis_Premalignant )
write_xlsx(sheets,paste(output_dir,paste("SupplementalTableS2",".xlsx",sep=""),sep=""))
########################################################################

########################################################################
# Third, sex-specific genes
# Add diagnosis collumn
metadata$sex_diagnostic <- paste(metadata$demographic.gender,metadata$Tissue.Type,sep="-")


# Create DESeqDataSet from your prepared matrix
dds_sex_diagnostic <- DESeqDataSetFromMatrix(countData = read_counts_table,
                              colData = metadata,
                              design = ~ sex_diagnostic)



# Run DeSeq2
dds_sex_diagnostic <- DESeq(dds_sex_diagnostic)

# Obtain the results
res_Male_Tumor              <- results(dds_sex_diagnostic, contrast=c("sex_diagnostic","male-Tumor","male-Normal"))
res_Female_Tumor            <- results(dds_sex_diagnostic, contrast=c("sex_diagnostic","female-Tumor","female-Normal"))

# Add the gene names and gene symbols
res_Male_Tumor<-cbind(Gene_symbol=correspondence_table[rownames(res_Male_Tumor),2],res_Male_Tumor)
res_Female_Tumor<-cbind(Gene_symbol=correspondence_table[rownames(res_Female_Tumor),2],res_Female_Tumor)

# Take 
res_Male_Tumor     <-data.frame(res_Male_Tumor[which(res_Male_Tumor$padj<0.05 & abs(res_Male_Tumor$log2FoldChange)>3.0),])
res_Female_Tumor   <-data.frame(res_Female_Tumor[which(res_Female_Tumor$padj<0.05 & abs(res_Female_Tumor$log2FoldChange)>3.0),])


sheets <- list("Male" = res_Male_Tumor, "Female" = res_Female_Tumor)
write_xlsx(sheets,paste(output_dir,paste("SupplementalTableS3",".xlsx",sep=""),sep=""))
########################################################################


########################################################################
# Fourth, sex-specific genes
# Add diagnosis collumn
metadata$age_groups_diagnostic <- paste(metadata$age_groups,metadata$Tissue.Type,sep="-")

# Take the sub values
metadata_sub<-metadata[!is.na(metadata$age_groups),]


# Create DESeqDataSet from your prepared matrix
dds_age_groups_diagnostic <- DESeqDataSetFromMatrix(countData = read_counts_table[,metadata_sub$File.ID],
                              colData = metadata_sub,
                              design = ~ age_groups_diagnostic)



# Run DeSeq2
dds_age_groups_diagnostic <- DESeq(dds_age_groups_diagnostic)

# Obtain the results
res_60_69           <- results(dds_age_groups_diagnostic, contrast=c("age_groups_diagnostic","60-69-Tumor","60-69-Normal"))
res_70_79           <- results(dds_age_groups_diagnostic, contrast=c("age_groups_diagnostic","70-79-Tumor","70-79-Normal"))
res_80_89           <- results(dds_age_groups_diagnostic, contrast=c("age_groups_diagnostic","80-89-Tumor","80-89-Normal"))

# Add the gene names and gene symbols
res_60_69<-cbind(Gene_symbol=correspondence_table[rownames(res_60_69),2],res_60_69)
res_70_79<-cbind(Gene_symbol=correspondence_table[rownames(res_70_79),2],res_70_79)
res_80_89<-cbind(Gene_symbol=correspondence_table[rownames(res_80_89),2],res_80_89)


# Take 
res_60_69   <-data.frame(res_60_69[which(res_60_69$padj<0.05 & abs(res_60_69$log2FoldChange)>3.0),])
res_70_79   <-data.frame(res_80_89[which(res_70_79$padj<0.05 & abs(res_70_79$log2FoldChange)>3.0),])
res_80_89   <-data.frame(res_80_89[which(res_80_89$padj<0.05 & abs(res_80_89$log2FoldChange)>3.0),])

sheets <- list("60_69" = res_60_69, "70_79" = res_70_79, "80_89" = res_80_89)
write_xlsx(sheets,paste(output_dir,paste("SupplementalTableS4",".xlsx",sep=""),sep=""))
########################################################################


########################################################################
# Fifth, race specific
# Add diagnosis collumn
metadata$demographic.race_diagnostic <- paste(metadata$demographic.race,metadata$Tissue.Type,sep="-")

# Take the sub values
metadata_sub<-metadata[which(metadata$demographic.race != "Unknown"),]
metadata_sub<-metadata_sub[which(metadata_sub$demographic.race != "not reported"),]


# Create DESeqDataSet from your prepared matrix
dds_race_diagnostic <- DESeqDataSetFromMatrix(countData = read_counts_table[,metadata_sub$File.ID],
                              colData = metadata_sub,
                              design = ~ demographic.race_diagnostic)



# Run DeSeq2
dds_race_diagnostic <- DESeq(dds_race_diagnostic)

# Obtain the results
res_white           <- results(dds_race_diagnostic, contrast=c("demographic.race_diagnostic","white-Tumor","white-Normal"))

# Add the gene names and gene symbols
res_white<-cbind(Gene_symbol=correspondence_table[rownames(res_white),2],res_white)


sheets <- list("white" = res_white)
write_xlsx(sheets,paste(output_dir,paste("SupplementalTableS5",".xlsx",sep=""),sep=""))
########################################################################

