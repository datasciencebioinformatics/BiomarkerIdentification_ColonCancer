# A R script to create metadata from gdc files.
##########################################################################################################################################################################################################
# Reading the contents of TSV file using read_tsv() method
gdc_sample_sheet_file<-paste(project_folder,"metadata/gdc_sample_sheet_star_gene_counts.tsv",sep="")

# Read data
gdc_sample_sheet_data<-read.table(file = gdc_sample_sheet_file, sep = '\t', header = TRUE,fill=TRUE)  

# Add collumn sample_id
gdc_sample_sheet_data$sample_submitter_id<-gdc_sample_sheet_data$Sample.ID
#####################################################################################################################
# Set path to files                                                                                                 
clinical_file=paste(project_folder,"metadata/clinical.tsv",sep="")
sample_file=paste(project_folder,"metadata/sample.tsv",sep="")
exposure_file=paste(project_folder,"metadata/exposure.tsv",sep="")

# Load data
clinical_data<-read.delim(file = clinical_file, sep = '\t', header = TRUE,fill=TRUE)    
sample_data<-read.delim(file = sample_file, sep = '\t', header = TRUE,fill=TRUE)                                    
exposure_data<-read.delim(file = exposure_file, sep = '\t', header = TRUE,fill=TRUE)                                #

# Merge data
merged_sample_clinical_data<-merge(sample_data,clinical_data,by.x="cases.case_id",by.y="cases.case_id")

# Merge all
merged_sample_clinical_data<-merge(merged_sample_clinical_data,exposure_data,by.x="cases.case_id",by.y="cases.case_id")

# Merge tables
merged_data_patient_info<-merge(merged_sample_clinical_data,gdc_sample_sheet_data,by.x="samples.submitter_id",by.y="sample_submitter_id")
#####################################################################################################################
# Set file name variable 
merged_data_patient_info<-merged_data_patient_info[merged_data_patient_info$Data.Category=="Transcriptome Profiling",]

# Check which entries contains the words .rna_seq.augmented_star_gene_counts.tsv
merged_data_patient_info<-merged_data_patient_info[which(grepl(pattern="*.rna_seq.augmented_star_gene_counts.tsv", x=merged_data_patient_info$File.Name)),]

# From the File.ID, only the ID is kept in the variable sample_id
merged_data_patient_info$sample_id<-gsub(".rna_seq.augmented_star_gene_counts.tsv", "", merged_data_patient_info$File.Name)

# From the File.ID, only the ID is kept in the variable sample_id
write_xlsx(merged_data_patient_info,"/home/felipe/Downloads/merged_data_patient_info")

