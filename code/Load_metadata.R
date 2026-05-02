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

# Load data
clinical_data<-read.delim(file = clinical_file, sep = '\t', header = TRUE,fill=TRUE)    
sample_data<-read.delim(file = sample_file, sep = '\t', header = TRUE,fill=TRUE)                                    

# Add collumns to sample_data
sample_data$cases.submitter_id_y            <-""
sample_data$demographic.age_at_index        <-""
sample_data$demographic.ethnicity           <-""
sample_data$demographic.gender              <-""
sample_data$demographic.race                <-""
sample_data$demographic.sex_at_birth        <-""

# Subset collumns
clinical_data<-unique(clinical_data[,c("cases.submitter_id","cases.case_id","demographic.age_at_index" ,"demographic.ethnicity", "demographic.gender", "demographic.race","demographic.sex_at_birth")])


# For each samples
for (sample_id in rownames(sample_data))
{  
  # Take the cases.case_id
  cases.case_id<-sample_data[sample_id,"cases.case_id"]

  cases.submitter_id_y      =clinical_data[which(clinical_data$cases.case_id == cases.case_id),"cases.submitter_id"]
  demographic.age_at_index  =clinical_data[which(clinical_data$cases.case_id == cases.case_id),"demographic.age_at_index"]
  demographic.ethnicity     =clinical_data[which(clinical_data$cases.case_id == cases.case_id),"demographic.ethnicity"]
  demographic.gender        =clinical_data[which(clinical_data$cases.case_id == cases.case_id),"demographic.gender"]
  demographic.race          =clinical_data[which(clinical_data$cases.case_id == cases.case_id),"demographic.race"]
  demographic.sex_at_birth  =clinical_data[which(clinical_data$cases.case_id == cases.case_id),"demographic.sex_at_birth"]

  # Take the cases.case_id
  sample_data[which(sample_data$cases.case_id == cases.case_id),"cases.submitter_id_y"]       <-  cases.submitter_id_y
  sample_data[which(sample_data$cases.case_id == cases.case_id),"demographic.age_at_index"]   <-  demographic.age_at_index
  sample_data[which(sample_data$cases.case_id == cases.case_id),"demographic.ethnicity"]      <-  demographic.ethnicity
  sample_data[which(sample_data$cases.case_id == cases.case_id),"demographic.gender"]         <-  demographic.gender
  sample_data[which(sample_data$cases.case_id == cases.case_id),"demographic.race"]           <-  demographic.race
  sample_data[which(sample_data$cases.case_id == cases.case_id),"demographic.sex_at_birth"]   <-  demographic.sex_at_birth  
}

# Add collumns to sample_data
gdc_sample_sheet_data$cases.submitter_id_y            <-""
gdc_sample_sheet_data$demographic.age_at_index        <-""
gdc_sample_sheet_data$demographic.ethnicity           <-""
gdc_sample_sheet_data$demographic.gender              <-""
gdc_sample_sheet_data$demographic.race                <-""
gdc_sample_sheet_data$demographic.sex_at_birth        <-""

# For each sample in sheet data
for (sample_id in rownames(gdc_sample_sheet_data))
{
  # Take the Sample.ID
  Case.ID<-gdc_sample_sheet_data[sample_id,"Case.ID"]

  gdc_sample_sheet_data[sample_id,"demographic.age_at_index"]<-unique(sample_data[which(sample_data$cases.submitter_id == Case.ID),"demographic.age_at_index"])
  gdc_sample_sheet_data[sample_id,"demographic.ethnicity"]   <-unique(sample_data[which(sample_data$cases.submitter_id == Case.ID),"demographic.ethnicity"])
  gdc_sample_sheet_data[sample_id,"demographic.gender"]      <-unique(sample_data[which(sample_data$cases.submitter_id == Case.ID),"demographic.gender"])
  gdc_sample_sheet_data[sample_id,"demographic.race"]        <-unique(sample_data[which(sample_data$cases.submitter_id == Case.ID),"demographic.race"])
  gdc_sample_sheet_data[sample_id,"demographic.sex_at_birth"]<-unique(sample_data[which(sample_data$cases.submitter_id == Case.ID),"demographic.sex_at_birth"])
}


#####################################################################################################################
# Select metadata 
metadata<-gdc_sample_sheet_data

# From the File.ID, only the ID is kept in the variable sample_id
write_xlsx(metadata,paste(project_folder,"tables/metadata.xlsx",sep=""))






