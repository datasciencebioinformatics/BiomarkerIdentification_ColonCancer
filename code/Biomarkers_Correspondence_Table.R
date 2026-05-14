# 1 - The corrspondence table between "gene_id" and	"gene_name"
# Load one single reads file to obtain the correspondence betrween enesemble id and gene name
sample=rownames(metadata)[1]

# Take the file id
file_id<-metadata[sample,"File.ID"]

# Take the name of the file
file_name<-metadata[sample,"File.Name"]

# Read the counts table
counts_table<-read.delim(paste(project_folder,"data/",file_name,sep=""), skip = 5)

# Set the colnames
colnames(counts_table)<-c("gene_id",	"gene_name",	"gene_type",	"unstranded",	"stranded_first",	"stranded_second",	"tpm_unstranded",	"fpkm_unstranded",	"fpkm_uq_unstranded")

# Set rownames
rownames(counts_table)<-counts_table$gene_id

# Filter the correspondence table
correspondence_table<-counts_table[,c("gene_id",	"gene_name")]
#######################################################################################

# ################################################################################################################################################
# Take the normal samples, tumor samples, primary samples, metastic samples
normal_sample_ids     <- rownames(sample_sheet_data[which(sample_sheet_data$Tissue.Type     == "Normal"),])
tumor_sample_ids      <- rownames(sample_sheet_data[which(sample_sheet_data$Tissue.Type     == "Tumor"),])


# Biomarkers whose fold change (FC) was ≥50 and average TPM of control samples ≤ 10.
# First, compile data.frame with 
# Take p-value
df_mean<-data.frame(
    foldChange_Tumor_Normal=rowMeans(read_counts_table_tpm[rownames(res_tumor_normal),tumor_sample_ids])/rowMeans(read_counts_table_tpm[rownames(res_tumor_normal),normal_sample_ids]),
    avg.normal=rowMeans(read_counts_table_tpm[rownames(res_tumor_normal),normal_sample_ids]),
    std.normal=0,
    avg.tumor=rowMeans(read_counts_table_tpm[rownames(res_tumor_normal),tumor_sample_ids]),
    std.tumor=0)

  

# For each gene, calculate the std too the 
for (gene in rownames(df_mean))
{
  df_mean[gene,"std.normal"]<-sd(read_counts_table_tpm[gene,normal_sample_ids])
  df_mean[gene,"std.tumor"] <-sd(read_counts_table_tpm[gene,tumor_sample_ids])
}      
# Add gene_name
df_mean<-cbind(correspondence_table[rownames(df_mean),],df_mean)
