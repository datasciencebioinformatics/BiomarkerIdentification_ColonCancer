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
