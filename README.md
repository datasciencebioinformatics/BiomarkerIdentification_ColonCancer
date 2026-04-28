# BiomarkerIdentification_ColonCancer
###################################################################################################
## Pre-configuration
project_folder="/home/felipe/Documents/ BiomarkerIdentificationCohort_ColonCancer/"
project_folder="C:/Users/felip/OneDrive/Documentos/GitHub/ BiomarkerIdentificationCohort_ColonCancer/"

### 1- Load R packages
source(paste(project_folder,"/code/Load_All_R_Packages.R",sep=""))

###################################################################################################
## Differential expression framework in R
### 2- Add the version control
source(paste(project_folder,"/code/Version_Control.R",sep=""))

### 3- Generate read counts table 
source(paste(project_folder,"/code/Generate_read_counts_table.R",sep=""))

### 4- Differential expression analyss
source(paste(project_folder,"/code/Differential_Expression_Analysis.R",sep=""))
###################################################################################################

## Biomarker assessment
### 5- Biomarkers identification
source(paste(project_folder,"/code/Biomarkers_Identification.R",sep=""))

### 6- Biomarkers assessment
source(paste(project_folder,"/code/Biomarkers_Assessment.R",sep=""))
###################################################################################################
