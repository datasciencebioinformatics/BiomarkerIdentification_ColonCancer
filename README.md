# BiomarkerIdentification_ColonCancer
###################################################################################################
## Pre-configuration
project_folder="/home/felipe/Documents/GitHub/BiomarkerIdentification_ColonCancer/"

### 1- Add the version control
source(paste(project_folder,"/code/Version_Control.R",sep=""))

### 2- Load R packages
source(paste(project_folder,"/code/Load_All_R_Packages.R",sep=""))

### 3- Assess the number of cases
source(paste(project_folder,"/code/Number_of_cases.R",sep=""))
###################################################################################################
## Data analayss
### 4- Load metadata
source(paste(project_folder,"/code/Load_metadata.R",sep=""))

## Differential expression framework in R
### 5- Generate read counts table 
source(paste(project_folder,"/code/Generate_read_counts_table.R",sep=""))

### 6- Differential expression analyss
source(paste(project_folder,"/code/Differential_Expression_Analysis.R",sep=""))

### 5- Assess metadata
source(paste(project_folder,"/code/Assess_metadata.R",sep=""))
###################################################################################################
