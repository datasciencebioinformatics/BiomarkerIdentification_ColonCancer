# Selected variables
selected_variables<-c("samples.tissue_type", "demographic.age_at_index", "demographic.ethnicity", "demographic.gender", "demographic.race", "diagnoses.tissue_or_organ_of_origin")

##############################################################################################
# Percentage of complete data
complete_data_per_variable<-data.frame(variable=c(),completeness=c())

# For each column, convert to numeric
for (variable in colnames(metadata[,selected_variables]))
{
    
  # Percentage of complete data
  complete_data_per_variable<-rbind(complete_data_per_variable,data.frame(variable=c(variable),completeness=c(sum(!is.na(metadata[,variable]))/length(metadata[,variable])*100)))
}
# Set the rownames
rownames(complete_data_per_variable)<-complete_data_per_variable$variable


# Barplot
p1<-ggplot(complete_data_per_variable, aes(x=variable, y=completeness)) +  geom_bar(stat = "identity") + coord_flip()  + theme_bw()

# bwplot               
png(filename=paste(output_dir,"Variable_completeness.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p1 + ylab("completeness %")
dev.off()
##############################################################################################

ggplot(metadata, aes(y = samples.tissue_type)) +  geom_bar(aes(fill = demographic.gender), position = position_stack(reverse = TRUE)) +  theme(legend.position = "top") + theme_bw()

"demographic.gender" with "samples.tissue_type"
"demographic.race" with "samples.tissue_type"
