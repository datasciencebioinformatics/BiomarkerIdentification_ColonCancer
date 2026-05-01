# Selected variables
selected_variables<-c("samples.tissue_type", "demographic.age_at_index", "demographic.ethnicity", "demographic.gender", "demographic.race", "diagnoses.tissue_or_organ_of_origin")

# Set zero to NA
metadata$demographic.age_at_index[metadata$demographic.age_at_index == "'--" ] <- NA
##############################################################################################
# Percentage of complete data
complete_data_per_variable<-data.frame(variable=c(),completeness=c())

# Set NA, unknown, --' as missing

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
# https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# Add the count table
df2<-reshape2::melt(table(metadata[,c("demographic.gender", "samples.tissue_type")]),by="demographic.gender")

# Sort by dose and supp
df_sorted <- df2

# Calculate the cumulative sum of len for each dose
df_cumsum <- ddply(df_sorted, "value", transform, label_ypos=cumsum(value))

# Add the total male
df_cumsum$total_male <-sum(df_cumsum[which(df_cumsum$demographic.gender=="male"),"value"])
df_cumsum$total_female <-sum(df_cumsum[which(df_cumsum$demographic.gender=="female"),"value"])

# Take the total male and total female
df_cumsum$total_male_tumor      <-sum(df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])
df_cumsum$total_female_tumor    <-sum(df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])

# Set the y-label for tumor samples
df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$samples.tissue_type=="Tumor" ),"total_male_tumor"] /2
df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$samples.tissue_type=="Tumor" ),"total_female_tumor"] /2

# Set the y-label for normal samples
df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$samples.tissue_type=="Tumor" ),"total_male_tumor"]+(df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$samples.tissue_type=="Tumor" ),"total_male"]-df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$samples.tissue_type=="Tumor" ),"total_male_tumor"])/2 + 1
df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$samples.tissue_type=="Tumor" ),"total_female_tumor"]+(df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$samples.tissue_type=="Tumor" ),"total_female"]-df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$samples.tissue_type=="Tumor" ),"total_female_tumor"])/2 + 1

# Create the barplot
p1<-ggplot(data=df_cumsum, aes(x=demographic.gender, y=value, fill=samples.tissue_type)) +  geom_bar(stat="identity")+  geom_text(aes(y=label_ypos, label=value), vjust=1.6,  color="white", size=3.5)+  scale_fill_brewer(palette="Paired")+  theme_minimal()  + theme(legend.position = "bottom",panel.grid = element_blank())  + coord_flip()
##############################################################################################
# bwplot               
png(filename=paste(output_dir,"Plot_demographic_gender_samples_tissue_type.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p1
dev.off()

##############################################################################################
# https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# Add the count table
df3<-reshape2::melt(table(metadata[,c("demographic.race", "samples.tissue_type")]),by="demographic.race")

# Sort by dose and supp
df_sorted <- df3

# Calculate the cumulative sum of len for each dose
df_cumsum <- ddply(df_sorted, "value", transform, label_ypos=cumsum(value))

# Add the total male
df_cumsum$total_white                            <-sum(df_cumsum[which(df_cumsum$demographic.race=="white"),"value"])
df_cumsum$total_Unknown                          <-sum(df_cumsum[which(df_cumsum$demographic.race=="Unknown"),"value"])
df_cumsum$total_not_reported                     <-sum(df_cumsum[which(df_cumsum$demographic.race=="not reported"),"value"])
df_cumsum$total_black_or_african_american        <-sum(df_cumsum[which(df_cumsum$demographic.race=="black or african american"),"value"])
df_cumsum$total_asian                            <-sum(df_cumsum[which(df_cumsum$demographic.race=="asian"),"value"])
df_cumsum$total_american_indian_or_alaska_native <-sum(df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native"),"value"])

# Take the total male and total female
df_cumsum$total_white_tumor                            <-sum(df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])
df_cumsum$total_Unknown_tumor                          <-sum(df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])
df_cumsum$total_not_reported_tumor                     <-sum(df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])
df_cumsum$total_black_or_african_american_tumor        <-sum(df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])
df_cumsum$total_asian_tumor                            <-sum(df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])
df_cumsum$total_american_indian_or_alaska_native_tumor <-sum(df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$samples.tissue_type=="Tumor" ),"value"])


# Set the y-label for tumor samples
df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"]                            <- df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Tumor" ),"total_white_tumor"] /2
df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"]                          <- df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$samples.tissue_type=="Tumor" ),"total_Unknown_tumor"] /2
df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"]                     <- df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$samples.tissue_type=="Tumor" ),"total_not_reported_tumor"] /2
df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"]        <- df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$samples.tissue_type=="Tumor" ),"total_black_or_african_american"] /2
df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"]                            <- df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Tumor" ),"total_asian"] /2
df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$samples.tissue_type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$samples.tissue_type=="Tumor" ),"total_american_indian_or_alaska_native"] /2

# Set the y-label for normal samples
df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Tumor" ),"total_white_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Normal" ),"total_white"]-df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Tumor" ),"total_white_tumor"])/2 
df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$samples.tissue_type=="Tumor" ),"total_Unknown_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$samples.tissue_type=="Normal" ),"total_Unknown"]-df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$samples.tissue_type=="Tumor" ),"total_Unknown_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$samples.tissue_type=="Tumor" ),"total_not_reported_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$samples.tissue_type=="Normal" ),"total_not_reported"]-df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$samples.tissue_type=="Tumor" ),"total_not_reported_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$samples.tissue_type=="Tumor" ),"total_black_or_african_american_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$samples.tissue_type=="Normal" ),"total_black_or_african_american"]-df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$samples.tissue_type=="Tumor" ),"total_black_or_african_american_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Tumor" ),"total_asian_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Normal" ),"total_asian"]-df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Tumor" ),"total_asian_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$samples.tissue_type=="Normal" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$samples.tissue_type=="Tumor" ),"total_asian_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Normal" ),"total_american_indian_or_alaska_native"]-df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$samples.tissue_type=="Tumor" ),"total_american_indian_or_alaska_native_tumor"])/2


# Set zero to NA
df_cumsum$label_ypos[df_cumsum$label_ypos == 0] <- NA
df_cumsum$label[df_cumsum$label == 0] <- NA
df_cumsum$value[df_cumsum$value == 0] <- NA

# Create the barplot
p2<-ggplot(data=df_cumsum, aes(x=demographic.race, y=value, fill=samples.tissue_type)) +  geom_bar(stat="identity")+  geom_text(aes(y=label_ypos, label=value), vjust=1.6,  color="white", size=3.5)+  scale_fill_brewer(palette="Paired")+  theme_minimal()  + theme(legend.position = "bottom",panel.grid = element_blank())  + coord_flip()

# bwplot               
png(filename=paste(output_dir,"Plot_demographic_race_samples_tissue_type.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p2
dev.off()

###########################################################################################################################
# Age plot

# bwplot               
png(filename=paste(output_dir,"Variable_completeness.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  ggplot(metadata, aes(x=as.factor(samples.tissue_type), y=as.numeric(demographic.age_at_index))) +  geom_boxplot(lpha=0.2)  + theme_bw()
dev.off()




