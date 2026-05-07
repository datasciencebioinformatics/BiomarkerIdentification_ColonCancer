# Selected variables
selected_variables<-c("Tissue.Type", "Tumor.Descriptor", "Specimen.Type", "Preservation.Method",  "demographic.age_at_index","demographic.ethnicity", "demographic.gender"  ,    "demographic.race", "demographic.sex_at_birth")

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
  p1 + ylab("completeness %") + scale_colour_grey() + geom_text(aes(label = round(completeness,2)), hjust = -0.10)
dev.off()

##############################################################################################
library("plyr")

# https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# Add the count table
df1<-reshape2::melt(table(metadata[,c("age_groups", "Tissue.Type")]),id.var=c("age_groups","Tissue.Type"))

# Sort by dose and supp
df_sorted <- df1

# Calculate the cumulative sum of len for each dose
df_cumsum <- ddply(df_sorted, "value", transform, label_ypos=cumsum(value))

# Add the total by age
df_cumsum$total_0_9 <-sum(df_cumsum[which(df_cumsum$age_groups=="0-9"),"value"])
df_cumsum$total_10_19 <-sum(df_cumsum[which(df_cumsum$age_groups=="10-19"),"value"])
df_cumsum$total_20_29 <-sum(df_cumsum[which(df_cumsum$age_groups=="20-29"),"value"])
df_cumsum$total_30_39 <-sum(df_cumsum[which(df_cumsum$age_groups=="30-39"),"value"])
df_cumsum$total_40_49 <-sum(df_cumsum[which(df_cumsum$age_groups=="40-49"),"value"])
df_cumsum$total_50_59 <-sum(df_cumsum[which(df_cumsum$age_groups=="50-59"),"value"])
df_cumsum$total_60_69 <-sum(df_cumsum[which(df_cumsum$age_groups=="60-69"),"value"])
df_cumsum$total_70_79 <-sum(df_cumsum[which(df_cumsum$age_groups=="70-79"),"value"])
df_cumsum$total_80_89 <-sum(df_cumsum[which(df_cumsum$age_groups=="80-89"),"value"])
df_cumsum$total_90_99 <-sum(df_cumsum[which(df_cumsum$age_groups=="90-99"),"value"])
df_cumsum$total_100_plus <-sum(df_cumsum[which(df_cumsum$age_groups=="100+"),"value"])


# Take the total male and total female
df_cumsum$total_0_9_tumor           <-sum(df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_10_19_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_20_29_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_30_39_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_40_49_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_50_59_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_60_69_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_70_79_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_80_89_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_90_99_tumor         <-sum(df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_100_plus_tumor      <-sum(df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Tumor" ),"value"])

# Take the total male and total female
df_cumsum$total_0_9_normal           <-sum(df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_10_19_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_20_29_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_30_39_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_40_49_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_50_59_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_60_69_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_70_79_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_80_89_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_90_99_normal         <-sum(df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Normal" ),"value"])
df_cumsum$total_100_plus_normal      <-sum(df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Normal" ),"value"])


# Set the y-label for tumor samples
df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Tumor" ),"total_0_9"] /2
df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Tumor" ),"total_10_19"] /2
df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Tumor" ),"total_20_29"] /2
df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Tumor" ),"total_30_39"] /2
df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Tumor" ),"total_40_49"] /2
df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Tumor" ),"total_50_59"] /2
df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Tumor" ),"total_60_69"] /2
df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Tumor" ),"total_70_79"] /2
df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Tumor" ),"total_80_89"] /2
df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Tumor" ),"total_90_99"] /2
df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Tumor" ),"total_100_plus"] /2


# Set the y-label for normal samples
df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Tumor" ),"total_0_9"]+(df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Normal" ),"total_0_9"]-df_cumsum[which(df_cumsum$age_groups=="0-9" & df_cumsum$Tissue.Type=="Tumor" ),"total_0_9"])/2 - 2
df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Tumor" ),"total_10_19"]+(df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Normal" ),"total_10_19"]-df_cumsum[which(df_cumsum$age_groups=="10-19" & df_cumsum$Tissue.Type=="Tumor" ),"total_10_19"])/2 -2
df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Tumor" ),"total_20_29"]+(df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Normal" ),"total_20_29"]-df_cumsum[which(df_cumsum$age_groups=="20-29" & df_cumsum$Tissue.Type=="Tumor" ),"total_20_29"])/2 -2
df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Tumor" ),"total_30_39"]+(df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Normal" ),"total_30_39"]-df_cumsum[which(df_cumsum$age_groups=="30-39" & df_cumsum$Tissue.Type=="Tumor" ),"total_30_39"])/2 +2
df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Tumor" ),"total_40_49"]+(df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Normal" ),"total_40_49"]-df_cumsum[which(df_cumsum$age_groups=="40-49" & df_cumsum$Tissue.Type=="Tumor" ),"total_40_49"])/2 -0.5
df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Tumor" ),"total_50_59"]+(df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Normal" ),"total_50_59"]-df_cumsum[which(df_cumsum$age_groups=="50-59" & df_cumsum$Tissue.Type=="Tumor" ),"total_50_59"])/2 -0.5
df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Tumor" ),"total_60_69"]+(df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Normal" ),"total_60_69"]-df_cumsum[which(df_cumsum$age_groups=="60-69" & df_cumsum$Tissue.Type=="Tumor" ),"total_60_69"])/2 -2
df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Tumor" ),"total_70_79"]+(df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Normal" ),"total_70_79"]-df_cumsum[which(df_cumsum$age_groups=="70-79" & df_cumsum$Tissue.Type=="Tumor" ),"total_70_79"])/2 -2
df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Tumor" ),"total_80_89"]+(df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Normal" ),"total_80_89"]-df_cumsum[which(df_cumsum$age_groups=="80-89" & df_cumsum$Tissue.Type=="Tumor" ),"total_80_89"])/2 -2
df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Tumor" ),"total_90_99"]+(df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Normal" ),"total_90_99"]-df_cumsum[which(df_cumsum$age_groups=="90-99" & df_cumsum$Tissue.Type=="Tumor" ),"total_90_99"])/2 -2
df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Tumor" ),"total_100_plus"]+(df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Normal" ),"total_100_plus"]-df_cumsum[which(df_cumsum$age_groups=="100+" & df_cumsum$Tissue.Type=="Tumor" ),"total_100_plus"])/2 -2





# Create the barplot
p1<-ggplot(data=df_cumsum, aes(x=age_groups, y=value, fill=Tissue.Type)) +  geom_bar(stat="identity")+  geom_text(aes(y=label_ypos, label=value), vjust=1.6,  color="white", size=3.5)+  scale_fill_brewer(palette="Paired")+  theme_minimal()  + theme(legend.position = "bottom",panel.grid = element_blank())  + coord_flip()
##############################################################################################
# bwplot               
png(filename=paste(output_dir,"Plot_Age_group_samples_tissue_type.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p1
dev.off()

##############################################################################################
# https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# Add the count table
df2<-reshape2::melt(table(metadata[,c("demographic.gender", "Tissue.Type")]),by="demographic.gender")

# Sort by dose and supp
df_sorted <- df2

# Calculate the cumulative sum of len for each dose
df_cumsum <- ddply(df_sorted, "value", transform, label_ypos=cumsum(value))

# Add the total male
df_cumsum$total_male <-sum(df_cumsum[which(df_cumsum$demographic.gender=="male"),"value"])
df_cumsum$total_female <-sum(df_cumsum[which(df_cumsum$demographic.gender=="female"),"value"])

# Take the total male and total female
df_cumsum$total_male_tumor      <-sum(df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_female_tumor    <-sum(df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$Tissue.Type=="Tumor" ),"value"])

# Set the y-label for tumor samples
df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$Tissue.Type=="Tumor" ),"total_male_tumor"] /2
df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$Tissue.Type=="Tumor" ),"total_female_tumor"] /2

# Set the y-label for normal samples
df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$Tissue.Type=="Tumor" ),"total_male_tumor"]+(df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$Tissue.Type=="Tumor" ),"total_male"]-df_cumsum[which(df_cumsum$demographic.gender=="male" & df_cumsum$Tissue.Type=="Tumor" ),"total_male_tumor"])/2 + 1
df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$Tissue.Type=="Tumor" ),"total_female_tumor"]+(df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$Tissue.Type=="Tumor" ),"total_female"]-df_cumsum[which(df_cumsum$demographic.gender=="female" & df_cumsum$Tissue.Type=="Tumor" ),"total_female_tumor"])/2 + 1

# Create the barplot
p1<-ggplot(data=df_cumsum, aes(x=demographic.gender, y=value, fill=Tissue.Type)) +  geom_bar(stat="identity")+  geom_text(aes(y=label_ypos, label=value), vjust=1.6,  color="white", size=3.5)+  scale_fill_brewer(palette="Paired")+  theme_minimal()  + theme(legend.position = "bottom",panel.grid = element_blank())  + coord_flip()
##############################################################################################
# bwplot               
png(filename=paste(output_dir,"Plot_demographic_gender_samples_tissue_type.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p1
dev.off()

##############################################################################################
# https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# Add the count table
df3<-reshape2::melt(table(metadata[,c("demographic.race", "Tissue.Type")]),by="demographic.race")

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
df_cumsum$total_white_tumor                            <-sum(df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_Unknown_tumor                          <-sum(df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_not_reported_tumor                     <-sum(df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_black_or_african_american_tumor        <-sum(df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_asian_tumor                            <-sum(df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Tumor" ),"value"])
df_cumsum$total_american_indian_or_alaska_native_tumor <-sum(df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$Tissue.Type=="Tumor" ),"value"])


# Set the y-label for tumor samples
df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"]                            <- df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Tumor" ),"total_white_tumor"] /2
df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"]                          <- df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$Tissue.Type=="Tumor" ),"total_Unknown_tumor"] /2
df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"]                     <- df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$Tissue.Type=="Tumor" ),"total_not_reported_tumor"] /2
df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"]        <- df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$Tissue.Type=="Tumor" ),"total_black_or_african_american"] /2
df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"]                            <- df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Tumor" ),"total_asian"] /2
df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$Tissue.Type=="Tumor" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$Tissue.Type=="Tumor" ),"total_american_indian_or_alaska_native"] /2

# Set the y-label for normal samples
df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Tumor" ),"total_white_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Normal" ),"total_white"]-df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Tumor" ),"total_white_tumor"])/2 
df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$Tissue.Type=="Tumor" ),"total_Unknown_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="white" & df_cumsum$Tissue.Type=="Normal" ),"total_Unknown"]-df_cumsum[which(df_cumsum$demographic.race=="Unknown" & df_cumsum$Tissue.Type=="Tumor" ),"total_Unknown_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <-df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$Tissue.Type=="Tumor" ),"total_not_reported_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$Tissue.Type=="Normal" ),"total_not_reported"]-df_cumsum[which(df_cumsum$demographic.race=="not reported" & df_cumsum$Tissue.Type=="Tumor" ),"total_not_reported_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$Tissue.Type=="Tumor" ),"total_black_or_african_american_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$Tissue.Type=="Normal" ),"total_black_or_african_american"]-df_cumsum[which(df_cumsum$demographic.race=="black or african american" & df_cumsum$Tissue.Type=="Tumor" ),"total_black_or_african_american_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Tumor" ),"total_asian_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Normal" ),"total_asian"]-df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Tumor" ),"total_asian_tumor"])/2
df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$Tissue.Type=="Normal" ),"label_ypos"] <- df_cumsum[which(df_cumsum$demographic.race=="american indian or alaska native" & df_cumsum$Tissue.Type=="Tumor" ),"total_asian_tumor"]+(df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Normal" ),"total_american_indian_or_alaska_native"]-df_cumsum[which(df_cumsum$demographic.race=="asian" & df_cumsum$Tissue.Type=="Tumor" ),"total_american_indian_or_alaska_native_tumor"])/2


# Set zero to NA
df_cumsum$label_ypos[df_cumsum$label_ypos == 0] <- NA
df_cumsum$label[df_cumsum$label == 0] <- NA
df_cumsum$value[df_cumsum$value == 0] <- NA

# Create the barplot
p2<-ggplot(data=df_cumsum, aes(x=demographic.race, y=value, fill=Tissue.Type)) +  geom_bar(stat="identity")+  geom_text(aes(y=label_ypos, label=value), vjust=1.6,  color="white", size=3.5)+  scale_fill_brewer(palette="Paired")+  theme_minimal()  + theme(legend.position = "bottom",panel.grid = element_blank())  + coord_flip()

# bwplot               
png(filename=paste(output_dir,"Plot_demographic_race_samples_tissue_type.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p2
dev.off()

###########################################################################################################################
# Age plot

# bwplot               
png(filename=paste(output_dir,"Variable_completeness.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  ggplot(metadata, aes(x=as.factor(Tissue.Type), y=as.numeric(demographic.age_at_index), fill=Tissue.Type)) +  geom_boxplot(lpha=0.2)  + theme_bw()+  scale_fill_brewer(palette="Paired") + scale_fill_brewer(palette="Paired")+  theme_minimal()  + theme(legend.position = "bottom",panel.grid = element_blank())
dev.off()




