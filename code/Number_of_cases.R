# Set the trend table
df_trend<-data.frame(read_excel(paste(project_folder,"/tables/dataset-asr-inc-males-and-females-age-0-74-colon.xlsx",sep="")))

# Add id collumn
df_trend$id<-rownames(df_trend)

# Melt table
df_trend<-reshape2::melt(df_trend[,c("id","Year","Crude.rate","Sex")] , id.vars=c("id","Year","Sex"))

# Rename collumns
colnames(df_trend)<-c("id","Year","Sex","variable","Statistics")

# Set the sex
df_trend[which(df_trend$Sex == 1),"Sex"]<-"Male"
df_trend[which(df_trend$Sex == 2),"Sex"]<-"Female"

# Set as factor
df_trend$Sex<-as.factor(df_trend$Sex)

# Plot the overall trend for both sexes.
p1<-ggplot(data=df_trend, aes(x=Year, y=Statistics, group=Sex, color=Sex)) +   geom_line()+  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_bw() + ggtitle("") + theme(legend.position = "bottom",panel.grid = element_blank()) + ylab("crude rate") + xlab("year") 
        

# bwplot               
png(filename=paste(output_dir,"Trned_number_of_cases.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p1
dev.off()
