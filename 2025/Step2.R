###Combinging all complete .csv's into one document with treatment columns

####################Start Here#####################



Run1=read.csv("RTNC PMC 2025 run1 calcs full.csv")
#Run2=read.csv("RTNC PMC 2025 run2 calcs full.csv")


trtcode = read.csv("List of samples RTNC 2025.csv") 
str(trtcode)
str(Run1)    


mergeAll=rbind(Run1)
head(mergeAll)
mergeClean=select(mergeAll,Location,plot,depth.in,sample.id,ugC.g.day)


write.csv(mergeClean, "PMC_RTNC_all_calcs_02Dec25.csv",
          row.names = FALSE)


#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
all.data<-read.csv("PMC_RTNC_all_calcs_02Dec25.csv", stringsAsFactors = FALSE) 

num <- all.data$number 

#Calculate mean ugC.g.day per sample 
#Calculates mean of 3 technical replicates, which now share the same sample.id for easy grouping. 
full.means <- all.data %>% 
  group_by(sample.id) %>%
  summarize(mean.resp = mean(ugC.g.day, na.rm = T))

no.NA = all.data[!is.na(all.data$ugC.g.day),]


#Calculate coefficient of variation (CV) 
#CV is standard deviation divided by mean, expressesed as a percent. 
add.cv <- no.NA %>% 
  group_by(sample.id) %>% 
  summarize(sample.cv = sd(ugC.g.day, na.rm = TRUE)/mean(ugC.g.day, na.rm = TRUE)*100)

#merge sample mean and cv data 
mean.cv <- merge(full.means, add.cv) 

#bring in full sample names from the name master. 
sample.id <- read.csv("List of samples RTNC 2025.csv", 
                      stringsAsFactors = FALSE)

#Join the mean and variability dataset with the sample name key 
pmc.var <- full_join(sample.id, mean.cv, by = "sample.id")

#keep only samples that have CV > 0. 
pmc.var <- subset(pmc.var, sample.cv > 0) 

write.csv(pmc.var, "PMC_RTNC_all_calcs_CV_02Dec25.csv")

##now look at the document for sampels that need to be looked into further. 
###example
###if CV > 20, maybe one of the samples is an outlire, once deleted no longer on the list 



