# put together metadata with pmc time file
# need to:
#     pull time series licor data
#     find final 5ish measurements before time gap
#     average them
#     save avg as ppm for pause #
#     match pause # with metadata file


library(dplyr)
library(ggplot2)

getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders




###Run 1




meta = read.csv("CCNRD_PMC_r1.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("CCNRD_Run1.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("Pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$plot=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days=(meta$end-meta$start)/(24*60*60)+1


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "PMC_run1_calcs.csv",
          row.names = FALSE)




####Now Run2




meta = read.csv("CCNRD_PMC_r2.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("CCNRD_Run2.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("Pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$plot=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days=(meta$end-meta$start)/(24*60*60)+1


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "PMC_run2_calcs.csv",
          row.names = FALSE)


####Now run3


meta = read.csv("CCNRD_PMC_r3.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("CCNRD_Run3.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("Pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$plot=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days=(meta$end-meta$start)/(24*60*60)+1


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "PMC_run3_calcs.csv",
          row.names = FALSE)

#####Run 4


meta = read.csv("CCNRD_PMC_r4.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("CCNRD_Run4.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("Pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$plot=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days=(meta$end-meta$start)/(24*60*60)+1


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "PMC_run4_calcs.csv",
          row.names = FALSE)



##### Run 5



meta = read.csv("CCNRD_PMC_r5.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("CCNRD_Run5.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("Pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$plot=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days=(meta$end-meta$start)/(24*60*60)+1


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "PMC_run5_calcs.csv",
          row.names = FALSE)


#### Now Run 6



meta = read.csv("CCNRD_PMC_r6.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("CCNRD_Run6.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("Pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$plot=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days=(meta$end-meta$start)/(24*60*60)+1


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "PMC_run6_calcs.csv",
          row.names = FALSE)

#####Now RTNC data



meta = read.csv("RTNC22_PMC_r1.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("RTNC22_run1.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("Pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$plot=="blank")])
# mean(meta$ppm[which(meta$sample.mass.g==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days=(meta$end-meta$start)/(24*60*60)+1


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "RTNC22_run1_calcs.csv",
          row.names = FALSE)



#merge all calculation docs
Run1=read.csv("PMC_run1_calcs.csv")
Run2=read.csv("PMC_run2_calcs.csv")
Run3=read.csv("PMC_run3_calcs.csv")
Run4=read.csv("PMC_run4_calcs.csv")
Run5=read.csv("PMC_run5_calcs.csv")
Run6=read.csv("PMC_run6_calcs.csv")

RTNC=read.csv("RTNC22_run1_calcs.csv")

mergeAll=rbind(Run1,Run2,Run3,Run4,Run5,Run6)
head(mergeAll)
mergeClean=select(mergeAll,study,sample.date,plot,depth.cm,sample.id,ugC.g.day)


write.csv(mergeAll, "PMC_CCNRD_all_calcs_07Apr23.csv",
          row.names = FALSE)





#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
all.data<-read.csv("PMC_CCNRD_all_calcs_07Apr23_edited.csv", stringsAsFactors = FALSE) 
RTNC.data<-read.csv("RTNC22_run1_calcs.csv", stringsAsFactors = FALSE)

num <- all.data$number 
num2<- RTNC.data

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
sample.id <- read.csv("List of samples CCNRD.csv", 
                      stringsAsFactors = FALSE)

#Join the mean and variability dataset with the sample name key 
pmc.var <- full_join(sample.id, mean.cv, by = "sample.id")

#keep only samples that have CV > 0. 
pmc.var <- subset(pmc.var, sample.cv > 0) 

write.csv(pmc.var, "PMC_CCNRD_all_calcs_CV_07Apr23_v2.csv")

##now look at the document for sampels that need to be looked into further. 
###example
###if CV > 20, maybe one of the samples is an outlire, once deleted no longer on the list 

write.csv(missing, "CCNRD_missing.csv", row.names = FALSE)



###### RTNC data CV
#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
RTNC.data<-read.csv("RTNC22_run1_calcs.csv", stringsAsFactors = FALSE)

num2<- RTNC.data

#Calculate mean ugC.g.day per sample 
#Calculates mean of 3 technical replicates, which now share the same sample.id for easy grouping. 
full.means <- RTNC.data %>% 
  group_by(sample.id) %>% 
  summarize(mean.resp = mean(ugC.g.day, na.rm = T))

no.NA = RTNC.data[!is.na(RNTC.data$ugC.g.day),]



#Calculate coefficient of variation (CV) 
#CV is standard deviation divided by mean, expressesed as a percent. 
add.cv <- RTNC.data %>% 
  group_by(sample.id) %>% 
  summarize(sample.cv = sd(ugC.g.day, na.rm = TRUE)/mean(ugC.g.day, na.rm = TRUE)*100)

#merge sample mean and cv data 
mean.cv <- merge(full.means, add.cv) 

#bring in full sample names from the name master. 
sample.id <- read.csv("List of samples RTNC.csv", 
                      stringsAsFactors = FALSE)

#Join the mean and variability dataset with the sample name key 
pmc.var <- full_join(sample.id, mean.cv, by = "sample.id")

#keep only samples that have CV > 0. 
pmc.var <- subset(pmc.var, sample.cv > 0) 

write.csv(pmc.var, "RTNC22_PMC_all_calcs_CV_07Apr23.csv")





#histogram with all samples 
ggplot(pmc.var, aes(sample.cv))+ 
  geom_histogram(binwidth = 1, na.rm = TRUE) 

#Isolate the samples with >20% CV 
gtr.20.cv <- subset(pmc.var, sample.cv >20) 

#histogram samples >20% CV 
ggplot(gtr.20.cv, aes(sample.cv))+ geom_histogram(binwidth = 5, na.rm = TRUE) 























###Junk###################
Run1=select(study,sample.date,plot,depth.cm,sample.id,ugC.g.day)
Run2=select(study,sample.date,plot,depth.cm,sample.id,ugC.g.day)
Run3=select(study,sample.date,plot,depth.cm,sample.id,ugC.g.day)
merge29 = merge(trtcode, 
                select(Run29, readdate, site, timing, Subplot, ugC.g.day))

#average by sample

library(dplyr)


meta.sub= meta %>%
  group_by(sample.id, subplot,)%>%
  summarize(mean.mgC.gsoil.day = mean(ugC.g.day),
            sd = sd(ugC.g.day),
            CV = (sd/mean.mgC.gsoil.day)*100,
            cnt = n())

meta.sub
#terrible variability

ggplot(meta,
       aes(x=Subplot, y = ugC.g.day))+
  geom_point()

# put together metadata with pmc time file
# need to:
#     pull time series licor data
#     find final 5ish measurements before time gap
#     average them
#     save avg as ppm for pause #
#     match pause # with metadata file

