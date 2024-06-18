###################################
###  NAMING TASK DATA ANALYSIS  ###
###################################


####------ Overview ------####

# 1) Make a DF of acoustic data


# 2) 



#lib
library("stringr") #for str_extract
library("tibble") #for add_row
library("dplyr") #for mutate
library("ggplot2") #for ggplot



#########  PART 1 Making DF of acoustics  ##########

### Loop through all files of these basic data ----------------------------

# --- Dealing BY PAIR ---#####

# import data
#desktop
# setwd("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/")
#laptop
currDir <- setwd("/Users/yuka/Desktop/SWF/namingdata/pair1")

# list
folderList <- c("Pre_S1", "Pre_S2", "Post_S1", "Post_S2")

fileList <- c()
for (n in 1:4){
  currfold <- folderList[n]
  folderpath <- paste0(currDir, "/", currfold)
  tempfileList <- list.files(folderpath, pattern = "means")
  fileList <- append(fileList,tempfileList)
}
filenum <- length(fileList)


# empty DF
basicinfoFULL<-data.frame(filename = character(0),
                          pair = numeric(0),
                          subject = character(0),
                          prepost = character(0),
                          surname = character(0),
                          jobtitle = character(0),
                          itemID = character(0),
                          duration = numeric(0),
                          f0mean = character(0),
                          f0range = numeric(0)
)
# appending data
for (k in 1:filenum){
  # prep to append
  datName <- str_replace_all(fileList[k], "_Naming.means", "")
  pair <- 1
  subject <- str_extract(datName, pattern = "S[:digit:]*")
  prepost <- sub("_.*", "", datName)
  split_string <- strsplit(datName, "_")[[1]]
  temp <- split_string[4]
  surname <- sub("([a-zA-Z]*[0-9]).*", "\\1", temp)
  jobtitle <- sub("^[^0-9]*[0-9]", "", temp)
  itemID <- split_string[3]
  
  # main ac data
  currcond <- paste0(prepost, "_", subject)
  setwd(paste0(currDir,"/",currcond))
  basicDf <- read.delim(fileList[k], header = TRUE)
  dur <- basicDf[1,8]
  f0mean <- basicDf[1,5]
  f0mean <- as.character(f0mean)
  f0range <- basicDf[1,2] - basicDf[1,3]
  
  # append data
  basicinfoFULL<- basicinfoFULL %>% add_row(filename = datName, pair = pair, subject = subject, 
                                            prepost = prepost, surname = surname, jobtitle = jobtitle, 
                                            itemID = itemID, duration = dur, f0mean = f0mean, f0range = f0range)
  
}


# If need to write out:
# set the output path
currDir
pathOut <- paste0(currDir,'/NamingbasicAcoutsitcs.csv')
# save the output
write.table(basicinfoFULL, pathOut, sep = ",", row.names = F)



###----------- matching behavioral data -------------

#for now:
mergedDf<-basicinfoFULL

###-----------descriptive stats-------------


## normalization -----------      
#get z score for each data, for columns: duration, f0mean, f0range

#first, some data do not have acoustic data, so make a new df only with those data that has acoustic info.
mergedDf<- subset(mergedDf, f0range!=0)

# norming (for each participant!!)
#prep an empty dataframe
colNum <- ncol(mergedDf)
listColNames <- colnames(mergedDf)
normDf <- data.frame(matrix(ncol=colNum,nrow=0, dimnames=list(NULL, listColNames)))

mergedDf$f0mean <- as.numeric(mergedDf$f0mean)

#loop for each subject
for (k in 1:2) {
  #subset the whole Df by the curr subjID
  tmpnormDf <- subset(mergedDf, mergedDf$subject == paste0("S",k))
  
  #mutate
  tmpnormDf<-tmpnormDf %>% mutate(zDuration = (duration - mean(duration))/sd(duration))
  tmpnormDf<-tmpnormDf %>% mutate(zF0mean = (f0mean - mean(f0mean))/sd(f0mean))
  tmpnormDf<-tmpnormDf %>% mutate(zF0range = (f0range - mean(f0range))/sd(f0range))
  
  #append to normDf
  normDf <- rbind(normDf,tmpnormDf)
  
}

normDf$tone1stSyllable <- str_extract(normDf$surname, "\\d")
normDf$tone2ndSyllable <- str_extract(normDf$jobtitle, "\\d")
normDf$sandhi <- ifelse(normDf$tone2ndSyllable == "3" & normDf$tone1stSyllable == "3", "yes", "no")
normDf$tonecond <- ifelse(normDf$tone1stSyllable %in% c("1", "2", "4"), normDf$tone1stSyllable,
                                           ifelse(normDf$tone1stSyllable == "3" & normDf$sandhi == "yes", "3 sandhi", "3 nonsandhi"))



##DURATION---------------

ggplot(normDf, aes(x = interaction(subject, prepost), y = zDuration, fill = prepost)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test Duration",
                          x = "participant",
                          y = "normed duration") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()

ggplot(normDf, aes(x = interaction(subject, prepost), y = zDuration, fill = tone1stSyllable)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test Duration",
                        x = "participant",
                        y = "normed duration") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()


ggplot(normDf, aes(x = interaction(subject, prepost), y = zDuration, fill = tonecond)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test Duration",
                        x = "participant",
                        y = "normed duration") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()



##F0MEAN---------------


ggplot(normDf, aes(x = interaction(subject, prepost), y = zF0mean, fill = prepost)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test f0mean",
                        x = "participant",
                        y = "normed f0mean") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()

ggplot(normDf, aes(x = interaction(subject, prepost), y = zF0mean, fill = tone1stSyllable)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test f0mean",
                        x = "participant",
                        y = "normed duration") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()


ggplot(normDf, aes(x = interaction(subject, prepost), y = zF0mean, fill = tonecond)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test f0mean",
                        x = "participant",
                        y = "normed duration") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()




##F0RANGE---------------

ggplot(normDf, aes(x = interaction(subject, prepost), y = zF0range, fill = prepost)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test f0range",
                        x = "participant",
                        y = "normed f0range") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()

ggplot(normDf, aes(x = interaction(subject, prepost), y = zF0range, fill = tone1stSyllable)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test f0mean",
                        x = "participant",
                        y = "normed duration") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()


ggplot(normDf, aes(x = interaction(subject, prepost), y = zF0range, fill = tonecond)) +
  geom_boxplot() + labs(title = "Pre-test and Post-test f0mean",
                        x = "participant",
                        y = "normed duration") +  scale_x_discrete(labels = c("Person 1\nPre-test", "Person 1\nPost-test", 
                                                                              "Person 2\nPre-test", "Person 2\nPost-test")) + theme_minimal()







