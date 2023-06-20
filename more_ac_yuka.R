#########################################
###  Yuka acoustic data exploration   ###
#########################################

#### notes for myself not looking at this for awhile ###----------
#make sure you are running the parts that is good for your own environment now -- laptop or lab desktop
#you can start from " # WORKING POINT " if just working on acoustics


# ----------------------- Table of Contents ---------------------######
# 1) Make an acoustic dataframe
#      - Just seeing some examples
#      - Loop through to make 1 dataframe
# == "WORKING POINT" ==
# 2) Make a merged dataframe (accuracy and acoustic data)
#      - read in acoustic dataframe made in step 1
#      - read in accuracy dataframe
#      - merge these
# 3) Accuracy data
#      - visualize
#      - glmer
# 4) Acoustic 
#      - Normalization
#      - General trend visualization (ignore pairs)
#         - duration
#         - f0 mean
#         - f0 range
#      - Zoomed in trend visualization
#         - duration by pair
#         - duration by pair & speaker
#         - f0 mean by pair
#         - f0 mean by pair & speaker
#         - f0 range by pair
#         - f0 range by pair & speaker
# 5) Creakiness (Voicesauce)
#      - pair 1 
#      - pair 5
# 6) Acoustic knowledge notes
# 7) Trash

library(tidyverse)


#Just seeing some examples-------------------------------------------------------

# to get all diff data files output from ProsodyPro in 1 list of dataframes (seeing only 1 data)
exfiles<-list.files(path = "/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/pair1/",
           pattern = "P1_lu2zhuren_B4_T3.*", 
           full.names = TRUE)
#exfiletypes<-sub(exfiles, pattern =)
exdat <- lapply(exfiles, function(file) {
  tryCatch(
    acdf <- read.delim(file, header = TRUE),
    error = function(e) {
      message(paste("Error reading", file, ": skipping file."))
      return(NULL)
    }
  )
})

#if using "read.table,".label, .PitchTier, .pulse, .semitonef0 were skipped. also textgrid.
#Nulls are: list#... 8,15,16,19 21
#label--- not needed
#PitchTier---- more for reading as a object in Praat
#pulse----same as above

###only extract needed info for ac stats later
#duration --- exdat[[9]]
basicinfo <-  exdat[[9]]
dur <- basicinfo[1,8]

#f0 --- exdat[[9]]
f0mean <- basicinfo[1,5]
f0range <- basicinfo[1,2] - basicinfo[1,3]

#norm time f0 --> this is more for contour and not the acoustic itself?

#creakiness
# I am thinking it is just Pratt to use Voicesauce or Praat, cus not much infoavaliable




### Loop through all files of these basic data ----------------------------

#read in all files
#desktop
setwd("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/")
#laptop
setwd("/Users/yuka/Desktop/SWF/data")

currDir <- getwd()
fileList <- c()
for (n in 1:10){
  folderpath <- paste0(currDir, "/pair", n)
  tempfileList <- list.files(folderpath, pattern = "means")
  fileList <- append(fileList,tempfileList)
}
filenum <- length(fileList)

#process each data 
basicinfoFULL<-data.frame(filename = character(0),
                          pair = character(0),
                          block = character(0),
                          tone = character(0),
                          itemID = character(0),
                          duration = numeric(0),
                          f0mean = numeric(0),
                          f0range = numeric(0)
                          )
for (k in 1:filenum){
        # prep to append
        datName <- str_replace_all(fileList[k], "_Matching.means", "")
        pair <- str_extract(datName, pattern = "P[:digit:]*")
        block <- str_extract(datName, pattern = "B[:digit:]*")
        tone <- str_extract(datName, pattern = "lu[:digit:]*")
        itemID <- str_extract(datName, pattern = "T[:digit:]*")
        
        # main data
        pairNum <- str_replace_all(pair, "P", "")
        setwd(paste0(currDir,"/pair", pairNum))#note on 6/19 : I changed to /pair from /Pair. to make it work for laptop. if needed to replace again.
        basicDf <- read.delim(fileList[k], header = TRUE)
        dur <- basicDf[1,8]
        f0mean <- basicDf[1,5]
        f0range <- basicDf[1,2] - basicDf[1,3]
        
        # append data
        basicinfoFULL<- basicinfoFULL %>% add_row(filename = datName, pair = pair, block = block, 
                                            tone = tone, itemID = itemID, duration = dur, f0mean = f0mean, f0range = f0range)
       
      }
# If need to write out:
      # set the output path
      pathOut <- paste0(currDir,'/basicAC.csv')
      # save the output
      write.table(basicinfoFULL, pathOut, sep = ",", row.names = F)

# Check what's in this basicinfoFULL
      table(basicinfoFULL$pair)
        #FULL DATA (has all 96 acoustic data)
          #Pair5,6,7
          #pair3 ...only has 95, but this is how many collected so full data.
        #missing one (95 data)
          #Pair10, 4, 8, 9
        #missing more
          #Pair 2
        #weird. 
          #Pair1 ...has 99. (prob duplicated)
      
##### WORKING POINT #####================================================================
      
      
    #Acoustic data from Prosody Pro (Run only 1 from below)
      # LAPTOP: 
        basicinfoFULL <- read.delim("/Users/yuka/Desktop/SWF/basicAC.csv", sep = ",")
      # DESKTOP:
        basicinfoFULL <- read.delim("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/basicAC.csv", sep = ",")
        
    #Get accuracy data (Run only 1 from below)
      #LAPTOP
      acDf <- read.delim("/Users/yuka/Desktop/SWF/processed_matching_data.csv", sep = ",") 
      #DESKTOP
      acDf <- read.csv("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/processed_matching_data.csv") 

    # need to match by: pair, block, trial (tone differs as a result).
      #change P_01 to P1
      acDf$pair <- gsub( "P_0(\\d+)", "P\\1", acDf$pair)
      acDf$pair <- gsub( "P_10", "P10", acDf$pair)
      #change Block 4 to B4
      acDf$block <- gsub("Block\\s+(\\d+)", "B\\1", acDf$block)
      #the original file is missing one data (Excel only has 959.)
      
      #make condition column here.
      rowNumAcdf<-nrow(acDf)
      acDf$condition<-""
      for (i in 1:rowNumAcdf){
        tg <- acDf[i,6]
        if (tg == "LisRSpkR"|tg == "LisLSpkR"){
          pngName<-acDf[i,5]
          toneTemp<-substr(pngName, start = 1, stop = 3)
          if (toneTemp == "lu2"|toneTemp == "lu4"){
            cond = toneTemp
          }
          else if (pngName == "lu3zhuren.png"|pngName == "lu3jingguan.png"){
            cond = "Tone 3 Sandhi"
          }
          else {
            cond = "Tone 3 noSandhi"
          }
        }
        
        if (tg == "LisRSpkL"|tg == "LisLSpkL"){
          pngName<-acDf[i,4]
          toneTemp<-substr(pngName, start = 1, stop = 3)
          if (toneTemp == "lu2"|toneTemp == "lu4"){
            cond = toneTemp
          }
          else if (pngName == "lu3zhuren.png"|pngName == "lu3jingguan.png"){
            cond = "Tone 3 Sandhi"
          }
          else {
            cond = "Tone 3 noSandhi"
          }
        }
        
        acDf[i,"condition"]<- cond
        }
        
      #replace lu2 with Tone 2 , and lu4 with Tone 4
      library(stringr)
      acDf$condition <- str_replace_all(acDf$condition, 'lu2', 'Tone 2')
      acDf$condition <- str_replace_all(acDf$condition, 'lu4', 'Tone 4')
      
            
      #-----no need to run these usually (START)-------------------------------------------#
                #let's investigate which one:
                    nrow(acDf[acDf$pair=="P1",])
                    nrow(acDf[acDf$pair=="P2",]) 
                    nrow(acDf[acDf$pair=="P3",]) #this is missing 1 data.
                    nrow(acDf[acDf$pair=="P4",])
                    nrow(acDf[acDf$pair=="P5",])
                    nrow(acDf[acDf$pair=="P6",])
                    nrow(acDf[acDf$pair=="P7",])
                    nrow(acDf[acDf$pair=="P8",])
                    nrow(acDf[acDf$pair=="P9",])
                    nrow(acDf[acDf$pair=="P10",])
                # which P3 data missing?
                    nrow(acDf[acDf$pair=="P3"&acDf$block=="B1",])#24
                    nrow(acDf[acDf$pair=="P3"&acDf$block=="B2",])#24
                    nrow(acDf[acDf$pair=="P3"&acDf$block=="B3",])#24
                    nrow(acDf[acDf$pair=="P3"&acDf$block=="B4",])#this is missing 1 data
                # missing data is: P3, B4, 24th item
                # Is this missing from basicInfoDf too?
                    basicinfoFULL[basicinfoFULL$pair=="P3"&basicinfoFULL$block=="B4"&basicinfoFULL$trial=="T24",]
                    #yup, definitely missing this in basic info full as well.
                    #=> checked the original. the final trial was not shown to the participant so they moved on to the next task without doing it.
                    #this missing one was supposed to be Lu2 data. (bc P3B4 has 7 Lu2, 8Lu3, 8Lu4)
      #-----no need to run these usually (END)-------------------------------------------#
     
      #merge dfs (FYI: acDf is accurate.)
      basicinfoFULL$trial <- basicinfoFULL$itemID
      library(dplyr)
      #  mergedDf <- left_join(basicinfoFULL, acDf, by = c("pair" = "pair", "block"="block", "trial"="trial"))
      mergedDf <- right_join(basicinfoFULL, acDf, by = c("pair" = "pair", "block"="block", "trial"="trial"))
      #  FmergedDf <- full_join(basicinfoFULL, acDf, by = c("pair" = "pair", "block"="block", "trial"="trial"))
      #nrow(basicinfofull) = 933, nrow(acDf) = 959, nrow(mergedDf) = 933, nrow(FmergedDf) = 962,  nrow(mergedDf) = 962
      # desgin wise, there should be 960 bc each of 3 tones is tested 32 times, and there are 10 pairs.
      #I checked if accuracy info is preserved even basicinfoFull does not have the data.
        #looking at: tail(mergedDf), prob they are remained as planned.
        #BUT, nrow(mergedDf[mergedDf$Accuracy==NA,]) is somehow ALL data...
        #BUT, mergedDf[mergedDf$Accuracy=="Correct",] is 911. nrow(mergedDf[mergedDf$Accuracy=="Incorrect",]) is 48, which is the all data.
      #Just a note that actually this works too:
        # mergedDf <- right_join(basicinfoFULL, acDf, by = c("pair", "block", "trial"))

      
  # Deleting some data that I realized later that are doubled
      nrow(mergedDf)
      mergedDf <- mergedDf %>% distinct()
      nrow(mergedDf) #checking
      #after this, 959.  bc we are missing P3B4T24.
  
  # make sure to have all important column values as factors     
      mergedDf$pair <- as.factor(mergedDf$pair)
      mergedDf$block <- as.factor(mergedDf$block)
      mergedDf$tone <- as.factor(mergedDf$tone)
      mergedDf$itemID <- as.factor(mergedDf$itemID)
      
      #-----no need to run these anymore (START)-------------------------------------------#
              #Making "Condition" Column
                  # add column with this information in the mergedDf
                  mergedDf<-mergedDf %>%
                    mutate(condition = case_when(
                      grepl("lu2", filename)  ~ "Tone 2",
                      grepl("lu3jiangjun", filename)  ~ "Tone 3 noSandhi",
                      grepl("lu3zhentan", filename)  ~ "Tone 3 noSandhi",
                      grepl("lu3zhuren", filename)  ~ "Tone 3 Sandhi",
                      grepl("lu3jingguan", filename)  ~ "Tone 3 Sandhi",
                      grepl("lu4", filename)  ~ "Tone 4"
                    ))
                  
                  # "NA" is hard to treat with, so replacing them as "Unknown" 
                  library(tidyr)
                  mergedDf$condition<-mergedDf$condition %>% replace_na("Unknown") 
                  #there are 29 NAs in mergedDf (26 are from the difference betw acDf and basicinfoFULL. but the other 3...?)
                  
                  #see which are the Unkown ones.    
                  View(mergedDf[mergedDf$condition=="Unknown",])
                  
      #-----no need to run these anymore (START)-------------------------------------------#
      
  #checking inside
      table(mergedDf$Accuracy,mergedDf$condition)
  

### Behavioral (Accuracy) -------  
      
      ##overall descriptive stats---
      #this shows everything at the same time
      table(mergedDf$Accuracy,mergedDf$condition,mergedDf$pair)
      
      #----no need to run (START)-----------
      
              #stats - T2
              mergedDfT2<- mergedDf[mergedDf$condition == "Tone 2",]
              table(mergedDfT2$pair, mergedDfT2$block)
              table(mergedDfT2$Accuracy)
               
              #stats - T3 noSandhi
              mergedDfT3ns<- FmergedDf[FmergedDf$condition == "Tone 3 noSandhi",]
              table(mergedDfT3ns$Accuracy)
                #across pairs, T3nS correct = 156, T2 incorrect = 4
              
              #stats - T3 Sandhi
              mergedDfT3s<- FmergedDf[FmergedDf$condition == "Tone 3 Sandhi",]
              table(mergedDfT3s$Accuracy)
              #across pairs, T3nS correct = 135, T2 incorrect = 24
              
              #stats - T4
              mergedDfT4<- FmergedDf[FmergedDf$condition == "Tone 4",]
              table(mergedDfT4$Accuracy)
              #across pairs, T3nS correct = 319, T2 incorrect = 1
      
              ##kind of want to consider by pair...
              
              #T2
              table(mergedDfT2$Accuracy, mergedDfT2$pair)
              # from 1, 10, 2 -- 9,
                # 2, 2, 1, 4, 2, 1, 0, 1, 1, 4
              
              #T3nS
              table(mergedDfT3ns$Accuracy, mergedDfT3ns$pair)
                # P1 - 1 Incorrect
                # P4 - 2 Incorrect
                # P9 - 1 Incorrect
                # all other pairs - 0 Incorrect
              
              #T3S
              table(mergedDfT3s$Accuracy, mergedDfT3s$pair)
                # from 1, 10, 2 -- 9,
                    # 3,2,2,5,1,2,1,2,1,5
              
              #T4
              table(mergedDfT4$Accuracy, mergedDfT4$pair)
                # 1 error from Pair 10
              
      #----not need to run (END)-----------
      
      
      ### Inferential for accuracy ------
              
      ## Contrast coding
      library(car)
      library(carData)
      # recoding "Correct" and "Incorrect" as "1" and "0"
        dataToFit <- mergedDf
        dataToFit$Accuracy<-ifelse(dataToFit$Accuracy=="Correct", 1,0)
              
      dataToFit$condition <- as.factor(dataToFit$condition)
      #seeing the currect state (tone 2 as a base)
      contrasts(dataToFit$condition)
      #sum contrast
      contrasts(dataToFit$condition) <- contr.Sum(levels(dataToFit$condition))
      #note: if wanted to change the baseline:
      #   levels(dataToFit$condition) <- factor(c( "Tone 2","Tone 3 Sandhi","Tone 3 noSandhi", "Tone 4"))
      
      ## glmer (for binary data)
      # Glmer
      library(lme4)
      glmer1<- glmer(Accuracy ~ condition + (1|pair), data = dataToFit, family=binomial)
      summary(glmer1)
      
      
# Acoustics   -----------------------------------------------------------
  
  ## normalization -----------      
      #get z score for each data, for columns: duration, f0mean, f0range
      
      #first, some data do not have acoustic data, so make a new df only with those data that has acoustic info.
      #i don't like "NA" so replace them with words
      library(tidyr)
      mergedDf$duration<-mergedDf$duration %>% replace_na(9999) 
      # delete 29 items that do not have acoustic data to avoid errors in later mutation
      mergedDf<- subset(mergedDf, duration!=9999) 
      # norming
      normDf<-normDf %>% mutate(zDuration = (duration - mean(duration))/sd(duration))
      normDf<-normDf %>% mutate(zF0mean = (f0mean - mean(f0mean))/sd(f0mean))
      normDf<-normDf %>% mutate(zF0range = (f0range - mean(f0range))/sd(f0range))
                      
      
  ## GENERAL TREND, not by pairs-----------
      
      #very general tone diff (across pairs, blocks, and individuals)
      
      library(ggplot2)
      
      #DURATION---------------
        #violin
        ggplot(normDf, aes(y=zDuration, x=condition)) +
          geom_violin(aes(fill = condition)) +theme_bw()+ 
        stat_summary(fun = "mean",
                     geom = "point",
                     aes(color = "Mean")) +
        stat_summary(fun = "median",
                     geom = "point",
                     aes(color = "Median")) +
        scale_colour_manual(values = c("red", "blue"), name = "")
        #boxplot
        ggplot(normDf, aes(y=zDuration, x=condition)) +
          geom_boxplot(aes(fill = condition)) +theme_bw()
        # +scale_fill_brewer(palette="BuPu")
      
        #(not good stats but just to see the trend...)
        anovaGenDur<-aov(zDuration ~ condition, data = normDf)
        summary(anovaGenDur)
        TukeyHSD(anovaGenDur)
        #the only difference observed is T3nS and T2 
      
      #f0 MEAN----------------
        #violin
        ggplot(normDf, aes(y=zF0mean, x=condition)) +
          geom_violin(aes(fill = condition)) +theme_bw()+ 
          stat_summary(fun = "mean",
                       geom = "point",
                       aes(color = "Mean")) +
          stat_summary(fun = "median",
                       geom = "point",
                       aes(color = "Median")) +
          scale_colour_manual(values = c("red", "blue"), name = "")
        #boxplot
        ggplot(normDf, aes(y=zF0mean, x=condition)) +
          geom_boxplot(aes(fill = condition)) +theme_bw()
        # +scale_fill_brewer(palette="BuPu")
        
        
        #(not good stats but just to see the trend...)
        anovaGenF0M<-aov(zF0mean ~ condition, data = normDf)
        summary(anovaGenF0M)
        TukeyHSD(anovaGenF0M)
        #SIG: T3nS - T2, T3S - T2, T4 - T2, T4 - T3nS, T4 - T3S
        #nonSIG: T3nS - T3S
        # differentiation between sandhi conditions within T3
        
      #f0 RANGE---------------- 
        #(each data point = range of Hz for the tone.)
        #violin
        ggplot(normDf, aes(y=zF0range, x=condition)) +
          geom_violin(aes(fill = condition)) +theme_bw()+ 
          stat_summary(fun = "mean",
                       geom = "point",
                       aes(color = "Mean")) +
          stat_summary(fun = "median",
                       geom = "point",
                       aes(color = "Median")) +
          scale_colour_manual(values = c("red", "blue"), name = "")
        #boxplot
        ggplot(normDf, aes(y=zF0range, x=condition)) +
          geom_boxplot(aes(fill = condition)) +theme_bw()
        # +scale_fill_brewer(palette="BuPu")
            #T4 has higher mean of f0range than the other 3 groups. = T4 has the widest range of f0
        
        
        #(not good stats but just to see the trend...)
        anovaGenF0R<-aov(zF0range ~ condition, data = normDf)
        summary(anovaGenF0R)
        TukeyHSD(anovaGenF0R)   
          #No difference among T2, T3, T3sandhi (diff only found with T4)
      
# ZOOMED IN TREND, by speakers/pairs-----------
   
  ##comparing duration of [lU2, lU4, lU3sandhi, lu3noSandhi]

  # == Duration by pair, by 4 tone conds ======
 
       #violin (good!)
       ggplot(normDf, aes(fill=condition, y=zDuration, x=pair)) +
         geom_violin(trim=FALSE) +
         geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.9)) +
         scale_fill_brewer(palette="Set2")+
         theme_minimal()
       
       #box (my favorite)
       ggplot(normDf, aes(fill=condition, y=zDuration, x=pair)) +
         geom_boxplot() + 
         geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
         scale_fill_brewer(palette="Set2")+
         theme_minimal()
         
       
  # == Duration by pair & speaker, by 4 tone conds ======
       # (following anovas are not good stats, but just to see the trend)
 
    # Big visualization by pair & person
       normDf$Participant <- as.factor(normDf$Participant)
       ggplot(normDf, aes(y = zDuration, x = Participant, fill = condition)) +
         geom_boxplot() +
         facet_grid(. ~ pair) +
         geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
         scale_fill_brewer(palette="YlGnBu")+
         theme_bw()  
       
    
     #pair1
      P1data<- normDf[normDf$pair == "P1",]
      P1data$condition <- as.factor(P1data$condition)
      P1data$zDuration<-  as.numeric(P1data$zDuration)
      
      anova1<-aov(zDuration ~ condition, data = P1data)
      summary(anova1)
      TukeyHSD(anova1)
        # RESULTS
          # T3nosandhi - T2 : ***
          # Tone 3 Sandhi-Tone 2: ***
          # T4 - T3nosandhi: ***
          # T4 - T3sandhi: ***
      aovdur1<-aov(zDuration ~ condition*Participant, data = P1data)
      summary(aovdur1)
      TukeyHSD(aovdur1)
      
      
      #pair2
      P2data<- normDf[normDf$pair == "P2",]
      P2data$condition <- as.factor(P2data$condition)
      P2data$zDuration<-  as.numeric(P2data$zDuration)
      
      anova2<-aov(zDuration ~ condition, data = P2data)
      summary(anova2)
      TukeyHSD(anova2)
      # RESULTS
      # T3 Sandhi-Tone 2: *
      # T3 Sandhi-T3 noSandhi: ***
      # T4 - T3sandhi: ***
      aovdur2<-aov(zDuration ~ condition*Participant, data = P2data)
      summary(aovdur2)
      TukeyHSD(aovdur2)
      
      #pair3
      P3data<- normDf[normDf$pair == "P3",]
      P3data$condition <- as.factor(P3data$condition)
      P3data$zDuration<-  as.numeric(P3data$zDuration)
      
      anova3<-aov(zDuration ~ condition, data = P3data)
      summary(anova3)
      TukeyHSD(anova3)
      # RESULTS
      # T4 - T3 noSandhi: *
      # T4 - T3sandhi: *
      aovdur3<-aov(zDuration ~ condition*Participant, data = P3data)
      summary(aovdur3)
      TukeyHSD(aovdur3)
      
      #pair4
      P4data<- normDf[normDf$pair == "P4",]
      P4data$condition <- as.factor(P4data$condition)
      P4data$zDuration<-  as.numeric(P4data$zDuration)
      
      anova4<-aov(zDuration ~ condition, data = P4data)
      summary(anova4)
      TukeyHSD(anova4)
      # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
      # T3 Sandhi-T2: ***
      # T3 Sandhi - T3 noSandhi: **
      # T4 - T3sandhi: ***
      aovdur4<-aov(zDuration ~ condition*Participant, data = P4data)
      summary(aovdur4)
      TukeyHSD(aovdur4)
      
      #pair5
      P5data<- normDf[normDf$pair == "P5",]
      P5data$condition <- as.factor(P5data$condition)
      P5data$zDuration<-  as.numeric(P5data$zDuration)
      
      anova5<-aov(zDuration ~ condition, data = P5data)
      summary(anova5)
      TukeyHSD(anova5)
      # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
      # T3 noSandhi-T2: *
      # T4 - T3 noSandhi: ***
      # T4 - T3sandhi: **
      aovdur5<-aov(zDuration ~ condition*Participant, data = P5data)
      summary(aovdur5)
      TukeyHSD(aovdur5)
      
      
      #pair6
      P6data<- normDf[normDf$pair == "P6",]
      P6data$condition <- as.factor(P6data$condition)
      P6data$zDuration<-  as.numeric(P6data$zDuration)
      
      anova6<-aov(zDuration ~ condition, data = P6data)
      summary(anova6)
      TukeyHSD(anova6)
      # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
      # T3 Sandhi-T2: ***
      # T4 - T2: **
      # T3 Sandhi - T3 noSandhi: **
      # T4 - T3 noSandhi: *
      # T4 - T3 sandhi: ***
      
      aovdur6<-aov(zDuration ~ condition*Participant, data = P6data)
      summary(aovdur6)
      TukeyHSD(aovdur6)
     
      #pair7
      P7data<- normDf[normDf$pair == "P7",]
      P7data$condition <- as.factor(P7data$condition)
      P7data$zDuration<-  as.numeric(P7data$zDuration)
      
      anova7<-aov(zDuration ~ condition, data = P7data)
      summary(anova7)
      TukeyHSD(anova7)
      # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
      # T3 Sandhi- T2: **
      # T4 - T3 sandhi: *
      
      aovdur7<-aov(zDuration ~ condition*Participant, data = P7data)
      summary(aovdur7)
      TukeyHSD(aovdur7)
      
      #pair8
      P8data<- normDf[normDf$pair == "P8",]
      P8data$condition <- as.factor(P8data$condition)
      P8data$zDuration<-  as.numeric(P8data$zDuration)
      
      anova8<-aov(zDuration ~ condition, data = P8data)
      summary(anova8)
      TukeyHSD(anova8)
      # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
      # Tone 3 Sandhi-Tone 2: **
      # Tone 4-Tone 2: **
      # Tone 4-Tone 3 noSandhi: ** (0.0010892)
      # Tone 4-Tone 3 Sandhi: ***
   
      aovdur8<-aov(zDuration ~ condition*Participant, data = P8data)
      summary(aovdur8)
      TukeyHSD(aovdur8)
      
      
      #pair9
      P9data<- normDf[normDf$pair == "P9",]
      P9data$condition <- as.factor(P9data$condition)
      P9data$zDuration<-  as.numeric(P9data$zDuration)
      
      anova9<-aov(zDuration ~ condition, data = P9data)
      summary(anova9)
      TukeyHSD(anova9)
      # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
      # Tone 4-Tone 3 noSandhi: *
      # Tone 4-Tone 3 Sandhi: **
      
      aovdur9<-aov(zDuration ~ condition*Participant, data = P9data)
      summary(aovdur9)
      TukeyHSD(aovdur9)
      
      #pair10
      P10data<- normDf[normDf$pair == "P10",]
      P10data$condition <- as.factor(P10data$condition)
      P10data$zDuration<-  as.numeric(P10data$zDuration)
      
      anova10<-aov(zDuration ~ condition, data = P10data)
      summary(anova10)
      TukeyHSD(anova10)
      # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
      # Tone 4-Tone 2: ***
      # Tone 3 Sandhi-Tone 3 noSandhi: ** (0.0033333)
      # Tone 4-Tone 3 Sandhi: ***
      
      aovdur10<-aov(zDuration ~ condition*Participant, data = P10data)
      summary(aovdur10)
      TukeyHSD(aovdur10)
      
      

  # == F0 MEAN by pair, by 4 tones ====
  
     #violin (good!)
      ggplot(normDf, aes(fill=condition, y=zF0mean, x=pair)) +
        geom_violin(trim=FALSE) +
        geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.9)) +
        scale_fill_brewer(palette="Set1")+
        theme_minimal()
      
      #box (my favorite)
      ggplot(normDf, aes(fill=condition, y=zF0mean, x=pair)) +
        geom_boxplot() + 
        geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette="Set1")+
        theme_minimal()
      
      normDf$Participant <- as.factor(normDf$Participant)
      
      
  # == F0 MEAN by Pair & Speaker, by 4 tones ====   
      #Visualization by pair & person (f0mean)
      normDf$Participant <- as.factor(normDf$Participant)
      ggplot(normDf, aes(y = zF0mean, x = Participant, fill = condition)) +
        geom_boxplot() +
        facet_grid(. ~ pair) +
        geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette="RdPu")+
        theme_bw()
  
      # stats
    
      #pair1
       aovf0mean1<-aov(zF0mean ~ condition*Participant, data = P1data)
      summary(aovf0mean1)
      TukeyHSD(aovf0mean1)
      
      #pair2
      aovf0mean2<-aov(zF0mean ~ condition*Participant, data = P2data)
      summary(aovf0mean2)
      TukeyHSD(aovf0mean2)
      
      
      #pair3 (this is completely dif betw individuals--)
      aovf0mean3<-aov(zF0mean ~ condition*Participant, data = P3data)
      summary(aovf0mean3)
      TukeyHSD(aovf0mean3)
    
      #pair4
      aovf0mean4<-aov(zF0mean ~ condition*Participant, data = P4data)
      summary(aovf0mean4)
      TukeyHSD(aovf0mean4)
      
      #pair5
      aovf0mean5<-aov(zF0mean ~ condition*Participant, data = P5data)
      summary(aovf0mean5)
      TukeyHSD(aovf0mean5)
      
      #pair6
      aovf0mean6<-aov(zF0mean ~ condition*Participant, data = P6data)
      summary(aovf0mean6)
      TukeyHSD(aovf0mean6)
      
      #pair7
      aovf0mean7<-aov(zF0mean ~ condition*Participant, data = P7data)
      summary(aovf0mean7)
      TukeyHSD(aovf0mean7)
      
      #pair8
      aovf0mean8<-aov(zF0mean ~ condition*Participant, data = P8data)
      summary(aovf0mean8)
      TukeyHSD(aovf0mean8)
      
      #pair9
      aovf0mean9<-aov(zF0mean ~ condition*Participant, data = P9data)
      summary(aovf0mean9)
      TukeyHSD(aovf0mean9)
      
      #pair10
      aovf0mean10<-aov(zF0mean ~ condition*Participant, data = P10data)
      summary(aovf0mean10)
      TukeyHSD(aovf0mean10)
    
 

  # == F0 RANGE by pair, by 4 tones ====
  
    #violin (updated for speaker-wise)
    ggplot(normDf, aes(fill=condition, y=zF0range, x= Participant)) +
      geom_violin(trim=FALSE) +
      facet_grid(. ~ pair) +
      geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.9)) +
      scale_fill_brewer(palette="Set1")+
      theme_bw()
    
    #box 
    ggplot(normDf, aes(fill=condition, y=zF0range, x=pair)) +
      geom_boxplot() + 
      geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
      scale_fill_brewer(palette="Set1")+
      theme_minimal()
  
  # == F0 RANGE by Pair & Speaker, by 4 tones ====
  
     normDf$Participant <- as.factor(normDf$Participant)
      ggplot(normDf, aes(y = zF0range, x = Participant, fill = condition)) +
        geom_boxplot() +
        facet_grid(. ~ pair) +
        geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette="OrRd")+
        theme_bw()
  
      #pair1
      aovf0range1<-aov(zF0range ~ condition*Participant, data = P1data)
      summary(aovf0range1)
      TukeyHSD(aovf0range1)
      
      #pair2
      aovf0range2<-aov(zF0range ~ condition*Participant, data = P2data)
      summary(aovf0range2)
      TukeyHSD(aovf0range2)
      
      
      #pair3 (this is completely dif betw individuals--)
      aovf0range3<-aov(zF0range ~ condition*Participant, data = P3data)
      summary(aovf0range3)
      TukeyHSD(aovf0range3)
      
      #pair4
      aovf0range4<-aov(zF0range ~ condition*Participant, data = P4data)
      summary(aovf0range4)
      TukeyHSD(aovf0range4)
      
      #pair5
      aovf0range5<-aov(zF0range ~ condition*Participant, data = P5data)
      summary(aovf0range5)
      TukeyHSD(aovf0range5)
      
      #pair6
      aovf0range6<-aov(zF0range ~ condition*Participant, data = P6data)
      summary(aovf0range6)
      TukeyHSD(aovf0range6)
      
      #pair7
      aovf0range7<-aov(zF0range ~ condition*Participant, data = P7data)
      summary(aovf0range7)
      TukeyHSD(aovf0range7)
      
      #pair8
      aovf0range8<-aov(zF0range ~ condition*Participant, data = P8data)
      summary(aovf0range8)
      TukeyHSD(aovf0range8)
      
      #pair9
      aovf0range9<-aov(zF0range ~ condition*Participant, data = P9data)
      summary(aovf0range9)
      TukeyHSD(aovf0range9)
      
      #pair10
      aovf0range10<-aov(zF0range ~ condition*Participant, data = P10data)
      summary(aovf0range10)
      TukeyHSD(aovf0range10)
  
  
  
  
  
  
  
  
  
# Creakness (Voicesauce data) -------------------

# pair 1-------------------------------------
    P1path <- paste0(currDir,'/pair1/output.txt')
    VSdataP1 <- read.delim(P1path, header = TRUE)
    #just checked the quality. it looks like perfectly taking the annotated segment and not the other parts.
    #concern: failing to get f0...
      # strf0= Straight by Kawahara 1998
      # sF0 = Snack Sound Toolkit 
      # pf0 = praat 
      # shrf0 = Sun's subharmonic-to-harmonic Ratio
          # for our data, pF0 and oF0 not working. (o is for "Other")., but str and s are working!
    
    #let's seethe measures.

      P1path <- paste0(currDir,'/pair1/output.txt')
      VSdataP1 <- read.delim(P1path, header = TRUE)
      
      #how many rows for 1 data file?
     Onedata <-  VSdataP1[VSdataP1$Filename == "P1_lu2zhuren_B3_T17_Matching.mat",]
      # 285 data for one file.
     
     Onedata2 <- VSdataP1[VSdataP1$Filename == "P1_lu2jiangjun_B1_T10_Matching.mat",]
     # 361 data for one file.
     
     # --> confirming that this is getting the right 10 msec each for the interval. (maybe 1 or 2 more than that?)
     
     # for each data, get creakiness measures. and compare by tones.
       #columns to use: f0 (for extra-low f0), H1-H2,  HNR (irregular F0)
     
     #one data has extra 1 space before Lu2... 
     VSdataP1[VSdataP1$Label == " Lu2",]
     #relabeling them
     VSdataP1$Label <- str_replace_all(VSdataP1$Label, " Lu2", "Lu2")

     #prep
     VSdataP1$Label <- as.factor(VSdataP1$Label)
     
     #strF0
     ggplot(VSdataP1, aes(y=strF0, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Set3")
     # + scale_fill_discrete(palette="Dark2")
       # + geom_boxplot(width = 0.1)
       

     #sF0
     ggplot(VSdataP1, aes(y=sF0, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Set3")
     
      #just by looking at these, strf0 is working in the most right way? Lu3 seem to be lower than the other 2.
      #but also this is getting average so not right?
      #maybe I should focus on minF0 for that data, and then do this visualizations.
     
     #for each data file, get the min strf0, and add a column for that.
     
            # do it later!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     
     # H1 (low = creakier)
      # H1c = corrected
     ggplot(VSdataP1, aes(y=H1c, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="YlOrRd")
     
      # H1u + uncorrected
     ggplot(VSdataP1, aes(y=H1u, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="YlOrRd")
     
          # ==> look pretty much the same? maybe Lu2 is diff from Lu3 and 4
     
     
     # H1-H2 (low = creakier)
        # H1H2c = corrected
     ggplot(VSdataP1, aes(y=H1H2c, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="PuBuGn")
     
     
        # H1H2u = uncorrected
     ggplot(VSdataP1, aes(y=H1H2u, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="PuBuGn")
     
               # ==> look pretty much the same
     
     
     # HNRs (lower = creakier)
        # HNR05 = HNR between 0-500Hz
     ggplot(VSdataP1, aes(y=HNR05, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Accent")
     
        # HNR15 = HNR between 0-1500Hz
     ggplot(VSdataP1, aes(y=HNR15, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Accent")
     
        # HNR25 = HNR between 0-2500Hz.
     ggplot(VSdataP1, aes(y=HNR25, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Accent")
     
               # ==> look exactly the same
     
     
 
# pair 5-------------------------------------
     P5path <- paste0(currDir,'/pair5/output.txt')
     VSdataP5 <- read.delim(P5path, header = TRUE)
     
     #how many rows for 1 data file?
     Onedata <-  VSdataP5[VSdataP5$Filename == "P5_lu2jiangjun_B1_T4_Matching.mat",]
     # 304 data for one file.
     
     Onedata2 <- VSdataP5[VSdataP5$Filename == "P5_lu2jiangjun_B1_T19_Matching.mat",]
     # 403 data for one file.
     
  
     
     #prep (commented out cus somhow worked without it )
    # VSdataP1$Label <- as.factor(VSdataP1$Label)
     
     #strF0
     ggplot(VSdataP5, aes(y=strF0, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Set3")
     # + scale_fill_discrete(palette="Dark2")
     # + geom_boxplot(width = 0.1)
     
     
     #sF0
     ggplot(VSdataP5, aes(y=sF0, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Set3")
     
     #just by looking at these, strf0 is working in the most right way? Lu3 seem to be lower than the other 2.
     #but also this is getting average so not right?
     #maybe I should focus on minF0 for that data, and then do this visualizations.
     
     #for each data file, get the min strf0, and add a column for that.
     
     # do it later!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     
     # H1 (low = creakier)
     # H1c = corrected
     ggplot(VSdataP5, aes(y=H1c, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="YlOrRd")
     
     # H1u + uncorrected
     ggplot(VSdataP5, aes(y=H1u, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="YlOrRd")
     
     # ==> look pretty much the same? maybe Lu2 is diff from Lu3 and 4
     
     
     # H1-H2 (low = creakier)
     # H1H2c = corrected
     ggplot(VSdataP5, aes(y=H1H2c, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="PuBuGn")
     
     
     # H1H2u = uncorrected
     ggplot(VSdataP5, aes(y=H1H2u, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="PuBuGn")
     
     # ==> a bit different?
     
     
     # HNRs (lower = creakier)
     # HNR05 = HNR between 0-500Hz
     ggplot(VSdataP5, aes(y=HNR05, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Accent")
     
     # HNR15 = HNR between 0-1500Hz
     ggplot(VSdataP5, aes(y=HNR15, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Accent")
     
     # HNR25 = HNR between 0-2500Hz.
     ggplot(VSdataP5, aes(y=HNR25, x=Label, fill = Label)) +
       geom_violin(trim=FALSE) + scale_fill_brewer(palette="Accent")
     
     # ==> look exactly the same
     
     
     
     
     
     
     
#MY ACOUSTIC NOTES----------------------------------------------------------------------------------------
     
#Acoustics to explore
         #list of measures
        #duration
        #voice quality (creak)
        #f0 range, duration, % tme (normed) with f0
        #(growth curve modeling, for contour tones)

#### Creak ####
     
    # (Keating etal 2015)
    # Prototypical creaky voice can be distinguished acoustically by its lower F0, by its irregular F0 (which results in lower values of various harmonicto-noise measures),
    # and by its lower H1 and H1-H2, and other harmonic difference measures. Just one or two of these prototypical properties apparently suffices to make a sample creaky. 
    # Creak that is vocal fry with a regular F0 could instead show higher HNR together with lower formant bandwidths.
    # Creak that is multiply pulsed can lack a clear F0 but instead show subharmonics (resulting in higher values of SHR). 
    # Non-constricted creak can instead show higher H1-H2, but still with a low and irregular F0. Creak that is more like tense or pressed 
    # voice can have a mid or high, and regular, F0. We hope to convey that there is no straightforward answer to the FAQ, 
    # “What is the best acoustic measure for creaky voice?”. It entirely depends on what kind(s) of creak the investigator wants to identify.
    # It cannot be expected that measures such as H1*-H2*, or jitter, etc., will always characterize creaky voice, since there are special sub-types 
    # that are not glottally constricted, or not irregular, etc. It is crucial to keep in mind that when different acoustic measures seem to “disagree”
    # about the creakiness of a speech sample, the set of measures as a whole is in fact giving valuable information about the specific voice quality in the sample.
    
    #so what kindn of creak do i want to ientify?
     # Huang, Y. (2020). Different attributes of creaky voice distinctly affect Mandarin tonal perception. 
    # => low f0 is the most prominent cue used in Mandarin tone identification.
    # Stimuli with low spectral tilt (low ST)
          # were created by lowering two parameters: Open Phase (H1–H2) and Spectral Tilt (H1–3000 Hz), resulting in 
          # less energy in the first harmonic relative to both the second harmonic and higher frequencies
    # The second acoustic attribute, irregular F0, 
          # was manipulated through the Flutter parameter. 
    # Period doubling, indicating an alternate pitch cycle along with the harmonics, 
          # was created by setting the Double Pulsing parameter to a value of 0.15
          # The specific parameter values were chosen so as to create a percept of creak while still ensuring a natural voice quality
    # For the extra-low F0 attribute
          # the lowest F0 value in the modal stimuli for each tone was lowered by three semitones to create a clear drop 
          # in the pitch contour relative to its surrounding pitch points. Given that the lowest F0 occurred in the different positions of the modal Tones
          # 2, 3, and 4, the corresponding extra-low F0s were in the same regions (beginning of T2, middle of T3, and end of T4). 

     
#Useful code to test-------------------------------------------------------------

     print(P1data %>% 
             +     group_by(filename) %>%
             +     summarise(no_rows = length(filename)), n= 10000)


#==trash=================================================================================

##these visualizations are merging both Tone3, and not really helpful, so took this out from main.
     #Duration, each tone, by block, across ppl   
     ggplot(normDf, aes(fill=tone, y=zDuration, x=block)) +
       geom_boxplot() +
       scale_fill_brewer(palette="Paired")
     
     #Duration, each tone, by Pair, across blocks and ppl
     ggplot(normDf, aes(fill=tone, y=zDuration, x=pair)) +
       geom_boxplot() +
       scale_fill_brewer(palette="Paired")
     
     #above but only seeing B4.
     mDfB4<-normDf[normDf$block=="B4",]
     ggplot(mDfB4, aes(fill=tone, y=zDuration, x=pair)) +
       geom_boxplot() +
       scale_fill_brewer(palette="Paired")
     
#attempt for accuracy stats       
     accuracyStats<-chisq.test(normDf$Accuracy, normDf$condition)
     contingTable <- table(normDf$Accuracy, normDf$condition)
     library("rstatix")
     pairwise_prop_test(contingTable)
     #gives me a warning message "Chi-squared approximation may be incorrect"
     #also this does not consider individual/pair level variations
     #but, in general, sig diff pairs are:
     # T2-T3s(good), T3ns-T3s(good), T3s-T4(good), T2-T4 (if compared to the most accurate cond, T2 is still considered to be lower.)
     #non sig are:
     # T2-T3ns(T3noSandhi was as fine as T2!), T3ns-T4(good)
     
     
     
# Other anova1 
     library(rstatix)
     res.aov <- anova_test(
       data = P1data, dv = zDuration, wid = filename,
       within = condition)
     #this gives me an error because 6 rows are sharing the same keys!! (there are 3 pairs of data that is the same name?!?!?)
     get_anova_table(res.aov)
     
     #another ANOVA for MSE(mean squared error)
     library("ez")
     anova2<-ezANOVA(data = P1data,
                     dv = .(zDuration),
                     wid = .(filename),
                     within = .(condition),
                     detailed = TRUE)
     
     
     
     
     
     VSdataP1[VSdataP1$Label] == as.character(factorsList[2]),]
factorsList<-unique(VSdataP1$Label)

     
     
basicinfoFULL<- rbind(basicinfoFULL, list(datName, pair, block, tone, itemID, dur, f0mean, f0range))


  file_type_list<- lapply(exfiles, function(file) {
    name<-basename(file)
  })
  
semitonef0<-read.delim("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/pair1/P1_lu2zhuren_B4_T3_Matching.semitonef0")

#take the sound out
exdat <- exdat[1:21]

exdat <- exfiles %>%
  set_names(.) %>%
  map_df(read_table2, .id = "FileName")
