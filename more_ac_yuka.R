#########################################
###  Yuka acoustic data exploration   ###
#########################################

#### notes for myself not looking at this for awhile ###
#make sure you are running the parts that is good for your own environment now -- laptop or lab desktop
#you can start from " # WORKING POINT " if just working on acoustics


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




###so let's loop through all files just for these basic data ----------------------------

#read in all files
setwd("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/")
currDir <- getwd()
fileList <- c()
for (n in 1:10){
  folderpath <- paste0(currDir, "/pair", n)
  tempfileList <- list.files(folderpath, pattern = "means+")
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
        setwd(paste0(currDir,"/Pair", pairNum))
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

      
      
      
##### WORKING POINT #####================================================================
      
      
      
    #Acoustic data from Prosody Pro
      # LAPTOP: 
        basicinfoFULL <- read.delim("/Users/yuka/Desktop/SWF/basicAC.csv", sep = ",")
      # DESKTOP:
        basicinfoFULL <- read.delim("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/basicAC.csv", sep = ",")
        
    #Get accuracy data  
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
        
      #merge df
      basicinfoFULL$trial <- basicinfoFULL$itemID
      library(dplyr)
      mergedDf <- left_join(basicinfoFULL, acDf, by = c("pair" = "pair", "block"="block", "trial"="trial"))
      RmergedDf <- right_join(basicinfoFULL, acDf, by = c("pair" = "pair", "block"="block", "trial"="trial"))
      FmergedDf <- full_join(basicinfoFULL, acDf, by = c("pair" = "pair", "block"="block", "trial"="trial"))
      #nrow(basicinfofull) = 933, nrow(acDf) = 959, nrow(mergedDf) = 933, nrow(FmergedDf) = 962,  nrow(RmergedDf) = 962
      # desgin wise, there should be 960 bc each of 3 tones is tested 32 times, and there are 10 pairs.

  #Deleting some data that I realized later that are doubled
      nrow(FmergedDf)
      FmergedDf <- FmergedDf %>% distinct()
      nrow(FmergedDf) #checking
      #after this, 959...  bc we are missing P3B4T24.
      
  #Making "Condition" Column
      # add column with this information in the mergedDf
      FmergedDf<-FmergedDf %>%
        mutate(condition = case_when(
          grepl("lu2", filename)  ~ "Tone 2",
          grepl("lu3jiangjun", filename)  ~ "Tone 3 noSandhi",
          grepl("lu3zhentan", filename)  ~ "Tone 3 noSandhi",
          grepl("lu3zhuren", filename)  ~ "Tone 3 Sandhi",
          grepl("lu3jingguan", filename)  ~ "Tone 3 Sandhi",
          grepl("lu4", filename)  ~ "Tone 4"
        ))

### Behavioral (Accuracy) -------  
      
      ##overall descriptive stats---
      
      #stats - T2
      FmergedDfT2<- FmergedDf[FmergedDf$condition == "Tone 2",]
      #this has 320 rows ( nrow(FmergedDfT2) was 320), but the tail of it is all NAs.
      table(FmergedDfT2$Accuracy)
        #across pairs, T2 correct = 273, T2 incorrect = 18
      
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
      
      ###Inferential stats------
      accuracyStats<-chisq.test(mergedDf$Accuracy, mergedDf$condition)
      contingTable <- table(mergedDf$Accuracy, mergedDf$condition)
      library("rstatix")
      pairwise_prop_test(contingTable)
        #gives me a warning message "Chi-squared approximation may be incorrect"
      #also this does not consider individual/pair level variations
        #but, in general, sig diff pairs are:
            # T2-T3s(good), T3ns-T3s(good), T3s-T4(good), T2-T4 (if compared to the most accurate cond, T2 is still considered to be lower.)
        #non sig are:
            # T2-T3ns(T3noSandhi was as fine as T2!), T3ns-T4(good)
      
      #glmer (for binary data... if i am understanding this right)
      dataToFit <- mergedDf
      dataToFit$Accuracy<-ifelse(dataToFit$Accuracy=="Correct", 1,0)
      library("lme4")
      glmer1<- glmer(Accuracy ~ condition + (1|pair), data = dataToFit, family=binomial)
      summary(glmer1)
      # T2 - T3ns ... marginal
      # T2 - T3S ... ** (0.00228)
      # T2 - T4 ... ** (0.00282)
      
      #Contrast coding
      library("car")
      dataToFit$condition <- as.factor(dataToFit$condition)
      #seeing the currect state (tone 2 as a base)
      contrasts(dataToFit$condition)
      #sum contrast
      contrasts(dataToFit$condition) <- contr.Sum(levels(dataToFit$condition))
      #note: if wanted to change the baseline.
      # levels(dataToFit$condition) <- factor(c("Tone 3 Sandhi","Tone 3 ", "Tone 2", "Tone 4"))
      
      
      glmer1<- glmer(Accuracy ~ condition + (1|pair), data = dataToFit, family=binomial)
      summary(glmer1)
      
### Basic stats   -------- 
      #possible things to look at:
        # duration  ---------------
            # differ by tones?
            # differ by correct vs incorrect items?
            # differ by block? (longer in 4 than 1?)
            # differ by pair? (successful pair has longer duration?)
      mergedDf$pair <- as.factor(mergedDf$pair)
      mergedDf$block <- as.factor(mergedDf$block)
      mergedDf$tone <- as.factor(mergedDf$tone)
      mergedDf$itemID <- as.factor(mergedDf$itemID)
      
      # skip 
      library(lme4)
      durlmer<-lmer(duration~tone*block+(1|pair),data=mergedDf)
      summary(durlmer)

      #very general tone diff (across pairs, blocks, and individuals)
      
      #DURATION---------------
        #violin
        ggplot(mergedDf, aes(y=duration, x=condition)) +
          geom_violin(aes(fill = condition)) +theme_bw()+ 
        stat_summary(fun = "mean",
                     geom = "point",
                     aes(color = "Mean")) +
        stat_summary(fun = "median",
                     geom = "point",
                     aes(color = "Median")) +
        scale_colour_manual(values = c("red", "blue"), name = "")
        #boxplot
        ggplot(mergedDf, aes(y=duration, x=condition)) +
          geom_boxplot(aes(fill = condition)) +theme_bw()
        # +scale_fill_brewer(palette="BuPu")
      
        anovaGenDur<-aov(duration ~ condition, data = mergedDf)
        summary(anovaGenDur)
        TukeyHSD(anovaGenDur)
        
        #the only difference observed is T3nS and T2 
      
      #f0mean----------------
        #violin
        ggplot(mergedDf, aes(y=f0mean, x=condition)) +
          geom_violin(aes(fill = condition)) +theme_bw()+ 
          stat_summary(fun = "mean",
                       geom = "point",
                       aes(color = "Mean")) +
          stat_summary(fun = "median",
                       geom = "point",
                       aes(color = "Median")) +
          scale_colour_manual(values = c("red", "blue"), name = "")
        #boxplot
        ggplot(mergedDf, aes(y=f0mean, x=condition)) +
          geom_boxplot(aes(fill = condition)) +theme_bw()
        # +scale_fill_brewer(palette="BuPu")
        
        anovaGenF0M<-aov(f0mean ~ condition, data = mergedDf)
        summary(anovaGenF0M)
        TukeyHSD(anovaGenF0M)
        #SIG: T3nS - T2, T3S - T2, T4 - T2, T4 - T3nS, T4 - T3S
        #nonSIG: T3nS - T3S
        # differentiation between sandhi conditions within T3
        
      #f0range----------------(each data point = range of Hz for the tone.)
        #violin
        ggplot(mergedDf, aes(y=f0range, x=condition)) +
          geom_violin(aes(fill = condition)) +theme_bw()+ 
          stat_summary(fun = "mean",
                       geom = "point",
                       aes(color = "Mean")) +
          stat_summary(fun = "median",
                       geom = "point",
                       aes(color = "Median")) +
          scale_colour_manual(values = c("red", "blue"), name = "")
        #boxplot
        ggplot(mergedDf, aes(y=f0range, x=condition)) +
          geom_boxplot(aes(fill = condition)) +theme_bw()
        # +scale_fill_brewer(palette="BuPu")
            #T4 has higher mean of f0range than the other 3 groups. = T4 has the widest range of f0
        
        
        anovaGenF0R<-aov(f0range ~ condition, data = mergedDf)
        summary(anovaGenF0R)
        TukeyHSD(anovaGenF0R)       
          #No difference among T2, T3, T3sandhi (diff only found with T4)
      
### Visualizations   --------  
      
library(ggplot2)
      
ggplot(mergedDf, aes(fill=tone, y=duration, x=block)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette="Paired")

#BY Pair. each bar merged all blocks.
ggplot(mergedDf, aes(fill=tone, y=duration, x=pair)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette="Paired")

#above but only seeing B4.
mDfB4<-mergedDf[mergedDf$block=="B4",]
ggplot(mDfB4, aes(fill=tone, y=duration, x=pair)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette="Paired")

#comparing duration of [lU2, lU4, lU3sandhi, lu3noSandhi]


 # visualize by pair
　#Bar (less informative)
 ggplot(mergedDf, aes(fill=condition, y=duration, x=pair)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette="Set2")
 
 #violin (good!)

 ggplot(mergedDf, aes(fill=condition, y=duration, x=pair)) +
   geom_violin(trim=FALSE) +
   geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.9)) +
   scale_fill_brewer(palette="Set2")+
   theme_minimal()
 
 #box (my favorite)
 ggplot(mergedDf, aes(fill=condition, y=duration, x=pair)) +
   geom_boxplot() + 
   geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
   scale_fill_brewer(palette="Set2")+
   theme_minimal()
   
#any sig differences between each condition, within each pair?
 
 #pair1
  P1data<- mergedDf[mergedDf$pair == "P1",]
  P1data$condition <- as.factor(P1data$condition)
  P1data$duration<-  as.numeric(P1data$duration)
  
  anova1<-aov(duration ~ condition, data = P1data)
  summary(anova1)
  TukeyHSD(anova1)
    # RESULTS
      # T3nosandhi - T2 : ***
      # Tone 3 Sandhi-Tone 2: ***
      # T4 - T3nosandhi: ***
      # T4 - T3sandhi: ***
  
  #pair2
  P2data<- mergedDf[mergedDf$pair == "P2",]
  P2data$condition <- as.factor(P2data$condition)
  P2data$duration<-  as.numeric(P2data$duration)
  
  anova2<-aov(duration ~ condition, data = P2data)
  summary(anova2)
  TukeyHSD(anova2)
  # RESULTS
  # T3 Sandhi-Tone 2: *
  # T3 Sandhi-T3 noSandhi: ***
  # T4 - T3sandhi: ***
  
  #pair3
  P3data<- mergedDf[mergedDf$pair == "P3",]
  P3data$condition <- as.factor(P3data$condition)
  P3data$duration<-  as.numeric(P3data$duration)
  
  anova3<-aov(duration ~ condition, data = P3data)
  summary(anova3)
  TukeyHSD(anova3)
  # RESULTS
  # T4 - T3 noSandhi: *
  # T4 - T3sandhi: *
  
  #pair4
  P4data<- mergedDf[mergedDf$pair == "P4",]
  P4data$condition <- as.factor(P4data$condition)
  P4data$duration<-  as.numeric(P4data$duration)
  
  anova4<-aov(duration ~ condition, data = P4data)
  summary(anova4)
  TukeyHSD(anova4)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  # T3 Sandhi-T2: ***
  # T3 Sandhi - T3 noSandhi: **
  # T4 - T3sandhi: ***
  
  #pair5
  P5data<- mergedDf[mergedDf$pair == "P5",]
  P5data$condition <- as.factor(P5data$condition)
  P5data$duration<-  as.numeric(P5data$duration)
  
  anova5<-aov(duration ~ condition, data = P5data)
  summary(anova5)
  TukeyHSD(anova5)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  # T3 noSandhi-T2: *
  # T4 - T3 noSandhi: ***
  # T4 - T3sandhi: **
  
  #pair6
  P6data<- mergedDf[mergedDf$pair == "P6",]
  P6data$condition <- as.factor(P6data$condition)
  P6data$duration<-  as.numeric(P6data$duration)
  
  anova6<-aov(duration ~ condition, data = P6data)
  summary(anova6)
  TukeyHSD(anova6)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  # T3 Sandhi-T2: ***
  # T4 - T2: **
  # T3 Sandhi - T3 noSandhi: **
  # T4 - T3 noSandhi: *
  # T4 - T3 sandhi: ***
 
  #pair7
  P7data<- mergedDf[mergedDf$pair == "P7",]
  P7data$condition <- as.factor(P7data$condition)
  P7data$duration<-  as.numeric(P7data$duration)
  
  anova7<-aov(duration ~ condition, data = P7data)
  summary(anova7)
  TukeyHSD(anova7)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  # T3 Sandhi- T2: **
  # T4 - T3 sandhi: *
  
  #pair8
  P8data<- mergedDf[mergedDf$pair == "P8",]
  P8data$condition <- as.factor(P8data$condition)
  P8data$duration<-  as.numeric(P8data$duration)
  
  anova8<-aov(duration ~ condition, data = P8data)
  summary(anova8)
  TukeyHSD(anova8)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  # Tone 3 Sandhi-Tone 2: **
  # Tone 4-Tone 2: **
  # Tone 4-Tone 3 noSandhi: ** (0.0010892)
  # Tone 4-Tone 3 Sandhi: ***
  
  #pair9
  P9data<- mergedDf[mergedDf$pair == "P9",]
  P9data$condition <- as.factor(P9data$condition)
  P9data$duration<-  as.numeric(P9data$duration)
  
  anova9<-aov(duration ~ condition, data = P9data)
  summary(anova9)
  TukeyHSD(anova9)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  # Tone 4-Tone 3 noSandhi: *
  # Tone 4-Tone 3 Sandhi: **
  
  #pair10
  P10data<- mergedDf[mergedDf$pair == "P10",]
  P10data$condition <- as.factor(P10data$condition)
  P10data$duration<-  as.numeric(P10data$duration)
  
  anova10<-aov(duration ~ condition, data = P10data)
  summary(anova10)
  TukeyHSD(anova10)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  # Tone 4-Tone 2: ***
  # Tone 3 Sandhi-Tone 3 noSandhi: ** (0.0033333)
  # Tone 4-Tone 3 Sandhi: ***
  
  
# Duration ～ Speaker  *  conditions !!!!!!!!!!
  
  #Visualization by pair & person (duration)
  mergedDf$Participant <- as.factor(mergedDf$Participant)
  ggplot(mergedDf, aes(y = duration, x = Participant, fill = condition)) +
    geom_boxplot() +
    facet_grid(. ~ pair) +
    geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette="Set2")+
    theme_bw()
  
  #pair1
  aovdur1<-aov(duration ~ condition*Participant, data = P1data)
  summary(aovdur1)
  TukeyHSD(aovdur1)
  
  #pair2
  aovdur2<-aov(duration ~ condition*Participant, data = P2data)
  summary(aovdur2)
  TukeyHSD(aovdur2)
  
  
  #pair3 (this is completely dif betw individuals--)
  aovdur3<-aov(duration ~ condition*Participant, data = P3data)
  summary(aovdur3)
  TukeyHSD(aovdur3)
  
  #pair4
  aovdur4<-aov(duration ~ condition*Participant, data = P4data)
  summary(aovdur4)
  TukeyHSD(aovdur4)
  
  #pair5
  aovdur5<-aov(duration ~ condition*Participant, data = P5data)
  summary(aovdur5)
  TukeyHSD(aovdur5)
  
  #pair6
  aovdur6<-aov(duration ~ condition*Participant, data = P6data)
  summary(aovdur6)
  TukeyHSD(aovdur6)
  
  #pair7
  aovdur7<-aov(duration ~ condition*Participant, data = P7data)
  summary(aovdur7)
  TukeyHSD(aovdur7)
  
  #pair8
  aovdur8<-aov(duration ~ condition*Participant, data = P8data)
  summary(aovdur8)
  TukeyHSD(aovdur8)
  
  #pair9
  aovdur9<-aov(duration ~ condition*Participant, data = P9data)
  summary(aovdur9)
  TukeyHSD(aovdur9)
  
  #pair10
  aovdur10<-aov(duration ~ condition*Participant, data = P10data)
  summary(aovdur10)
  TukeyHSD(aovdur10)
  
  
#Visualization by pair (f0mean)------
  
  #Bar (less informative)
  ggplot(mergedDf, aes(fill=condition, y=f0mean, x=pair)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_brewer(palette="Set1")
  
  #violin (good!)
  
  ggplot(mergedDf, aes(fill=condition, y=f0mean, x=pair)) +
    geom_violin(trim=FALSE) +
    geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.9)) +
    scale_fill_brewer(palette="Set1")+
    theme_minimal()
  
  #box (my favorite)
  ggplot(mergedDf, aes(fill=condition, y=f0mean, x=pair)) +
    geom_boxplot() + 
    geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette="Set1")+
    theme_minimal()
  
  mergedDf$Participant <- as.factor(mergedDf$Participant)
  #Visualization by pair & person (f0mean)
  mergedDf$Participant <- as.factor(mergedDf$Participant)
  ggplot(mergedDf, aes(y = f0mean, x = Participant, fill = condition)) +
    geom_boxplot() +
    facet_grid(. ~ pair) +
    geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette="Set1")+
    theme_bw()
  
  
#stats ---
  #pair1
   aovf0mean1<-aov(f0mean ~ condition*Participant, data = P1data)
  summary(aovf0mean1)
  TukeyHSD(aovf0mean1)
  # RESULTS (* < 0.05, ** < 0.01, *** < 0.001)
  #  Tone 4:Listener-Tone 2:Listener                   33.057837  20.845546  45.270127 0.0000000
  #  Tone 2:Speaker-Tone 2:Listener                    39.735514  27.523223  51.947804 0.0000000
  #  Tone 3 noSandhi:Speaker-Tone 2:Listener           26.770041  11.813100  41.726981 0.0000078
  #  Tone 4:Speaker-Tone 2:Listener                    65.548164  53.335874  77.760455 0.0000000
  #  Tone 4:Listener-Tone 3 noSandhi:Listener          39.415904  24.458964  54.372844 0.0000000
  #  Tone 2:Speaker-Tone 3 noSandhi:Listener           46.093581  31.136641  61.050521 0.0000000
  #  Tone 3 noSandhi:Speaker-Tone 3 noSandhi:Listener  33.128108  15.857321  50.398895 0.0000014
  #  Tone 4:Speaker-Tone 3 noSandhi:Listener           71.906232  56.949292  86.863172 0.0000000
  #  Tone 4:Listener-Tone 3 Sandhi:Listener            28.971498  14.014558  43.928439 0.0000011
  #  Tone 2:Speaker-Tone 3 Sandhi:Listener             35.649175  20.692235  50.606116 0.0000000
  #  Tone 3 noSandhi:Speaker-Tone 3 Sandhi:Listener    22.683702   5.412915  39.954489 0.0024301
  #  Tone 4:Speaker-Tone 3 Sandhi:Listener             61.461826  46.504886  76.418766 0.0000000
  #  Tone 3 Sandhi:Speaker-Tone 4:Listener            -31.268289 -46.225229 -16.311348 0.0000001
  #  Tone 4:Speaker-Tone 4:Listener                    32.490328  20.278037  44.702618 0.0000000
  #  Tone 3 Sandhi:Speaker-Tone 2:Speaker             -37.945966 -52.902906 -22.989025 0.0000000
  #  Tone 4:Speaker-Tone 2:Speaker                     25.812651  13.600360  38.024941 0.0000001
  #  Tone 3 Sandhi:Speaker-Tone 3 noSandhi:Speaker    -24.980493 -42.251280  -7.709706 0.0005485
  #  Tone 4:Speaker-Tone 3 noSandhi:Speaker            38.778124  23.821184  53.735064 0.0000000
  #  Tone 4:Speaker-Tone 3 Sandhi:Speaker              63.758616  48.801676  78.715557 0.0000000
  
  #pair2
  aovf0mean2<-aov(f0mean ~ condition*Participant, data = P2data)
  summary(aovf0mean2)
  TukeyHSD(aovf0mean2)
  
  
  #pair3 (this is completely dif betw individuals--)
  aovf0mean3<-aov(f0mean ~ condition*Participant, data = P3data)
  summary(aovf0mean3)
  TukeyHSD(aovf0mean3)

  #pair4
  aovf0mean4<-aov(f0mean ~ condition*Participant, data = P4data)
  summary(aovf0mean4)
  TukeyHSD(aovf0mean4)
  
  #pair5
  aovf0mean5<-aov(f0mean ~ condition*Participant, data = P5data)
  summary(aovf0mean5)
  TukeyHSD(aovf0mean5)
  
  #pair6
  aovf0mean6<-aov(f0mean ~ condition*Participant, data = P6data)
  summary(aovf0mean6)
  TukeyHSD(aovf0mean6)
  
  #pair7
  aovf0mean7<-aov(f0mean ~ condition*Participant, data = P7data)
  summary(aovf0mean7)
  TukeyHSD(aovf0mean7)
  
  #pair8
  aovf0mean8<-aov(f0mean ~ condition*Participant, data = P8data)
  summary(aovf0mean8)
  TukeyHSD(aovf0mean8)
  
  #pair9
  aovf0mean9<-aov(f0mean ~ condition*Participant, data = P9data)
  summary(aovf0mean9)
  TukeyHSD(aovf0mean9)
  
  #pair10
  aovf0mean10<-aov(f0mean ~ condition*Participant, data = P10data)
  summary(aovf0mean10)
  TukeyHSD(aovf0mean10)
    
 
  
  
###Visualization by pair (f0range) -------
  
  #Bar (less informative)
  ggplot(mergedDf, aes(fill=condition, y=f0range, x=pair)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_brewer(palette="Set1")
  
  #violin (updated for speaker-wise)
  ggplot(mergedDf, aes(fill=condition, y=f0range, x= Participant)) +
    geom_violin(trim=FALSE) +
    facet_grid(. ~ pair) +
    geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.9)) +
    scale_fill_brewer(palette="Set1")+
    theme_bw()
  
  #box 
  ggplot(mergedDf, aes(fill=condition, y=f0range, x=pair)) +
    geom_boxplot() + 
    geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette="Set1")+
    theme_minimal()
  
  #Visualization by pair & person 
  mergedDf$Participant <- as.factor(mergedDf$Participant)
  ggplot(mergedDf, aes(y = f0range, x = Participant, fill = condition)) +
    geom_boxplot() +
    facet_grid(. ~ pair) +
    geom_point(aes(fill = condition), size = 1.2, shape = 21, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette="Set1")+
    theme_bw()
  
  #pair1
  aovf0range1<-aov(f0range ~ condition*Participant, data = P1data)
  summary(aovf0range1)
  TukeyHSD(aovf0range1)
  
  #pair2
  aovf0range2<-aov(f0range ~ condition*Participant, data = P2data)
  summary(aovf0range2)
  TukeyHSD(aovf0range2)
  
  
  #pair3 (this is completely dif betw individuals--)
  aovf0range3<-aov(f0range ~ condition*Participant, data = P3data)
  summary(aovf0range3)
  TukeyHSD(aovf0range3)
  
  #pair4
  aovf0range4<-aov(f0range ~ condition*Participant, data = P4data)
  summary(aovf0range4)
  TukeyHSD(aovf0range4)
  
  #pair5
  aovf0range5<-aov(f0range ~ condition*Participant, data = P5data)
  summary(aovf0range5)
  TukeyHSD(aovf0range5)
  
  #pair6
  aovf0range6<-aov(f0range ~ condition*Participant, data = P6data)
  summary(aovf0range6)
  TukeyHSD(aovf0range6)
  
  #pair7
  aovf0range7<-aov(f0range ~ condition*Participant, data = P7data)
  summary(aovf0range7)
  TukeyHSD(aovf0range7)
  
  #pair8
  aovf0range8<-aov(f0range ~ condition*Participant, data = P8data)
  summary(aovf0range8)
  TukeyHSD(aovf0range8)
  
  #pair9
  aovf0range9<-aov(f0range ~ condition*Participant, data = P9data)
  summary(aovf0range9)
  TukeyHSD(aovf0range9)
  
  #pair10
  aovf0range10<-aov(f0range ~ condition*Participant, data = P10data)
  summary(aovf0range10)
  TukeyHSD(aovf0range10)
  
  
  
  
  
  
  
  
  
# Creakness (using Voicesauce data) -------------------

  #pair 1-------------------------------------
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
     
     
 
#pair 5-------------------------------------
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
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
         
     
     
     
     
#----------------------------------------------------------------------------------------
#Acoustics to explore-------------------------------------------------------
 #list of measures
#duration
#voice quality (creak)
#f0 range, duration, % tme (normed) with f0
#(growth curve modeling, for contour tones)

#Creak---------------------------------
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

#-------------------------------------------------------------
# useful code to test
     print(P1data %>% 
             +     group_by(filename) %>%
             +     summarise(no_rows = length(filename)), n= 10000)




#==========================================================
#==========================trash===========================
#==========================================================
# Other anova1 
     library(rstatix)
     res.aov <- anova_test(
       data = P1data, dv = duration, wid = filename,
       within = condition)
     #this gives me an error because 6 rows are sharing the same keys!! (there are 3 pairs of data that is the same name?!?!?)
     get_anova_table(res.aov)
     
     #another ANOVA for MSE(mean squared error)
     library("ez")
     anova2<-ezANOVA(data = P1data,
                     dv = .(duration),
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