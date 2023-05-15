#########################################
###  Yuka acoustic data exploration   ###
#########################################

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

    #path of this df for my laptop: "C:\Users\yuka\Desktop\SWF\basicAC.csv"
    #to read it when working on laptop: 
      basicinfoFULL <- read.delim("/Users/yuka/Desktop/SWF/basicAC.csv", sep = ",")
      
    # combine accuracy data  
      acDf <- read.delim("/Users/yuka/Desktop/SWF/processed_matching_data.csv", sep = ",")
      #need to match by: pair, block, trial (tone differs as a result).
      #change P_01 to P1
      acDf$pair <- gsub( "P_0(\\d+)", "P\\1", acDf$pair)
      acDf$pair <- gsub( "P_10", "P10", acDf$pair)
      #change Block 4 to B4
      acDf$block <- gsub("Block\\s+(\\d+)", "B\\1", acDf$block)
      #merge df
      basicinfoFULL$trial <- basicinfoFULL$itemID
      library(dplyr)
      mergedDf <- left_join(basicinfoFULL, acDf, by = c("pair" = "pair", "block"="block", "trial"="trial"))
      
      
# Basic stats
      #possible things to look at:
        # duration 
            # differ by tones?
            # differ by correct vs incorrect items?
            # differ by block? (longer in 4 than 1?)
            # differ by pair? (successful pair has longer duration?)
      


# Visualizations     

      
      
###so let's loop through all files just for these basic data ----------------------------     
      
      P1path <- paste0(currDir,'/pair1/output.txt')
      VSdataP1 <- read.delim(P1path, header = TRUE)
      
      
      
      

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

#-----
#




#==========================================================
#==========================trash===========================
#==========================================================
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