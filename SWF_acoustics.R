
#########################################
#      Plot SWF tones (Matching task)                ## Original is by Eric Pelzl; Yuka is editting for my own use
#########################################

#####----assumptions-----#####
# you have 
#   1) accuracy data in csv (processed_matching_data.csv)
#   2) acoustic data files obtained from ProsodyPro ("DATANAME.actutimenormf0" files), subfoldered by "Pair"

#####----- Libraries -----#####
library(tidyverse)
library(ggplot2)
library(scico)

#####----- LOAD ACCURACY DATA -----#####
setwd("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)")
accuracy_data <- read.csv("yuk_ac_analysis/processed_matching_data.csv")

# renaming data in the pair column
accuracy_data$pair <- str_replace_all(accuracy_data$pair, "P_", "P")

#####----- LOAD ACOUSTIC DATA -----#####
#set the relevant data type from ProsodyPro files
filetype <- ".actutimenormf0$"
#read files (that are .actutimenormf0)
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = paste("\\", filetype, sep=""), 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
# this may take a little bit of time
df <- list_of_files %>%
  set_names(.) %>%
  map_df(read_table2, .id = "FileName")
df_backup <- df # load into backup dataframe to save time on reloading if playing with code below
df <- df_backup
head(df)
df <- df %>% filter(rowLabel != "error") %>% droplevels() # remove items labeled as errors
df$item <- str_replace_all(df$FileName, pattern = "./Pair[:digit:]/", replacement = "")
df$item <- str_replace_all(df$item, pattern = filetype, replacement = "")
df$FileName <- NULL
df$rowLabel <- NULL

# create column for surnames w/ proper labels
df$title <- str_extract(df$item, pattern = "lu[:digit:][:alpha:]*")
df$surname <- str_extract(df$item, pattern = "lu[:digit:]")
df$surname <- str_replace_all(df$surname, "l", "L")

# rename titles as sandhi/noSandhi
df$title <- str_sub(df$title, 4,12)
df$sandhi <- str_replace_all(df$title, "jiangjun", "no Sandhi")
df$sandhi <- str_replace_all(df$sandhi, "zhentan", "no Sandhi")
df$sandhi <- str_replace_all(df$sandhi, "jingguan", "Sandhi")
df$sandhi <- str_replace_all(df$sandhi, "zhuren", "Sandhi")

df$trial <- str_extract(df$item, pattern = "T[:digit:]*")
df$block <- str_extract(df$item, pattern = "B[:digit:]")
df$block <- str_replace_all(df$block, "B", "Block ")
df$pair <- str_extract(df$item, pattern = "Pair[:digit:][:digit:]")
df$pair <- str_replace_all(df$pair, "Pair", "P")
head(df)

# set as factors
df$surname <- as.factor(df$surname)
df$sandhi <- as.factor(df$sandhi)
df$pair <- as.factor(df$pair)
df$block <- as.factor(df$block)
df$trial <- as.factor(df$trial)

head(df)
head(accuracy_data)
new_df <- left_join(df, accuracy_data, by = c("pair" = "pair", "block"="block", "trial"="trial"))
head(new_df)
str(new_df)

SWF <- new_df %>% dplyr::select(pair, surname, sandhi, trial, block, ActualTime, F0, Accuracy) %>% rename("time" = "ActualTime")
SWF$msec <- SWF$time * 1000
head(SWF)

# remove items missing labels (and fix those items!)
SWF <- SWF %>% filter(SWF$surname != "NA") %>% droplevels()
str(SWF)
# F0 normalization
# for each data point, 
# F0.zi = (F0i – F0mean)/F0sd
SWF$z_scores <- (SWF$F0- mean(SWF$F0))/sd(SWF$F0)
SWFi <- SWF %>% filter(Accuracy == "Incorrect") %>% droplevels()
SWF$pair <- as.factor(SWF$pair)
levels(SWF$pair)

SWF_P1 <- SWF %>% filter(pair == "P01") %>% droplevels()
SWF_P2 <- SWF %>% filter(pair == "P02") %>% droplevels()
SWF_P3 <- SWF %>% filter(pair == "P03") %>% droplevels()
SWF_P4 <- SWF %>% filter(pair == "P04") %>% droplevels()
SWF_P5 <- SWF %>% filter(pair == "P05") %>% droplevels()
SWF_P6 <- SWF %>% filter(pair == "P06") %>% droplevels()
SWF_P7 <- SWF %>% filter(pair == "P07") %>% droplevels()
SWF_P8 <- SWF %>% filter(pair == "P08") %>% droplevels()
SWF_P9 <- SWF %>% filter(pair == "P09") %>% droplevels()
SWF_P10 <- SWF %>% filter(pair == "P10") %>% droplevels()

head(SWF_P10)

#SWF_P3 <- SWF_P3 %>% group_by(block, surname, sandhi, msec) %>% summarise(F0 = mean(F0)) 

# create color palette for plotting
myColors <- scico(9, palette = 'berlin')
myColors <- myColors[c(2,5,8)]
myColors
myColors <- c( "#519FD3" , "#BC6D61" , "#EAEDE9" ) #180B09

#Raw f0 plotting 
g1 <- 
  ggplot( SWF_P3 , aes( x = msec, y = F0, group = trial)) + theme_minimal()+
  geom_smooth( method = "loess", se = F, aes( colour = surname, fill = surname ), alpha = .5, size = .35) +
  facet_grid( sandhi ~ block ) + # for overall view, consider sandhi ~ surname
  scale_y_continuous("F0 (Hz)")+
  scale_x_continuous("duration (ms)")+
  scale_color_manual(values = myColors) + 
  scale_fill_manual(values = myColors) +
  theme(
    #    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = .25),
    axis.line = element_line(color = "black", size = .25),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    panel.background = element_rect(fill = "gray99", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
print(g1)


#ggsave(g1,
#       file = "SWF_matching_P2_new.svg",
#       width = 6, 
#       height = 2.5, 
#       units = "in",
#       dpi = 300)


##########################
# CODE UNDER CONSTRUCTION (4/18/2023)
##########################
library(data.table) # for 'fread' to help troubleshoot (it provides better descriptions of problem files)

# read in SWF matching data for means
filetype2 <- ".means$"
list_of_files_means <- list.files(path = ".", recursive = TRUE,
                                  pattern = paste("\\", filetype2, sep=""), 
                                  full.names = TRUE)

# Read all the files and create a FileName column to store filenames
# Note: file `./Pair07/P7_lu3jingguan_B3_T24_Matching.means$meanf0` has an error (probably an empty boundary that got analyzed)
dfmeans <- 
  list_of_files_means %>% 
  set_names(.) %>%
  map_df(fread, .id = "FileName") 
dfmeans_backup <- dfmeans
dfmeans <- dfmeans_backup
head(dfmeans)
dfmeans$item <- str_replace_all(dfmeans$FileName, pattern = "./Pair[:digit:]/", replacement = "")
dfmeans$item <- str_replace_all(dfmeans$item, pattern = filetype, replacement = "")
dfmeans$FileName <- NULL
dfmeans$rowLabel <- NULL

# create column for surnames w/ proper labels
dfmeans$title <- str_extract(dfmeans$item, pattern = "lu[:digit:][:alpha:]*")
dfmeans$surname <- str_extract(dfmeans$item, pattern = "lu[:digit:]")
dfmeans$surname <- str_replace_all(dfmeans$surname, "l", "L")

# rename titles as sandhi/noSandhi
dfmeans$title <- str_sub(dfmeans$title, 4,12)
dfmeans$sandhi <- str_replace_all(dfmeans$title, "jiangjun", "no Sandhi")
dfmeans$sandhi <- str_replace_all(dfmeans$sandhi, "zhentan", "no Sandhi")
dfmeans$sandhi <- str_replace_all(dfmeans$sandhi, "jingguan", "Sandhi")
dfmeans$sandhi <- str_replace_all(dfmeans$sandhi, "zhuren", "Sandhi")

dfmeans$trial <- str_extract(dfmeans$item, pattern = "T[:digit:]*")
dfmeans$block <- str_extract(dfmeans$item, pattern = "B[:digit:]")
dfmeans$block <- str_replace_all(dfmeans$block, "B", "Block ")
dfmeans$pair <- str_extract(dfmeans$item, pattern = "P[:digit:]")

# set as factors
dfmeans$surname <- as.factor(dfmeans$surname)
dfmeans$sandhi <- as.factor(dfmeans$sandhi)
dfmeans$pair <- as.factor(dfmeans$pair)
dfmeans$block <- as.factor(dfmeans$block)
dfmeans$trial <- as.factor(dfmeans$trial)

dfmeans$speaker <- ifelse(dfmeans$block == "Block 1" | dfmeans$block == "Block 3",  "A", "B")
head(dfmeans)
str(dfmeans)

# mean duration across conditions
dfmeans %>% group_by(sandhi,surname) %>% summarise(m_duration = mean(duration), 
                                                   m_intensity = mean(mean_intensity), 
                                                   m_finalF0 = mean(finalf0), sd_finalF0 = sd(finalf0), 
                                                   k = length(duration)) %>% print(n = 130)

dfmeans %>% group_by(pair, block, surname, sandhi, speaker) %>% summarise(m_duration = mean(duration), m_intensity = mean(mean_intensity), k = length(duration)) %>% print(n = 130)
df_duration <- dfmeans %>% group_by(block, surname, sandhi) %>% summarise(m_dur = mean(duration))
df_duration_pairs <- dfmeans %>% group_by(pair, speaker, block, surname, sandhi) %>% summarise(m_dur = mean(duration))
df_duration_pairs %>% print(n=120)


library("plotrix")

# create color palette for plotting
myColors <- scico(9, palette = 'berlin')
myColors <- myColors[c(2,5,8)]
myColors <- c( "#519FD3" , "#BC6D61" , "#180B09" )

# Plots comparing mean durations by Sandhi condition for each pair
g2 <- 
  ggplot( df_duration , aes( x = block, y = m_dur, group = pair)) + theme_minimal() +
  geom_line(data = df_duration_pairs, aes(x = block, y = m_dur, group = interaction( speaker, surname), 
                                          color = surname, linetype = speaker), size = .25, alpha = .8) +
  #  geom_line(aes(group = surname, color = surname), size = 1.5, alpha = .75) +
  geom_point(data = df_duration_pairs, aes(x = block, y = m_dur, group = pair, color = surname, shape = surname), 
             size = 1, alpha = .75) +
  
  facet_wrap( sandhi ~ pair, nrow = 4, ncol = 5) +
  scale_y_continuous("duration (msec)") +
  scale_x_discrete("") +
  scale_color_manual(values = myColors) + 
  scale_fill_manual(values = myColors) +
  theme(
    #    legend.position = "none",
    panel.grid = element_line(size = .1),
    axis.ticks = element_line(color = "black", size = .25),
    axis.line = element_line(color = "black", size = .25),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    panel.border = element_rect(fill = NA, color = "gray"),
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 7)
  )

print(g2)

#ggsave(g2,
#       file = "SWF_matching_durations.svg",
#       width = 6, 
#       height = 2.5, 
#       units = "in",
#       dpi = 300)

