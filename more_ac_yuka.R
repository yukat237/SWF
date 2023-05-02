#########################################
###  Yuka acoustic data exploration   ###
#########################################


#Just seeing some examples

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



#==========================trash================================
  
  file_type_list<- lapply(exfiles, function(file) {
    name<-basename(file)
  })
  
semitonef0<-read.delim("/Users/yzt5262/OneDrive - The Pennsylvania State University/Desktop/Eric's study (SWF)/yuk_ac_analysis/pair1/P1_lu2zhuren_B4_T3_Matching.semitonef0")

#take the sound out
exdat <- exdat[1:21]

exdat <- exfiles %>%
  set_names(.) %>%
  map_df(read_table2, .id = "FileName")