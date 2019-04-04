
rm(list=ls())
# You first want to merge all individual subject edat files into one using E-Merge
# Then read the data
raw.data <- read.delim("raw/TDS 071_096 merge.txt"
                          # ,skip = 1
                       ,header = T
                       ,na.strings = ""
                       ,fileEncoding = "UTF-16" # Damn you, encoding!
                       )

# # # If E-Merge isn't handy, read each individual file here and merge later
# d1 <- read.delim("raw/TDS 071_084 merge.txt",
# #                  skip = 1,
#                  header = T,
#                  na.strings = "",
#                  fileEncoding = "UTF-16")
# #
# d2 <- read.delim("raw/TDS-LD-080-1.txt",
#                  #                  skip = 1,
#                  header = T,
#                  na.strings = "",
#                  fileEncoding = "UTF-16")
# #
# # d3 <- read.delim("raw/MSAS 055_063.txt", 
# #                  #                  skip = 1,
# #                  header = T,
# #                  na.strings = "",
# #                  fileEncoding = "UTF-16")

# Merge
# raw.data <- merge(d1, d2, all=T)
# raw.data <- merge(raw.data, d3, all=T)

# rm(d1,d2,d3,d4) # Clean up

# Drop unnecessary columns/E-Prime attributes, update datafile
drops <- c("Age","BlockDisplay1", "BlockDisplay2", "BlockDisplay3", "BlockDisplay4",
           "BlockDisplay5", "BlockDisplay6", 
           "Clock.Information", "Clock.Scale", "DataFile.Basename", "Display.RefreshRate", 
           "ExperimentVersion", "Group", "Handedness", "RandomSeed", "ResearcherID", 
           "RuntimeCapabilities", "RuntimeVersion", "RuntimeVersionExpected", 
           "SessionStartDateTimeUtc", "SessionTime", "StudioVersion", 
           "BlocList","BlockList.Cycle", "BlockList.Sample", "Procedure.Block.", "Running.Block." ,
           "ListA.Cycle", "ListA.Sample", "ListB.Cycle", "ListB.Sample",
           "PracListA.Cycle", "PracListA.Sample", "PracListB.Cycle", "PracListB.Sample",
           "Feedback.ACC", "Feedback.CRESP", "Feedback.DurationError", "Feedback.OnsetDelay", "Feedback.OnsetTime", "Feedback.OnsetToOnsetTime", "Feedback.RESP", "Feedback.RT", "Feedback.RTTime",
           "Feedback1.ACC", "Feedback1.CRESP", "Feedback1.DurationError", "Feedback1.OnsetDelay", "Feedback1.OnsetTime", "Feedback1.OnsetToOnsetTime", "Feedback1.RESP", "Feedback1.RT", "Feedback1.RTTime",
           "Stim.DurationError", "Stim.OnsetDelay", "Stim.OnsetTime", "Stim.OnsetToOnsetTime", "Stim.RTTime",
           "Stim1.ACC", "Stim1.CRESP", "Stim1.DurationError", "Stim1.OnsetDelay", "Stim1.OnsetTime", "Stim1.OnsetToOnsetTime", "Stim1.RTTime"
           ) 

dat <- raw.data[,!(names(raw.data) %in% drops)]

rm(raw.data, drops) # Clean up
