# Drop trials if they meet certain criteria, once you've dropped problematic subjects

require(plyr)
dat2 <- dat1
total.trials = 672 ###

# # The following steps occur in sequence. Make sure step 0 (remove practice trials) has been followed.
# 
# # 1. Drop trials containing erroneous RespTypes (RespType = 3)
# dat2.NoIncorrect <- dat2[!(dat2$RespType=="3"), ]
# 
# ## Calculate mean number of error trials per subject (count RespType = 3's per subject, get average)
# Subject.by.RespType.by.Condition <- count(dat2, 
#                                           c("Subject","RespType", 
#                                             "RP", "Stim", "Cue")) # Count of trials each Subject made each type of "RespType"
# 
# Subject.by.RespType <- Subject.by.RespType.by.Condition
# 
# nb.incorrect.trials.per.sub <- subset(Subject.by.RespType, RespType=="3") # Get only the counts of incorrect RespTypes
#   attach(nb.incorrect.trials.per.sub) 
#   mean.nb.incorrect.trials = mean(freq) # Mean
#   sd.nb.incorrect.trials = sd(freq) # SD
#   min.nb.incorrect.trials = min(freq) # Min
#   max.nb.incorrect.trials = max(freq) # Max
#   detach(nb.incorrect.trials.per.sub)
# 
# 
# mean.incorrect.trials.per.sub=ddply(nb.incorrect.trials.per.sub,
#                             .(Subject,RP,Stim,Cue),summarize,
#                             mean.freq.incorrect.trials=mean(freq))
# 
# mean.incorrect.trials=ddply(nb.incorrect.trials.per.sub,
#                  .(RP,Stim,Cue),summarize,
#                          mean.freq.incorrect.trials=mean(freq),
#                         sd.freq.nonRespType.trials=sd(freq))
# 
# percent.incorrect.trials=ddply(nb.incorrect.trials.per.sub,
#                             .(RP,Stim,Cue),summarize,
#                             prop.mean.incorrect.trials=mean(freq)/total.trials,
#                             prop.sd.incorrect.trials=sd(freq)/total.trials)
# 
# 
# #==========================================================================================
# # 2. Remove trials without a RespType (RespType = 0)
# 
# dat2.NoIncorrectEmpty <- dat2.NoIncorrect[!(dat2.NoIncorrect$RespType=="0"), ]
# 
# ## Calculate mean number of non-RespType trials per subject (count RespType = 0's per subject, get average)
# 
# nb.nonRespType.trials.per.sub <- subset(Subject.by.RespType, RespType=="0") # Get only the counts of nonRespType RespTypes
#   attach(nb.nonRespType.trials.per.sub) 
#   mean.nb.nonRespType.trials = mean(freq) # Mean
#   sd.nb.nonRespType.trials = sd(freq) # SD
#   min.nb.nonRespType.trials = min(freq) # Min
#   max.nb.nonRespType.trials = max(freq) # Max
#   detach(nb.nonRespType.trials.per.sub)
# 
# mean.nonRespType.trials.per.sub=ddply(nb.nonRespType.trials.per.sub,
#                                     .(Subject,RP,Stim,Cue),summarize,
#                                     mean.freq.nonRespType.trials=mean(freq))
# 
# mean.nonRespType.trials=ddply(nb.nonRespType.trials.per.sub,
#                             .(RP,Stim,Cue),summarize,
#                             mean.freq.nonRespType.trials=mean(freq),
#                             sd.freq.nonRespType.trials=sd(freq))
# 
# percent.nonRespType.trials=ddply(nb.nonRespType.trials.per.sub,
#                                .(RP,Stim,Cue),summarize,
#                                prop.mean.nonRespType.trials=mean(freq)/total.trials,
#                                prop.sd.nonRespType.trials=sd(freq)/total.trials)

#==========================================================================================
# Just count the ones coded RespType = 1 and 3 because they're the only ones where the person had to hit a key

## Many ways to do this. Either drop the ones to be disregarded:
# resDrops <- c("2", "4", "5", "0") # Disregard these resDrops
# dat3 <- dat2[!((dat2$RespType) %in% resDrops),]

## Or only subset out the ones to be counted:
resKeeps <- c("1") #1 was coded for trials where they correctly pressed the key to a Target
dat3 <- dat2[((dat2$RespType) %in% resKeeps),]

#==========================================================================================
# 4. Remove individual subject outliers (RT > 3 SD from own mean) from data that 
# should be free of practice trials, erroneous RespTypes, and non-RespTypes.

# dat3 <- dat2.NoIncorrectEmpty

attach(dat3)
## Factorize subjects and factors
Subject = factor(Subject)
TaskType = factor(TaskType)
StimType = factor(StimType)
# table(Subject, Task, Stim) # Check number of subjs/cells per condition
detach(dat3)

## For each subject, calculate mean RT, SD
require(plyr)
mean.RT.per.subject = ddply(dat3,.(Subject),summarize,
                            meanRT=mean(Stim.RT), # Mean
                            sdRT=sd(Stim.RT), # SD
                            cutoff.coeff=3*sd(Stim.RT), # 3*SD
                            lb.cutoff=meanRT-cutoff.coeff, # Mean - 3*SD
                            ub.cutoff=meanRT+cutoff.coeff # Mean + 3*SD
                            ) 

## Update entire original dataset by matching each person to their individual cutoff values
dat4.NoIncorrectEmpty.outlier.cutoff <- merge(dat3,mean.RT.per.subject, by.x="Subject", by.y="Subject",
                                      all.x=T)

dat4.NoIncorrectEmpty.outlier.cutoff$Is.Outlier = ifelse((
  (dat4.NoIncorrectEmpty.outlier.cutoff$Stim.RT < dat4.NoIncorrectEmpty.outlier.cutoff$lb.cutoff)
  | 
    (dat4.NoIncorrectEmpty.outlier.cutoff$Stim.RT > dat4.NoIncorrectEmpty.outlier.cutoff$ub.cutoff)
  ),
  1, 0)

## Count outlier trials per subject
Subject.by.Outlier.by.Condition <- count(dat4.NoIncorrectEmpty.outlier.cutoff, 
                            c("Subject","Is.Outlier", 
                              "TaskType", "StimType")) # Count of outlier trials each Subject has

Subject.by.Outlier <- Subject.by.Outlier.by.Condition

nb.outlier.trials.per.sub <- subset(Subject.by.Outlier, Is.Outlier=="1") # Get only the counts of trials marked Outlier
  attach(nb.outlier.trials.per.sub)
  mean.nb.outlier.trials = mean(freq) # Mean
  sd.nb.outlier.trials = sd(freq) # SD
  min.nb.outlier.trials = min(freq) # Min
  max.nb.outlier.trials = max(freq) # Max
  detach(nb.outlier.trials.per.sub)
# 
# mean.outlier.trials.per.sub=ddply(nb.outlier.trials.per.sub,
#                                       .(Subject,RP,Stim,Cue),summarize,
#                                       mean.freq.outlier.trials=mean(freq))
# 
# mean.outlier.trials=ddply(nb.outlier.trials.per.sub,
#                               .(RP,Stim,Cue),summarize,
#                               mean.freq.outlier.trials=mean(freq),
#                               sd.freq.outlier.trials=sd(freq))
# 
# percent.outlier.trials = ddply(nb.outlier.trials.per.sub,
#                                  .(RP,Stim,Cue),summarize,
#                                  prop.mean.outlier.trials=mean(freq)/total.trials,
#                                  prop.sd.outlier.trials=sd(freq)/total.trials)

## Drop trials where RT is shorter than cutoff lower bound or longer than cutoff upper bound

# dat4.NoIncorrectEmpty.NoOutlier <- dat4.NoIncorrectEmpty.outlier.cutoff[
#   !(
#     (dat4.NoIncorrectEmpty.outlier.cutoff$Stim.RT < dat4.NoIncorrectEmpty.outlier.cutoff$lb.cutoff)
#      |
#       (dat4.NoIncorrectEmpty.outlier.cutoff$Stim.RT > dat4.NoIncorrectEmpty.outlier.cutoff$ub.cutoff)
#     ), ]

dat5 <- subset(dat4.NoIncorrectEmpty.outlier.cutoff, Is.Outlier=="0") 

## For now, dat5 is my final dataset
# View(dat5)

## =======

# ## Count final number of (counted) accurate, on-time trials per condition per subject
# Cleaned.Subject.by.RespType.by.Condition <- count(dat2.NoIncorrectEmpty.NoOutlier, 
#                                           c("Subject", "Expect", "DotVal")) # Count of trials each Subject made in each condition
# 
# 
# cleaned.mean.trials.per.sub=ddply(Cleaned.Subject.by.RespType.by.Condition,
#                                   .(Subject,Expect,DotVal),summarize,
#                                   mean.freq.trials=mean(freq))
# 
# cleaned.mean.trials=ddply(Cleaned.Subject.by.RespType.by.Condition,
#                           .(Expect,DotVal),summarize,
#                           mean.freq.trials=mean(freq),
#                           sd.freq.trials=sd(freq))
# 
# cleaned.percent.trials=ddply(Cleaned.Subject.by.RespType.by.Condition,
#                              .(Expect,DotVal),summarize,
#                              prop.mean.freq.trials=mean(freq)/total.trials,
#                              prop.sd.freq.trials=sd(freq)/total.trials)
