# Drop practice trials by only counting Procedure.Trial.=="TrialProc"
dat1 <- subset(dat, Procedure.Trial.=="TrialProc")

# Make a new variable that for the current row is the StimType of the previous row for each participant
require(data.table)
require(tidyverse)

dat1$PrevTrial <- shift(dat1$StimType, 1L, type = "lag")

dat1 <- dat1 %>% 
  mutate(VF = case_when(
    (LocIndex==1) ~"Left",
    (LocIndex == 3) ~"Left",
    (LocIndex == 2) ~"Right",
    (LocIndex == 4) ~"Right",
    TRUE ~ NA_character_
  )
  )


# Code as 0 (false) or 1 (true)...
# but only if it's not the first trial of each block (Trial != 1)
# attach(dat1)

# dat1$PrevTrialStim = ifelse((StimType=="Target" & PrevTrial=="Reward" & Trial!=1),"Reward"
#                             ,ifelse((StimType=="Target" & PrevTrial=="Nontarget" & Trial!=1),"Nontarget"
#                                     ,ifelse((StimType=="Target" & PrevTrial=="Target" & Trial!=1),"Target","0")))

# dat1$RewTarg = ifelse((StimType=="Target" & PrevTrial=="Reward" & Trial!=1),1,0) # if current trial is a target and previous trial was a reward:
# dat1$NntTarg = ifelse((StimType=="Target" & PrevTrial=="Nontarget" & Trial!=1),1,0) # if current trial is a target and previous trial was a nontarget:
# dat1$TarTarg = ifelse((StimType=="Target" & PrevTrial=="Target" & Trial!=1),1,0) # if current trial is a target and previous trial was a target:
# detach(dat1)

# # Check that no first trials have been coded as yeses:
# check <- subset(dat1,Trial==1,select=c(Trial,StimType,PrevTrial,RewTarg,NntTarg,TarTarg))
