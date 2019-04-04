# Output dataframes to csv
# These dataframes could be from any of the scripts so check those first

# write.csv(dat4.NoIncorrectEmpty.outlier.cutoff, "recoded raw/dat4.NoIncorrectEmpty.outlier.cutoff.csv", row.names=F)
# write.csv(dat4.NoIncorrectEmpty.NoOutlier, "recoded raw/dat4.NoIncorrectEmpty.NoOutlier.csv", row.names=F)

# write.csv(dat5, "recoded raw/dat5.csv", row.names=F)
#==========================================================================================
# First trial of each block pruned
require(reshape)
require(tidyverse)
Task.PrevTrial.RT.spss <- cast(statFocus, Subject ~ TaskType + PrevTrial) # Prep data file for SPSS

Task.PrevTrial.RT.spss <- Task.PrevTrial.RT.spss %>% ## Reorder columns for consistency with ERP condition order
  select(Subject, 
         Location_Target, Location_Reward, Location_Nontarget,
         Object_Target, Object_Reward, Object_Nontarget)

write.csv(Task.PrevTrial.RT.spss, "recoded raw/SPSS-RT-Task-PrevTrial-Data.csv", row.names=F)

#==========================================================================================
require(reshape)

Task.RT.spss <- cast(statFocus, Subject ~ TaskType) # Prep data file for SPSS
write.csv(Task.RT.spss, "recoded raw/SPSS-RT-Task-Data.csv", row.names=F)
