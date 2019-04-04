# Sequence of scripts

# install.packages("tidyverse") # ggplot2 and dplyr
# install.packages("Rmisc") # Error bars
# install.packages("reshape")
# install.packages("data.table") # contains shift() function


## Open everything
file.edit("CleanData.R")
file.edit("RecodeVars.R")
file.edit("DropSubjects.R")
file.edit("DropTrials.R")
file.edit("outputCSV.R")
file.edit("task-barGraph.R") # Any of these depending on your conditions
# file.edit("poststim-barGraph.R")
file.edit("task-poststim-barGraph.R")
                          ## Others that did not pan out are under /abandoned
file.edit("dist.R")
file.edit("interf.R")
file.edit("quantiles.R")
file.edit("prevTrialCheck.R")


# file.edit("task-ttest.R")
# file.edit("ANOVA.R")
# file.edit("means.R")
