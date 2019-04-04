# Inherited from statistics/P1 N1 2Way
# Statistics for ___ project
# Factors: ____ x ____
# Equivalent to segmentation to <stim>, according to <segmentation tool>

# First, assign wherever your factorized mean RT-per-subject data are to object called "melted"
melted <- meanSubxTasktype 

names(melted)[names(melted)=="TaskType"] <- "Task" ### Rename the damn column

#Factorize first
melted$Subject = factor(melted$Subject) ### Change condition name
melted$Task = factor(melted$Task) ### Change condition name
melted$value = as.numeric(as.character(melted$value))

# Paired contrasts, one-tailed if you comment in 'alternative="greater"' back
task.ttest <- with(
  melted
  #, subset(melted, DotValidity=="Valid", select= -DotValidity)
              ,t.test(value[Task == "Location"], 
                     value[Task == "Object"],
                     paired=T
                     #                      ,alternative="greater"
              ))


# Print results
task.ttest
