# If you need to recode any variables, change RecodeVarsDot.R, re-source from CleanData.R
title = "Mean RTs to Targets" ### Graph title

data <- subset(dat5,Trial!=1) # Drop the first trial of every block

attach(data)

# Factorize subjects and factors
Subject = factor(Subject)
PrevTrial = factor(PrevTrial)
# table(Subject, TaskType, PrevTrial) # Check number of subjs/cells per condition
detach(data)

# Compute statistics and ddply over subjects
require(plyr)

# new value = old value – subject average + grand average

meanSubxCond=ddply(data,.(Subject,PrevTrial),summarize,
                   value=mean(Stim.RT)
)
# 
# grandAveRT = mean(data$Stim.RT) # Grand average RT
# 
# dataNormed <- merge(data,meanSubxRPxCue, by.x="Subject", by.y="Subject", all.x=T)
# 
# dataNormed$subNormRT = dataNormed$Stim.RT - dataNormed$subAve + grandAveRT
# 
# meansRPxCue=ddply(dataNormed,.(Subject,RP,Cue),summarize,
#                   value=mean(subNormRT)
#                   )
# mediansRPxCue=ddply(data,.(Subject,RP,Cue),summarize,
#                            value=median(Stim.RT)
# )

### Change handle
statFocus <- meanSubxCond
# statFocus <- mediansRPxCue

# Load Rmisc package to run summarySE function. Used to be # source("summarySE.R")
require(Rmisc)

# # Norm <- normDataWithin(data=statFocus, idvar="Subject", measurevar="value", betweenvar="group")
# 
# summWithin <- summarySEwithin(statFocus,
#                          measurevar="value",
#                          withinvars=c("RP", "Cue"),
#                          betweenvars="group",
#                          idvar="Subject")
# 
# summNormed <- summarySE(Norm,
#                               measurevar="valueNormed"
#                               # withinvars=c("RP", "Cue"),
#                               # betweenvars="group",
#                               # idvar="Subject"
#                         )
# 
# summWithin$valueNormed = summNormed$valueNormed
# 
Condbar <- summarySEwithin(statFocus,
                           measurevar="value",
                           withinvars=c("PrevTrial"),
                           idvar = "Subject")

#==========================================================================================
# Normalize subject average for accurate error bars -- FOR VISUALIZATION ONLY, NOT STATISTICS


#==========================================================================================

# Use grouping vars as a factor rather than numeric

# Bar graph, Prediction on x-axis, color fill grouped by Outcome -- use position_dodge()
# Error bars represent standard error of the mean
require(ggplot2)

theme_set(theme_grey(base_size = 18))  ### Set default font size 

a.raw <- ggplot(Condbar, aes(x=PrevTrial, y=value, fill=PrevTrial
)) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                #   geom_errorbar(aes(ymin=value-ci, ymax=value+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

a <- a.raw + coord_cartesian(ylim = c(315, 375)) # Stretch graph without cutting/changing the data

a.official <- a +
      scale_fill_brewer(type="qual", palette=1) + #Neat auto colors
  # scale_fill_manual(name="TaskType", values=c("palevioletred", "palegreen3")) +  
  # scale_colour_manual(name="Cue", values=c("slategray", "sienna", "black")) +
  ggtitle(title) +
  ylab("RT(ms)") + 
  xlab("Stimulus on Previous Trial") +
# coord_fixed(ratio=1/.3) +
#   scale_y_continuous(breaks=seq(-1.2,0,0.25), limits=c(-1.4,0)) + 
guides(fill=F,colour=F)
#   guides(fill=guide_legend(title=c("Dot Validity","Expectation")))

# print(a)
print(a.official)

ggsave("pictures/Prev Trial.png", ### Image file title
       a.official, width=5, height=8)
# ggsave("pictures/CNS graph.png", ### Image file title
#        a.cns, width=7, height=8)
