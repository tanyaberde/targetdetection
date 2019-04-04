# If you need to recode any variables, change RecodeVarsDot.R, re-source from CleanData.R
title = "Mean Keypress Speeds to Targets: \nControl Condition" ### Graph title

ConditionFocus <- c ("Control")

# Get only the Control Trials 
# data <- dat4.NoIncorrectEmpty.outlier.cutoff[(
#   (dat4.NoIncorrectEmpty.outlier.cutoff$RP) %in% ConditionFocus),]

data <- dat5[(
  (dat5$RP) %in% ConditionFocus),]


attach(data)

# Factorize subjects and factors
Subject = factor(Subject)
Reward = factor(RP)
Stimulus = factor(Stim)
Cue = factor(Cue)

table(Subject, Cue) # Check number of subjs/cells per condition

detach(data)

# Compute statistics and ddply over subjects
require(plyr)

meansCue=ddply(data,.(Subject,Cue),summarize,
               value=mean(Stim.RT))

mediansCue=ddply(data,.(Subject,Cue),summarize,
                 value=median(Stim.RT))

### Change handle
statFocus <- meansCue
# statFocus <- mediansCue

# Load Rmisc package to run summarySE function. Used to be # source("summarySE.R")
require(Rmisc)

Cuebar <- summarySEwithin(statFocus, 
                          measurevar="value", 
                          withinvars=c("Cue"), 
                          idvar="Subject")

# Use grouping vars as a factor rather than numeric

# Bar graph, Prediction on x-axis, color fill grouped by Outcome -- use position_dodge()
# Error bars represent standard error of the mean
require(ggplot2)

theme_set(theme_grey(base_size = 22))  ### Set default font size 

a.raw <- ggplot(Cuebar, aes(x=Cue, y=value, fill=Cue, colour=Cue)) + 
  geom_bar(stat="identity", position=position_dodge())
  # geom_errorbar(aes(ymin=value-se, ymax=value+se),
                #   geom_errorbar(aes(ymin=value-ci, ymax=value+ci),
                # width=.2,                    # Width of the error bars
                # position=position_dodge(.9))

a <- a.raw + coord_cartesian(ylim = c(390, 410)) # Stretch graph without cutting/changing the data

a.official <- a +
  #     scale_fill_brewer(type="qual", palette=1) + #Neat auto colors 
  scale_colour_manual(values=c(rep("snow3",3))) +
  scale_fill_manual(name="Cue", values=c("slategray", "sienna", "black")) +
  # ggtitle(title) +
  ylab("RT(ms)") + 
  xlab("Cue") +
#   coord_fixed(ratio=1/.3) +
#   scale_y_continuous(breaks=seq(-1.2,0,0.25), limits=c(-1.4,0)) + 
  guides(colour=F,fill=F)
#   guides(fill=guide_legend(title=c("Dot Validity","Expectation")))

# print(a)
print(a.official)

ggsave("pictures/Cue on Control Trials.png", ### Image file title
       a.official, width=5, height=8)
# ggsave("pictures/CNS graph.png", ### Image file title
#        a.cns, width=7, height=8)
