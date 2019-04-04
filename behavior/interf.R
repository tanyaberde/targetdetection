xmin=100
xmax=600
xSkip=100
title <- "Median RTs to Target on Current Trial"
#============================================================
require(tidyverse)

data <- subset(dat5,Trial!=1) # Drop the first trial of every block
nSub = length(unique(data$Subject))

attach(data)
# Factorize subjects and factors
Subject = factor(Subject)
TaskType = factor(TaskType)
PrevTrial = factor(PrevTrial)
detach(data)

# First get only the columns you need
histDat <- data %>% 
  select(Subject, Trial, TaskType, PrevTrial, Stim.RT)

# Filter out the trials based on artbitrary RT cutoff
binDat <- histDat %>% 
  mutate(speedBin = case_when(
    (Stim.RT <= 350) ~ "Fast",
    (Stim.RT >= 400) ~ "Slow",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(speedBin)) ## Take out the NA values in speedBin aka neither Fast nor Slow

# Check number of trials
binDatn <- binDat %>% 
  group_by(Subject,TaskType,
           # PrevTrial,
           speedBin) %>% 
  tally()

fastObjn <- binDat %>% 
  group_by(Subject,TaskType,
           speedBin) %>% 
  filter(speedBin=="Fast" & TaskType=="Object") %>% 
  tally()

slowLocn <- binDat %>% 
  group_by(Subject,TaskType,
           speedBin) %>% 
  filter(speedBin=="Slow" & TaskType=="Location") %>% 
  tally()


# Calculate means or medians per subject x condition
meanSubxCond=ddply(binDat,.(Subject,TaskType,PrevTrial,speedBin),summarize,
                 value=mean(Stim.RT))

mdSubxCond=ddply(binDat,.(Subject,TaskType,PrevTrial,speedBin),summarize,
                 value=median(Stim.RT))

require(Rmisc)
binMeans <- summarySEwithin(meanSubxCond,
                            measurevar="value",
                            withinvars=c("TaskType","PrevTrial","speedBin"),
                            idvar = "Subject")


binMd <- summarySEwithin(mdSubxCond,
                           measurevar="value",
                           withinvars=c("TaskType","PrevTrial","speedBin"),
                           idvar = "Subject")


# Histograms
s <- ggplot(binDat,
               aes(x=Stim.RT
                   ,fill=PrevTrial
                   # ,color=PrevTrial
               )) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=10,alpha=.5) +
  ylim(0,.01) +
  scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,by=xSkip)) +
  facet_wrap(~TaskType) +
  # stat_function(fun = dnorm, color="blue", args=list(mean=meanLocT$meanRT, sd=meanLocT$sdRT)) + 
  # stat_function(fun = dnorm, color="green", args=list(mean=meanLocR$meanRT, sd=meanLocR$sdRT)) +
  # stat_function(fun = dnorm, color="orange", args=list(mean=meanLocN$meanRT, sd=meanLocN$sdRT)) 
  ggtitle("RT Distribution by Stimulus on Previous Trial") +
  theme_minimal()
print(s)

# Bar graphs of means

t <- ggplot(binMd,
            aes(x=speedBin
                ,y=value
                ,fill=PrevTrial
)) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  facet_wrap(~TaskType) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  # geom_errorbar(aes(ymin=value-ci, ymax=value+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(250, 450)) +
  scale_fill_manual(name="Stimulus on \n Previous Trial", values=c("tan2","green4","dodgerblue")) +
  ggtitle(title) +
  ylab("RT(ms)") + 
  xlab("Speed") +
  (theme_minimal(base_size = 16))
print(t)

ggsave("pictures/Task x Prev Trial x Speed md.png", ### Image file title
       t, width=6, height=8)
