# Plot individual subject distributions

# source("task-poststim-barGraph.R")

xmin=100
xmax=600
xSkip=100

require(tidyverse)

# Plot histograms, facet by subject
# First get only the columns you need
histDat <- data %>% 
  select(Subject
         , Trial, TaskType, PrevTrial
         , Stim.RT)

# Calculate mean and sds per task for the group dnorm curve
meanLoc <- histDat %>% 
  filter(TaskType== "Location") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))

meanObj <- histDat %>% 
  filter(TaskType== "Object") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))

## Now per task and per previous stimtype
meanLocT <- histDat %>% 
  filter(TaskType== "Location") %>% 
  filter(PrevTrial=="Target") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))

meanLocR <- histDat %>% 
  filter(TaskType== "Location") %>% 
  filter(PrevTrial=="Reward") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))

meanLocN <- histDat %>% 
  filter(TaskType== "Location") %>% 
  filter(PrevTrial=="Nontarget") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))

meanObjT <- histDat %>% 
  filter(TaskType== "Object") %>% 
  filter(PrevTrial=="Target") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))

meanObjR <- histDat %>% 
  filter(TaskType== "Object") %>% 
  filter(PrevTrial=="Reward") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))

meanObjN <- histDat %>% 
  filter(TaskType== "Object") %>% 
  filter(PrevTrial=="Nontarget") %>% 
  summarise(meanRT = mean(Stim.RT), sdRT=sd(Stim.RT))


# Grand average histograms, with fitted curve
p <- ggplot(histDat,
            aes(x=Stim.RT
                ,fill=TaskType
                ,color=TaskType
            )) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=10,alpha=.6) +
  ylim(0,.01) +
  scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,by=xSkip)) +
  stat_function(fun = dnorm, color="red", args=list(mean=meanLoc$meanRT, sd=meanLoc$sdRT)) + 
  stat_function(fun = dnorm, color="blue", args=list(mean=meanObj$meanRT, sd=meanObj$sdRT)) +
  ggtitle("RT Distribution by Task") +
  theme_minimal()
print(p)

pLoc <- ggplot((histDat %>% filter(TaskType== "Location")),
            aes(x=Stim.RT
                ,fill=PrevTrial
                ,color=PrevTrial
            )) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=10,alpha=.6) +
  ylim(0,.01) +
  scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,by=xSkip)) +
  stat_function(fun = dnorm, color="blue", args=list(mean=meanLocT$meanRT, sd=meanLocT$sdRT)) + 
  stat_function(fun = dnorm, color="green", args=list(mean=meanLocR$meanRT, sd=meanLocR$sdRT)) +
  stat_function(fun = dnorm, color="orange", args=list(mean=meanLocN$meanRT, sd=meanLocN$sdRT)) +
  ggtitle("RT Distribution by Stimulus on Previous Trial in Location Task") +
  theme_minimal()
print(pLoc)

pObj <- ggplot((histDat %>% filter(TaskType== "Object")),
               aes(x=Stim.RT
                   ,fill=PrevTrial
                   ,color=PrevTrial
               )) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=10,alpha=.6) +
  ylim(0,.01) + 
  scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,by=xSkip)) +
  stat_function(fun = dnorm, color="blue", args=list(mean=meanObjT$meanRT, sd=meanObjT$sdRT)) + 
  stat_function(fun = dnorm, color="green", args=list(mean=meanObjR$meanRT, sd=meanObjR$sdRT)) +
  stat_function(fun = dnorm, color="orange", args=list(mean=meanObjN$meanRT, sd=meanObjN$sdRT)) +
  ggtitle("RT Distribution by Stimulus on Previous Trial in Object Task") +
  theme_minimal()
print(pObj)

# Individual subject histograms, no curves
q <- ggplot(histDat,
            aes(x=Stim.RT
                ,fill=TaskType
                ,color=TaskType
                )) +
  # geom_point(stat="count") +
  facet_wrap(~Subject) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=10, alpha=.6) +
  ylim(0,.04) +
  scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,by=xSkip)) +
  ggtitle("RT Distribution by Task per Participant") +
  theme_minimal()
print(q)

rLoc <- ggplot((histDat %>% filter(TaskType== "Location")),
            aes(x=Stim.RT
                # ,fill=TaskType
                ,fill=PrevTrial
                ,group=PrevTrial
            )) +
  # geom_point(stat="count") +
  facet_wrap(~Subject) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=10, alpha=.8) +
  ylim(0,.04) +
  scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,by=xSkip)) +
  ggtitle("RT Distribution per Participant: \nStimulus on Previous Trial in Location Task") +
  theme_minimal()
print(rLoc)

rObj <- ggplot((histDat %>% filter(TaskType== "Object")),
               aes(x=Stim.RT
                   # ,fill=TaskType
                   ,fill=PrevTrial
                   ,group=PrevTrial
               )) +
  # geom_point(stat="count") +
  facet_wrap(~Subject) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=10, alpha=.8) +
  ylim(0,.04) +
  scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,by=xSkip)) +
  ggtitle("RT Distribution per Participant: \nStimulus on Previous Trial in Object Task") +
  theme_minimal()
print(rObj)

ggsave("pictures/groupDist.png", ### Image file title
       p, width=6, height=6)

ggsave("pictures/indivDist.png", ### Image file title
       q, width=6, height=6)

ggsave("pictures/indivLocDist.png", ### Image file title
       rLoc, width=6, height=6)

ggsave("pictures/indivObjDist.png", ### Image file title
       rObj, width=6, height=6)

ggsave("pictures/groupDistLoc.png", ### Image file title
       pLoc, width=6, height=6)

ggsave("pictures/groupDistObj.png", ### Image file title
       pObj, width=6, height=6)

