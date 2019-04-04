# source("CleanData.R")
# source("RecodeVars.R")
# source("DropSubjects.R")
# source("DropTrials.R")

data <- dat3 # Use the recoded but unpruned data (only counting the correct keypresses tho)

title <- "Quantile plots of RTs to target on current trial"
numBins = 5
#============================================================
require(tidyverse)
data <- data %>% 
  filter(Trial!=1) %>% # Drop the first trial of every block
  select(Subject, Trial, TaskType, PrevTrial, Stim.RT) # Select only the columns you need

nSub = length(unique(data$Subject))

require(plyr)
quant.sub.cond = ddply(data,.(Subject,TaskType,PrevTrial),summarize,
                      quant = quantile(Stim.RT,probs=seq(.1,.9,.2))
                      )
numRows = nrow(quant.sub.cond)

# Get quantiles per subject x condition
quant.sub.cond <- quant.sub.cond %>% 
  mutate(prob = rep(seq(.1,.9,.2),numRows/numBins)) # Make new column containing probability bins that quantile drops above
attach(quant.sub.cond)
prob=factor(prob)
detach(quant.sub.cond)

# Now average across subjects for each quantile prob bin x condition
quant.cond = ddply(quant.sub.cond,.(TaskType,PrevTrial,prob),summarize,
                   quant=mean(quant)
                   )

# Plot it
p <- ggplot(quant.cond,
            aes(x=prob,
                y=quant,
                color=PrevTrial,
                group=PrevTrial)) +
  geom_point(stat="identity") +
  geom_line(stat="identity") +
  scale_x_continuous(limits=c(0,1),
                     breaks=c(0,.2,.4,.6,.8,1)
                     ) +
  facet_wrap(~TaskType) +
  scale_colour_manual(name="Stimulus on \n Previous Trial", values=c("tan2","green4","dodgerblue")) +
  ggtitle(title) +
  ylab("RT quantile mean (ms)") + 
  xlab("Response Proportion") +
  (theme_bw(base_size = 16))

print(p)

ggsave("pictures/Quantiles.png", ### Image file title
       p, width=6, height=8)
