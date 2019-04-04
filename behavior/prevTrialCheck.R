# Check that the PrevTrial variable was recoded properly. Should have correct number of trials after pruning first trial of each block.

source("RecodeVars.R")
require(tidyverse)

## Include all trials
t1 <- dat1 %>% 
  mutate(TT=case_when(
    (Trial!=1 & PrevTrial=="Target" & StimType=="Target") ~ "TaTa",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Target") ~ "ReTa",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Target") ~ "NoTa",
    
    (Trial!=1 & PrevTrial=="Target" & StimType=="Reward") ~ "TaRe",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Reward") ~ "ReRe",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Reward") ~ "NoRe",
    
    (Trial!=1 & PrevTrial=="Target" & StimType=="Nontarget") ~ "TaNo",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Nontarget") ~ "ReNo",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Nontarget") ~ "NoNo",
    TRUE ~ "Trial1"
  ))
head(t1)

t1_summ <- t1 %>% 
  group_by(Subject,TT) %>% 
  tally() %>% 
  select(Subject:n) %>%
  spread(key=TT,value=n)

t1_summ_total <- t1 %>% 
  group_by(Subject) %>% 
  tally()


## Include only correct target trials
t2 <- dat1 %>% 
  filter(RespType %in% c(1)) %>% 
  mutate(TT=case_when(
    (Trial!=1 & PrevTrial=="Target" & StimType=="Target") ~ "TaTa",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Target") ~ "ReTa",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Target") ~ "NoTa",
    
    (Trial!=1 & PrevTrial=="Target" & StimType=="Reward") ~ "TaRe",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Reward") ~ "ReRe",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Reward") ~ "NoRe",
    
    (Trial!=1 & PrevTrial=="Target" & StimType=="Nontarget") ~ "TaNo",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Nontarget") ~ "ReNo",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Nontarget") ~ "NoNo",
    TRUE ~ NA_character_ 
  ))
head(t2)

t2_summ <- t2 %>% 
  group_by(Subject,TT) %>% 
  tally() %>% 
  select(Subject:n) %>%
  spread(key=TT,value=n)

t2_summ_total <- t2 %>% 
  group_by(Subject) %>% 
  tally()


## Include only correct target/nontarget/reward trials
t3 <- dat1 %>%
  filter(RespType %in% c(1,2,3)) %>% 
  mutate(TT=case_when(
    (Trial!=1 & PrevTrial=="Target" & StimType=="Target") ~ "TaTa",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Target") ~ "ReTa",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Target") ~ "NoTa",
    
    (Trial!=1 & PrevTrial=="Target" & StimType=="Reward") ~ "TaRe",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Reward") ~ "ReRe",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Reward") ~ "NoRe",
    
    (Trial!=1 & PrevTrial=="Target" & StimType=="Nontarget") ~ "TaNo",
    (Trial!=1 & PrevTrial=="Reward" & StimType=="Nontarget") ~ "ReNo",
    (Trial!=1 & PrevTrial=="Nontarget" & StimType=="Nontarget") ~ "NoNo",
    TRUE ~ NA_character_ 
  ))
head(t3)

t3_summ <- t3 %>% 
  group_by(Subject,TT) %>% 
  tally() %>% 
  select(Subject:n) %>%
  spread(key=TT,value=n)

t3_summ_total <- t3 %>% 
  group_by(Subject) %>% 
  tally()
