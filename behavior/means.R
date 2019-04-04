# Statistics for ___ project
# Factors: ____ x ____
# Equivalent to segmentation to <stim>, according to <segmentation tool>

# install.packages("ez") # Un-comment back in if you don't have these packages.

# GribbleLab tutorial note: To adhere to the sum-to-zero convention 
# for effect weights, you should always do this before running anovas in R.
# This ensures your output matches what SPSS would spit out.
# Thanks to this blog post: 
# https://gribblelab.wordpress.com/2009/03/09/repeated-measures-anova-using-r/
options(contrasts=c("contr.sum","contr.poly"))

#Factorize first
melted$Expect = factor(melted$Expect) ### Change condition name
melted$DotValidity = factor(melted$DotValidity) ### Change condition name
melted$Subject = factor(melted$Subject)
melted$value = as.numeric(as.character(melted$value))

# Un-comment lines 23-28 below if you want to use R's native anova method. 
# You have to put in the actual condition names instead of 'cond1' and 'cond2'.
# Traditional R ANOVA
options(contrasts = c("contr.helmert", "contr.treatment"))
anova<-aov(value~(Expect*DotValidity)+Error(Subject/Expect)+DotValidity, data=melted) ## DV ~ IV or IV*IV2
anova.summary <- summary(anova)
anova.effects <- model.tables(anova)

# Means
anova.means <- model.tables(anova, type="means", se=T)

# Print outputs
s <- as.matrix(anova.summary) # Turns results into a matrix
write.csv(s, file="anova-summary.csv")

capture.output(anova.means, 
               file="test.doc")
