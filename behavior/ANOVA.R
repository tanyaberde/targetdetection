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
meansExpectxDotVal$Expect = factor(meansExpectxDotVal$Expect) ### Change condition name
meansExpectxDotVal$DotVal = factor(meansExpectxDotVal$DotVal) ### Change condition name
meansExpectxDotVal$Subject = factor(meansExpectxDotVal$Subject)
meansExpectxDotVal$value = as.numeric(as.character(meansExpectxDotVal$value))

# # Un-comment lines 23-28 below if you want to use R's native anova method. 
# # You have to put in the actual condition names instead of 'cond1' and 'cond2'.
# # Traditional R ANOVA
# anova<-aov(RT~(cond1*cond2)+Error(Subject/cond1)+cond2, data=meansExpectxDotVal) ## DV ~ IV or IV*IV2
# summary(anova)
# model.tables(anova)

# Check number of subjs/cells per condition
table(meansExpectxDotVal$Expect, meansExpectxDotVal$DotVal) ### Change condition names

# cond1 = "Expect"
# cond2 = "DotVal"

# Do ANOVA using ez package
require(ez)
ez.ANOVA <- ezANOVA(data=meansExpectxDotVal, dv=.(value), 
                    wid=.(Subject), 
                    within=.(Expect, DotVal), ### Change condition names
                    detailed=T) 

print(ez.ANOVA)
e <- as.data.frame(ez.ANOVA) # Turns results into a table
write.csv(e,             # Exports e table as csv
          file="anova-out.csv") ### Change filename as desired