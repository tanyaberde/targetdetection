# Drop subjects because #reasons

Subject.Drops <- c(
 # "example sub number here"
"94"  )

# Drop cases where subject numbers match the ones in Subject.Drops
dat1 <- dat1[!((dat1$Subject) %in% Subject.Drops),]
