## Homework 6 - Analyzing congressional speech

# Before starting on the questions we need to do a little setup
# Import the 'textir' library
library(textir)

# Pull in the congress109 data
data(congress109)
# This brings in two sets: congress109Counts and congress109Ideology
# congress109Counts has representatives as the rows and speech n-grams
# as the columns with each intersection representing a count
# congress109Ideology has information on each representative including
# party, state, chamber, repshare => the share of the representatives
# district that voted for George Bush in 2004, cs1, cs2 => show a
# measure of how strongly a given representative votes along party lines

## Q1 - Fit K -means to speech text for K in 5,10,15,20,25. Use BIC to
## choose the K and interpret the selected model.

# Following the w8there example, we scale the counts by computing the
# frequency they appear relative to the average.
cong.counts.scaled <- scale(as.matrix( congress109Counts/rowSums(congress109Counts) ))

# Compute k-means for 5,10,15,20,25 groups
kfit.ccs <- lapply(c(5,10,15,20,25), function(k) kmeans(cong.counts.scaled,k))

# Use the kIC script to choose the appropriate # of clusters
source("../Utility Scripts/kIC.R")
kbic.ccs <- sapply(kfit.ccs,kIC,"B")

# Let's plot to see what it looks like
kaicc.ccs <- sapply(kfit.ccs,kIC)
## plot 'em
plot(c(5,10,15,20,25),kaicc.ccs, xlab="K", ylab="IC",
  ylim=range(c(kaicc.ccs,kbic.ccs)),xlim=c(5,25),
  bty="n", type="l", lwd=2)
abline(v=which.min(kaicc.ccs)*5,lty=2)
lines(c(5,10,15,20,25),kbic.ccs, col=4, lwd=2)
abline(v=which.min(kbic.ccs)*5,col=4,lty=2)
legend(6,600000,c("AICc","AICc Min","BIC","BIC Min"),lty=c(1,2,1,2),col=c("black","black","blue","blue"))
# Not a good picture as aicc appears to select a very complex model with
# >25 clusters and bic looks to select potentially no models at all.

# Within the bounds of this problem, we use BIC to select 5 clusters.
summary(kfit.ccs[1])


