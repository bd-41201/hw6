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
