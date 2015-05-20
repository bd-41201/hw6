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
# png('aic_bic_vs_k.png')
plot(c(5,10,15,20,25),kaicc.ccs, xlab="K", ylab="IC",
  main="IC vs Number of clusters (K)",ylim=range(c(kaicc.ccs,kbic.ccs)),xlim=c(5,25),
  bty="n", type="l", lwd=2)
abline(v=which.min(kaicc.ccs)*5,lty=2)
lines(c(5,10,15,20,25),kbic.ccs, col=4, lwd=2)
abline(v=which.min(kbic.ccs)*5,col=4,lty=2)
legend(6,600000,c("AICc","AICc Min","BIC","BIC Min"),lty=c(1,2,1,2),col=c("black","black","blue","blue"))
# dev.off()
# Not a good picture as aicc appears to select a very complex model with
# >25 clusters and bic looks to select potentially no models at all.

# Within the bounds of this problem, we use BIC to select 5 clusters.
kfit.ccs.5c <- kfit.ccs[[1]]
kfit.ccs.5c.slices <- unlist(lapply(1:5, function(x) length(kfit.ccs.5c$cluster[kfit.ccs.5c$cluster==x])))

lbls <- c("Cluster 1 - ", "Cluster 2 - ", "Cluster 3 - ", "Cluster 4 - ", "Cluster 5 - ")
pct <- round(kfit.ccs.5c.slices/sum(kfit.ccs.5c.slices)*100)
lbls <- paste(lbls, kfit.ccs.5c.slices)
lbls <- paste(lbls, " (")
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%)",sep="") # ad % to labels
pie(kfit.ccs.5c.slices,labels = lbls, col=rainbow(length(lbls)),
    main="Share of Representatives by Cluster",clockwise=TRUE,cex=0.75)

cluster4 <- kfit.ccs.5c$cluster[kfit.ccs.5c$cluster==4]
cong.109.repub <- congress109Ideology[congress109Ideology$party=="R",]
head(cong.109.repub)

## Q2
# Bring in the topic maps
library(maptpx)

c109.stm <- as.simple_triplet_matrix(congress109Counts)
c109.tpcs <- topics(c109.stm,K=5*(1:5), verb=10)
# Bayes factor is maximized at K=10 so we select that for our topic model

# Print the most frequently used words by topic
lapply(1:10, function(x) rownames(c109.tpcs$theta)[order(c109.tpcs$theta[,x], decreasing=TRUE)[1:10]])

# Show a word cloud to visualize common words from each topic
par(mfrow=c(1,2))
wordcloud(row.names(c109.tpcs$theta),
  freq=c109.tpcs$theta[,1], min.freq=0.006, col="maroon")
wordcloud(row.names(c109.tpcs$theta),
  freq=c109.tpcs$theta[,2], min.freq=0.006, col="navy")


