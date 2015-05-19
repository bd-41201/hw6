# Homework 6

[1] Fit K -means to speech text for K in 5,10,15,20,25.
Use BIC to choose the K and interpret the selected model.

[2] Fit a topic model for the speech counts. Use Bayes factors to choose the number of topics, and interpret your chosen model.

[3] Connect the unsupervised clusters to partisanship.

.,. tabulate party membership by K -means cluster. Are there any non-partisan topics?

.,. fit topic regressions for each of party and repshare. Compare to regression onto phrase percentages:

x<-100*congress109Counts/rowSums(congress109Counts)
