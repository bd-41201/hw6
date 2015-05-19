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

