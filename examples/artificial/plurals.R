##
## Analysis of artificial plurals data from NDL package
##

source("../utilities.R")

if (FALSE) {
  ## convert plurals data to standard format from theory paper
  library(ndl)
  data(plurals)

  plurals$Cues <- orthoCoding(plurals$WordForm, grams=1)
  X.pl.types <- ortho2matrix(plurals$Cues, row.names=plurals$WordForm)
  Z.pl.types <- ortho2matrix(plurals$Outcomes, row.names=plurals$WordForm)

  ## X and Z encode event tokens in mathematical approach --> need to replicate rows
  idx <- rep(1:nrow(plurals), times=plurals$Frequency)
  X.pl <- X.pl.types[idx, ]
  Z.pl <- Z.pl.types[idx, ]
  
  save(X.pl, Z.pl, X.pl.types, Z.pl.types, file="plurals.rda") # save matrices, so examples don't depend on NDL package
}

load("plurals.rda", verbose=TRUE)

## X.pl and Z.pl contain these types with suitable repetition counts
X.pl.types
Z.pl.types

## W-H learning and Danks equilibrium for outcome PLURAL
X <- X.pl # note that "a" forms a background cue in this example
Z <- Z.pl[, "PLURAL", drop=FALSE]

## co-occurrence counts for Danks equilibrium
fOC <- crossprod(X, Z)
fCC <- crossprod(X)

fOC  # both look inconspicuous at first sight
fCC

round(svd(fCC)$d, 3) # and fCC is reasonably well conditioned

## TODO: test problematic case found by Antti (stochastic RW learner vs. sequential RW vs. expectation)

## TODO: fCC becomes singular if explicit background cue is added -> possible cause of problems?

