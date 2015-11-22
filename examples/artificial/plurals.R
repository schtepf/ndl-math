##
## Analysis of artificial plurals data from NDL package
##

source("../utilities.R")

quartz(width=8, height=5)
par(mar=c(2,4,2,1)+.1)


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

## R-W learning for outcome PLURAL (cue "s" doesn't seem to converge to equilibrium state) 
res.orig <- rw.updates(X, Z, beta1=0.1, verbose=TRUE)
rw.plot(res.orig, ylim=c(-.5, .7), legend=TRUE, pos="topright", main="outcome: PLURAL | sorted R-W learner")
dev.copy2pdf(file="img/plural_rw_sorted.pdf")

for (pass in 1:5) {
  res.random <- rw.updates(X, Z, sample=nrow(X), beta1=0.1, verbose=TRUE)
  first.pass <- pass == 1
  rw.plot(res.random, lwd=1, ylim=c(-.5, .7), main="outcome: PLURAL | stochastic R-W learner", add=!first.pass)
}
res.exp <- rw.expected(X, Z, nrow(X), beta=0.1)
rw.plot(res.exp, lwd=4, add=TRUE, legend=TRUE, pos="topleft")

res.danks <- danks.equilibrium(X, Z, plot=TRUE)
dev.copy2pdf(file="img/plural_rw_random.pdf")

## successful R-W learning takes much longer -- Antti needed about 30,000 steps
res.random <- rw.updates(X, Z, sample=30000, beta1=0.01)
res.exp <- rw.expected(X, Z, 30000, beta=0.01)
rw.plot(res.random, lwd=.8, ylim=c(-.5, .7), main="outcome: PLURAL | stochastic R-W learner")
rw.plot(res.exp, lwd=3, legend=TRUE, pos="bottom", add=TRUE)
danks.equilibrium(X, Z, plot=TRUE)

text(0, -.4, expression(beta == .01), pos=4, cex=1.6, col="blue")
dev.copy2pdf(file="img/plural_rw_random_30k.pdf")

## cycling through a randomized version of the data set also converges to Danks
idx1 <- sample(1:nrow(X))
idx2 <- rep(idx1, length.out=30000)
X2 <- X[idx2, ]
Z2 <- Z[idx2, "PLURAL", drop=FALSE]
res2 <- rw.updates(X2, Z2, beta1=0.01, verbose=TRUE)
rw.plot(res2, lwd=1, legend=TRUE, pos="bottom")
danks.equilibrium(X, Z, plot=TRUE)


## TODO: fCC becomes singular if explicit background cue is added -> possible cause of problems?

