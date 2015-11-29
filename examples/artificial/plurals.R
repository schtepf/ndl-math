##
## Analysis of artificial plurals data from NDL package
##

source("../utilities.R")
library(ndl)

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
  
  save(plurals, X.pl, Z.pl, X.pl.types, Z.pl.types, file="plurals.rda") # save matrices, so examples don't depend on NDL package
}

load("plurals.rda", verbose=TRUE)


o## X.pl and Z.pl contain these types with suitable repetition counts
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
text(0, -.4, expression(beta == .1), pos=4, cex=1.6, col="blue")
dev.copy2pdf(file="img/plural_rw_sorted.pdf")

tmpX <- do.call(rbind, lapply(1:10, function (i) X))
tmpZ <- do.call(rbind, lapply(1:10, function (i) Z))
res.orig.10 <- rw.updates(tmpX, tmpZ, beta1=0.01, verbose=TRUE)
rw.plot(res.orig.10, ylim=c(-.5, .7), legend=TRUE, pos="topright", main="outcome: PLURAL | sorted R-W learner")
text(0, -.4, expression(beta == .01), pos=4, cex=1.6, col="blue")
dev.copy2pdf(file="img/plural_rw_sorted_x10.pdf")

for (pass in 1:5) {
  res.random <- rw.updates(X, Z, sample=nrow(X), beta1=0.1, verbose=TRUE)
  first.pass <- pass == 1
  rw.plot(res.random, lwd=1, ylim=c(-.5, .7), main="outcome: PLURAL | stochastic R-W learner", add=!first.pass)
}
res.exp <- rw.expected(X, Z, nrow(X), beta=0.1)
rw.plot(res.exp, lwd=4, add=TRUE, legend=TRUE, pos="topleft")

res.danks <- danks.equilibrium(X, Z, plot=TRUE)
text(0, -.4, expression(beta == .1), pos=4, cex=1.6, col="blue")
dev.copy2pdf(file="img/plural_rw_random.pdf")

## successful R-W learning takes much longer -- Antti needed about 30,000 steps
res.random <- rw.updates(X, Z, sample=30000, beta1=0.01)
res.exp <- rw.expected(X, Z, 30000, beta=0.01)
rw.plot(res.random, lwd=.8, ylim=c(-.5, .7), main="outcome: PLURAL | stochastic R-W learner")
rw.plot(res.exp, lwd=3, legend=TRUE, pos="bottom", add=TRUE)
danks.equilibrium(X, Z, plot=TRUE)

text(0, -.4, expression(beta == .01), pos=4, cex=1.6, col="blue")
dev.copy2pdf(file="img/plural_rw_random_30k.pdf")

## the NDL package does not simulate a stochastic R-W learner, but uses "sampling without replacement",
## which means that it repeatedly cycles through the data set, reordering the tokens after each cycle
system.time( res <- RescorlaWagner(plurals, traceOutcome="PLURAL", traceCue="s", nruns=100) ) # ca. 7.5 s
plot(res) # confirms failure of R-W simulation to converge to Danks equilibrium

## NB: NDL has a strange implementation of sampling without replacement, which permutes the tokens before
## each cycle (starting from the previous permutation). Even if a particular random order is specified as
## an argument, it is applied as a permutation before each cycle, so multiple iterations through the
## data set are carried out in different orders. The $randomOrder value returned by RescorlaWagner() contains
## only the last permutation, so a perfect direct comparison would be very difficult to achieve.

idx <- do.call(c, lapply(1:100, function (i) sample.int(nrow(X)))) # emulate the RescorlaWagner() behaviour 
X2 <- X[idx, ]
Z2 <- Z[idx, "PLURAL", drop=FALSE]
system.time( res2 <- rw.updates(X2, Z2, beta1=0.01, verbose=TRUE) ) # ca. 1.7 s
rw.plot(res2, lwd=1, legend=TRUE, pos="bottom")
res2.eq <- danks.equilibrium(X, Z, plot=TRUE)

## compare NDL package against R implementation
cat(sprintf("Danks equilibrium for s -> PL:  NDL = %.5f  R = %.5f\n", res$equilibriumWeight, res2.eq$V["s", "PLURAL"])) # agree on the Danks solution

rw.plot(cbind(R=res2[, "s"], NDL=c(0, res$weightvector)), lwd=1, legend=TRUE, pos="bottomright", ylim=c(-0.1, 0.3), main="cue: S / outcome: PLURAL") # but the R-W simulation in NDL converges to a smaller association value

## A close look at the NDL implementation reveals that cues that are listed multiple times in an event pattern
## (which shouldn't happen!) are added repeatedly to the total activation
##     Vtotal = sum(weightvec[currentCues])
## they are also updated multiple times with
##     weightvec[currentCues] = weightvec[currentCues]+alpha*beta1*(Lambda-Vtotal)
## but this always writes the same value, so it doesn't lead to a distortion.
## Such a case is present in the plurals data set:
subset(plurals, WordForm == "lass")

