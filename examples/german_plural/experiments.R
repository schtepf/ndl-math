##
## First experiments with German noun plurals
##

source("utilities.R")
load("GerNouns.rda", verbose=TRUE)
GerNouns$background <- TRUE

## full matrix of cues and vector of outcomes
X0 <- as.matrix(GerNouns[, -(1:2)]) + 0
Z0 <- matrix(ifelse(GerNouns$number == "Pl", 1, 0), ncol=1)

## small example matrix for illustration of mathematical arguments
Sample <- noun.subset(GerNouns, c("Bäume", "Flasche", "Baum", "Gläser", "Flaschen", "Latte", "Hütten", "Glas", "Bäume"))
Sample

X <- as.matrix(Sample[, -(1:2)]) + 0
Z <- matrix(ifelse(Sample$number == "Pl", 1, 0), ncol=1)

## R-W learning with the simplified Widrow-Hoff rule
res <- rw.updates(X, Z, beta1=.2, show.activation=TRUE)
round(res, 3)




if (FALSE) {
  res <- rw.updates(X0, Z0, verbose=TRUE, beta1=.01)
  matplot(res, type="l", lty="solid")
}
