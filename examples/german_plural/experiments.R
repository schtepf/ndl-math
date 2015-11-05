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
Sample <- noun.subset(GerNouns, c("Bäume", "Flasche", "Baum", "Gläser", "Flaschen", "Latte", "Hütten", "Glas", "Bäume", "Füße"))
Sample

X <- as.matrix(Sample[, -(1:2)]) + 0
Z <- matrix(ifelse(Sample$number == "Pl", 1, 0), ncol=1)

## plot R-W learning curves in 16:10 aspect ratio
quartz(width=8, height=5)
par(mar=c(2,4,1,1)+.1)

## R-W learning with the simplified Widrow-Hoff rule (beta = .2)
res <- rw.updates(X, Z, beta1=.2, show.activation=TRUE)
round(res, 3)

## incremental visualization with matplot
act.mat <- res[, 1:5] + rep(seq(0, .01, length.out=5), each=nrow(res)) # activation levels with small shifts to avoid overplotting
t.vec <- seq_len(nrow(act.mat)) # first row = activations at time step 1 (will be updated by first event)

for (step in t.vec) {
  idx <- 1:step
  matplot(t.vec[idx], act.mat[idx, , drop=FALSE], type="b", lty="solid", col=ten.colors, pch=20, lwd=3,
          xlim=range(t.vec), ylim=c(-.2, .6), yaxs="i", xlab="", ylab=expression(paste("association strength ", V[i])), lab=c(10,5,7), las=1)
  abline(h=0, lwd=1)
  text(idx + .5, -0.19, Sample$word[idx], adj=c(0, 0.5), srt=90, cex=1.2)
  legend("topleft", inset=.02, bg="white", legend=c("-e", "-n", "-s", "umlaut", "dbl cons"), col=ten.colors, lwd=4, cex=1.2)
  dev.copy2pdf(file=sprintf("img/german_plural_rw_step_%d.pdf", step))
}

## train R-W learner on stochastic input
for (step in list(list(b=.5,n=200,l="b050_n200"), list(b=.2,n=200,l="b020_n200"), list(b=.1,n=200,l="b010_n200"), list(b=.05,n=200,l="b005_n200"), list(b=.01,n=200,l="b001_n200"), list(b=.01,n=2000,l="b001_n2000"))) {
  set.seed(42) # use same sequence for all plots
  act.mat <- rw.updates(X, Z, beta1=step$b, sample=step$n)
  t.vec <- seq_len(nrow(act.mat)) - 1
  matplot(t.vec, act.mat, type="l", lty="solid", lwd=3, col=ten.colors, ylim=c(-.5, 1), yaxs="i", xlab="", ylab=expression(paste("association strength ", V[i])), las=1)
  abline(h=0, lwd=1)
  legend("topleft", legend=bquote(beta == .(step$b)), bty="n", cex=1.4)
  dev.copy2pdf(file=sprintf("img/german_plural_rw_%s.pdf", step$l))
}

## individual leaners vs. expected values
for (step in 1:5) {
  set.seed(step) # for reproducibility
  act.mat <- rw.updates(X, Z, beta1=0.2, sample=150)
  t.vec <- seq_len(nrow(act.mat)) - 1
  if (step == 1) {
    plot(0, 0, type="n", xlim=range(t.vec), ylim=c(-.5, 1), yaxs="i", xlab="", ylab=expression(paste("association strength ", V[i])), las=1)
    abline(h=0, lwd=1)
    legend("topleft", legend=bquote(beta == 0.2), bty="n", cex=1.4)
  }
  matplot(t.vec, act.mat, type="l", lty="solid", lwd=1, col=ten.colors, add=TRUE)
  dev.copy2pdf(file=sprintf("img/german_plural_exp_rw_step_%d.pdf", step))
}
act.mat <- rw.expected(X, Z, 150, beta=.2)
t.vec <- seq_len(nrow(act.mat)) - 1
matplot(t.vec, act.mat, type="l", lty="solid", lwd=4, col=ten.colors, add=TRUE)
dev.copy2pdf(file="img/german_plural_exp_rw_final.pdf")

## compute the Danks equilibrium solution
m <- nrow(X) # number of event tokens in population

fOC <- crossprod(X, Z)                  # co-occurrence frequencies
fOC   # f(O, C_i)
fCC <- crossprod(X)
fCC   # f(C_i, C_j)

pOC <- fOC / m                          # co-occurrence probabilities
pOC   # P(O, C_i) ... no roudning needed for m == 10
pCC <- fCC / m
pCC   # P(C_i, C_j)

round(svd(pCC)$d, 3) # matrix is reasonably well conditioned, i.e. not singular

V.danks <- solve(pCC, pOC) # = (X' X)^{-1} X' o
n <- length(V.danks)
x.max <- par("usr")[2]
points(rep(x.max, n), V.danks, col=ten.colors[1:n], pch=18, cex=3)
dev.copy2pdf(file="img/german_plural_exp_rw_danks.pdf")

act.mat <- rw.expected(X, Z, 500, beta=.2)
t.vec <- seq_len(nrow(act.mat)) - 1
matplot(t.vec, act.mat, type="l", lty="solid", lwd=4, col=ten.colors, xlim=range(t.vec), ylim=c(-.5, 1), yaxs="i", xlab="", ylab=expression(paste("association strength ", V[i])), las=1)
abline(h=0, lwd=1)
x.max <- par("usr")[2]
points(rep(x.max, n), V.danks, col=ten.colors[1:n], pch=18, cex=3)
dev.copy2pdf(file="img/german_plural_exp_rw_danks_500.pdf")

## and compare with least-squares regression
V.lm <- lm.fit(X, Z)$coefficients
round(rbind(LM=V.lm, Danks=drop(V.danks)), 5) # identical

SampleLM <- transform(Sample, Outcom=as.numeric(number == "Pl"), suff_e=as.numeric(suff_e), suff_n=as.numeric(suff_n), suff_s=as.numeric(suff_s), umlaut=as.numeric(umlaut), double_cons=as.numeric(double_cons), background=as.numeric(background))
res <- lm((number == "Pl") ~ suff_e + suff_n + suff_s + umlaut + double_cons + background - 1, data=SampleLM)
round(rbind(LM=V.lm, Danks=drop(V.danks), LM2=res$coefficients), 5) # identical





## analysis of the full data set (rather slow)
if (FALSE) {
  res <- rw.updates(X0, Z0, verbose=TRUE, beta1=.01)
  matplot(res, type="l", lty="solid")
}
