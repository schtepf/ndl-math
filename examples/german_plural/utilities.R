##
## Utility functions for illustration of NDL, R-W updates and Danks equilibrium
##

## take subset of GerNouns data frame with one entry for each specified word form
noun.subset <- function (x, words) {
  res <- lapply(words, function (w) {
    idx <- which(x$word == w)
    if (length(idx) == 0) stop(sprintf("word '%s' not found in table", w))
    GerNouns[idx[1], ]
  })
  do.call(rbind, res)
}

## apply one or more R-W updates, returning matrix of associations after each step
##  - X = indicator matrix of cues (c_j in original notation)
##  - Z = column vector of outcomes (o in original notation)
rw.updates <- function (X, Z, V=0, alpha=1, beta1=.1, beta2=beta1, lambda=1, output.init=TRUE, verbose=FALSE, show.activation=FALSE) {
  if (show.activation && !output.init) stop("output.init=FALSE and show.activation=TRUE cannot be combined")
  if (!is.matrix(V)) V <- matrix(V, nrow=1, ncol=ncol(X)) # initial associations
  stopifnot(ncol(X) == ncol(V))
  if (!is.matrix(Z)) Z <- matrix(Z, ncol=1)
  stopifnot(nrow(X) == nrow(Z) && ncol(Z) == 1)
  n <- nrow(X) # number of time steps
  V.list <- vector("list", n + 1)
  V.list[[1]] <- V
  act.list <- vector("list", n + 1)
  v. <- V
  if (verbose) pb <- txtProgressBar(min=0, max=nrow(X), style=3)
  for (t in seq_len(n)) {
    o. <- Z[t]
    c. <- X[t, ]
    beta <- if (o.) beta1 else beta2
    activ. <- sum(c. * v.) # current activation level
    v. <- v. + c. * alpha * beta * (lambda * o. - activ.)
    act.list[[t]] <- c(activ., lambda * o.)
    V.list[[t + 1]] <- v.
    if (verbose) setTxtProgressBar(pb, t)
  }
  if (verbose) close(pb)
  V.mat <- do.call(rbind, V.list)
  colnames(V.mat) <- colnames(X)
  if (show.activation) {
    act.list[[n + 1]] <- c(NA, NA)
    act.mat <- do.call(rbind, act.list)
    colnames(act.mat) <- c("Activation", "Target")
    cbind(V.mat, act.mat)
  } else {
    V.mat
  }
}
