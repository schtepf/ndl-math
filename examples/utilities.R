##
## Utility functions for illustration of NDL, R-W updates and Danks equilibrium
##

## a nice colour palette
ten.colors <- c("black", "red", "green3", "blue", "grey65", "magenta", "yellow2", "cyan3", "orange", "olivedrab1")

## take subset of GerNouns data frame with one entry for each specified word form
noun.subset <- function (x, words) {
  res <- lapply(words, function (w) {
    idx <- which(x$word == w)
    if (length(idx) == 0) stop(sprintf("word '%s' not found in table", w))
    GerNouns[idx[1], ]
  })
  do.call(rbind, res)
}

## transform NDL coding of cues/outcomes into binary indicator matrix
ortho2matrix <- function (x, sep="_", bool=FALSE, background=FALSE, row.names=NULL) {
  x.split <- strsplit(x, sep, perl=TRUE)
  cats <- sort(unique(unlist(x.split)))
  mat.rows <- lapply(x.split, function (v) cats %in% v)
  mat <- do.call(rbind, mat.rows)
  if (background) {
    mat <- cbind(mat, rep(1, nrow(mat)))
    colnames(mat) <- c(cats, "BGRD")
  } else {
    colnames(mat) <- cats
  }
  if (!is.null(row.names)) rownames(mat) <- row.names
  if (bool) mat else mat + 0
}

## apply one or more R-W updates, returning matrix of associations after each step
##  - X = indicator matrix of cues (c_j in original notation)
##  - Z = column vector of outcomes (o in original notation)
##  - V = optional vector of initial associations
##  - alpha, beta1, beta2, lambda = R-W parameters (only set beta1 for Widrow-Hoff)
##  - sample = randomly sample specified number of rows from X, Z (with replacement)
rw.updates <- function (X, Z, V=0, alpha=1, beta1=.1, beta2=beta1, lambda=1, sample=NA, output.init=TRUE, verbose=FALSE, show.activation=FALSE) {
  if (show.activation && !output.init) stop("output.init=FALSE and show.activation=TRUE cannot be combined")
  if (!is.matrix(V)) V <- matrix(V, nrow=1, ncol=ncol(X)) # initial associations
  stopifnot(ncol(X) == ncol(V))
  if (!is.matrix(Z)) Z <- matrix(Z, ncol=1)
  stopifnot(nrow(X) == nrow(Z) && ncol(Z) == 1)
  alpha <- rep_len(alpha, ncol(X))
  if (!is.na(sample)) {
    idx <- sample.int(nrow(X), sample, replace=TRUE)
    X <- X[idx, , drop=FALSE]
    Z <- Z[idx, , drop=FALSE]
  }
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
    V.mat <- cbind(V.mat, act.mat)
  }
  if (output.init) V.mat else V.mat[-1, , drop=FALSE]
}

## update expected R-W associations based on population defined by X, Z
##  - X = indicator matrix of cues (c_j in original notation)
##  - Z = column vector of outcomes (o in original notation)
##  - steps = number of update steps to carry out
##  - alpha, beta, lambda = R-W parameters (under simplification beta1 = beta2 = beta)
rw.expected <- function (X, Z, steps, alpha=1, beta=.1, lambda=1, output.init=TRUE) {
  if (!is.matrix(Z)) Z <- matrix(Z, ncol=1)
  stopifnot(nrow(X) == nrow(Z) && ncol(Z) == 1)
  n <- ncol(X) # number of cues
  m <- nrow(X) # population size
  alpha <- rep_len(alpha, n)
  pOC <- crossprod(X, Z) / m # P(O, C_i)
  pCC <- crossprod(X) / m    # P(C_i, C_j)
  V <- matrix(0, nrow=n, ncol=1)
  V.list <- vector("list", steps + 1)
  V.list[[1]] <- t(V) # so be can rbind() the expected activations
  for (t in seq_len(steps)) {
    V <- V + beta * alpha * (lambda * pOC - pCC %*% V)
    V.list[[t + 1]] <- t(V)
  }
  V.mat <- do.call(rbind, V.list)
  colnames(V.mat) <- colnames(X)
  if (output.init) V.mat else V.mat[-1, , drop=FALSE]
}

