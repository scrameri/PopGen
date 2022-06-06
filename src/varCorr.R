## find highly correlated variables
varCorr <- function(cor, cor.thr = 0.9, dev = NULL) {
  
  # check
  stopifnot(length(unique(dim(cor))) == 1,
            !anyNA(cor[upper.tri(cor)]))
  # if (!is.null(names(dev))) {
  #   stopifnot(all.equal(names(dev)[1:ncol(cor)], colnames(cor)))
  # }
  
  # keep upper triangle only
  cor[lower.tri(cor)] <- diag(cor) <- NA
  
  # define groups of correlated varialbes
  l.cor <- apply(cor, 1, function(x) {w <- which(abs(x) > cor.thr) ; if (length(w) > 0) colnames(cor)[w] else NA})
  
  # loop through groups and find variables to be eliminated (keep variable with high deviance gain)
  correlated <- character()
  for (i in seq(length(l.cor))) {
    
    # variables
    corr.base <- names(l.cor)[i]
    corr.with <- l.cor[[i]]
    
    if (!is.null(dev)) {
      # criterion: deviances (good criterion, invariant to scale of predictor)
      dev.base <- dev[pmatch(corr.base, names(dev))] # dev[i]
      dev.with <- dev[pmatch(corr.with, names(dev))]
      dev.max <- c(corr.base, corr.with)[which.max(c(dev.base, dev.with))]
      torm <- c(corr.base, corr.with)[!c(corr.base, corr.with) %in% dev.max]
    } else {
      # criterion: use the first variable in the correlation matrix order
      torm <- corr.with[!corr.with %in% names(l.cor)[1:i]]
    }    
    correlated <- as.character(na.omit(unique(c(correlated, torm))))
  }
  return(correlated)
}

## filter variables with high correlation
varFilter <- function(X, cor.thr = 0.9, cor.method = "spearman", rank = NULL,
                      freqCut = 95/5, uniqueCut = 10, verbose = T) {
  
  # find variables with near-zero variation (monomorphic)
  nzv <- caret::nearZeroVar(x = X, freqCut = freqCut, uniqueCut = uniqueCut)
  monomorphic <- colnames(X)[nzv]
  
  # find variables with high collinearity
  dcor <- cor(X[,colnames(X)[!colnames(X) %in% monomorphic]], method = cor.method)
  if (is.null(rank)) dev <- rank else dev <- -1 * rank
  if (!is.null(dev) & any(!colnames(X) %in% names(dev))) {
    miss <- colnames(dcor)[!colnames(X) %in% names(dev)]
    warning("found ", length(miss), " variables missing in <rank>, these will be discarded: ", paste(miss, collapse = ","))
  }
  correlated <- varCorr(cor = dcor, cor.thr = cor.thr, dev = dev)
  
  # filter out monomorphic / highlX correlated variables
  torm <- union(monomorphic, correlated)
  message(paste0("found ", length(monomorphic), "/", ncol(X), " (", round(100*length(monomorphic)/ncol(X),2), "%) highly monomorphic variables"))
  if (verbose) print(monomorphic)
  message(paste0("found ", length(correlated), "/", ncol(X), " (", round(100*length(correlated)/ncol(X),2), "%) highly correlated variables"))
  if (verbose) print(correlated)
  message(paste0("filtered ", length(torm), "/", ncol(X), " (", round(100*length(torm)/ncol(X),2), "%) variables"))
  v <- names(X)[!names(X) %in% torm]
  
  # select variables for downstream analysis
  message(paste0("selected ", length(v), "/", ncol(X), " (", round(100*length(v)/ncol(X), 2), "%) variables"))
  if (verbose) print(v)
  invisible(X[,v])
}
