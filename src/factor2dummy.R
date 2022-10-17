# factor2dummy
factor2dummy <- function(df, factorname, keep.levels = levels(factor(df[,factorname])), na.method = "zero") {
  
  dfac <- data.frame(factor(df[,factorname]))
  names(dfac) <- factorname
  keep.levels <- levels(dfac[,factorname])[levels(dfac[,factorname]) %in% keep.levels]
  
  m <- model.matrix(~dfac[,factorname])
  m <- m[match(rownames(dfac), rownames(m)), , drop = FALSE]
  m[,1] <- ifelse(dfac[,factorname] == levels(dfac[,factorname])[1], 1, 0) # first column is a dummy intercept (first level not returned in model.matrix)
  colnames(m) <- paste0(factorname, "_", levels(dfac[,factorname]))
  rownames(m) <- rownames(dfac)
  if (na.method == "zero") m[is.na(m)] <- 0
  
  stopifnot(all.equal(sum(m), sum(table(dfac[,factorname]))))
  df2 <- df[,!colnames(df) %in% factorname]
  pos <- which(colnames(df) == factorname)
  if (pos == 1) {
    df2 <- cbind(m[,paste0(factorname, "_", keep.levels),drop=FALSE], df2)
  } else if (pos == ncol(df)) {
    df2 <- cbind(df2, m[,paste0(factorname, "_", keep.levels),drop=FALSE])
  } else {
    df2 <- cbind(df2[,1:(pos-1),drop=FALSE], m[,paste0(factorname, "_", keep.levels),drop=FALSE], df2[,pos:ncol(df2),drop=FALSE])
  }
  names(df2)[pos:(pos + length(keep.levels) - 1)] <- paste0(factorname, "_", keep.levels)
  rownames(df2) <- rownames(df)
  return(df2)
}
