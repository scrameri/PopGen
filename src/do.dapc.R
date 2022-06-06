## Wrapper for DAPC analysis using xval-derived n.pca and var.loadings = TRUE
do.dapc <- function(x, grp, center = TRUE, scale = FALSE, n.rep = 100,
                    var.contrib = TRUE, var.loadings = TRUE) {
  
  require(adegenet)
  
  # check input
  stopifnot(inherits(x, c("data.frame","matrix")),
            identical(length(grp), nrow(x)),
            is.logical(center), is.logical(scale),
            is.numeric(n.rep), n.rep > 0,
            is.logical(var.contrib), is.logical(var.loadings))
  
  if (!is.factor(grp)) grp <- factor(grp)
  
  # cross validation analysis (determine best n.pca)
  xval <- xvalDapc(x = x, grp = grp, scale = scale, center = center,
                   n.da = nlevels(grp)-1, n.pca = 1:ncol(x), n.rep = n.rep)
  
  # dapc with var.loadings
  dapc <- dapc(x = x, grp = grp,
               scale = scale, center = center, n.da = nlevels(grp)-1,
               n.pca = as.numeric(n.rep$`Number of PCs Achieving Highest Mean Success`),
               var.contrib = var.contrib, var.loadings = var.loadings)
  
  # return results
  xval$DAPC <- dapc
  return(xval)
  
}
