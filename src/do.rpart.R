do.rpart <- function(df, fac, nmin = 5, control, plot = TRUE) {
  
  ## helperfunctions
  prune.rpart2 <- function(tree, cp) {
    ff <- tree$frame
    id <- as.integer(row.names(ff))
    toss <- id[ff$complexity <= cp & ff$var != "<leaf>"]
    if (length(toss) == 0L) 
      return(tree)
    newx <- snip.rpart(tree, toss)
    temp <- pmax(tree$cptable[, 1L], cp)
    keep <- match(unique(temp), temp)
    newx$cptable <- tree$cptable[keep, , drop = FALSE]
    newx$cptable[as.character(max(keep)), 1L] <- cp # fix
    # newx$variable.importance <- importance(newx) # hidden function
    newx
  }
  library(rpart)
  library(rpart.plot)
  library(adegenet)
  
  ## check input
  stopifnot(any(is.data.frame(df), is.matrix(df)),
            is.character(fac), fac %in% colnames(df), is.factor(df[,fac]),
            is.numeric(nmin), nmin > 0,
            is.list(control),
            is.logical(plot))
  
  ## prepare output
  factab <- table(df[,fac])
  if (nmin > sort(factab, decreasing = T)[2]) stop("nmin too large (only a single class remaining)!")
  levomit <- names(which(factab < nmin))
  if (length(levomit) > 0) df.omit <- df[-which(df[,fac] %in% levomit),] else df.omit <- df
  
  ## large rpart
  # set.seed(1234)
  d.train <- df.omit
  d.train[,fac] <- droplevels(d.train[,fac])
  parms <- list(prior = rep(1/nlevels(d.train[,fac]), nlevels(d.train[,fac])))
  rp.large <- rpart(as.formula(paste(fac, "~", ".")), data = d.train, control = control, parms = parms)
  
  ## complexity parameter pruning
  cp.tab <- rp.large$cptable
  cp.err <- cp.tab[,"xerror"]
  cp.sel <- cp.tab[min(which(cp.err <= cp.err[which.min(cp.err)] + 
                               cp.tab[,"xstd"][which.min(cp.err)])), "CP"]
  
  # rule: cp threshold for pruning at 1 SE above sd of minimum of the curve
  ethresh <- min(cp.tab[,"xerror"]) + cp.tab[which.min(cp.tab[,"xerror"]),"xstd"] 
  cp <- cp.tab[which(cp.tab[,"xerror"] < ethresh),"CP"][1]
  
  ## pruned rpart
  rp.final <- try(rpart::prune.rpart(rp.large, cp = cp), silent = TRUE)
  if (inherits(rp.final, "try-error")) {
    rp.final <- prune.rpart2(tree = rp.large, cp = cp)
  }

  ## plot pruned rpart model
  if (plot) {
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    split.fun <- function(x, labs, digits, varlen, faclen) {
      labs <- gsub("<", "\n<", gsub(">", "\n>", gsub(" ", "\n", gsub(":", "\n", labs))))
      for (i in 1:length(labs)) {
        
        # split labs[i] into multiple lines
        labs[i] <- paste(strwrap(labs[i], width = 10), collapse = "\n")
        rm(i)
      }
      labs
    }
    p <- capture.output(
      prp(rp.final, type = 3, extra = 0, # or use extra = 1
          box.col = transp(gg_color_hue(nlevels(df[,fac]))[rp.final$frame$yval], .6),  
          fallen.leaves = F, clip.right.labs = T, varlen = 0, space = 0, cex = 1.5, 
          gap = 8, ygap = 3, xflip = F, boxes.include.gap = T,
          under = T, branch.lty = 2, split.fun = split.fun)
    )
  }
  
  ## return results
  accuracy <- 1-sum(residuals(rp.final) == 1)/nrow(d.train)
  res <- list()
  res[["rp.input"]] <- d.train
  res[["rp.large"]] <- rp.large
  res[["cp.thresh"]] <- cp
  res[["rp.final"]] <- rp.final
  res[["accuracy"]] <- accuracy
  return(res)
}
