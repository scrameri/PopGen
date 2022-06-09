impute <- function(df, fun = "mean", classes = NULL, summary = TRUE,
                   heatmap = TRUE, plot = TRUE) {
  
  ## Arguments
  # df        data.frame with missing data
  # fun       imputation function, choose between c("mean","zero","class.mean")
  # classes   factor of the same length as nrow(df). Used if fun = "class.mean".
  # summary   boolean; if TRUE, returns the total fraction of missing data ($total),
  #           the fraction missing per variable ($per.variable) and per individual
  #           ($per.individual).
  # heatmap   boolean; if TRUE, plots the distribution of missingness across df.
  #           Useful to spot structural missingness, which should be avoided.
  # plot      boolean; if TRUE, prints the heatmap.
  
  ## Author: sfcrameri@gmail.com, Apr 2022
  
  ## Check input ##
  stopifnot(inherits(df, "data.frame"),
            fun %in% c("mean", "zero", "class.mean"))
  if (fun == "class.mean") {
    stopifnot(!is.null(classes),
              length(classes) == nrow(df),
              inherits(classes, c("character","factor")))
    if (is.character(classes)) classes <- factor(classes)
  }
  
  ## Define helperfunctions
  var.heatmap <- function(df, low = muted("red"), mid = "white", high = muted("blue"), limits = NULL,
                          midpoint = mean(as.matrix(df)), keyname = "",
                          xlab = FALSE, cex.xlab = 8, col.xlab = 1, ylab = TRUE, cex.ylab = 8, col.ylab = 1,
                          width = 1, height = 1, na.value = "grey50", plot = TRUE) {
    
    ## Arguments
    # df        data.frame
    # low       color for min value
    # mid       color for mean value
    # high      color for max value
    # limits    passed to ?scale_fill_gradient2
    # midpoint  passed to ?scale_fill_gradient2
    # keyname   name of heatmap color legend
    # xlab      heatmap x axis label
    # cex.xlab  size of x axis label
    # col.xlab  color of x axis label
    # ylab      heatmap y axis label
    # cex.ylab  size of y axis label
    # col.ylab  color of y axis label
    # width     heatmap tile width, passed to ?geom_tile 
    # height    heatmap tile height, passed to ?geom_tile
    # na.value  heatmap color for NA values
    # plot      boolean, if TRUE, heatmap will be plotted
    
    # check dependencies
    needs <- c("tibble","tidyr","dplyr","ggplot2","scales")
    inst <- which(!needs %in% installed.packages())
    if (length(inst) > 0) {
      for (i in inst) {
        toinst <- readline(prompt = paste0("The <", needs[i], "> package is needed. Do you wish to install it? [y/n]: "))
        if (toinst == "y") install.packages(needs[i])
      }
    }
    
    # Library
    library(ggplot2)
    library(dplyr)
    library(scales)
    
    # reshape
    dd <- tibble::as_tibble(df, rownames = NA) %>% 
      tibble::rownames_to_column(var = "Y") %>% 
      tidyr::pivot_longer(cols = -1, names_to = "X", values_to = "Z") %>%
      dplyr::mutate(X = factor(X, levels = colnames(df))) %>%
      dplyr::mutate(Y = factor(Y, levels = rev(rownames(df))))
    
    p <- ggplot(dd, aes(X, Y, fill = Z)) + 
      geom_tile(width = width, height = height) +
      scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = midpoint, na.value = na.value, limits = limits) +
      labs(x = "", y = "", fill = keyname) +
      theme_minimal() +
      theme(axis.text.x = if (xlab) element_text(angle = 90, hjust = 1, vjust = 0.5, size = cex.xlab, colour = col.xlab) else element_blank(), 
            axis.text.y = if (ylab) element_text(colour = col.ylab, size = cex.ylab) else element_blank(),
            axis.ticks = element_blank()) #+
    # scale_x_discrete(position = "top")
    
    if (plot) print(p)
    invisible(p)
  }
  .impute <- function(x, fun, ...) {
    
    # check input
    stopifnot(is.numeric(x))
    if (any(is.na(x))) {
      
      # impute missing data
      switch(fun,
             mean = {x[is.na(x)] <- mean(x, na.rm = TRUE)},
             zero = {x[is.na(x)] <- 0}
      )
    }
    
    # return imputed vector
    return(x) 
  }
  
  ## Determine numeric columns
  on <- colnames(df)
  idx <- sapply(df, class) %in% c("numeric","integer")
  xmis <- df[,idx,drop=F]
  
  ## Get missingness summary ##
  missingness <- list()
  
  if (summary) {
    # dfmis <- apply(xmis, 2, function(x) {gsub("^NA$", NA, x)})
    
    # total missingness (displayed in summary(genind))
    # df.perc <- sum(is.na(dfmis)) / (nrow(dfmis)*ncol(dfmis))
    df.perc <- sum(is.na(xmis)) / (nrow(xmis)*ncol(xmis))
    missingness[["total"]] <- df.perc
    
    # missingness per variable
    # df.var.perc <- apply(dfmis, 2, function(x) {sum(is.na(x)) / length(x)})
    df.var.perc <- apply(xmis, 2, function(x) {sum(is.na(x)) / length(x)})
    missingness[["per.variable"]] <- df.var.perc
    
    # missingness per individual
    # df.ind.perc <- apply(dfmis, 1, function(x) {sum(is.na(x)) / length(x)})
    df.ind.perc <- apply(xmis, 1, function(x) {sum(is.na(x)) / length(x)})
    missingness[["per.individual"]] <- df.ind.perc
    
  }
  
  ## Plot missingness (check for structural missingness)
  if (heatmap) {
    xp <- as.matrix(xmis)
    xp[!is.na(xp)] <- 1
    xp[is.na(xp)] <- 0
    p <- var.heatmap(xp, low = muted("red"), high = "white", midpoint = 0.5,
                     xlab = TRUE, limits = c(1,1), na.value = muted("red")) +
      geom_hline(yintercept = (1:(nrow(xp)+1))-0.5, size = 0.1) +
      geom_vline(xintercept = (1:(ncol(xp)+1))-0.5, size = 0.1) +
      theme(legend.position = "none")
    
    missingness[["plot"]] <- p
    if (plot) print(p)
  }
  
  ## Impute missing numeric data ##
  switch(fun, 
         mean = {
           missingness[["imputed"]] <- data.frame(apply(xmis, MARGIN = 2, FUN = .impute, fun = "mean"))
         }, 
         zero = {
           missingness[["imputed"]] <- data.frame(apply(xmis, MARGIN = 2, FUN = .impute, fun = "zero"))
         },
         class.mean = {
           tab <- array(NA, dim = c(0, ncol(xmis)), dimnames = list(NULL, colnames(xmis)))
           
           for (i in levels(droplevels(classes))) {
             xmissub <- xmis[classes == i,]
             tmp <- apply(xmissub, MARGIN = 2, FUN = .impute, fun = "mean", simplify = T)
             if (!is.array(tmp)) tmp <- data.frame(t(tmp))
             rownames(tmp) <- rownames(xmissub)
             colnames(tmp) <- colnames(tab)
             tab <- rbind(tab, tmp)
           }
           stopifnot(all(rownames(tab) %in% rownames(xmis)))
           tab <- tab[rownames(xmis),]
           
           # if a class is entirely NA, use the global mean
           if (anyNA(tab)) tab <- data.frame(apply(tab, MARGIN = 2, FUN = .impute, fun = "mean"), check.names = F)
           
           missingness[["imputed"]] <- data.frame(df[,!idx,drop=F], tab, check.names = F)[,on]
         }
  )
  
  
  ## Get imputation info ##
  na <- which(apply(xmis, 2, anyNA))
  switch(fun, 
         mean = {
           mean.imp <- apply(xmis[,na], 2, mean, na.rm = TRUE)
           missingness[["imputation"]] <- unlist(mean.imp)
         },
         zero = {
           zero.imp <- rep(0, length(na))
           names(zero.imp) <- names(na)
           missingness[["imputation"]] <- zero.imp
         },
         class.mean = {
           class.mean.imp <- apply(xmis, 2, tapply, classes, mean, na.rm = TRUE)
           if (nlevels(classes) == 1) {
             class.mean.imp <- data.frame(t(class.mean.imp))
             rownames(class.mean.imp) <- levels(classes)
           }
           missingness[["imputation"]] <- class.mean.imp[,na]
         }
  )
  
  ## return results
  return(missingness)
}
