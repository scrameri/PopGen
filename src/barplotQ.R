# # debug
# idvar = "ID"; qvars = NULL;
# popvar = "Species"; sortvar = "LatitudeDecimal";
# sortby = c(popvar, sortvar, idvar);
# palette = NULL; plot = TRUE; xlab = ""; ylab = "Admixture Proportion";
# rot.top = 90; rot.bottom = 90; size.top = 2.5; size.bottom = 5;
# hjust.top = 0; hjust.bottom = 1; vjust.top = 0.5; vjust.bottom = 0.5

# clumpp function
clumppQ <- function(qpaths, labels, K = 2, FUN = mean, order = seq(nrow(Q))) {
  stopifnot(file.exists(qpaths))
  Q <- read.table(qpath[1])
  stopifnot(identical(length(labels), nrow(Q)))
  
  l <- list()
  o <- order
  for (qpath in qpaths) {
    l[[basename(qpath)]] <- read.table(qpath)
    rownames(l[[basename(qpath)]]) <- labels
    colnames(l[[basename(qpath)]]) <- paste0("C",1:K)
    l[[basename(qpath)]] <- l[[basename(qpath)]][o,]
  }
  
  # clumpp Q matrices according to first rep
  for (r in 2:length(l)) {
    co <- paste0("C", apply(cor(l[[1]],l[[r]]), 2, which.max))
    l[[r]] <- l[[r]][,co]
  }
  
  # mean Q matrix
  Q <- data.frame(data.table::rbindlist(l, use.names = FALSE, idcol = T))
  Q <- data.frame(ID = rep(rownames(l[[1]]), length(l)), Q)
  QM <- aggregate(x = Q[,paste0("C", 1:K)], by = list(Q$ID), FUN = FUN)
  names(QM)[1] <- "ID"
  
  # return
  return(QM)
}

# barplotQ function
barplotQ <- function(Q, df, idvar = NULL, qvars = NULL,
                     popvar = NULL, sortvar = NULL,
                     sortby = c(popvar, sortvar, idvar),
                     palette = NULL, plot = TRUE, xlab = "", ylab = "Admixture Proportion",
                     rot.top = 90, rot.bottom = 90, size.top = 2.5, size.bottom = 5,
                     hjust.top = 0, hjust.bottom = 1, vjust.top = 0.5, vjust.bottom = 0.5) {
  
  ## Usage
  # Q         data.frame   Q matrix of admixture proportions, 1 column for each cluster.
  # df        data.frame   with annotation data, containing idvar, popvar, sortvar (if available). Must not be in the same order as Q, and can contain additional samples.
  # idvar     character    name of the sample ID variable in df and Q (optional, inferred from rownames if NULL).
  # qvars     character    string of variable names in Q containing admixture proportions. If NULL, these will be guessed to comprise all numeric variables in [0,1].
  # popvar    character    name of the population/species variable in df.
  # sortvar   character    name of an additional variable in df used for sorting.
  # sortby    character    string of variable names used for hierarchical sorting. Up to 3 may be given. By default, sorting is by popvar, then sortvar, then idvar.
  # palette   function     name of the color palette function. adegenet::funky by default. Can also be a color vector of the same length as K.
  # plot      boolean      if TRUE, will print the ggplot, otherwise the plot is invisibly returned.
  
  ## Helperfunctions
  ask <- function(package) {
    stop_quietly <- function() {
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }
    if (!package %in% installed.packages()) {
      q <- readline(prompt = message(paste0("The <", package, "> R package is required to proceed.\nDo you wish to install it? [y/n] ")))
      if (q %in% c("y","yes","Y","Yes","YES")) {
        install.packages(package)
      } else {
        message("Stopping.")
        stop_quietly()
      }
    }
  }
  funky <- function(n) {
    ramp <- grDevices::colorRampPalette(c("#A6CEE3","#1F78B4","#B2DF8A",
                                          "#33A02C","#FB9A99","#E31A1C",
                                          "#FDBF6F","#FF7F00","#CAB2D6",
                                          "#6A3D9A","#FFFF99","#B15928"))
    
    ramp(n)
  }
  factorize <- function(x) {
    if (!is.factor(x)) factor(x) else x
  }
  
  ## Libraries
  ask("ggplot2")
  ask("reshape2")
  library(ggplot2)
  
  ## Set defaults
  if (!is.null(idvar)) {
    stopifnot(idvar %in% colnames(Q),
              idvar %in% colnames(df))
  } else {
    if ("ID" %in% c(colnames(Q),colnames(df))) {
      stop("Please rename columns in Q and/or df (<ID> not allowed)")
    } 
    idvar <- "ID"
    Q[,idvar] <- rownames(Q)
    df[,idvar] <- rownames(df)
  }
  if (!is.null(qvars)) {
    stopifnot(all(qvars %in% colnames(Q)))
  } else {
    cl <- sapply(seq(ncol(Q)), function(x) class(Q[,x]))
    qvars <- colnames(Q)[cl %in% "numeric"]
    qvars <- qvars[apply(Q[,qvars], 2, function(x) min(x) >= 0 & max(x) <= 1)]
  }
  if (!is.null(popvar)) {
    stopifnot(popvar %in% colnames(df))
  } else {
    popvar <- "POP"
    df[,popvar] <- factor("")
  }
  if (!is.null(sortvar)) {
    stopifnot(sortvar %in% colnames(df))
  } else {
    sortvar <- "SORT"
    df[,sortvar] <- 1
  }
  
  Q <- data.frame(Q[,c(idvar, qvars)])
  df <- data.frame(df[,c(idvar, popvar, sortvar)])
  
  # factorize
  df[,popvar] <- factorize(df[,popvar])
  
  ## Check input
  stopifnot(inherits(Q, c("data.frame","matrix")),
            all(Q[,idvar] %in% df[,idvar]))
  
  ## Reshape to long format
  dQ <- reshape2::melt(Q, id = idvar)
  
  
  ## Annotate Q matrix
  dQ[,popvar] <- droplevels(df[,popvar][match(dQ[,idvar], df[,idvar])])
  dQ[,sortvar] <- df[,sortvar][match(dQ[,idvar], df[,idvar])]
  
  
  ## Sort Q matrix
  o <- switch(as.character(length(sortby)),
              "1" = {order(dQ[,sortby[1]])},
              "2" = {order(dQ[,sortby[1]], dQ[,sortby[2]])},
              "3" = {order(dQ[,sortby[1]], dQ[,sortby[2]], dQ[,sortby[3]])}
  )
  dQ <- dQ[o,]
  dQ[,idvar] <- factor(dQ[,idvar], levels = unique(dQ[,idvar]))
  dQ$SORT <- as.numeric(dQ[,idvar])
  
  ## Plot
  # vertical separators
  K <- length(qvars)
  # R <- length(which(Q[,idvar] %in% Q[1,idvar]))
  t <- table(dQ[!duplicated(dQ[,idvar]),popvar])
  coords <- t[1]/2
  for (i in 2:length(t)) {
    coords <- c(coords, sum(t[1:(i-1)]) + t[i]/2)
  }
  
  # ggplot
  if (is.null(palette)) {
    palette <- funky
  }
  if (is.character(palette)) {
    cpalette <- palette
    stopifnot(length(palette) >= K)
    palette <- function(n) {cpalette[1:n]}
  }
  
  p <- ggplot(dQ, aes(x = SORT, y = value,
                      fill = variable)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual(values = palette(K), guide = "none") +
    scale_x_continuous(breaks = coords,
                       labels = names(coords),
                       sec.axis = sec_axis(~.,
                                           breaks = 1:nlevels(dQ[,idvar]),
                                           labels = levels(dQ[,idvar]))) +
    labs(x = xlab, y = ylab, fill = "") +
    geom_vline(xintercept = cumsum(t)+0.5, size = 0.5) +
    theme_classic() +
    theme(
      axis.text.x.top = element_text(angle = rot.top, size = size.top,
                                     hjust = hjust.top, vjust = vjust.top),
      axis.text.x.bottom = element_text(angle = rot.bottom, size = size.bottom,
                                        hjust = hjust.bottom, vjust = vjust.bottom),
      
      legend.position = "bottom")
  
  # return
  if (plot) print(p)
  invisible(p)
  
}
