splot <- function(df, x = names(df)[1], y = names(df)[2], flipx = FALSE, flipy = FALSE,
                  xlab = NULL, ylab = NULL, spiderlab = NULL, collab = NULL,
                  spiderfac = NULL, spider = ifelse(nlevels(spiderfac) > 1 | nlevels(df[,spiderfac]) > 1, TRUE, FALSE),
                  spider.size = 1, spider.alpha = 0.8,
                  colfac = spiderfac, points = TRUE, point.size = 3, point.alpha = 1,
                  palette.spider = NULL, palette.col = palette.spider,
                  labelfac = rownames(df), labels = FALSE, label.size = 2.5,
                  max.overlaps = 1000, plot = TRUE) {

  
  ## Arguments
  # df              input data.frame, with variables to plot on (x,y) axes, as well as variables to map as spider, colors and labels
  # x               df column name of variable on x axis [DEFAULT: 1st variable in df]
  # y               df column name of variable on y axis [DEFAULT: 2nd variable in df]
  # flipx           if TRUE, will invert the x axis (useful for ordination axes from PCA etc.) [DEFAULT: FALSE]
  # flipy           if TRUE, will invert the y axis (useful for ordination axes from PCA etc.) [DEFAULT: FALSE]
  # xlab            x axis label [DEFAULT: variable name]
  # ylab            y axis label [DEFAULT: variable name]
  # spiderlab       spider factor label [DEFAULT: variable name]
  # collab          color factor label [DEFAULT: variable name]
  # spiderfac       df column name of variable used to add spider graphs [DEFAULT: NULL]
  # spider          if TRUE, will add spider graph [DEFAULT: TRUE if spiderfac not NULL]
  # spider.size     width of spider lines [DEFAULT: 1]
  # spider.alpha    transparency of spider lines [DEFAULT: 0.8]
  # colfac          df column name of variable used to map colors to [DEFAULT: same as spider variable]
  # points          if TRUE, will draw sample points [DEFAULT: TRUE]
  # point.size      size of sample points [DEFAULT: 3]
  # point.alpha     transparency of sample points [DEFAULT: 1]
  # palette.spider  color palette for spider [DEFAULT: adegenet::funky]
  # palette.col     color palette for samples points [DEFAULT: same as spider color palette]
  # labelfac        df column name of sample labels variable [DEFAULT: rownames(df)]
  # labels          if TRUE, will draw repulsive point labels (needs ggrepel package) [DEFAULT: FALSE]
  # label.size      size of sample labels [DEFAULT: 2.5]
  # max.overlaps    passed to ggrepel::geom_text_repel: Exclude text labels that overlap too many things
  # plot            if TRUE, will print the ggplot [DEFAULT: TRUE]
  
  ## Details
  # The arguments x, y, spiderfac, colfac and/or labelfac can also be given as vectors rather than
  # variable names. This is especially useful if a variable to be mapped onto the plot is not
  # contained in the data.frame df. However, in such cases, the arguments xlab, ylab, spiderlab,
  # and/or collab must be given.
  
  # The color palettes can be specified as functions or vectors of valid colors.
  
  ## Author: sfcrameri@gmail.com, May 2022  
  
  # libraries
  library(ggplot2)
  
  # check input
  stopifnot(inherits(df, "data.frame"),
            is.logical(flipx), is.logical(flipy),
            is.logical(spider), is.logical(points), is.logical(labels))
  
  # check factors
  if (!is.null(x)) {
    if (is.character(x) & length(x) == 1) {
      stopifnot(x %in% names(df))
      if (is.null(xlab)) xlab <- x
      x <- df[,x]
    }
    stopifnot(length(x) == nrow(df),
              !xlab %in% c("", NA),
              is.character(xlab), length(xlab) == 1)
  }
  if (!is.null(y)) {
    if (is.character(y) & length(y) == 1) {
      stopifnot(y %in% names(df))
      if (is.null(ylab)) ylab <- y
      y <- df[,y]
    }
    stopifnot(length(y) == nrow(df),
              !ylab %in% c("", NA),
              is.character(ylab), length(ylab) == 1)
  }
  if (!is.null(spiderfac)) {
    if (is.character(spiderfac) & length(spiderfac) == 1) {
      stopifnot(spiderfac %in% names(df))
      if (is.null(spiderlab)) spiderlab <- spiderfac
      spiderfac <- df[,spiderfac]
    }
    stopifnot(length(spiderfac) == nrow(df),
              !spiderlab %in% c("", NA))
  } else {
    spiderfac <- rep(1, nrow(df))
    spiderlab <- ""
  }
  if (!is.null(colfac)) {
    if (is.character(colfac) & length(colfac) == 1) {
      stopifnot(colfac %in% names(df))
      if (is.null(collab)) collab <- colfac
      colfac <- df[,colfac]
    }
    stopifnot(length(colfac) == nrow(df))
  } else {
    colfac <- rep(1, nrow(df))
    collab <- ""
  }
  if (!is.null(labelfac) | identical(labelfac, rownames(df))) {
    if (is.character(labelfac) & length(labelfac) == 1) {
      stopifnot(labelfac %in% names(df))
      labelfac <- df[,labelfac]
    }
    stopifnot(length(labelfac) == nrow(df))
  } 
  
  # check factor labels
  if (is.null(spiderlab)) spiderlab = ""
  if (is.null(collab)) collab <- ""
  if (!is.null(labelfac)) {
    stopifnot(length(labelfac) == nrow(df))
  }
  
  # factorize
  if (!is.factor(colfac)) {
    colfac <- factor(colfac)
  }
  if (!is.factor(spiderfac)) {
    spiderfac <- factor(spiderfac)
  }
  
  # color palette
  funky <- function (n) {
    ramp <- grDevices::colorRampPalette(c("#A6CEE3","#1F78B4","#B2DF8A",
                                          "#33A02C","#FB9A99","#E31A1C",
                                          "#FDBF6F","#FF7F00","#CAB2D6",
                                          "#6A3D9A","#FFFF99","#B15928"))
    
    ramp(n)
  }
  if (is.null(palette.col)) {
    palette.col <- funky
  }
  if (is.null(palette.spider)) {
    palette.spider <- funky
  }
  if (!is.function(palette.col)) {pc <- palette.col ; palette.col = function(n) pc[1:n]}
  if (!is.function(palette.spider)) {ps <- palette.spider ; palette.spider = function(n) ps[1:n]}
  
  # flip
  if (flipx) x <- -1*x
  if (flipy) y <- -1*y
  
  # get plot data
  dplot <- data.frame(x = x,
                      y = y,
                      colfac = colfac,
                      spiderfac = spiderfac,
                      label = labelfac)
  
  # attach centroids
  if (spider) {
    centroids <- data.frame(apply(X = dplot[,c("x", "y")], MARGIN = 2,
                                  FUN = function(x) {tapply(x, INDEX = dplot[,"spiderfac"], FUN = mean)}))
    if (ncol(centroids) == c(1)) centroids <- data.frame(t(centroids))
    colnames(centroids) <- paste0(colnames(centroids), ".c")
    centroids[,"spiderfac"] <- levels(dplot[,"spiderfac"])
    dplot <- merge(dplot, centroids, by = "spiderfac", all = T, sort = F)
  }
  
  ## PLOT
  # base plot
  p <- ggplot(dplot, aes(x = x, y = y)) +
    labs(x = paste0(xlab),
         y = paste0(ylab))
  
  # add spider
  if (spider) {
    p <- p +
      geom_segment(aes_string(x ="x.c", 
                              xend = "x",
                              y = "y.c",
                              yend = "y",
                              color = "spiderfac"),
                   size = spider.size, alpha = spider.alpha,
                   show.legend = (!spiderlab %in% c("", NA))) +
      labs(color = spiderlab) +
      scale_color_manual(values = palette.spider(nlevels(dplot[,"spiderfac"])))
  }
  
  # add points
  if (points) {
    p <- p +
      geom_point(aes_string(x = "x", y = "y", fill = "colfac"), shape = 21,
                 size = point.size, alpha = point.alpha) +
      scale_fill_manual(values = palette.col(nlevels(dplot[,"colfac"])),
                        guide = if (collab %in% c("", NA)) "none" else "legend") +
      labs(fill = collab) +
      theme_bw()
  }
  
  # add point labels
  if (labels) {
    if (!"ggrepel" %in% installed.packages()) install.packages("ggrepel")
    p <- p +
      ggrepel::geom_text_repel(inherit.aes = FALSE,
                               aes_string(x = "x", y = "y"),
                               label = dplot[,"label"], size = label.size,
                               segment.size = 0.2, max.overlaps = max.overlaps)
  }
  
  ## Return plot
  if (plot) print(p)
  invisible(p)
}
