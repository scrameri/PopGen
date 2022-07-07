# # debug
# x = 1; y = 2; flipx = 1; flipy = 1; zoom = 0;
# variates = "X"; add = NULL; drop = NULL;
# .fillfac = NULL; .colfac = NULL; .hullfac = NULL; .spiderfac = NULL; .shapefac = NULL; .alphafac = NULL; .sizefac = NULL;
# fill.name = NULL; col.name = NULL; hull.name = NULL; spider.name = NULL; shape.name = NULL; alpha.name = NULL; size.name = NULL; 
# palette.fill = NULL; palette.col = NULL; palette.hull = NULL; palette.spider = NULL;
# points = NULL; point.size = 2.5; point.shape = 21; point.alpha = 0.4; point.lwd = 1;
# labels = FALSE; labels.cex = 2.5; labels.add = labels;
# hull = !is.null(.hullfac); hull.concavity = 100; hull.alpha = 0.2; hull.expand = unit(.15,"cm"); hull.labels = FALSE; hull.labels.cex = 6; hull.legend = TRUE;
# spider = !is.null(.spiderfac); spider.alpha = 0.8; spider.lwd = 0.5;
# biplot = FALSE; loadings = NULL; quantile = NULL; f = 1; biplot.col = c("tomato","blue"); biplot.cex = 2.5; biplot.lwd = 0.25; biplot.alpha = 0.5;
# plot = TRUE; legend.pos = "bottom"; legend.size = 5; legend.spacing = unit(0, "cm"); legend.key.size = unit(0.25, "cm")

cplot <- function(X, x = 1, y = 2, flipx = 1, flipy = 1, zoom = 0,
                  variates = "X", add = NULL, drop = NULL,
                  .fillfac = NULL, .colfac = NULL, .hullfac = NULL, .spiderfac = NULL, .shapefac = NULL, .alphafac = NULL, .sizefac = NULL,
                  fill.name = NULL, col.name = NULL, hull.name = NULL, spider.name = NULL, shape.name = NULL, alpha.name = NULL, size.name = NULL, 
                  palette.fill = NULL, palette.col = NULL, palette.hull = NULL, palette.spider = NULL,
                  points = NULL, point.size = 2.5, point.shape = 21, point.alpha = 0.4, point.lwd = 1,
                  labels = FALSE, labels.cex = 2.5, labels.add = labels,
                  hull = !is.null(.hullfac), hull.concavity = 100, hull.alpha = 0.2, hull.expand = unit(.15,"cm"), hull.labels = FALSE, hull.labels.cex = 6, hull.legend = TRUE,
                  spider = !is.null(.spiderfac), spider.alpha = 0.8, spider.lwd = 0.5,
                  biplot = FALSE, loadings = NULL, quantile = NULL, f = 1, biplot.col = c("tomato","blue"), biplot.cex = 2.5, biplot.lwd = 0.25, biplot.alpha = 0.5,
                  plot = TRUE, legend.pos = "bottom", legend.size = 5, legend.spacing = unit(0, "cm"), legend.key.size = unit(0.25, "cm"), ...) {
  
  ## Usage
  # X       data.frame, matrix or projection object of class prcomp, lda, and various related classes (see list of projected)
  # x       numeric or character    variable name, variable index, or index of component to plot on x axis
  # y       numeric or character    variable name, variable index, or index of component to plot on y axis
  # flipx   numeric     constant to multiply with x (-1 for flip)
  # flipy   numeric     constant to multiply with y (-1 for flip)
  # zoom    numeric     zoom in (positive) or out (negative). Zero is no zoom.
  # variates character  character string denoting the scores component, if that is a list (e.g. for mixOmics).
  # add     data.frame  new data to be projected to the same plot using the loadings matrix
  # drop    character  name of rows to drop from the plot
  # .fillfac factor or numeric   Additional variables (numeric or factor) mapped to X
  # .colfac factor or numeric
  # .hullfac factor
  # .spiderfac factor
  # .shapefac factor
  # .alphafac numeric or factor
  # .sizefac numeric or factor
  # X.name [X %in% c("fill","col","hull","spider","shape","alpha","size")] character (factor legend title) or NULL (no legend title)
  # pallete.X [X %in% c("fill","col","hull","spider")] factor color palette
  # points boolean ; draw points if TRUE
  # point.size/shape/alpha/lwd customize points drawn
  # labels boolean ; print repelled labels if TRUE (uses ggrepel)
  # labels.cex customize labels
  # labels.add boolean ; if TRUE, adds labels of points added via <add> argument
  # hull boolean ; draw hulls if TRUE
  # hull.concavity/alpha/expand/labels/labels.cex/legend customize hulls
  # spider boolean ; draw spiders if TRUE
  # spider.alpha/lwd customize spiders
  # biplot boolean ; if TRUE, adds ordination variable loadings as arrows
  # loadings character vector with name(s) of loading matrix or matrices. Set to "var.load" for dapc objects.
  # quantile numeric ; value [0,1] denoting the fraction of variables shown as arrows. 0 = all variables. 0.5 = best 50% of variables, 1 = no variables. By default, quantile is 0 if there are <= 100 variables, or 0.9 if there are > 100 variables.
  # f numeric ; variable loading arrow length expansion. 1 (DEFAULT) prevents loadings from sticking out of the plot, higher values make arrows longer
  # biplot.col/cex/lwd/alpha customize biplot arrows
  # plot boolean ; if TRUE, prints the ggplot
  # legend.pos character denoting the position of the legend ("top","left","bottom","right")
  # legend.size/spacing/key.size customize legend
  
  # author: sfcrameri@gmail.com, Jun 2022
  library(ggplot2)
  
  ## Check and re-organize input -----------------------------------------------
  
  # determine object class
  X.class <- sub("^pca$|^spca$|^ipca$|^sipca$|^mixo_pls$|^mixo_spls$|^mixo_plsda$|^mixo_splsda$", "mixOmics", class(X)[1])
  if ("prcomp" %in% class(X)) X.class <- "prcomp"
  if ("dudi" %in% class(X)) X.class <- "dudi"
  projected <- c("prcomp","princomp","Pca","pcoa","FAMD","PCAmix","dudi","Kpca","mixOmics","dapc","glPca","lda","LDA") # "Kplsr")
  
  # check input
  stopifnot(X.class %in% c("data.frame","matrix","umap","Kplsr","MDS","NMDS",projected))
  
  # determine x and y variables
  if (X.class %in% c("data.frame","matrix")) {
    
    if (length(x) == 1) {
      stopifnot(x %in% colnames(X),
                is.numeric(X[,x]))
      x <- which(colnames(X) == x)
    } else {
      stopifnot(identical(nrow(X), length(x)),
                is.numeric(x))
      X[,".X"] <- x
      x <- which(colnames(X) == ".X")
    }
    
    if (length(y) == 1) {
      stopifnot(y %in% colnames(X),
                is.numeric(X[,y]))
      y <- which(colnames(X) == y)
    } else {
      stopifnot(identical(nrow(X), length(y)),
                is.numeric(y))
      X[,".Y"] <- y
      y <- which(colnames(X) == ".Y")
    }
  }
  
  # factorize character vectors
  check.fac <- function(fac) {
    if (!is.null(fac)) {
      if (is.character(fac)) {
        if (length(fac) == 1) {
          if (!X.class %in% c("data.frame","matrix")) {
            stop("plot factor is a single character, X expected to be matrix-like")
          }
          if (!fac %in% colnames(X)) {
            stop("plot factor is a single character, variable not found in X")
          }
          facname <- fac
          fac <- X[,fac]
          if (is.character(fac)) {
            fac <- as.factor(fac)
          }
        } else {
          if (X.class %in% c("data.frame","matrix")) {
            stopifnot(identical(nrow(X), length(fac)))
          }
          facname <- NULL
          fac <- as.factor(fac)
        }
      }
      if (is.numeric(fac)) {
        if (X.class %in% c("data.frame","matrix")) {
          stopifnot(identical(nrow(X), length(fac)))
        }
        facname <- NULL
        #fac <- as.factor(fac)
      }
      if (is.factor(fac)) {
        if (X.class %in% c("data.frame","matrix")) {
          stopifnot(identical(nrow(X), length(fac)))
        }
        facname <- NULL
      }
      if (is.logical(fac)) {
        fac <- facname <- NULL
      }
    } else {
      facname <- NULL
    }
    return(list(fac = fac, facname = facname))
  }
  .colfac <- check.fac(.colfac) ; colfac.name <- .colfac$facname ; .colfac <- .colfac$fac
  .fillfac <- check.fac(.fillfac) ; fillfac.name <- .fillfac$facname ; .fillfac <- .fillfac$fac
  .shapefac <- check.fac(.shapefac) ; shapefac.name <- .shapefac$facname ; .shapefac <- .shapefac$fac
  .alphafac <- check.fac(.alphafac) ; alphafac.name <- .alphafac$facname ; .alphafac <- .alphafac$fac
  .sizefac <- check.fac(.sizefac) ; sizefac.name <- .sizefac$facname ; .sizefac <- .sizefac$fac
  .hullfac <- check.fac(.hullfac) ; hullfac.name <- .hullfac$facname ; .hullfac <- .hullfac$fac
  .spiderfac <- check.fac(.spiderfac) ; spiderfac.name <- .spiderfac$facname ; .spiderfac <- .spiderfac$fac
  
  # factor names
  if (is.null(col.name)) col.name <- colfac.name
  if (is.null(fill.name)) fill.name <- fillfac.name
  
  if (is.null(shape.name)) shape.name <- shapefac.name
  if (is.null(alpha.name)) alpha.name <- alphafac.name
  if (is.null(size.name)) size.name <- sizefac.name
  
  if (is.null(hull.name)) hull.name <- hullfac.name
  if (is.null(spider.name)) spider.name <- spiderfac.name  
  
  # (re)set points argument
  if (is.null(points)) {
    points <- any(c(!is.null(.colfac), !is.null(.fillfac), !is.null(.shapefac), !is.null(.alphafac), !is.null(.sizefac)))
  }
  if (!points & all(c(is.null(.colfac), is.null(.fillfac), is.null(.shapefac), is.null(.alphafac), is.null(.sizefac), is.null(.hullfac), is.null(.spiderfac)))) {
    points <- TRUE 
  }
  
  # reset loadings argument
  if (is.null(loadings)) {
    loadings <- c("X","Y", # mixOmics
                  "var.load","loadings", # DAPC
                  "quanti.var","quali.var", # FAMD
                  "quanti","quali" # PCAmix
    )
  }
  
  # reset palette argument
  funky <- function (n) {
    ramp <- grDevices::colorRampPalette(c("#A6CEE3","#1F78B4","#B2DF8A",
                                          "#33A02C","#FB9A99","#E31A1C",
                                          "#FDBF6F","#FF7F00","#CAB2D6",
                                          "#6A3D9A","#FFFF99","#B15928"))                             
    ramp(n)
  }
  eco <- function(n) {
    ramp <- grDevices::colorRampPalette(c("yellow","purple","green4",
                                          "pink","red","darkgreen","orange"))
    ramp(n)
  }
  viridis <- function(n) {
    ramp <- grDevices::colorRampPalette(c("#440154FF","#482173FF","#433E85FF",
                                          "#38598CFF","#2D708EFF","#25858EFF",
                                          "#1E9B8AFF","#2BB07FFF","#51C56AFF",
                                          "#85D54AFF","#C2DF23FF","#FDE725FF"))
    ramp(n)
  }
  plasma <- function(n) {
    ramp <- grDevices::colorRampPalette(c("#0D0887FF","#3E049CFF","#6300A7FF",
                                          "#8707A6FF","#A62098FF","#C03A83FF",
                                          "#D5546EFF","#E76F5AFF","#F58C46FF",
                                          "#FDAD32FF","#FCD225FF","#F0F921FF"))
    ramp(n)
  }
  
  if (is.null(palette.spider)) palette.spider <- funky
  if (is.null(palette.hull)) palette.hull <- eco
  if (is.null(palette.col)) palette.col <- viridis
  if (is.null(palette.fill)) palette.fill <- plasma
  
  # specify internal parameters
  add_biplot <- biplot & X.class %in% projected
  add_to_plot <- !is.null(add) & X.class %in% projected
  drop_from_plot <- !is.null(drop)
  
  
  ## Exctract S (scores), L (loadings) and V (variance) comopnents -------------
  
  # scores matrix
  S <- switch(X.class,
              "data.frame" = data.frame(X),
              "matrix" = data.frame(X),
              "umap" = data.frame(X$layout),
              "prcomp" = data.frame(X$x),
              "princomp" = data.frame(X$scores),
              "Pca" = data.frame(X$T),
              "pcoa" = data.frame(X$vectors),
              "FAMD" = data.frame(X$ind$coord),
              "PCAmix" = data.frame(X$ind$coord),
              "Kpca" = data.frame(X$T),
              "dudi" = data.frame(X$li),
              "mixOmics" = data.frame(X$variates[[variates]]), # $x is same as $variates$X
              "dapc" = data.frame(X$ind.coord),
              "glPca" = data.frame(X$scores),
              "lda" = data.frame(predict(X)$x),
              "LDA" = data.frame(X$mod.pred$x),
              "Kplsr" = data.frame(X$T),
              "MDS" = data.frame(X$x),
              "NMDS" = data.frame(X$points))
  
  # loadings matrix
  L <- switch(X.class,
              # "data.frame" = NULL,
              # "matrix" = NULL,
              # "umap" = NULL,
              "prcomp" = X$rotation,
              "princomp" = X$loadings,
              "Pca" = X$P,
              "pcoa" = {
                if (biplot & is.null(X$Y)) {
                  stop("If biplot = TRUE, you need to supply the original or rescaled input data.frame to PCoA as X$Y ")
                }
                if (!is.null(X$Y)) {
                  colnames(X$vectors) <- colnames(S) <- paste0("PCo", 1:ncol(X$vectors))
                  COV <- cov(x = X$Y, y = scale(S[,c(x,y)]))
                  U <- COV %*% diag((X$values$Eigenvalues[c(x,y)]/(nrow(X$Y) - 1))^(-0.5))
                  colnames(U) <- colnames(COV)
                } else {
                  U <- NULL
                }
                U
              },
              "FAMD" = {
                
                dqual <- X$call$X[,X$call$type == "n"]
                dquant <- X$call$X[,!X$call$type %in% "n"]
                
                var.quant <- colnames(dquant)
                var.qual <-  colnames(dqual)
                
                load.quant <- X$quanti.var$coord

                ll <- sapply(names(dqual), function(x) levels(dqual[,x]))
                lev.qual <- unlist(sapply(seq(length(ll)), function(x) {paste(names(ll)[x], ll[[x]], sep = "_")}))
                load.qual <- X$quali.var$coord
                rownames(load.qual) <- lev.qual
                
                L <- list(quanti.var = load.quant, quali.var = load.qual)
                L
              },
              "PCAmix" = {
                L <- list(quanti = X$quanti$coord,
                          quali = X$levels$coord,
                          qualicontrib = X$quali$contrib)
                L
              },
              "Kpca" = X$P,
              "dudi" = {
                L <- X$c1
                # colnames(L) <- colnames(S)
                as.matrix(L)
              },
              "mixOmics" = X$loadings[loadings], # $rotation is same as $loadings$X
              "dapc" = {
                L.pc <- !is.null(X$loadings)
                L.v <- !is.null(X$var.load)
                l.pc <- "loadings" %in% loadings
                l.v <- "var.load" %in% loadings
                
                # PCA loadings
                if (L.pc & l.pc & !l.v) L <- X$loadings
                
                # var loadings
                if (L.v & l.v & !l.pc) L <- X$var.load
                
                # PCA and var loadings
                if (L.v & L.pc & l.pc & l.v) L <- X[c("loadings","var.load")]
                
                L
                # if (!is.null(X$var.load)) X$var.load else X$loadings # X$loadings gives PCA loadings
              },
              "glPca" = {
                if (!is.null(X$loadings)) L <- X$loadings else L <- NULL
              },
              "lda" = X$scaling,
              "LDA" = X$mod$scaling,
              "Kplsr" = NULL,
              "MDS" = NULL,
              "NMDS" = NULL)
  
  # convert L matrix to list for compatibility with X and Y loadings (mixOmics)
  if (!is.list(L)) {
    L <- list(X = L)
    loadings <- "X"
  }
  loadings <- loadings[loadings %in% names(L)]
  
  
  # percent explained variation
  v <- switch(X.class,
              "data.frame" = NULL,
              "matrix" = NULL,
              "umap" = NULL,
              "prcomp" = X$sdev^2 / sum(X$sdev^2),
              "princomp" = X$sdev^2 / sum(X$sdev^2),
              "Pca" = X$eig / sum(X$eig),
              "pcoa" = X$values$Rel_corr_eig,
              "FAMD" = X$eig[,"percentage of variance"]/100,
              "PCAmix" = X$eig[,"Proportion"]/100,
              "dudi" = X$eig / sum(X$eig),
              "Kpca" = X$eig / sum(X$eig),
              "mixOmics" = X$prop_expl_var[[variates]],
              "dapc" = X$eig / sum(X$eig),
              "glPca" = X$eig / sum(X$eig),
              "lda" = X$svd^2 / sum(X$svd^2),
              "LDA" = X$mod$svd^2 / sum(X$mod$svd^2),
              "Kplsr" = NULL,
              "MDS" = NULL,
              "NMDS" = NULL)
  
  # component name
  n <- switch(X.class,
              "data.frame" = colnames(X),
              "matrix" = colnames(X),
              "umap" = "Comp.",
              "prcomp" = "PC",
              "princomp" = "PC",
              "Pca" = "PC",
              "pcoa" = "PCo",
              "FAMD" = "Dim",
              "PCAmix" = "Dim",
              "dudi" = sub("[0-9 ]+", "", colnames(S)),
              "Kpca" = "KPC",
              "mixOmics" = unique(sub("[0-9 ]+", "", colnames(S))),
              "dapc" = "LD",
              "glPca" = "PC",
              "lda" = "LD",
              "LDA" = "LD",
              "Kplsr" = "LV",
              "MDS" = "MDS",
              "NMDS" = "MDS")
  
  compname <- function(X, x, class = X.class, v = NULL) {
    if (class %in% c("data.frame","matrix")) {
      n[x]
    } else if (class %in% c("umap","Kplsr","MDS","NMDS")) {
      paste(n, x)
    } else if (class %in% projected) {
      paste0(n, " ", x, " (", round(100*v[x], 2), "%)")
    }
  }
  
  ## Add or drop points from data ----------------------------------------------
  
  # add
  if (add_to_plot) {
    stopifnot(inherits(add, c("matrix","data.frame")),
              all(rownames(L[[loadings]]) %in% colnames(add)))
    
    Sadd <- as.matrix(add[,rownames(L[[loadings]])]) %*% L[[loadings]]
    Sadd[,x] <- Sadd[,x]*flipx
    Sadd[,y] <- Sadd[,y]*flipy
    Sadd <- data.frame(Sadd)
  }
  
  # drop
  if (drop_from_plot) {
    stopifnot(inherits(drop, c("matrix","data.frame","character","factor")))
    
    if (class(drop) %in% c("character","factor")) {
      if (!all(as.character(drop) %in% rownames(S))) {
        warning("Undefined rows selected in <drop>")
      }
      keep <- !rownames(S) %in% as.character(drop)
      S <- S[keep,]
    } else if (class(drop) %in% c("matrix","data.frame")) {
      if (!all(rownames(drop) %in% rownames(S))) {
        warning("Not all rownames(drop) in rownames(X)!")
      }
      keep <- !rownames(S) %in% rownames(drop)
      S <- S[keep,]
    } else if (class(drop) %in% c("integer")) {
      if (!all(drop) %in% seq_along(nrow(S))) {
        warning("Undefined rows selected in <drop>")
      }
      keep <- !seq_along(nrow(X)) %in% drop
      S <- S[keep,]
    }
    .hullfac <- .hullfac[keep]
    .spiderfac <- .spiderfac[keep]
    .colfac <- .colfac[keep]
    .fillfac <- .fillfac[keep]
    .shapefac <- .shapefac[keep]
    .alphafac <- .alphafac[keep]
    .sizefac <- .sizefac[keep]
  }
  
  ## Plot data -----------------------------------------------------------------
  
  # flip
  if (ncol(S) < 2) x <- y <- 1
  S[,x] <- S[,x]*flipx
  S[,y] <- S[,y]*flipy
  
  print(str(L))
  print(x)
  print(y)
  print(loadings)
  
  if (biplot) {
    for (i in seq(length(L))) {
      L[[i]][,x] <- L[[i]][,x]*flipx
      L[[i]][,y] <- L[[i]][,y]*flipy
    }
  }
  
  # visualize
  xlim <- range(S[,x])
  ylim <- range(S[,y])
  if (!is.null(.hullfac)) {
    fillname <- substitute(.hullfac)
  }  else if (!is.null(.fillfac)) {
    fillname <- substitute(.fillfac)
  } else {
    fillname = ""
  }
  if (!is.null(.spiderfac)) {
    colorname <- substitute(.spiderfac)
  }  else if (!is.null(.colfac)) {
    colorname <- substitute(.colfac)
  } else {
    colorname = ""
  }
  p <- ggplot(data = S, aes_string(x = colnames(S)[x], y = colnames(S)[y])) +
    labs(x = compname(X = X, x = x, v = v),
         y = compname(X = X, x = y, v = v),
         # fill = fillname,
         fill = "",
         color = "", alpha = "", size = "", shape = "")
  
  if (hull) {
    
    library(ggforce)
    
    col.hull <- palette.hull(nlevels(.hullfac))[which(levels(.hullfac) %in% unique(.hullfac))]
    
    p <- p +
      geom_mark_hull(inherit.aes = F, 
                     aes_string(x = colnames(S)[x], y = colnames(S)[y],
                                fill = .hullfac, label = if (hull.labels) .hullfac else NULL),
                     concavity = hull.concavity, 
                     radius = unit(.15,"cm"),
                     expand = hull.expand,
                     alpha = hull.alpha,
                     size = .1,
                     label.fontsize = hull.labels.cex,
                     con.size = 0.25,
                     con.type = "elbow",
                     con.cap = unit(0.5, "mm"),
                     show.legend = hull.legend,
                     na.rm = TRUE)
    
    if (hull.legend) {
      p <- p +
        scale_fill_manual(values = col.hull) +
        guides(fill = guide_legend(title = hull.name,
                                   # title.theme = element_text(size = legend.size),
                                   # label.position = "bottom",
                                   override.aes = list(stroke = 0, size = 0, linetype = 0) # does not work!?
                                   #title.position = "top"
        ))
    }
    
  }
  
  if (spider) {
    if (is.factor(.spiderfac)) {
      centroids <- data.frame(apply(X = S[,c(x,y)], MARGIN = 2, FUN = function(x) {tapply(x, INDEX = droplevels(.spiderfac), FUN = mean)}))
      colnames(centroids) <- paste0(colnames(centroids), ".c")
      centroids$.spiderfac <- levels(droplevels(.spiderfac))
      Sc <- merge(data.frame(S[,c(x,y)], .spiderfac, id = seq(nrow(S))), centroids, by = ".spiderfac", all = T, sort = F)
      Sc <- Sc[order(Sc$id),]
    } else {
      Sc <- data.frame(rep(mean(S[,x],na.rm=T), nrow(S)), rep(mean(S[,y], na.rm=T), nrow(S)))
      colnames(Sc) <- paste0(colnames(S[,c(x,y)]), ".c")
      Sc <- cbind(S[,c(x,y)], Sc)
    }
    
    col.spider <- palette.spider(nlevels(.spiderfac))[which(levels(.spiderfac) %in% unique(.spiderfac))]
    
    p <- p + geom_segment(data = Sc, inherit.aes = FALSE,
                          aes_string(x = paste0(colnames(S)[x], ".c"), 
                                     xend = colnames(S)[x], 
                                     y = paste0(colnames(S)[y], ".c"),
                                     yend = colnames(S)[y],
                                     color = .spiderfac),
                          size = spider.lwd, alpha = spider.alpha,
                          show.legend = TRUE) +
      scale_color_manual(values = col.spider, name = spider.name)
  }
  
  if (points) {
    # cleg = "legend"
    if (!is.null(.shapefac)) point.shape <- NULL
    
    if (!is.null(.colfac) & !is.null(.spiderfac) | !is.null(.fillfac) & !is.null(.hullfac)) {
      need.pckg <- "ggnewscale"
      if (any(!need.pckg %in% installed.packages())) {
        for (i in need.pckg[!need.pckg %in% installed.packages()]) {
          msg <- paste0("The <", i, "> package is needed. Do you wish to install it? [y/n]")
          q <- readline(prompt = message(msg))
          if (grepl("^y|^Y", q)) install.packages(i)
        }
      }
      if (!all(need.pckg %in% installed.packages())) {
        warning("Multiple color aestetics needed. Please install <ggnewscale>")
      }
    }
    if (!is.null(.colfac) & !is.null(.spiderfac)) {
      p <- p + 
        ggnewscale::new_scale_color() + labs(color = "")
    }
    if (!is.null(.fillfac) & !is.null(.hullfac)) {
      p <- p + 
        ggnewscale::new_scale_fill() + labs(fill = "")
    }
    
    # set dots (point.size only if .sizefac = NULL, etc. for alpha and shape)
    # dots <- list(...)
    
    if (!is.null(.fillfac) & is.null(.shapefac)) {
      p <- p +
        geom_point(inherit.aes = FALSE,
                   aes_string(x = colnames(S)[x], y = colnames(S)[y], 
                              color = .colfac, fill = .fillfac, size = .sizefac,
                              shape = .shapefac, alpha = .alphafac),
                   shape = point.shape, ...)
    } else {
      p <- p +
        geom_point(inherit.aes = FALSE,
                   aes_string(x = colnames(S)[x], y = colnames(S)[y], 
                              color = .colfac, fill = .fillfac, size = .sizefac,
                              shape = .shapefac, alpha = .alphafac),
                   ...)
    }
    
    # set scales
    if (!is.null(.shapefac)) {
      p <- p +
        scale_shape_manual(values = 21:25)
    }
    if (!is.null(.colfac) & is.factor(.colfac)) {
      col.color <- palette.col(nlevels(.colfac))[which(levels(.colfac) %in% unique(.colfac))]
      p <- p +
        scale_color_manual(values = col.color) # , guide = cleg
    }
    if (!is.null(.colfac) & is.numeric(.colfac)) {
      N <- 3
      col.color <- palette.col(N)
      p <- p +
        scale_color_gradient2(low = col.color[1], mid = col.color[(N+1)/2], high = col.color[N],
                              midpoint = mean(.colfac, na.rm = TRUE))
    }
    if (!is.null(.fillfac) & is.factor(.fillfac)) {
      col.fill <- palette.fill(nlevels(.fillfac))[which(levels(.fillfac) %in% unique(.fillfac))]
      p <- p +
        scale_fill_manual(values = col.fill)
    }
    if (!is.null(.fillfac) & is.numeric(.fillfac)) {
      N <- 3
      col.fill <- palette.fill(N)
      p <- p +
        scale_fill_gradient2(low = col.fill[1], mid = col.fill[(N+1)/2], high = col.fill[N],
                             midpoint = mean(.fillfac, na.rm = TRUE))
    }
  }
  
  if (add_biplot) {
    add.biplot <- function(L, quantile = NULL, f = 1) {
      # set quantile
      if (is.null(quantile)) {
        if (nrow(L) > 100) quantile <- 0.9 else quantile <- 0
      }
      
      # scale loadings to scores
      sfac <- diff(range(S[,c(x,y)]))/2.5
      m <- max(abs(L[,c(x,y)]))
      L.scaled <- apply(L[,c(x,y)], 2, function(x) x/(m/sfac))
      
      # get arrow lengths
      l <- apply(L.scaled, 1, function(x) sqrt(x[1]^2 + x[2]^2))
      t <- quantile(l, quantile)
      a <- L.scaled[l >= t,,drop=F]
      
      # subset top-quantile arrows
      darr <- data.frame(x = a[,1]*f, y = a[,2]*f, row.names = rownames(a))
    }
    
    # check quantile, f, biplot.cex, biplot.col
    if (length(loadings) > 1) {
      if (!is.null(quantile) & length(quantile) == 1) quantile <- rep(quantile, 2)
      if (length(f) == 1) f <- rep(f, 2)
      if (length(biplot.cex) == 1) biplot.cex <- rep(biplot.cex, 2)
      if (length(biplot.col) == 1) biplot.col <- rep(biplot.col, 2)
      if (length(biplot.lwd) == 1) biplot.lwd <- rep(biplot.lwd, 2)
      if (length(biplot.alpha) == 1) biplot.alpha <- rep(biplot.alpha, 2)
    }
    
    for (i in loadings) {
      
      library(ggrepel)
      
      id <- which(loadings == i)
      
      # create arrow segments
      darr <- add.biplot(L = L[[i]], quantile = quantile[id], f = f[id])
      darr$label <- rownames(darr)
      assign(paste0("darr.", i), darr)
      
      # plot arrows
      p <- p +
        geom_segment(data = get(paste0("darr.", i)), inherit.aes = FALSE,
                     aes(x = 0, y = 0, xend = x, yend = y),
                     arrow = arrow(angle = 30, length = unit(.25,"cm")),
                     size = biplot.lwd[id], alpha = biplot.alpha[id],
                     color = biplot.col[id]) +
        geom_text_repel(data = get(paste0("darr.", i)), inherit.aes = FALSE,
                        aes(x = x, y = y, label = label),
                        color =  biplot.col[id],
                        size = biplot.cex[id])
      xlim <- range(c(xlim, darr$x))
      ylim <- range(c(ylim, darr$y))
    }
  }
  
  if (add_to_plot) {
    p <- p +
      geom_point(data = Sadd, inherit.aes = FALSE,
                 aes_string(x = paste0(n, x), y = paste0(n, y)),
                 size = point.size*1.5, alpha = point.alpha*1.5,
                 fill = "tomato", colour = "black", shape = point.shape)
    xlim <- range(c(xlim, Sadd[,x]))
    ylim <- range(c(ylim, Sadd[,y]))
  }
  
  if (labels) {
    library(ggrepel)
    p <- p +
      geom_text_repel(inherit.aes = FALSE,
                      aes_string(x = paste0(n, x), y = paste0(n, y)),
                      label = rownames(S), size = labels.cex, segment.size = 0.2, ...)
  }
  
  if (add_to_plot & labels.add) {
    library(ggrepel)
    p <- p +
      geom_text_repel(data = Sadd, inherit.aes = FALSE,
                      aes_string(x = paste0(n, x), y = paste0(n, y)),
                      label = rownames(Sadd), size = labels.cex*1.2, segment.size = 0.2, ...)
  }
  
  # add zoom and theme
  margin <- c(-1,1)*zoom*-0.1
  xlim <- xlim + margin*abs(diff(xlim))
  ylim <- ylim + margin*abs(diff(ylim))
  
  p <- p +
    lims(x = xlim, y = ylim) +
    labs(
      fill = fill.name,
      color = if (!is.null(.spiderfac)) spider.name else col.name,
      shape = shape.name,
      alpha = alpha.name,
      size = size.name
    ) +
    theme_bw() +
    theme(legend.position = legend.pos,
          legend.text = element_text(size = legend.size),
          legend.spacing = legend.spacing,
          legend.key.height = legend.key.size,
          legend.key.width = legend.key.size)
  
  # plot
  if (plot) {try(print(p), silent = TRUE)}
  
  # return
  invisible(p)
  
}
