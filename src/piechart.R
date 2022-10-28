# # debug
# var.pop = "pop";
# var.lon = "x"; var.lat = "y";
# map = NULL; xlim = NULL; ylim = NULL;
# repel.pie = 12; col.pies = NULL; alpha.pie = 1;
# size.pie = 2; size.segement = 0.5; col.segment = "black";
# size.label = 2; nudge.label.x = 0; nudge.label.y = 0;
# plot = TRUE; legend.label = NULL; legend.pos = "bottom"

piechart <- function(df, var.pies, var.pop = "pop",
                     var.lon = "x", var.lat = "y",
                     map = NULL, xlim = NULL, ylim = NULL,
                     repel.pie = 12, col.pies = NULL, alpha.pie = 1,
                     size.pie = 2, size.segement = 0.5, col.segment = "black",
                     size.label = 2, nudge.label.x = 0, nudge.label.y = 0,
                     plot = TRUE, legend.label = NULL, legend.pos = "bottom") {
  
  ## Description
  # Visualize frequency pie charts on a map using repelled coordinates.
  
  ## Arguments
  # df            Input data.frame with one row per individual (if var.pies is a single variable 
  #               giving the majority cluster membership) or population (if var. pies is a vector
  #               of variable names giving the admixture proportions), containing variables specified
  #               with <var.pies>, <var.pop>, <var.lon>, and <var.lat>.
  # var.pies      Column name in <df> with major cluster membership (or haplogroup), or a vector of length > 1 giving the variables representing the admixture proportions.
  # var.pop       Column name in <df> with coordinate / population identifier. DEFAULT: "pop"
  # var.lon       Column name in <df> with longitude. DEFAULT: "x"
  # var.lat       Column name in <df> with latitude. DEFAULT: "y"
  # map           Object of class c("sf", "data.frame") used as map background. DEFAULT: empty map.
  # xlim          X axis limits. By default, range(x) + c(-1,1) * 0.1*range(x).
  # ylim          Y axis limits. By default, range(y) + c(-1,1) * 0.1*range(y).
  # repel.pie     Repulsion factor for pies, passed to fontsize argument of repel::repel_text()
  # col.pies      Vector of pie segment colors. Must be of the same length as the number of groups.
  # alpha.pie     Transparency for pie colors. 0 = transparent ; 1 = opaque.
  # size.pie      Size for pies.
  # size.segment  Linewidth of segments connecting lon/lat and repelled pie centers.
  # col.segment   Color of segments connecting lon/lat and repelled pie centers.
  # size.label    Size of pie labels.
  # nudge.label.x Amount of pie label nudging in x direction.
  # nudge.label.y Amount of pie label nudging in y direction.
  # plot          Whether to print the pie chart.
  # legend.label  Group factor legend title.
  # legend.pos    Legend position.
  
  ## Load / Install packages
  if (!"repel" %in% installed.packages()) {
    if (!"remotes" %in% installed.packages()) install.packages("remotes")
    remotes::install_github("hms-dbmi/repel")
  }
  if (!"scatterpie" %in% installed.packages()) install.packages("scatterpie")
  if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
  
  require(repel)
  require(scatterpie)
  require(dplyr)
  
  ## Check input
  stopifnot(inherits(df, "data.frame"), var.pies %in% names(df),
            var.pop %in% names(df), var.lon %in% names(df),
            var.lat %in% names(df),
            is.numeric(alpha.pie), alpha.pie >= 0, alpha.pie <= 1,
            is.numeric(size.pie), size.pie >= 0)
  
  ## Get frequencies per population
  # factorize
  pop <- df[,var.pop]
  if (!is.factor(pop)) pop <- factor(as.character(pop)) else pop <- droplevels(pop)
  
  if (length(var.pies) == 1) {
    fac <- df[,var.pies]
    if (!is.factor(fac)) fac <- factor(as.character(fac)) else fac <- droplevels(fac)
    
    # frequencies per population
    xtab <- tapply(X = fac, INDEX = pop,
                   FUN = function(x) {
                     data.frame(t(as.matrix(table(x)/length(na.omit(x)))), check.names = F)
                   })
  } else {
    fac <- factor(var.pies, levels = var.pies)
    
    # frequencies per population
    xtab <- lapply(df[,var.pop], function(x) df[df[,var.pop] == x,var.pies,drop=F])
    names(xtab) <- df[,var.pop]
  }
  
  dpie <- data.frame(label = names(xtab), dplyr::bind_rows(xtab), check.names = FALSE)
  names(dpie)[1] <- var.pop
  dpie <- merge(unique(df[,c(var.pop, var.lon, var.lat)]), dpie, by = var.pop, all = T)
  
  ## Repel coordinates
  label_coords <- dpie[,c(var.lon, var.lat)]
  label_coords$label <- "PP"
  names(label_coords) <- c("x","y","label")
  repels <- repel::repel_text(label_coords, fontsize = repel.pie)
  
  # stopifnot(all.equal(dpie[,var.pop], repels[,"label"]))
  names(repels) <- c("x_repelled", "y_repelled", "label", "too_many_overlaps")
  dpie <- data.frame(dpie, repels[,c("x_repelled", "y_repelled", "too_many_overlaps")], check.names = FALSE)
  
  ## Set default arguments
  if (is.null(xlim)) {
    xlim <- range(c(dpie[,var.lon], dpie[,c("x_repelled")]), na.rm = T)
    xlim <- xlim + c(-1,1)*diff(xlim)*0.1
  }
  if (is.null(ylim)) {
    ylim <- range(c(dpie[,var.lat], dpie[,c("y_repelled")]), na.rm = T)
    ylim <- ylim + c(-1,1)*diff(ylim)*0.1
  }
  if (is.null(col.pies)) {
    funky <- function (n) {
      ramp <- grDevices::colorRampPalette(c("#A6CEE3","#1F78B4","#B2DF8A",
                                            "#33A02C","#FB9A99","#E31A1C",
                                            "#FDBF6F","#FF7F00","#CAB2D6",
                                            "#6A3D9A","#FFFF99","#B15928"))
      ramp(n)
    }
    col.pies <- funky(nlevels(fac))
  }
  if (is.null(legend.label)) legend.label <- var.pies
  
  ## Plot map
  p <- ggplot(map) +
    geom_sf() +
    xlim(xlim) +
    ylim(ylim) +
    geom_segment(data = dpie, size = size.segement, color = col.segment,
                 aes_string(x = var.lon, xend = "x_repelled",
                            y = var.lat, yend = "y_repelled")) +
    geom_scatterpie(data = dpie, aes(x = x_repelled, y = y_repelled), 
                    cols = levels(fac), pie_scale = size.pie, alpha = alpha.pie) +
    scale_fill_manual(values = col.pies) +
    geom_text(data = dpie, aes_string(x = "x_repelled", y = "y_repelled",
                                      label = var.pop), size = size.label,
              nudge_x = nudge.label.x, nudge_y = nudge.label.y) +
    labs(x = "", y = "", fill = legend.label) +
    coord_sf() +
    theme_minimal() +
    theme(legend.position = legend.pos)
  
  if (plot) print(p)
  
  ## Return results
  invisible(list(dpie = dpie, df = df, p = p))
}
