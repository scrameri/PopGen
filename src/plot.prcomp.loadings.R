plot.prcomp.loadings <- function(prcomp, x = 1, y = 2, flipx = -1, flipy = -1,
                                 cex = 5, scale = FALSE, scaling = 0.8) {
  
  ## Check
  stopifnot(inherits(prcomp, "prcomp"),
            is.numeric(scaling),
            scaling >= 0, scaling <=1)
  
  ## get scores and rotation matrix
  scores <- prcomp$x
  rot <- prcomp$rotation
  prcomp$expl.var <- 100*prcomp$sdev^2/sum(prcomp$sdev^2)
  space <- 1:2
  
  ## rescale
  # scaling factor: <scaling> times the smaller ratio of
  # maximum absolute coordinate and
  # maximum absolute variable loading
  fac <- scaling * min(apply(scores[,1:2], 2, function(x) max(abs(x))) / 
                         apply(rot[,1:2], 2, function(x) max(abs(x))))
  if (scale) {
    rot.scaled <- rot * fac
    # xlim <- ylim <- c(NA,NA)
  } else {
    rot.scaled <- rot
    xlim <- c(-.5,.5)
    ylim <- c(-.5,.5)
  }
  
  ## plot
  p <- ggplot(data.frame(rot.scaled)) +
    geom_segment(aes(x = 0, xend = flipx*rot.scaled[,x],                     
                     y = 0, yend = flipy*rot.scaled[,y]), arrow = arrow()) +
    geom_label(aes(x = flipx*rot.scaled[,x],
                   y = flipy*rot.scaled[,y],
                   label = rownames(rot.scaled)),
              size = cex) +
    labs(x = paste0("PC ", x," (", round(prcomp$expl.var[x], 2), "%)"),
         y = paste0("PC ", y, " (", round(prcomp$expl.var[y], 2), "%)")) +
    theme_bw() +
    theme(axis.text = element_text(size = axis.cex),
          axis.title = element_text(size = axis.cex))
  
  if (!scale) {
    p <- p + lims(x = xlim, y = ylim)
  }
  
  print(p)
  invisible(p)
}
