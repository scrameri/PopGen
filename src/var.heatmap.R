# var.heatmap
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
