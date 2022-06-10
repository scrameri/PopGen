## filter genind
filter.gi <- function(gi, biallelic.only = TRUE, exp.char = c("A","T","C","G"), expected.only = TRUE,
                      min.mac = 2, max.missing = 0.05) {
  
  stopifnot(class(gi)[1] == "genind")
  
  ## Reduce to biallelic SNPs only
  if (biallelic.only) {
    locbi <- which(gi@loc.n.all == 2)
    gibi <- gi[loc = locbi, drop = TRUE]
    cat(paste0("excluded ", nLoc(gi)-length(locbi), " (", round(100*(nLoc(gi)-length(locbi))/nLoc(gi),2), "%) non-biallelic SNPs, ", nLoc(gibi), " remain\n"))
  } else {
    gibi <- gi
  }
  
  ## Exclude SNPs with other than exp.char
  if (expected.only) {
    weird <- sapply(gibi@all.names, function(x) any(!x %in% exp.char))
    gigood <- gibi[loc = which(!weird), drop = TRUE]
    cat(paste0("excluded ", sum(weird), " (", round(100*sum(weird)/nLoc(gibi),2), "%) SNPs with characters deviating from ", paste(exp.char, collapse = ", "), ", ", nLoc(gigood), " remain\n"))
  } else {
    gigood <- gibi
  }
  
  ## Remove SNPs with minor allele count (MAC) less than mac (mac = 2 removes singletons)
  ac <- apply(gigood@tab, 2, sum, na.rm = TRUE)
  locmac <- unique(sapply(strsplit(names(which(ac < min.mac)), split = "[.]"), "[", 1))
  gimin <- gigood[loc = locNames(gigood)[!locNames(gigood) %in% locmac], drop = TRUE]
  cat(paste0("excluded ", length(locmac), " (", round(100*length(locmac)/nLoc(gigood),2), "%) SNPs with minor allele count smaller than ", min.mac, ", ", nLoc(gimin), " remain\n"))
  
  ## Exclude SNPs with > max.missing missingness
  # missingness >= 1 is interpreted as number of missing genotypes
  # missingness <1 is interpreted as percentage of missing genotypes
  if (max.missing >= 1) max.missing <- max.missing / nInd(gimin)
  dmis <- apply(gimin@tab, 2, function(x) {sum(is.na(x))/length(x)})
  locsel <- unique(sapply(strsplit(names(which(dmis <= max.missing)), split = "[.]"), "[", 1))
  if (length(locsel) == 0) {
    print(summary(dmis))
    stop("max.missing is set too low, no variants remain. See missingness summary above.")
  }
  gi.cleaned <- gimin[loc = locsel, drop = TRUE]
  cat(paste0("excluded ", nLoc(gimin)-length(locsel), " (", round(100*(nLoc(gimin)-length(locsel))/nLoc(gimin),2), "%) SNPs with missingness above ", round(100*max.missing, 2), "%, ", nLoc(gi.cleaned), " remain\n"))
  
  ## Return filtered gi
  return(gi.cleaned)
}

## subset genind
subset.gi <- function(gi, subset.ind = TRUE, ind = NULL, excl = NULL, class = "Species", level = NULL,
                      biallelic.only = TRUE, expected.only = TRUE, exp.char = c("A", "T", "G", "C"),
                      min.mac = 2, max.missing = 0.05,
                      subset.regions = FALSE, subset.snps = FALSE, n = 1) {
  
  # Check input
  stopifnot(inherits(gi, "genind"))
  if (is.null(gi@other$locus)) gi@other$locus <- factor(sapply(strsplit(locNames(gi), split = "_"), "[", 1))
  if (is.null(gi@other$pos)) gi@other$pos <- as.numeric(sapply(strsplit(locNames(gi), split = "_"), "[", 2))
  if (subset.regions) {
    if (is.null(gi@other$lg)) gi@other$lg <- d.probes[gsub("merged","",as.character(gi@other$locus)),"LG"] # d.probes
    if (is.null(gi@other$knownlg)) gi@other$knownlg <- factor(ifelse(substring(gi@other$lg, 1, 3) == "Sca", 0, 1))
  }
  
  # Subset individuals
  if (subset.ind) {
    cat("subsampling individuals...\n")
    
    if (is.character(gi@other[[class]])) gi@other[[class]] <- factor(gi@other[[class]])
    if (is.null(level)) level <- levels(gi@other[[class]])
    
    if (!is.null(ind)) {
      gisub <- gi[gi@other[[class]] %in% level | indNames(gi) %in% ind, drop = TRUE]
    } else {
      gisub <- gi[gi@other[[class]] %in% level, drop = TRUE]
    }
    if (!is.null(excl)) {
      gisub <- gisub[!indNames(gisub) %in% excl, drop = TRUE]
    }
  } else {
    gisub <- gi
  }
  if (any(excl %in% indNames(gisub))) gisub <- gisub[-which(indNames(gisub) %in% excl), drop = F]
  cat(paste0(nInd(gisub), " (", round(100*nInd(gisub)/nInd(gi), 2), "%) individuals remain\n"))
  
  # Subset Variants
  gi.filt <- filter.gi(gi = gisub, biallelic.only = biallelic.only, expected.only = expected.only,
                       exp.char = exp.char, min.mac = min.mac, max.missing = max.missing)
  gi.filt@other$locus <- factor(sapply(strsplit(locNames(gi.filt), split = "_"), "[", 1))
  gi.filt@other$pos <- as.numeric(sapply(strsplit(locNames(gi.filt), split = "_"), "[", 2))
  if (subset.regions) {
    if (!exists("d.probes")) stop("<d.probes> not loaded")
    gi.filt@other$lg <- d.probes[gsub("merged","",as.character(gi.filt@other$locus)),"LG"] # needs d.probes
    gi.filt@other$knownlg <- factor(ifelse(substring(gi.filt@other$lg, 1, 3) == "Sca", 0, 1))
  }
  
  if (subset.regions) {
    cat("subsampling regions...\n")
    
    nloc1 <- nLoc(gi.filt)
    nreg1 <- nlevels(droplevels(gi.filt@other$locus))
    stopifnot(nloc1 == length(gi.filt@other$knownlg))
    gi.filt <- gi.filt[loc = locNames(gi.filt)[which(gi.filt@other$knownlg == "1")], drop = TRUE]
    gi.filt@other$locus <- factor(sapply(strsplit(locNames(gi.filt), split = "_"), "[", 1))
    gi.filt@other$pos <- as.numeric(sapply(strsplit(locNames(gi.filt), split = "_"), "[", 2))
    gi.filt@other$lg <- d.probes[as.character(gi.filt@other$locus),"LG"] # needs d.probes
    gi.filt@other$knownlg <- factor(ifelse(substring(gi.filt@other$lg, 1, 3) == "Sca", 0, 1))
    nloc2 <- nLoc(gi.filt)
    nreg2 <- nlevels(droplevels(gi.filt@other$locus))
    cat(paste0("kept ", nloc2, " / ", nloc1, " (", round(100*nloc2/nloc1, 2), "%) SNPs in ", nreg2, " / ", nreg1, " (", round(100*nreg2/nreg1, 2), "%) regions\n"))
  }
  if (subset.snps) {
    cat("subsampling SNPs...\n")
    
    nloc1 <- nLoc(gi.filt)
    nreg1 <- nlevels(droplevels(gi.filt@other$locus))
    gi.filt@other$sel <- factor(rep("0", length(gi.filt@other$locus)), levels = c("0","1"))
    
    for (loc in levels(gi.filt@other$locus)) {
      # cat(which(levels(gi.filt@other$locus) == loc), "\r")
      ids <- which(gi.filt@other$locus == loc)
      
      # take n SNPs from left, center and right of each region
      brk <- floor(length(ids)/3)
      rl <- 1:brk
      rc <- (brk+1):(2*brk)
      rr <- (2*brk+1):(length(ids))
      if (length(rl) >= n) lft <- sample(ids[rl], n) else lft <- rl
      if (length(rc) >= n) cnt <- sample(ids[rc], n) else cnt <- rc
      if (length(rr) >= n) rgt <- sample(ids[rr], n) else cgt <- rr
      gi.filt@other$sel[c(lft,cnt,rgt)] <- "1"
    }
    gi.filt <- gi.filt[loc = locNames(gi.filt)[which(gi.filt@other$sel == "1")], drop = TRUE]
    gi.filt@other$locus <- factor(sapply(strsplit(locNames(gi.filt), split = "_"), "[", 1))
    gi.filt@other$pos <- as.numeric(sapply(strsplit(locNames(gi.filt), split = "_"), "[", 2))
    if (subset.regions) {
      if (!exists("d.probes")) stop("<d.probes> not loaded")
      gi.filt@other$lg <- d.probes[as.character(gi.filt@other$locus),"LG"] # needs d.probes
      gi.filt@other$knownlg <- factor(ifelse(substring(gi.filt@other$lg, 1, 3) == "Sca", 0, 1))
    }
    gi.filt@other$sel <- NULL
    nloc2 <- nLoc(gi.filt)
    nreg2 <- nlevels(droplevels(gi.filt@other$locus))
    cat(paste0("kept ", nloc2, " / ", nloc1, " (", round(100*nloc2/nloc1, 2), "%) SNPs in ", nreg2, " / ", nreg1, " (", round(100*nreg2/nreg1, 2), "%) regions\n"))
  }
  return(gi.filt)
}

