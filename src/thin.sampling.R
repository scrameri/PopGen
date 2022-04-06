thin.sampling <- function(t.layer, df, lon, lat,
                          fac, id = rownames(df),
                          actel = FALSE, dist_thresh = 1E04,
                          max = 5, criterion = NULL, criterion_thresh = NULL,
                          minimize = FALSE, verbose = TRUE) {
  
  # sfcrameri@gmail.com, Feb 2022
  
  ## Usage
  # t.layer       TransitionLayer as outputted from actel::transitionLayer
  # df            data.frame with lon, lat, fac columns
  # lon/lat       name of column in df with longitude (lon) / latitude (lat)
  # fac           name of column in df with grouping factor (will be converted to factor)
  # id            name of column in df with sample identifyer [DEFAULT: rownames]
  # actel         passed to actel::distancesMatrix
  # dist_tresh    distance threshold in meters. Only a single sample out of a group of samples with distance < dist_thresh is selected [DEFAULT: 10000], except if < max samples exist.
  # max           maximum number of samples selected per group
  # criterion     name of column in df with criterion to choose from multiple individuals [DEFAULT: NULL = no criterion]
  # criterion_thresh numeric threshold. Samples not meeting this threshold will not be selected, except if < max samples exist.
  # minimize      if TRUE, will select samples with a small value of criterion. If FALSE [DEFAULT], will select samples with a large value of criterion.
  
  # libraries
  require(actel)
  require(plyr)
  
  # check input
  stopifnot(inherits(t.layer, "TransitionLayer"),
            inherits(df, "data.frame"),
            lon %in% colnames(df),
            lat %in% colnames(df),
            fac %in% colnames(df),
            is.numeric(dist_thresh), dist_thresh >= 0,
            is.logical(minimize))
  if (inherits(df, "tbl")) df <- data.frame(df)
  if (!is.null(criterion)) {
    stopifnot(criterion %in% colnames(df), is.numeric(df[[criterion]]))
    
    if (is.null(criterion_thresh)) {
      criterion_thresh <- if (minimize) {
        max(df[[criterion]], na.rm = T)
      } else{
        min(df[[criterion]], na.rm = T)
      }
    } else {
      stopifnot(is.numeric(criterion_thresh))
    }
    
    # make sure NA are interpreted correctly
    NA.char <- c("NA", "", NA)
    df[df[[criterion]] %in% NA.char,criterion] <- NA
  } else {
    df[,".criterion"] <- 1
    criterion <- ".criterion"
    criterion_thresh <- 0
    minimize <- FALSE
  }
  if (identical(id, rownames(df))) {
    df[,".id"] <- rownames(df)
  } else {
    stopifnot(id %in% colnames(df))
    df[,".id"] <- df[,id]
  }
  id <- ".id"
  
  # loop over taxa
  if (!is.factor(df[,fac])) df[,fac] <- factor(df[,fac])
  taxa <- levels(df[,fac])
  res <- dis <- pas <- str <- list()
  
  for (taxon in taxa) {
    
    if (verbose) cat(paste0(taxon, " [", which(taxa == taxon), "/", length(taxa), "]\n"))
    idx <- df[,fac] %in% taxon & !is.na(df[,lon])
    dsub <- df[idx,]
    
    ## determine samples strictly meeting criterion_thresh
    if (minimize) {
      good.strict <- dsub[dsub[,criterion] < criterion_thresh,id]
    } else {
      good.strict <- dsub[dsub[,criterion] > criterion_thresh,id]
    }
    
    if (nrow(dsub) <= max) {
      ## if only one sample exists, this one is chosen
      sel <- ind <- dsub[,id]
    } else {

      ## remove samples not meeting criterion_thresh, if enough remain
      if (minimize) {
        good <- dsub[dsub[,criterion] < criterion_thresh,id]
        if (length(good) < max) {
          good <- dsub[order(dsub[,criterion], decreasing = FALSE),][1:max,id]
          good <- good[good %in% dsub[,id]]
        }
      } else {
        good <- dsub[dsub[,criterion] > criterion_thresh,id]
        if (length(good) < max) {
          good <- dsub[order(dsub[,criterion], decreasing = TRUE),][1:max,id]
          good <- good[good %in% dsub[,id]]
        }
      }
      dsub <- dsub[dsub[,id] %in% good,]
      
      ## if >1 samples exist, they are grouped according to <t.layer> and <dist_thresh>
      # compute distance over resistance (transition) layer
      dist.mat <- distancesMatrix(t.layer = t.layer, coord.x = lon, coord.y = lat,
                                  starters = dsub, actel = actel)
      rownames(dist.mat) <- colnames(dist.mat) <- dsub[,id]
      
      # fill in dist_thresh for NA values
      if (anyNA(dist.mat)) dist.mat[is.na(dist.mat)] <- dist_thresh
      
      # get individuals closer than dist_thresh using alply (this always returns a list)
      ll <- plyr::alply(.data = as.matrix(dist.mat), .margins = 1,
                        .fun = function(y) {which(y < dist_thresh)}, .expand = FALSE, .dims = TRUE)
      
      # reduce redundancy of groups
      lr <- ll[names(which(!duplicated(ll)))]
      lr <- lr[order(lengths(lr), decreasing = TRUE)]
      mid <- sort(table(names(unlist(unname(lr)))), decreasing = TRUE)
      ind.mid <- names(mid[mid > 2])
      lr <- lr[!names(lr) %in% ind.mid] # remove "central" samples
      stopifnot(length(unique(names(unlist(unname(lr))))) == nrow(dsub)) # still all there?
      
      # from each group of close individuals, select 1 sample randomly or according to criterion
      l <- lapply(lr, FUN = function(x) {
        di <- dsub[match(names(x), dsub[,id]),]
        
        if (nrow(di) == 1) {
          ind <- di[,id]
        } else {
          if (!is.null(criterion)) {
            if (minimize) {
              ind <- di[which.min(di[,criterion]),id]
            } else {
              ind <- di[which.max(di[,criterion]),id]
            }
          } else {
            ind <- sample(di[,id], size = 1)
          }
        }
        return(ind)
      })
      sel <- unique(unlist(unname(l))) # sel should all meet dist_thresh
      
      ## reduce to groups
      if (sum(!duplicated(ll)) >= max) {

        ## if there are more groups than <max>, select samples according to distance
        if (length(sel) > max) {
          smat <- as.matrix(dist.mat[sel,sel])
          smat[upper.tri(smat)] <- NA
          diag(smat) <- NA
          # if (verbose & !min(smat, na.rm = TRUE) > dist_thresh) message("Close individuals detected!")
          
          # use the total distance to select one from each cluster
          cl <- cutree(hclust(as.dist(smat), method = "complete"), k = max)
          ind <- sapply(1:max, FUN = function(x) names(which.max(rowSums(dist.mat[names(cl[cl==x]),]))))
          
          # check
          stopifnot(identical(as.integer(length(ind)), as.integer(max))) # still enough here?
          
          imat <- as.matrix(dist.mat[ind,ind])
          imat[upper.tri(imat)] <- NA
          diag(imat) <- NA
          if (verbose & !min(imat, na.rm = TRUE) > dist_thresh) message("Close individuals detected!")
          
          ## old way using large pairwise distances
          # nmax <- max
          # repeat{
          #   nmax <- nmax - 1
          #   cat("hi\n")
          #   ismax <- sort(smat, decreasing = TRUE)[1:nmax]
          #   dmat <- apply(smat, 2, function(x) x %in% ismax)
          #   ind <- unique(rep(rownames(smat), times = ncol(smat))[c(which(unlist(dmat)), which(unlist(t(dmat))))])
          #   if (length(ind) <= max) break()
          # }
          # stopifnot(identical(as.integer(length(ind)), as.integer(max)))
          
          ## use the criterion
          # di <- dsub[match(sel, dsub[,id]),]
          # if (!is.null(criterion)) {
          #   ind <- di[order(di[,criterion], decreasing = !minimize),id]
          # } else {
          #   ind <- sample(di[,id], size = max)
          # }
        
        } else {
          ind <- sel
        }
      
      } else {
        
        ## if many samples are < dist_thresh, use the total distance to select one from each cluster
        cl <- cutree(hclust(as.dist(dist.mat), method = "complete"), k = max)
        ind <- sapply(1:max, FUN = function(x) names(which.max(rowSums(dist.mat[names(cl[cl==x]),]))))
      }
      
      # # check on map
      # dsub$selected <- 0
      # dsub[dsub[,id] %in% ind,"selected"] <- 1
      # 
      # library(leaflet)
      # sp <- SpatialPointsDataFrame(coords = dsub[c(lon,lat)],
      #                              data = dsub, proj4string =  CRS(proj.coord))
      # sp@data[,"Species"] <- droplevels(sp@data[,"Species"])
      # m <- get.leaflet(sp, map.layers = NULL,
      #                  idfac = "Collection", classfac = "selected", layerfac = "Species",
      #                  popup = c("ID.Miseq.orig","ID_Lab","Collection","selected","missingness"),
      #                  link = "SpecimenID",
      #                  providers = c("Esri.WorldGrayCanvas","Esri.WorldImagery",
      #                                "Esri.WorldTopoMap","OpenStreetMap.Mapnik"),
      #                  print = F)
      # m
      # print(ind)
      
    }
    res[[taxon]] <- ind
    str[[taxon]] <- intersect(sel, good.strict)
    pas[[taxon]] <- good.strict
    dis[[taxon]] <- sel
  }
  
  # return results
  d.res <- list(selected = res, strict = str, pass_criterion = pas, distant = dis)
  return(d.res)
}
