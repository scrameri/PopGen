thin.sampling <- function(df, lon, lat, fac,
                          t.layer = NULL, id = rownames(df),
                          actel = FALSE, dist_thresh = 1E04, proj4string = CRS("+init=epsg:4326"),
                          max = 5, criterion = NULL, criterion_thresh = NULL,
                          minimize = FALSE, verbose = TRUE) {
  
  # sfcrameri@gmail.com, Feb 2022
  
  ## Usage
  # df            data.frame with lon, lat, fac columns
  # lon/lat       name of column in df with longitude (lon) / latitude (lat)
  # fac           name of column in df with grouping factor (will be converted to factor)
  # t.layer       TransitionLayer as outputted from actel::transitionLayer. If NULL, the euclidean distances between individuals is taken.
  # id            name of column in df with sample identifyer [DEFAULT: rownames(df)]
  # actel         passed to actel::distancesMatrix
  # dist_tresh    distance threshold in meters. Only a single sample out of a group of samples with distance < dist_thresh is selected [DEFAULT: 10000].
  # proj4string   projection string of class CRS-class [DEFAULT: WGS84 = EPSG 4326].
  # max           maximum number of samples selected per group
  # criterion     name of column in df with criterion to choose from multiple individuals [DEFAULT: NULL = no criterion, in which case the maximum distance is used]
  # criterion_thresh numeric threshold. Samples will be selected by taking this criterion into account (res$sel) and by applying a strict threshold (res$strict)
  # minimize      if TRUE, will prioritize/select samples with a small value of criterion. If FALSE [DEFAULT], will prioritize/select samples with a large value of criterion.
  
  # libraries
  # require(plyr)
  # require(actel) # if t.layer is used
  # require(sp)    # if t.layer is not used
  
  # check input
  stopifnot(inherits(df, "data.frame"),
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
    
    if (minimize) {
      df[df[[criterion]] %in% NA,criterion] <- max(df[[criterion]], na.rm = TRUE)
    } else {
      df[df[[criterion]] %in% NA,criterion] <- min(df[[criterion]], na.rm = TRUE)
    }

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
  dist <- crit <- dist.max <- dist.crit <- dist.crit.max <- list()
  for (taxon in taxa) {
    
    if (verbose) cat(paste0(taxon, " [", which(taxa == taxon), "/", length(taxa), "]\n"))
    idx <- df[,fac] %in% taxon & !is.na(df[,lon])
    dsub <- df[idx,]
    
    ## determine groups of geographically close samples
    # if >1 samples exist, they are grouped
    if (nrow(dsub) > 1) {
      
      require(plyr)
      
      # compute distance over resistance (transition) layer
      if (!is.null(t.layer)) {
        require(actel)
        stopifnot(inherits(t.layer, "TransitionLayer"))
        dist.mat <- distancesMatrix(t.layer = t.layer, coord.x = lon, coord.y = lat,
                                    starters = dsub, actel = actel)
        
        # set diagonal to zero
        dist.mat <- as.matrix(dist.mat)
        diag(dist.mat) <- 0
        rownames(dist.mat) <- colnames(dist.mat)
      } else {
        library(sp)
        sp <- SpatialPointsDataFrame(coords = dsub[,c(lon,lat)], data = dsub,
                                     proj4string =  proj4string)
        dist.mat <- round(spDists(x = sp, y = sp)*1000) # kilometers in meters
      }
      rownames(dist.mat) <- colnames(dist.mat) <- dsub[,id]
      
      # # test it
      # dsub <- data.frame(.id = c("A","B","C","D"), data_NIRS_core_hw_Bruker = c(0,1,0,1))
      # dist.mat <- matrix(c(0,10000,2000,10000,10000,0,8000,12000,2000,8000,0,8000,10000,12000,8000,0), ncol = 4, dimnames = list(c("A","B","C","D"),c("A","B","C","D")))
      
      # fill in dist_thresh for NA values
      if (anyNA(dist.mat)) dist.mat[is.na(dist.mat)] <- dist_thresh
      
      # iterate
      dist.mat.all <- dist.mat
      # dist.mat <- dist.mat.all
      repeat{
        
        dim1 <- nrow(dist.mat)
        # <ll>: list of close individuals using alply (this always returns a list)
        ll <- plyr::alply(.data = as.matrix(dist.mat), .margins = 1,
                          .fun = function(y) {names(which(y < dist_thresh))}, .expand = FALSE, .dims = TRUE)
        ll <- ll[order(lengths(ll), decreasing = FALSE)]
        
        ## sort according to criterion
        lt <- lapply(ll, FUN = function(x) {
          dgr <- dsub[dsub[,id] %in% x,]
          if (minimize) {
            as.character(na.omit(dgr[order(dgr[,criterion], decreasing = FALSE),id]))
          } else {
            as.character(na.omit(dgr[order(dgr[,criterion], decreasing = TRUE),id]))
          }
        })
        
        ## trick: take first of every non-duplicated list element
        lb <- lt[!duplicated(lt)]
        ok.dist <- unique(unname(sapply(lb, "[", 1)))
        
        ## check and repeat until dist_thresh is satisfied
        dist.mat <- dist.mat[ok.dist, ok.dist, drop = FALSE]
        dim2 <- nrow(dist.mat)
        dist.check <- dist.mat[lower.tri(dist.mat)]
        if (!any(dist.check < dist_thresh) | dim1 == dim2) {
          break()
        }
        # print(dim(dist.mat))
      }

    } else {
      
      # if  1 sample  exists, it is taken
      if (nrow(dsub) == 1) {
        ll <- list(dsub[,id])
        names(ll) <- dsub[,id]
      } else {
        # if  0 samples exist, none are taken
        ll <- list()
      }
    }

    ## apply <max> using criterion or maximum distance
    if (is.null(criterion)) {
      
      ## <ok.dist.max>: select at most <max> using maximum distance
      smat <- as.matrix(dist.mat.all[ok.dist,ok.dist])
      smat[upper.tri(smat)] <- NA
      diag(smat) <- NA
      if (verbose & min(smat, na.rm = TRUE) < dist_thresh) message("Close individuals detected!")
      
      # use the total distance to select one from each cluster
      cl <- cutree(hclust(as.dist(smat), method = "complete"), k = min(c(max, nrow(smat))))
      ok.dist.max <- sapply(1:max, FUN = function(x) names(which.max(rowSums(dist.mat.all[names(cl[cl==x]),,drop=F]))))
      
      # check
      stopifnot(identical(as.integer(length(ok.dist.max)), as.integer(max))) # still enough here?
      
    } else {
      
      ## <ok.dist.max>: select at most <max> using criterion
      ddist <- dsub[dsub[,id] %in% ok.dist,]
      ok.dist.max <- if (minimize) {
        sort(as.character(na.omit(ddist[order(ddist[,criterion], decreasing = FALSE),][1:max,id])))
      } else {
        sort(as.character(na.omit(ddist[order(ddist[,criterion], decreasing = TRUE),][1:max,id])))
      }
    }
    dmax <- dsub[dsub[,id] %in% ok.dist.max,]
    
    ## <ok.dist.max.crit>: select subset that also meets criterion_thresh
    dstrict <- if (minimize) {
      dsub[dsub[,criterion] <= criterion_thresh,]
    } else {
      dsub[dsub[,criterion] >= criterion_thresh,]
    }
    ok.crit <- dstrict[,id]
    
  
    # ## check on map
    # dsub$selected <- 0
    # dsub[dsub[,id] %in% ok.dist, "selected"] <- 1
    # 
    # library(leaflet)
    # sp <- SpatialPointsDataFrame(coords = dsub[c(lon,lat)],
    #                              data = dsub, proj4string =  CRS("+init=epsg:4326"))
    # sp@data[,"Species"] <- droplevels(sp@data[,"Species"])
    # m <- get.leaflet(sp, map.layers = NULL,
    #                  idfac = "Collection", colorfac = "selected", layerfac = "Species",
    #                  popup = c("ID_Lab","Collection","selected",criterion),
    #                  link = "SpecimenID",
    #                  providers = c("Esri.WorldGrayCanvas","Esri.WorldImagery",
    #                                "Esri.WorldTopoMap","OpenStreetMap.Mapnik"),
    #                  print = F)
    # m
    # print(ok.dist)
  
  
    ## bind results  
    # we have 5 categories
    # 1) <dist> samples meeting <dist_thresh>, not necessarily also <criterion_thresh> or <max>
    # 2) <crit> samples meeting <criterion_thresh>, not necessarily also <dist_thresh> or <max>
    
    # 3) <dist.max> samples, same as <ok.dist> but downsampled to <max> using <criterion_thresh> (if given) or maximum distance
    # 4) <dist.crit> samples meeting <dist_thresh> and <criterion_thresh>
    
    # 5) <dist.crit.max> samples meeting <dist_thresh> and <criterion_thresh> and <max>
    
    dist[[taxon]] <- sort(ok.dist)
    crit[[taxon]] <- sort(ok.crit)
    dist.max[[taxon]] <- sort(ok.dist.max)
    dist.crit[[taxon]] <- sort(intersect(ok.dist, ok.crit))
    dist.crit.max[[taxon]] <- as.character(na.omit(sort(intersect(ok.dist, ok.crit))[1:max]))
  } # end for loop over taxa
  
  # return results
  d.res <- list(dist = dist, crit = crit, dist.max = dist.max, dist.crit = dist.crit, dist.crit.max = dist.crit.max)
  return(d.res)
}
