## create SpatialPointsDataFrame
get.sp <- function(df, geovars = NULL, proj = "epsg:4326") {
  library(sp)
  
  # check input
  stopifnot(inherits(df, c("matrix","data.frame")),
            all(geovars %in% colnames(df)))
  
  # set default
  if (is.null(geovars)) {
    if (all(c("LongitudeDecimal","LatitudeDecimal") %in% colnames(df))) {
      geovars <- c("LongitudeDecimal","LatitudeDecimal")
    }
    if (all(c("x","y") %in% colnames(df))) {
      geovars <- c("x","y")
    }
  }
  
  # remove specimens with missing coordinates
  torm <- which(is.na(df[,geovars[1]]) | is.na(df[,geovars[2]]))
  if (length(torm) > 0) {
    rm <- rownames(df[torm,])
    message("removed ", length(rm), " specimens with missing coordinates: ", paste(rm, collapse = ", "))
    df <- df[-torm,]
  }
  
  # create SpatialPointsDataFrame
  df <- data.frame(df)
  sp <- SpatialPointsDataFrame(coords = df[,geovars], data = df,
                               coords.nrs = which(colnames(df) %in% geovars),
                               proj4string = sp::CRS(paste0("+init=",proj)))
  
  return(sp)
}

# # debug
# map.layers = NULL; idfac = "ID_Lab"; colorfac = "Species";
# layerfac = NULL; popup = c(idfac, colorfac, layerfac);
# link = NULL; radius = 4; opacity = 1; fillOpacity = 0.6 ; jitter.amount = 0.001; print = TRUE;
# providers = c("OpenStreetMap.Mapnik","Esri.WorldImagery","Esri.WorldTopoMap","Esri.WorldGrayCanvas");
# color.palette = adegenet::funky(nlevels(sp@data[,colorfac]))
# 
# draw.polygons = TRUE ; weight = 4 ; exclude.area = FALSE ; country_map = NULL ;
# export_shp = TRUE ; write_shp = FALSE ; alpha = 1 ; buff.alpha = 0.1 ;
# method.range = "alpha.hull" ; buff_width = 0.1 ; method.less.than3 = "arbitrary" ;
# write_results = FALSE ; file.name = "EOO.results" ; show.progress = FALSE

# function
get.leaflet <- function(sp, map.layers = NULL, idfac = NULL, colorfac = NULL, color.palette = NULL,
                        layerfac = NULL, popup = NULL, link = NULL, linkbase = "https://tropicos.org/Specimen/",
                        radius = 4, weight = 4, opacity = 1, fillOpacity = 0.6, jitter.amount = 0,
                        providers = c("OpenStreetMap.Mapnik","Esri.WorldImagery","Esri.WorldTopoMap","Esri.WorldGrayCanvas"),
                        draw.polygons = FALSE, exclude.area = TRUE, country_map = NULL,
                        export_shp = TRUE, write_shp = FALSE,
                        alpha = 1, buff.alpha = 0.1, method.range = "alpha.hull",
                        buff_width = 0.1, method.less.than3 = "arbitrary",
                        write_results = FALSE, file.name = "EOO.results",
                        show.progress = FALSE, print = TRUE
) {

  ## Arguments
  # sp          spatialPointsDataFrame
  # map.layers  list of data map layers. Each list element is a character vector of the form 
  #             c(<name of SpatialPolygonsDataFrame object>,
  #               <name of SpatialPolygonsDataFrame@data variable>,
  #               <name of the color palette function>).
  #             The respective R objects need to be available and will be accessed using get(<name of object>).
  # idfac       character denoting point label variable in sp@data. If NULL, will be set to ID=rownames(sp@data).
  # colorfac    character denoting point color variable in sp@data. If NULL, will be set to CLASS="Unknown".
  # color.palette character vector with colours used to distinguish <colorfac>.
  # layerfac    character denoting overlayGroups factor variable in sp@data. If NULL, will be set to LAYER=CLASS.
  # popup       character vector denoting point variables in sp@data used for popups. If NULL, will be set to c(idfac,colorfac,layerfac). The idfac is omitted if draw.polygons=TRUE.
  # link        character denoting variable in sp@data with database specimen identifyer.
  # linkbase    character containing the complete URL to an online database specimen entry, except for the last bit contained in <link>.
  # radius      numeric giving the size of points drawn.
  # weight      numeric giving the width of lines around points or polygons drawn.
  # opacity     numeric [0,1] giving the opacity of points/polygon lines drawn.
  # fillOpacity numeric [0,1] giving the opacity of points/polygon areas drawn.
  # jitter.amount numeric >= 0 giving the amount of jitter added to precise geolocations, to prevent overlaps. Useful if several points have identical coordinates.
  # providers   character vector indicating background maps to integrate. See ?addProviderTiles for links to lists of available maps.
  # draw.polygons boolean. If TRUE, will draw polygons around <colorfac> levels rather than individual points.
  # exclude.area  boolean. If TRUE, will use country_map and prevent polygons from extending country or coastline borders.
  # country_map   SpatialPolygonsDataFrame containing the physical borders for drawn polygons.
  #             See ?ConR::EOO.computing for the following arguments passed on to that function.
  # export_shp, write_shp, alpha, buff.alpha, method.range, buff_width, method.less.than3, write_results, file.name, show.progress
  # print       boolean. If TRUE, will print the map, otherwise invisibly return it.
  
  ## Value
  # an annotated interactive leaflet map. Can be saved as standalone .html using e.g. htmlwidgets::saveWidget().

  ## Author(s)
  # sfcrameri@gmail.com, July 2022.
    
  
  ## Check input
  stopifnot(inherits(sp, "SpatialPointsDataFrame"),
            idfac %in% colnames(sp@data),
            colorfac %in% colnames(sp@data),
            layerfac %in% colnames(sp@data),
            all(popup %in% colnames(sp@data)))
  
  
  ## Helperfunctions
  get.popup <- function(sp, popup) {
    unname(gsub("^</br>", "", apply(
      matrix(sapply(seq(length(popup)), FUN = function(x) {
        apply(sp@data[,popup[x],drop=F], MARGIN = 1, FUN = function(y) {paste0(paste0("</br>", popup[x], ": "), y)})
      }), ncol = length(popup)), MARGIN = 1, FUN = paste, collapse = "")))
  }
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
  
  ## Load libraries
  ask("sp")
  ask("leaflet")
  ask("dplyr")
  ask("purrr")
  library(leaflet)
  library(sp)
  
  
  ## Set defaults
  if (is.null(country_map)) exclude.area <- FALSE
  if (!is.null(layerfac) & is.null(colorfac)) {
    colorfac <- layerfac
  }
  if (is.null(colorfac)) {
    colorfac <- "CLASS"
    sp@data[,colorfac] <- factor("Unknown")
  }
  if (is.null(layerfac)) {
    layerfac <- "LAYER"
    sp@data[,layerfac] <- sp@data[,colorfac]
  }
  if (is.null(popup)) {
    if (draw.polygons) {
      popup <- unique(c(colorfac, layerfac))
    } else {
      popup <- unique(c(idfac, colorfac, layerfac))
    }
  }
  if (is.null(idfac)) {
    idfac <- "ID"
    sp@data[,idfac] <- rownames(sp@data)
  }
  
  if (is.null(names(alpha))) a <- alpha[1] else a <- 1
  if (is.null(names(buff.alpha))) b <- buff.alpha[1] else b <- 0.1
  
  if (!is.factor(sp@data[,colorfac])) {
    sp@data[,colorfac] <- factor(sp@data[,colorfac])
  } else {
    sp@data[,colorfac] <- droplevels(sp@data[,colorfac])
  }
  if (!is.factor(sp@data[,layerfac])) {
    sp@data[,layerfac] <- factor(sp@data[,layerfac])
  } else {
    sp@data[,layerfac] <- droplevels(sp@data[,layerfac])
  }
  if (!is.null(link)) {
    stopifnot(link %in% colnames(sp@data))
    sp@data["LINK"] <- paste0("<a href='", linkbase, sp@data[,link], "', target=\"blank\">", sp@data[,link], "</a>")
    popup <- unique(c("LINK", popup))
  }
  
  
  ## Check color palette
  if (is.null(color.palette)) color.palette <- funky(nlevels(sp@data[,colorfac]))
  
  
  ## Add tiles
  M <- leaflet()
  for (i in 1:length(providers)) {
    M <- M %>% 
      addProviderTiles(providers[i], group = providers[i])
  }
  
  
  ## Jitter points
  set.seed(4065)
  sp@coords <- apply(sp@coords, 2, jitter, factor = 0, amount = jitter.amount)
  
  
  ## Add layers
  map.groups <- character()
  for (i in map.layers) {
    M <- M %>% 
      addPolygons(data = get(i[1]),
                  group = i[1],
                  popup = get(i[1])[[i[2]]],
                  fillColor = get(i[3])(get(i[1])[,i[2]][[1]]))
    map.groups <- c(map.groups, i[1])
  }
  
  ## Create SpatialPolygons if needed
  z <- character()
  if (draw.polygons) {
    ask("ConR")

    ALPHA <- rep(a, nlevels(sp@data[,colorfac]))
    names(ALPHA) <- levels(sp@data[,colorfac])
    if (!is.null(names(alpha))) {
      stopifnot(is.numeric(alpha))
      ALPHA[names(ALPHA) %in% names(alpha)] <- alpha[names(ALPHA[names(ALPHA) %in% names(alpha)])]
    }
    BUFF <- rep(b, nlevels(sp@data[,colorfac]))
    names(BUFF) <- levels(sp@data[,colorfac])
    if (!is.null(names(buff.alpha))) {
      stopifnot(is.numeric(buff.alpha))
      BUFF[names(BUFF) %in% names(buff.alpha)] <- buff.alpha[names(BUFF[names(BUFF) %in% names(buff.alpha)])]
    }
    
    l <- list()
    for (class in levels(sp@data[,colorfac])) {
      ids <- sp@data[,colorfac] %in% class
      XY <- data.frame(sp@coords[ids,c(2,1),drop=F], tax = sp@data[ids,colorfac])
      
      if (nrow(XY) < 3) {
        da1 <- data.frame(t(apply(XY[,1:2], 2, jitter, amount = 0.001)), tax = class)
        da2 <- data.frame(t(apply(XY[,1:2], 2, jitter, amount = 0.001)), tax = class)
        XY <- rbind(XY, da1, da2)[1:3,]
        z <- c(z, class)
      }
      
      e <- ConR::EOO.computing(XY = XY, exclude.area = exclude.area, country_map = country_map,
                               export_shp = export_shp, write_shp = write_shp,
                               alpha = ALPHA[class], buff.alpha = BUFF[class],
                               method.range = method.range, buff_width = buff_width,
                               method.less.than3 = method.less.than3,
                               write_results = write_results, file.name = file.name,
                               show_progress = show.progress)
      e[[paste0("spatial.polygon_", "1")]]@proj4string <- sp@proj4string
      names(e) <- c(paste0("EOO_", class), class)
      l <- c(l, e)
    }
    
    # collapse
    ask("purrr")
    S <- list(l[levels(sp@data[,colorfac])], makeUniqueIDs = T) %>% 
      purrr::flatten() %>% 
      do.call(rbind, .)
    S <- SpatialPolygonsDataFrame(S, data = data.frame(Taxa = levels(sp@data[,colorfac])), match.ID = FALSE)
  }
  if (length(z) > 0) {
    message("Class(es) with single observation(s): ", paste(z, collapse = ", "))
  }
  
  ## Add jittered points or polygons
  pal <- colorFactor(palette = color.palette, domain = sp@data[,colorfac])
  if (is.null(layerfac)) {
    if (draw.polygons) {
      M <- M %>%
        addPolygons(data = S, color = color.palette,
                    weight = weight, opacity = opacity, fillOpacity = fillOpacity,
                    popup = get.popup(sp, popup),
                    label = S@data[,"Taxa"],
                    group = colorfac)
    } else {
      M <- M %>%
        addCircleMarkers(data = sp, color = pal(sp@data[,colorfac]),
                         radius = radius, weight = weight, opacity = opacity, fillOpacity = opacity, stroke = FALSE,
                         popup = get.popup(sp, popup),
                         label = sp@data[,idfac],
                         group = colorfac)
    }
  } else {
    for (i in 1:nlevels(sp@data[,layerfac])) {
      layer <- levels(sp@data[,layerfac])[i]
      sp.layer <- sp[sp@data[,layerfac] %in% layer,]
      if (draw.polygons) {
        M <- M %>%
          addPolygons(data = l[[layer]],
                      color = pal(sp.layer@data[,colorfac]), fillColor = pal(sp.layer@data[,colorfac]),
                      weight = weight, opacity = opacity, fillOpacity = fillOpacity, stroke = FALSE,
                      popup = get.popup(sp.layer, popup),
                      label = sp.layer@data[,layerfac],
                      group = layer)
      } else {
        M <- M %>%
          addCircleMarkers(data = sp.layer, color = pal(sp.layer@data[,colorfac]),
                           radius = radius, weight = weight, opacity = opacity, fillOpacity = opacity, stroke = FALSE,
                           popup = get.popup(sp.layer, popup),
                           label = sp.layer@data[,idfac],
                           group = layer)
      }
    }
  }
  
  ## Add layer control
  M <- M %>% addLayersControl(baseGroups = providers,
                              overlayGroups = c(map.groups, levels(sp@data[,layerfac])),
                              position = "topleft") 
  
  
  ## Finish
  if (print) print(M)
  invisible(M)
}
