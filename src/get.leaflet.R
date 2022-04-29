get.leaflet <- function(sp, map.layers = NULL, idfac = "ID_Lab", classfac = "Species", layerfac = NULL, popup = c(idfac, classfac, layerfac),
                        link = NULL, radius = 4, opacity = 0.6, jitter.amount = 0.001, print = TRUE,
                        providers = c("OpenStreetMap.Mapnik","Esri.WorldImagery","Esri.WorldTopoMap","Esri.WorldGrayCanvas"),
                        palette = adegenet::funky(nlevels(sp@data[,classfac]))) {
  library(leaflet)
  
  ## Arguments
  # sp  spatialPointsDatFrame
  # map.layers  list of data map layers. each element is a character vector of the form c(<polygon>,<variable>,<palette>). The polygon and palette will be accessed using get()
  # idfac character denoting point label variable 
  # classfac character denoting point color variable
  # layerfac character denoting overlayGroups factor variable
  # popup character vector denoting point variables used for popups
  # link character denoting variable with SpecimenID
  # palette character vector with point colours
  
  ## Check input
  stopifnot(inherits(sp, "SpatialPointsDataFrame"),
            idfac %in% colnames(sp@data),
            classfac %in% colnames(sp@data),
            all(popup %in% colnames(sp@data)))
  if (!is.factor(sp@data[,classfac])) {sp@data[,classfac] <- factor(sp@data[,classfac])}
  if (!is.null(layerfac)) {
    stopifnot(layerfac %in% colnames(sp@data))
    if (!is.factor(sp@data[,layerfac])) sp@data[,layerfac] <- factor(sp@data[,layerfac])
  }
  if (!is.null(link)) {
    stopifnot(link %in% colnames(sp@data))
    linkbase = "https://tropicos.org/Specimen/"
    sp@data["LINK"] <- paste0("<a href='", linkbase, sp@data[,link], "', target=\"blank\">", sp@data[,link], "</a>")
    popup <- unique(c("LINK", popup))
  }
  
  ## Helperfunctions
  get.popup <- function(sp, popup) {
    unname(gsub("^</br>", "", apply(
      matrix(sapply(seq(length(popup)), FUN = function(x) {
      apply(sp@data[,popup[x],drop=F], MARGIN = 1, FUN = function(y) {paste0(paste0("</br>", popup[x], ": "), y)})
    }), ncol = length(popup)), MARGIN = 1, FUN = paste, collapse = "")))
  }
  
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
  
  ## Add jittered oints
  pal <- colorFactor(palette = palette, domain = sp@data[,classfac])
  if (is.null(layerfac)) {
    M <- M %>% 
      addCircleMarkers(data = sp, color = pal(sp@data[,classfac]),
                       radius = radius, opacity = opacity, fillOpacity = opacity, stroke = FALSE,
                       popup = get.popup(sp, popup),
                       label = sp@data[,idfac],
                       group = classfac)
  } else {
    for (i in 1:nlevels(droplevels(sp@data[,layerfac]))) {
      layer <- levels(sp@data[,layerfac])[i]
      sp.layer <- sp[sp@data[,layerfac] %in% layer,]
      M <- M %>%
        addCircleMarkers(data = sp.layer, color = pal(sp.layer@data[,classfac]),
                         radius = radius, opacity = opacity, fillOpacity = opacity, stroke = FALSE,
                         popup = get.popup(sp.layer, popup),
                         label = sp.layer@data[,idfac],
                         group = layer)
    }
  }
  
  ## Add layer control
  if (is.null(layerfac)) {
    M <- M %>% addLayersControl(baseGroups = providers,
                                overlayGroups = c(map.groups, classfac),
                                position = "topleft")
  } else {
    M <- M %>% addLayersControl(baseGroups = providers,
                                overlayGroups = c(map.groups, levels(sp@data[,layerfac])),
                                position = "topleft") %>%
      hideGroup(levels(sp@data[,layerfac])[-1])
  }
  if (print) print(M)
  invisible(M)
}
