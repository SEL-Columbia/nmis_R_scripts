suppressPackageStartupMessages(
  lapply(c('maptools', 'sp', 'plyr', 'stringr'), require, character.only=T))

xx <- readShapeSpatial(raw_data("nga_lgas/nga_lgas.shp"), proj4string=CRS("+proj=longlat +datum=WGS84"))
regions <- setNames(slot(xx, "polygons"), xx@data$lga_id)
regions <- llply(regions, function(x) SpatialPolygons(list(x)))

# df = dataframe with latitude / longitude in it, and colToReDerive = lga_id
newlgaids = function(df, colToReDerive, uuid_col="uuid") {
  latcol <- names(df)[str_detect(names(df), "latitude")][[1]]
  lngcol <- names(df)[str_detect(names(df), "longitude")][[1]]
  
  errs <- subset(df, is.na(df[[colToReDerive]]) & !is.na(df[[latcol]]) & !is.na(df[[lngcol]]))
  hxy <- cbind(as.numeric(errs[[lngcol]]), as.numeric(errs[[latcol]]))
  pts <- SpatialPoints(hxy)

  output = rep(NA, nrow(errs))
  l_ply(names(regions), function(rid) {
    r = regions[[rid]]
    #print(over(pts, r))
    output <<- replace(output,
                       over(pts, r) == 1,
                       rid)
  })
  na.omit(data.frame(cbind('uuid'=as.character(errs[[uuid_col]]), 
                           new_col=as.numeric(output)), stringsAsFactors=FALSE))
}
