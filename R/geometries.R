#' Determines wether spatial points are in area or not
#' @param points SpatialPoints object
#' @param area Extending SpatialPolygons object
#' @return Logical vector with TRUE for points in area
#' @export
#' @import sp
IsPointInArea <- function(points, area) {
  isInArea <- !is.na(sp::over(points, as(area, "SpatialPolygons")))
  names(isInArea) <- NULL
  return(isInArea)
}

#' Wrapper for IsPointInArea for lat lon data
#' @inheritParams IsPointInArea
#' @inheritParams CreateCorrespondingSpatialPoints
#' @import sp
#' @export
IsLonLatPointInArea <- function(lon, lat, area) {
  points <- CreateCorrespondingSpatialPoints(lon, lat, area@proj4string)
  return(IsPointInArea(points, area))
}

#' Returns appropriate SpatialPoints obeject for lon lat coords
#' @param lat numeric vector of latitudes
#' @param lon numeric vector of longitudes
#' @param proj4string projection string of class CRS-class (used by the area)
#' @export
#' @import sp
#' @import rgdal
CreateCorrespondingSpatialPoints <- function(lon, lat, proj4string) {
  stopifnot(length(lon) == length(lat))
  points <- cbind(lon, lat)
  points <- sp::SpatialPoints(points, CRS("+proj=longlat +datum=WGS84"))
  points <- sp::spTransform(points, proj4string)
  return(points)
}


#' Bbox to SpatialPolygons object
#' @param bbox Bounding box object
#' @param proj4string projection string of class CRS-class
#' @export
#' @import sp
#' @importFrom raster extent
ConvertBboxToSpatialPolygon <- function(bbox, proj4string) {
  stopifnot(class(bbox)=="matrix")
  b_poly <- as(extent(as.vector(t(bbox))), "SpatialPolygons")
  b_poly@proj4string <- proj4string
  return(b_poly)
}

#' Clip spatial geometry to bounding box
#' @description obtained from Robin Lovelace \url{http://robinlovelace.net/r/2014/07/29/clipping-with-r.html}
#' @param shp Spatial geometry
#' @param bb bounding box
#' @export
#' @import sp
#' @importFrom raster extent
#' @importFrom rgeos gIntersection
gClip <- function(shp, bb){
  # requires raster and sp
  if(class(bb) == "matrix") {
    b_poly <- ConvertBboxToSpatialPolygon(bb, shp@proj4string)
  }
  else {
    b_poly <- as(extent(bb), "SpatialPolygons")
    b_poly@proj4string <- shp@proj4string
  }
  return(gIntersection(shp, b_poly, byid = T))
}
