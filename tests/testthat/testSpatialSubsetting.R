library(evanHelpers)
library(data.table)
context("Testing spatial subsetting")

latArea <- c(4, 6, 4, 4)
lonArea <- c(1, 1, 3, 1)

area <- SpatialPolygons(
  list(Polygons(list(Polygon(cbind(lonArea, latArea))), 1)),
  proj4string = CRS("+proj=longlat +datum=WGS84"))

bboxArea <- ConvertBboxToSpatialPolygon(area@bbox, area@proj4string)

latPoints <- c(4.5, 5.5, 3.5)
lonPoints <- c(1.5, 2.5, 1.5)
points <- SpatialPoints(data.frame(lon = lonPoints, lat= latPoints), proj4string = CRS("+proj=longlat +datum=WGS84"))

test_that("Correct subsetting", {
  expect_equal(CreateCorrespondingSpatialPoints(lonPoints, latPoints, area@proj4string), points)
  expect_equal(IsLonLatPointInArea(lonPoints, latPoints, area), c(TRUE, FALSE, FALSE))
  expect_equal(IsLonLatPointInArea(lonPoints, latPoints, bboxArea), c(TRUE, TRUE, FALSE))
  expect_equal(IsPointInArea(points, area), c(TRUE, FALSE, FALSE))
  expect_equal(IsPointInArea(points, bboxArea), c(TRUE, TRUE, FALSE))
})

#library(ggplot2)
#areaFort <- fortify(area)
#ggplot(areaFort, aes(x = long, y = lat)) + geom_polygon() + geom_point(aes(x = lon), col = 2, data = as.data.frame(points@coords))

context("Closest neighbors")

test_that("Closest neighbor", {
  point <- data.table(lon = 7, lat = 54)
  grid <- data.table(lon = c(2, 7, 9), lat = c(52, 54, 56), pointID = 1 : 3)
  expect_equal(FindClosestGridPoints(point, grid)[, pointID], 2)
})
