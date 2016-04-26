library(evanHelpers)
context("Testing spatial subsetting")

latArea <- c(1, 1, 2)
lonArea <- c(2, 1, 1)

area <- SpatialPolygons(
  list(Polygons(list(Polygon(cbind(latArea, lonArea))), 1)),
  proj4string = CRS("+proj=longlat +datum=WGS84"))

bboxArea <- ConvertBboxToSpatialPolygon(area@bbox, area@proj4string)

latPoints <- c(0.5, 1.5, 1.75)
lonPoints <- c(1.25, 1.25, 1.75)
points <- SpatialPoints(cbind(latPoints, lonPoints), proj4string = CRS("+proj=longlat +datum=WGS84"))

CreateCorrespondingSpatialPoints(lonPoints, latPoints, area@proj4string)

test_that("Correct subsetting", {
  expect_equal(IsLonLatPointInArea(lonPoints, latPoints, area), c(FALSE, TRUE, FALSE))
  expect_equal(IsLonLatPointInArea(lonPoints, latPoints, bboxArea), c(FALSE, TRUE, TRUE))
  expect_equal(IsPointInArea(points, area), c(FALSE, TRUE, FALSE))
  expect_equal(IsPointInArea(points, bboxArea), c(FALSE, TRUE, TRUE))
})
