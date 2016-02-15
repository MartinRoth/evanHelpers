library(evanHelpers)

load("./testData.Rdata")

context("Input testing for gamlss modeling")
test_that("Correct input handling", {
  expect_error(GetGamlssTrend(y ~ x, data, "BCPE"), "BCPE not available")
  expect_error(GetBoxCoxAdjustment("a"), "data should be numeric")
  expect_error(GetBoxCoxAdjustment(1), "data should have length > 1")
})

context("Output testing for gamlss modeling")
test_that("Testing Box Cox adjustment", {
  expect_equal(GetBoxCoxAdjustment(c(1,2,3)), 0)
  expect_equal_to_reference(GetGamlssTrend(TG ~ pb(year), testData, method = RS(200)), "testGamlssFit.rds" )
})

