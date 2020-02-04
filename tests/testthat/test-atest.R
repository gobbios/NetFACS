test_that("reading data works", {
  xdata <- read_mydata(path = system.file("anexamplefile.xlsx", package = "netfacstestr"))
  xdata$context <- as.factor(xdata$context)

  expect_equal(sum(xdata$AU1), expected = 2)
  expect_true(nlevels(xdata$context) == 3)

})
