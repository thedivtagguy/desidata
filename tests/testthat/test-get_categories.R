test_that("get_categories() gets list of categories", {
  expect_equal(get_categories(), data.frame(
    categories =  c('agriculture', 'economics', 'education', 'entertainment', 'geography', 'health', 'politics', 'sports')
  ) )
})
