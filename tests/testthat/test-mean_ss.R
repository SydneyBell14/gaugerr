test_that("mean_ss function works", {
  expect_equal(mean_ss(data1), list("SSP"=437.3284, "SSO"=19.6333333, "SSE"=0.51111111, "MSPO"=2.6950617))
  expect_error(mean_ss("cat"))
})
