test_that("point_estimate works", {
  expect_equal(point_estimate(data2),  list(0.004698568, 0.0030826445, 0.0,
                  "SSP"= 0.0142610763,  0.0, 0.0095625083,
                  0.491353094, "SSP" = 0.670532018,
               "SSE" = 0.404223385 ))
  expect_error(point_estimate("cat"))
  expect_equal(point_estimate(data1), list(48.29259259, 0.56460905, 0.72798354,
                                           "SSP" = 50.09629630, 1.29259259, 1.80370370,
                                           26.77412731, "SSP" =0.036004732,
                                           "SSE" = 94.48550725))
})

