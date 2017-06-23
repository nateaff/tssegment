test_that("Segment helper functions output is minimally valid", {

   len = 30
   pts = c(10, 20)
   pts <- add_end_pts(pts, len)
   ret <- segment_ranges(pts)
   retlen = length(unlist(ret))
   expect_that((len == retlen), is_true()) 
})


test_that("segment outputs the correct type", {
  
  len = 150
  x <- rep(1, 150)
  y <- rep(3, 150) 
  z <- rep(c(1, 5, 9), each = 50) 

  labels <- data.frame(alpha = factor("a"), numbers = factor("1"))
  df <- data.frame(x,y,z)

  nlabs = dim(labels)[2]
  input_cols = dim(df)[2]

  kout <- palarm(z)$kout
  res <- segment(df, z, labels, mean)
  output_rows = dim(res)[1]
  output_cols = dim(res)[2]
  colnames = c("x", "y", "z", "alpha", "numbers", "weights")

  expect_that((length(kout) == 2), is_true())
  expect_that((output_rows == 3), is_true())
  expect_that((output_cols == (nlabs + input_cols + 1)), is_true())
  expect_that(all(names(res) == colnames), is_true())

})


