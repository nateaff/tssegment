test_that("Classify output is valid", {
  segvec <- lapply(1:10, function(x) rep(x, 20)) %>% unlist
  classA = replicate(20, rnorm(200)) 
  classB = replicate(20, rnorm(200, 0, 2))

  dfA <- segment(classA, segvec, labels = data.frame(id =factor("1"))) 
  dfB <- segment(classB, segvec, labels = data.frame(id =factor("2")))
  dfAB <- rbind(dfA ,dfB)
  ids <- unique(dfAB$id)
  xtest <- 1:18 
  xtrain <- 19:20
  test <- dfAB[xtest, c(1:21), ]
  mod <- segmentRF(id ~., data = test[xtest,])
  # mod <- segmentRF(dfAB[,1:20], y= df$AB)
  out <- predict(mod, newdata = dfAB[xtrain, 1:21], weights = dfAB[xtrain,]$weights)
  expect_that(is.factor(out$pred), is_true())
  expect_that(length(out$pred) == 1, is_true() )
  expect_that(length(out$prob) == length(xtrain), is_true())
  expect_that((dim(out$prob)[1] == 1), is_true())
  expect_that((dim(out$prob)[2] == length(ids)), is_true())
  expect_error(predict(mod, newdata = dfAB[xtrain, 1:21], weights = dfAB$weights))

})
