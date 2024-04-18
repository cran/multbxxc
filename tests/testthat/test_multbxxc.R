context("bop")
n=3
a=matrix(1, n, n)
b=matrix(1:n, n, 1)

test_that("bop +=", {
  # matrix modified by column vector
  a0=a+0
  bop(a, 2, "+=", b)
  expect_equal(a, a0+rep(b, n))
  
  # matrix modified in arbitrary places by a scalar
  im=cbind(2L:3L, 1L:2L) # operate on elements (2,1) and (3,2)
  ip=structure(1L:length(a), dim=dim(a)) # plain indexes
  a0=a+0
  bop(a, im, "+=", pi)
  expect_equal(a[im], a0[im]+pi) # modified region
  expect_equal(a[-ip[im]], a0[-ip[im]]) # non modified region
  
  # vector modified in arbitrary places by a scalar
  v=(1:n)+pi
  iv=c(1L,3L)
  v0=v+0;
  bop(v, as.matrix(iv), "+=", pi)
  expect_equal(v[iv], v0[iv]+pi)
  expect_equal(v[-iv], v0[-iv])
})
