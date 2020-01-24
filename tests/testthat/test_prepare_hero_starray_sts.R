data(tasmax)
border = border.grid(lon, lat)
rs = radspline(nknots = 36, poverlap = 3,
               border = border, longlat = TRUE)
bs = bspline(c(1, 30), nbasis = 6)
sta = starray(tasmax)
sts = as.sts(tasmax)
p_sts = prepare(sts, coords = cbind(c(lon), c(lat)),
                times = 1:30, rs = rs, bs = bs)
p_starray = prepare(sta, x = lon, y = lat, times = 1:30,
                    rs = rs, bs = bs)
p_sts = enhance(p_sts)
p_starray = enhance(p_starray)
h_sts = hero(p_sts)
h_sta = hero(p_starray)

context("compare starray and sts results")

test_that("starray and sts results match", {
  expect_equal(h_sts$fitted, h_sta$fitted)
  expect_equal(h_sts$coefficents, h_sta$coefficents)
})

