context("check assemble.hero_bsplines")

s = seq(0, 1, len = 101)

test_that("assemble.hero_splines matches pspline.setting", {
  nk = seq(10, 25, len = 4)
  o = 1:4
  mc = 1:5
  for (i in seq_along(nk)) {
    for (j in seq_along(o)) {
      for (k in seq_along(mc)) {
        list1 = pspline.setting(s, knots = nk[i], p = o[j] - 1, m = mc[k])
        x = bspline(knots = list1$knots, norder = o[j])
        list2 = assemble(x, x = s, m = mc[k])
        expect_equivalent(as.matrix(list1$B), as.matrix(list2$B))
        expect_equivalent(list1$s, list2$s)
      }
    }
  }
})
