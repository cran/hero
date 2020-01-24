# library(ncdf4)
#
# setwd("~/Dropbox/UCD_Files/Research/In Progress/tasmax_WRFG_cgcm3")
# wrfg_covar = nc_open("orog_WRFG.nc") #open covariates file
# wrfg_lon <- ncvar_get(wrfg_covar, "lon") #longitude
# wrfg_lat <- ncvar_get(wrfg_covar, "lat") #latitude
# wrfg_alt <- ncvar_get(wrfg_covar, "orog") #altitude
# nc_close(wrfg_covar)
#
# wrfg_covar2 = nc_open("sftlf_WRFG.nc") #open covariates file
# wrfg_sftlf <- ncvar_get(wrfg_covar2, "sftlf") #altitude
# nc_close(wrfg_covar2)
#
# mydim = dim(wrfg_lon)
#
# # Get tasmax from 2038 file
# wrfg_cgcm3_2038 = nc_open("tasmax_WRFG_cgcm3_2038010106.nc")
# tasmax_2038 <- ncvar_get(wrfg_cgcm3_2038, varid = "tasmax")
# time_bnds_2038 <- ncvar_get(wrfg_cgcm3_2038, varid = "time_bnds")
# nc_close(wrfg_cgcm3_2038)
#
# # Get tasmax from 2041 file
# wrfg_cgcm3_2041 = nc_open("tasmax_WRFG_cgcm3_2041010106.nc")
# tasmax_2041 <- ncvar_get(wrfg_cgcm3_2041, varid = "tasmax")
# time_bnds_2041 <- ncvar_get(wrfg_cgcm3_2041, varid = "time_bnds")
# nc_close(wrfg_cgcm3_2041)
#
# # Get tasmax from 2046 file
# wrfg_cgcm3_2046 = nc_open("tasmax_WRFG_cgcm3_2046010106.nc")
# tasmax_2046 <- ncvar_get(wrfg_cgcm3_2046, varid = "tasmax")
# time_bnds_2046 <- ncvar_get(wrfg_cgcm3_2046, varid = "time_bnds")
# nc_close(wrfg_cgcm3_2046)
#
# # Get tasmax from 2051 file
# wrfg_cgcm3_2051 = nc_open("tasmax_WRFG_cgcm3_2051010106.nc")
# tasmax_2051 <- ncvar_get(wrfg_cgcm3_2051, varid = "tasmax")
# time_bnds_2051 <- ncvar_get(wrfg_cgcm3_2051, varid = "time_bnds")
# nc_close(wrfg_cgcm3_2051)
#
# # Get tasmax from 2056 file
# wrfg_cgcm3_2056 = nc_open("tasmax_WRFG_cgcm3_2056010106.nc")
# tasmax_2056 <- ncvar_get(wrfg_cgcm3_2056, varid = "tasmax")
# time_bnds_2056 <- ncvar_get(wrfg_cgcm3_2056, varid = "time_bnds")
# nc_close(wrfg_cgcm3_2056)
#
# # Get tasmax from 2061 file
# wrfg_cgcm3_2061 = nc_open("tasmax_WRFG_cgcm3_2061010106.nc")
# tasmax_2061 <- ncvar_get(wrfg_cgcm3_2061, varid = "tasmax")
# time_bnds_2061 <- ncvar_get(wrfg_cgcm3_2061, varid = "time_bnds")
# nc_close(wrfg_cgcm3_2061)
#
# # Get tasmax from 2066 file
# wrfg_cgcm3_2066 = nc_open("tasmax_WRFG_cgcm3_2066010106.nc")
# tasmax_2066 <- ncvar_get(wrfg_cgcm3_2066, varid = "tasmax")
# time_bnds_2066 <- ncvar_get(wrfg_cgcm3_2066, varid = "time_bnds")
# nc_close(wrfg_cgcm3_2066)
#
# wrfg_cgcm3_tasmax = abind::abind(tasmax_2041,
#                                  tasmax_2046,
#                                  tasmax_2051,
#                                  tasmax_2056,
#                                  tasmax_2061,
#                                  tasmax_2066)
# wrfg_lon = wrfg_lon - 360
# wrfg_cgcm3_tasmax = wrfg_cgcm3_tasmax - 273.15
#
# usapoly = maps::map("usa")
# p1 = list(x = usapoly$x[1:533], y = usapoly$y[1:533])
# plot(p1, type = "l")
#
# wrfg_lonv = c(wrfg_lon)
# wrfg_latv = c(wrfg_lat)
# wrfg_cgcm3_tasmaxm = matrix(c(wrfg_cgcm3_tasmax[,,1:365]), ncol = 365)
#
# pip = sp::point.in.polygon(wrfg_lonv, wrfg_latv, p1$x, p1$y)
# w = which(pip == 1)
# usa_sp = sp::SpatialPointsDataFrame(coords = cbind(wrfg_lonv, wrfg_latv)[w, ],
#                                     data = as.data.frame(wrfg_cgcm3_tasmaxm[w, ]))
# plot(p1, type = "l")
# points(wrfg_lonv, wrfg_latv)
# spplot(usa_sp, c("V1", "V2"))
# autoimage::autoimage(wrfg_lonv, wrfg_latv, wrfg_cgcm3_tasmaxm[,1:4])
#
# usa_sppoly = sp::Polygon(cbind(p1$x, p1$y), hole = FALSE)
# usa_sppoly = sp::Polygons(list(usa_sppoly), "usa_main")
# usa_sppoly = sp::SpatialPolygons(list(usa_sppoly))
#
# usa_coords = cbind(wrfg_lonv, wrfg_latv)[w, ]
# usa_tasmax = wrfg_cgcm3_tasmaxm[w, ]
# save(usa_sppoly, usa_coords, usa_tasmax, file = "usa_tasmax.rda", compress = "xz")
#
# for (i in 1:nrow(wrfg_sftlf)) {
#   for (j in 1:ncol(wrfg_sftlf)) {
#     if (wrfg_sftlf[i, j] < 1) {
#       wrfg_sftlf[i, j] = NA
#     }
#   }
# }
#
# # $x
# [1] 273.7302 303.5238 304.7276
#
# $y
# [1] 24.35004 70.04290 57.66775
#
# u1 = which(wrfg_lon > 273 & wrfg_lat < 24.35, arr.ind = TRUE)
# u2 = which(wrfg_lon > 303.5 & wrfg_lat > 57 & wrfg_lat < 70, arr.ind = TRUE)
# wrfg_sftlf[u1] = NA
# wrfg_sftlf[u2] = NA
#
# nonna = which(!is.na(c(wrfg_sftlf)))
#
#
# image(wrfg_sftlf)
#
# for (i in seq_len(dim(wrfg_cgcm3_tasmax)[3])) {
#   wrfg_cgcm3_tasmax[,,i] = wrfg_cgcm3_tasmax[,,i] * wrfg_sftlf
# }
#
# wrfg_cgcm3_tasmax = wrfg_cgcm3_tasmax[,,1:30]
# save(wrfg_lon, wrfg_lat, wrfg_cgcm3_tasmax, file = "wrfg_cgcm3_tasmax.rda", compress = "xz")
