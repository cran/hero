# library(ncdf4)
#
# tasmax_array = function(a, cuts) {
#   aperm(apply(a, 1:2, function(x) tapply(x, cuts, max)), c(2:3, 1))
# }
#
# timecuts = function(x) {
#   cut(x, floor(min(x)):ceiling(max(x)))
# }
#
# setwd("~/Dropbox")
#
# nc68 = nc_open("tas_ECP2_gfdl_1968010103.nc")
# lon_ecp2 = ncvar_get(nc68, "lon") - 360
# lat_ecp2 = ncvar_get(nc68, "lat")
# alt_ecp2 = ncvar_get(nc68, "level")
# nc_close(nc68)
#
# nc71 = nc_open("tas_ECP2_gfdl_1971010103.nc")
# tas71 = ncvar_get(nc71, "tas")
# t71 = ncvar_get(nc71, "time")
# nc_close(nc71)
# tasmax71 = tasmax_array(tas71, timecuts(t71))
# rm(tas71)
#
# nc76 = nc_open("tas_ECP2_gfdl_1976010103.nc")
# tas76 = ncvar_get(nc76, "tas")
# t76 = ncvar_get(nc76, "time")
# nc_close(nc76)
# tasmax76 = tasmax_array(tas76, timecuts(t76))
# rm(tas76)
#
# nc81 = nc_open("tas_ECP2_gfdl_1981010103.nc")
# tas81 = ncvar_get(nc81, "tas")
# t81 = ncvar_get(nc81, "time")
# nc_close(nc81)
# tasmax81 = tasmax_array(tas81, timecuts(t81))
# rm(tas81)
#
# nc86 = nc_open("tas_ECP2_gfdl_1986010103.nc")
# tas86 = ncvar_get(nc86, "tas")
# t86 = ncvar_get(nc86, "time")
# nc_close(nc86)
# tasmax86 = tasmax_array(tas86, timecuts(t86))
# rm(tas86)
#
# nc91 = nc_open("tas_ECP2_gfdl_1991010103.nc")
# tas91 = ncvar_get(nc91, "tas")
# t91 = ncvar_get(nc91, "time")
# nc_close(nc91)
# tasmax91 = tasmax_array(tas91, timecuts(t91))
# rm(tas91)
#
# nc96 = nc_open("tas_ECP2_gfdl_1996010103.nc")
# tas96 = ncvar_get(nc96, "tas")
# t96 = ncvar_get(nc96, "time")
# nc_close(nc96)
# tasmax96 = tasmax_array(tas96, timecuts(t96))
# rm(tas96)
#
# tasmax_ecp2_gfdl_1971_2000 = abind::abind(tasmax71, tasmax76,
#                                 tasmax81, tasmax86,
#                                 tasmax91, tasmax96) - 273.14
#
# save(lon_ecp2, lat_ecp2, alt_ecp2, file = "ecp2_covar.rda",
#      compress = "xz")
#
# save(tasmax_ecp2_gfdl_1971_2000,
#      file = "tasmax_ecp2_gfdil_1971_2000.rda",
#      compress = "bzip2")
#
# lon = lon_ecp2
# lat = lat_ecp2
# tasmax = tasmax_ecp2_gfdl_1971_2000[,,1:30]
# save(lon, lat, tasmax, file = "tasmax.rda", compress = "xz")

