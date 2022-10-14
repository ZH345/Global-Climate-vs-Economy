require(ncdf4)
require(raster)
library(rgdal)
library(GADMTools)
library(lattice)
library(sp)
library(dplyr)
library(sf)
#安装方法
library(RColorBrewer)
#RColorBrewer中的所有调色板
display.brewer.all()
#选取Set1调色板中的四种颜色

#cols表示的是四种不同颜色的名称
#创建一个跟T1变量的因子水平相对应的颜色向量



#ERA5
file = 'D:/0 job/0 DataFusion/literature/application/global_economy/supplement/data/ERA5/era5_singleLevel_20010501_temperature_2m.nc'
files_nc = ncdf4::nc_open(file,write=FALSE, readunlim=T, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
name_files_nc = names(files_nc$var)
name_files_nc
nc_raster = stack(file, varname = 't2m')
length(nc_raster@layers)
typeof(nc_raster@layers[[2]])
cols<-brewer.pal(n=32,name="RdYlGn")
cols = as.array(cols)
cols_list = as.array(cols)
cols_list = rev(cols_list)
plot(nc_raster@layers[[3]],xlab = nc_raster@layers[[3]]@data@names,main='ERA5 SFC t2m',col = cols_list)

#ERA5-Land
file = 'D:/0 job/0 DataFusion/literature/application/global_economy/supplement/data/ERA5/era5_land_20010501_temperature_2m.nc'
files_nc = ncdf4::nc_open(file,write=FALSE, readunlim=T, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
name_files_nc = names(files_nc$var)
name_files_nc
nc_raster = stack(file, varname = 't2m')
length(nc_raster@layers)
typeof(nc_raster@layers[[2]])
cols<-brewer.pal(n=32,name="RdYlGn")
cols = as.array(cols)
cols_list = as.array(cols)
cols_list = rev(cols_list)
plot(nc_raster@layers[[3]],xlab = nc_raster@layers[[3]]@data@names,main='ERA5-Land SFC t2m',col = cols_list)

#ERA5-PL
file = 'D:/0 job/0 DataFusion/literature/application/global_economy/supplement/data/ERA5/era5_pressLevel_20010501_temperature.nc'
files_nc = ncdf4::nc_open(file,write=FALSE, readunlim=T, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
name_files_nc = names(files_nc$var)
name_files_nc
nc_raster = stack(file, varname = 't')
length(nc_raster@layers)
typeof(nc_raster@layers[[2]])
plot(nc_raster@layers[[3]],xlab = nc_raster@layers[[3]]@data@names,main='ERA5 pressure-level  t',col = cols_list)



'GBR'
ly1 = raster(nc_raster,layer = 1)
plot(ly1,xlab = nc_raster@layers[[1]]@data@names,main='ERA5-Land SFC t2m',col = cols_list)

ly1 = rotate(ly1)
plot(ly1,xlab = nc_raster@layers[[1]]@data@names,main='ERA5-Land SFC t2m',col = cols_list)
max(values(ly1),na.rm = T)

values(ly1)[values(ly1)==-Inf] = NA

values(ly1)[values(ly1)==Inf] = NA

map <- gadm_sp_loadCountries(c("GBR"), level=2, basefile = "./")
map$spdf

gadm_plot(map)
#UN = readOGR(dsn = 'D:/0 job/0 DataFusion/literature/application/global_economy/supplement/data/ERA5/UN.shp',layer = 'UN')

#spTransform(map$spdf, CRS("+proj=eqc +lon_0=360"))



UN_DF = extract( ly1, map$spdf, fun = mean,
        df = T, na.rm = T)


map$spdf@data$t2m = UN_DF$X2001.05.01.00.00.00
map$spdf@data$t2m[map$spdf@data$t2m == -Inf] = NA
gadm_plot(map)
breaks<-c(0,3,4,5)*10000
library(tmap)

tm4<-tm_shape(map$spdf)+tm_polygons(col = "t2m",palette="RdYlGn" ,breaks=NULL)
tm4














