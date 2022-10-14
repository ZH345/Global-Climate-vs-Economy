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
file = 'D:/zz/0 laptop_yoga/0 DataFusion/literature/application/global_economy/supplement/data/ERA5/era5_singleLevel_20010501_temperature_2m.nc'
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
















#MISR

library(data.table)
fn = 'D:/0 job/0 DataFusion/R/SSDF_Plus/data/MISR2/202206145234/MISR_AM1_AS_AEROSOL_P169_O005708_F13_0023.nc'
file_paths = list.files(path = 'D:/0 job/0 DataFusion/R/SSDF_Plus/data/MISR2/202206145234/',full.names = T,pattern = '*.nc$')

AOD_all = NULL
library(FRK)
edge = data.frame(lon=c(0,0,30,30),lat=c(-30,0,0,-30))
coordinates(edge) = ~lon+lat
proj4string(edge) = CRS("+proj=longlat +datum=WGS84 +no_defs")

GridBAUs1 <- auto_BAUs(manifold = plane(), # 2D plane
                       cellsize = c(0.125,0.125), # BAU cellsize
                       type = "grid", # grid (not hex)
                       data = edge, # data around which to create BAUs
                       convex=-0.05, # border buffer factor
                       nonconvex_hull=FALSE) # convex hull
poly_125 = sp::as.SpatialPolygons.GridTopology(GridBAUs1@grid)
poly_125@proj4string = GridBAUs1@proj4string
plot(GridBAUs1)

fi = 2
for (fn in file_paths){  
  fi = fi+1
  files_nc = ncdf4::nc_open(fn)
  files_nc$var$`Longitude` = ncdf4::ncvar_get(nc = files_nc,varid = '4.4_KM_PRODUCTS/Longitude')
  files_nc$var$`Latitude` = ncdf4::ncvar_get(nc = files_nc,varid = '4.4_KM_PRODUCTS/Latitude')
  #nr = brick(fn,varname = '4.4_KM_PRODUCTS/Aerosol_Optical_Depth')
  
  #plot(nr)
  
  aod = ncdf4::ncvar_get(nc = files_nc,varid = '4.4_KM_PRODUCTS/Aerosol_Optical_Depth')
  lon = ncdf4::ncvar_get(nc = files_nc,varid = '4.4_KM_PRODUCTS/Longitude')
  lat = ncdf4::ncvar_get(nc = files_nc,varid = '4.4_KM_PRODUCTS/Latitude')
  
  
  aod = melt(data = aod)
  lon = melt(data = lon)
  lat = melt(data = lat)
  
  aod = aod$value
  
  aod_df = data.frame(aod)
  
  aod_df$lon = lon$value
  aod_df$lat = lat$value

  
  #aod_df$aod[is.na(aod_df$aod),] = -1
  
  aod_df = aod_df[aod_df$lon> -1 & aod_df$lon < 31,]
  aod_df = aod_df[aod_df$lat> -31 & aod_df$lat < 1,]
  aod_df = aod_df[!is.na(aod_df$aod),]
  AOD_all = rbind(AOD_all,aod_df)
  
  #t = merge(aod_df2,aod_df[,c('lon','lat','aod')],by=c('lon','lat'))

  
  
  length(AOD_all$aod)

}

if (F){
  aodlen = length(AOD_all$aod)
  ri = 0
  GridBAUs1@data[,3] = 0
  GridBAUs1@data[,4] = 0
  while (ri < aodlen){
    aod_point_1 = AOD_all[(ri+1):(ri+1000),]
    
    
    coordinates(aod_point_1) = ~lon+lat
    proj4string(aod_point_1) = CRS("+proj=longlat +datum=WGS84 +no_defs")
    

    t = raster::extract(poly_125,aod_point_1['aod'])
    t = t[!duplicated(t$point.ID),]
    AOD_all_mean = aggregate(aod_point_1$aod,by=list(t$poly.ID),mean)
    
    GridBAUs1@data[AOD_all_mean$Group.1,3]=GridBAUs1@data[AOD_all_mean$Group.1,3]+AOD_all_mean$x
    GridBAUs1@data[AOD_all_mean$Group.1,4]=GridBAUs1@data[AOD_all_mean$Group.1,4]+1
    ri= ri+1000
    print(ri)
    print(ri/374249*100)
  }
  
}

plot(GridBAUs1)


AOD_all$lon_c = substr(as.character(AOD_all$lon),1,8)
AOD_all$lat_c = substr(as.character(AOD_all$lat),1,8)

AOD_all$aod[duplicated(AOD_all[,c('lon_c')])]

AOD_all$aod[is.na(AOD_all$aod)]
#AOD_all$aod[AOD_all$aod>1] = 1
length(AOD_all$aod)
aod_point = AOD_all
coordinates(aod_point) = ~lon+lat
proj4string(aod_point) = CRS("+proj=longlat +datum=WGS84 +no_defs")




AOD_all$group = paste0(substr(x = as.character(AOD_all$lon),start = 1,stop = 6),',',substr(as.character(AOD_all$lat),1,6))
AOD_all$aod[duplicated(AOD_all$group)]


library(ggplot2)
GridBAUs1@data$V3[GridBAUs1@data$V3==0] = NA

GridBAUs1@data$aod_mean = GridBAUs1@data$V3/GridBAUs1@data$V4
g1 = ggplot() +
  geom_tile(data = GridBAUs1@data, aes(x=lon,y=lat,fill = aod_mean),na.rm = T)+
  #geom_tile(size = 0.1,na.rm = T)+
  #geom_line(size = 0.5) +
  #scale_fill_continuous(type = "viridis") +
  #geom_abline(intercept=0.75,slope=0,color = 'black')+
  #geom_vline(xintercept=1.64,color = 'red4',size = 0.15,lty = "dashed")+
  #geom_smooth(method = 'loess',formula = y~x,size = 0.2,se=F,na.rm = T)+
  #scale_color_manual(values = c("red","red4","aquamarine1","aquamarine4",'blue1',"blue4"),name = 'Model(Level)',labels = c('FRK(mix)','Kriging(mix)','FRK(point)','Kriging(point)','FRK(area)','Kriging(area)'))+
  scale_fill_gradient2(low = "steelblue",mid =  "#FFFFB9",high = "#FF0000",midpoint = c(0.5))+
  #ylim(-30,0)+
  #xlim(0,30)+
  ylab("Lattitude")+
  xlab('Longitude')+
  #scale_color_manual(name ="Values", values = value, labels = c("* 0.1","* 0.3")) +
  #facet_grid(~ value)+
  #scale_colour_gradient2(low = 'red', mid = 'cyan',high='#F08080',midpoint = 0.7,name = 'Distance Power',breaks = c(0.01,0.5,1.0,1.5,2.0),labels = c('0.01','0.5','1.0','1.5','2.0'))+
  theme_bw()+
  theme(panel.grid =element_blank())+
  theme(axis.title.x = element_text(size = 7,color="black"),axis.title.y = element_text(size = 7,color="black"))+
  theme(axis.text.x = element_text(size = 7,color="black"),axis.text.y = element_text(size = 7,color="black"))+
  theme(legend.key.size = unit(7, "pt"),legend.title = element_text(size = 7,color="black"),axis.text.y = element_text(size = 7,color="black"))+
  theme(legend.text = element_text(size = 6,color="black"),axis.text.y = element_text(size = 6,color="black"))+
  theme(legend.position = c(0.8,0.2))
g1



plot(aod_pixel)

plot(aod_pixel)
levelplot(aod_point$aod~aod_point$lon_wgs+aod_point$lat_wgs)



spplot(aod_point)


t = as.array(lon)
aod_raster = data.frame(aod)



raster::rasterFromXYZ()
raster:

raster::raster(aod,lo)
nc_raster = raster::raster(file, varname = '4.4_KM_PRODUCTS/Aerosol_Optical_Depth')

writeGDAL(nc_raster,fname = 'D:/0 job/0 DataFusion/R/SSDF_Plus/data/MISR2/202206145234/tt.tif',drivername  = 'GTiff')
writeRaster(nc_raster,filename = 'D:/0 job/0 DataFusion/R/SSDF_Plus/data/MISR2/202206145234/栅格.tif',format='GTiff')


projectRaster(nc_raster,ly1)

length(nc_raster@layers)
typeof(nc_raster@layers[[2]])
cols<-brewer.pal(n=32,name="RdYlGn")
cols = as.array(cols)
cols_list = as.array(cols)
cols_list = rev(cols_list)
plot(nc_raster,xlab = nc_raster@data@names,main='ERA5 SFC t2m',col = cols_list)



library('gdalUtils')
library(rgdal)
gdal_setInstallation(search_path = "D:/softs/pythonProject2/venv/Lib/site-packages/osgeo")

sds = get_subdatasets("D:/0 job/0 DataFusion/R/SSDF_Plus/data/MISR2/202206145234/MISR_AM1_AS_AEROSOL_P169_O005708_F13_0023.nc")
sds
md = gdalwarp(sds[11])
gdal_translate(sds[11], dst_dataset = "D:/0 job/0 DataFusion/R/SSDF_Plus/data/MODIS/tif/t1.tif")


