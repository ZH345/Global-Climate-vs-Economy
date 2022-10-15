library(ncdf4)
library(raster)
library(rgdal)
library(GADMTools)
library(lattice)
library(sp)
library(dplyr)
library(sf)
library(haven)
#安装方法
library(RColorBrewer)

#设置工作路径
setwd('D:/0 job/0 research/Global-Climate-vs-Economy/')
#RColorBrewer中的所有调色板
display.brewer.all()
#选取Set1调色板中的四种颜色

#cols表示的是四种不同颜色的名称
#创建一个跟T1变量的因子水平相对应的颜色向量

#ERA5
era5_list = list.files('./data/ERA5/')
iso_country = read.table("./ISO_CountryCode.csv",header = T,sep = ',')
countryList = list.files('./data/gadm/')
remV = c('VARNAME_0','VARNAME_1','NL_NAME_1','ISO_1','HASC_1','CC_1','ENGTYPE_1','VALIDFR_1','VARNAME_2','NL_NAME_2','HASC_2','CC_2','ENGTYPE_2','VALIDFR_2','VARNAME_3','NL_NAME_3','HASC_3','CC_3','ENGTYPE_3','VALIDFR_3','VARNAME_4','CC_4','ENGTYPE_4','VALIDFR_4','CC_5','ENGTYPE_5','GOVERNEDBY','SOVEREIGN','DISPUTEDBY','REGION','VARREGION','COUNTRY','SUBCONT')
economic_data = read_dta('./data/T_econ.dta')
economic_data$GID_1 = paste0(economic_data$iso,'.',economic_data$wrld1id_1,'_1')



# construct GADM n-levle
if (F){
  for (fname in iso_country$ISO_ID){
    #gadm_sf_loadCountries(fileNames = fname,level = 2,basefile = './data/gadm_l2/')
  }
}

if (F){
  map0 <- gadm_sf_import_shp(dir = './data/gadm/',name = 'ABW', level=5,del = remV,keepall = T)
  sf_use_s2(FALSE)
  gadm_plot(map0) %>% gadm_showNorth("tl") %>% gadm_showScale("bl")
  map0$sf <- map0$sf %>%  dplyr::group_by(GID_1) %>%
    dplyr::summarise()
  gadm_plot(map0) %>% gadm_showNorth("tl") %>% gadm_showScale("bl")
  map0$level = 2
  countryList[-4]
  #construct global map
  for (country in countryList){
    if (endsWith(country,'.shp')){
      curren_map <- gadm_sf_import_shp(dir = './data/gadm/',name = substr(country,1,3), level=5,del = remV,keepall = T)
      #融合尺度对应NAME_2中的数字，NAME_'level'
      curren_map$sf <- curren_map$sf %>%  dplyr::group_by(GID_1) %>%
        dplyr::summarise()
      map0$sf = rbind(map0$sf,curren_map$sf)
      #gadm_plot(map0) %>% gadm_showNorth("tl") %>% gadm_showScale("bl")
    }
  }
  GADMTools::gadm_exportToShapefile(x = map0,name = './data/gadm_global_l2/gadm_global_l2.shp')
}

map0 = GADMTools::gadm_sf_import_shp(dir = './data/gadm_global_l2/',name = 'gadm_global_l2', level = 2, keepall = T)

for (ncf in era5_list){
  if (endsWith(ncf,'.nc')){
    ncf = 'tm2_2021.nc'
    file = paste0('E:/0 job/data/ERA5/',ncf)
    files_nc = ncdf4::nc_open(file,write=FALSE, readunlim=T, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
    name_files_nc = names(files_nc$var)
    
    nc_raster = stack(file, varname = name_files_nc)
    if (F){#plot
      length(nc_raster@layers)
      typeof(nc_raster@layers[[2]])
      cols<-brewer.pal(n=32,name="RdYlGn")
      cols = as.array(cols)
      cols_list = as.array(cols)
      cols_list = rev(cols_list)
      plot(nc_raster@layers[[3]],xlab = nc_raster@layers[[3]]@data@names,main='ERA5 SFC t2m',col = cols_list)
    }
    
    dayt2mCount = 0
    mont2mCount = 0
    datalen = length(nc_raster@layers)
    ly_daysum = 0
    ly_monsum = 0
    ly_daymeanlist = NULL
    ly_monmeanlist = NULL
    ly_monvarlist = NULL
    region.monvarlist = NULL
    
    for (lyi in 1:datalen){
      ly1 = nc_raster@layers[[lyi]]
      nc_raster@layers[]
      #extract
      #plot(ly1,xlab = nc_raster@layers[[1]]@data@names,main='ERA5-Land SFC t2m',col = cols_list)
      ly1 = rotate(ly1)
      #plot(ly1,xlab = nc_raster@layers[[1]]@data@names,main='ERA5-Land SFC t2m',col = cols_list)
      values(ly1)[values(ly1)==-Inf] = NA
      values(ly1)[values(ly1)==Inf] = NA
      year = strsplit(ly1@data@names,split = '[.]')[[1]][1]
      month = strsplit(ly1@data@names,split = '[.]')[[1]][2]
      day = strsplit(ly1@data@names,split = '[.]')[[1]][3]
      date = paste0(year,month,day)
      
      
      
      if (lyi!=1){
        ly_daysum = ly_daysum+ly1
        dayt2mCount = dayt2mCount+1
        ly_monsum = ly_monsum+ly1
        mont2mCount = mont2mCount+1
        
      }else{
        ly0 = nc_raster@layers[[(lyi-1)]]
        ly0 = rotate(ly1)
        #day average
        day0 = strsplit(ly0@data@names,split = '[.]')[[1]][3]
        if (day == day0){
          ly_daysum = ly_daysum+ly1
          dayt2mCount = dayt2mCount+1
        }else{
          ly_daymean = ly_daysum/dayt2mCount
          ly_daymeanlist = append(ly_daymeanlist,ly_daymean)
          ly_daysum = 0
          dayt2mCount = 0
          if(F){
            library(ggplot2)
            ggplot()+
              geom_sf(aes(fill = ''),data = map0$sf)+
              scale_fill_viridis_c(option = "D", trans = "sqrt")
          }
        }
        
        #month average
        month0 = strsplit(ly0@data@names,split = '[.]')[[1]][2]
        if (month0 == month){
          ly_monsum = ly_monsum+ly1
          mont2mCount = mont2mCount+1
        }else{
          ly_monmean = ly_monsum/mont2mCount
          ly_monmeanlist = append(ly_monmeanlist,ly_monmean)
          
          ly_daymeanlist = c(ly1,ly1/3)
          
          ly.diffsum = 0
          ly_monvarsum <- sapply(ly_daymeanlist, function(ly.day){
            (ly.day - ly_monmean)^2
          }) %>% sapply(., function(ly.diff){
            ly.diffsum + ly.diff
          }) %>% last()
          
          
          ly_monvar = ly_monvarsum/mont2mCount
          ly_monvarlist = append(ly_monvarlist,ly_monvar)
          
          ly_daymeanlist = NULL
          
          ly_monsum = 0
          mont2mCount = 0
          
          #lydiffsum = 0
          region.monvar = extract(ly_monvar, map0$sf, fun = mean,
                                  df = T, na.rm = T,weights = T,small = T)
          #UN_DF = extract(ly_sum, map0$sf, fun = mean,
          #               df = T, na.rm = T,exact = T,small= T)
          
          region.monvar[region.monvar == -Inf] = NA
          
          region.monvarlist = append(region.monvarlist,region.monvar)
          
          if(F){
            library(ggplot2)
            ggplot()+
              geom_sf(aes(fill = ''),data = map0$sf)+
              scale_fill_viridis_c(option = "D", trans = "sqrt")
          }
        }
        
      }
      print(lyi)
      
    }
    
  }
  
}









library (rgdal)
library (RSQLite)

# Explore the layers available
ogrListLayers("./data/gadm_410.gpkg")
library(dplyr)

GPKGpath <- "./data/gadm_410.gpkg"
dta <- src_sqlite (GPKGpath)
tbldata <- tbl (dta, layer)
tbldf <- as.data.frame (tbldata)

db.hitters<-dbConnect(SQLite(),dbname = "./data/gadm_410.gpkg")
tt = dbGetQuery(db.hitters,"select * from gadm_410")[1:5,]
gadm_exportToShapefile(tt,name ="./data/tt.shp" )
gadm_sf.loadCountries()

gadm_afg <- readRDS("./data/AFG_adm2.rds")


rgdal::
  
  gadm410 = load_databasegpkg(GPKG = "./data/gadm_410.gpkg",layer = "gadm_410")
gadmsf = as(gadm410$geom,'sf')
as.vector(gadm410$geom[[1]])


map0_file <- gadm_sp_loadCountries(fileNames = "AFG",basefile = './data/',level =2)
gadm_plot(map_file) %>% gadm_showNorth("tl") %>% gadm_showScale("bl")


gm = gadm_sf_import_shp(dir = './data/',name ='Afghanistan',level = 1, keepall = T)
Badakhshan <- gadm_subset(gm, level=1, regions="Badakhshan")

gadm_plot(gadm410) %>% gadm_showNorth("tl") %>% gadm_showScale("bl")















