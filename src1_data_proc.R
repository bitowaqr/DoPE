# generate and merge data 

library(sp)
library(geosphere)
lsoa_cntrds = raster::shapefile("./raw_data/England_lsoa_2011_centroids/england_lsoa_2011_centroids")

lsoa_pop = read.csv("./raw_data/IoD2019_Population_Denominators.csv",stringsAsFactors = F)
lsoa_pop[,-c(1:4)] = data.frame(apply(lsoa_pop[,-c(1:4)],2,function(x){as.numeric(as.character(gsub(",","",x)))}),stringsAsFactors = F)
lsoa_pop = lsoa_pop[,c(1,5)]
names(lsoa_pop) = c("code","total_pop")

lsoa_participation = read.csv("./raw_data/parkrun_data/runs_per_lsoa_010117_101218.csv", stringsAsFactors = F)

lsoa_imd = read.csv("./raw_data/IoD2019_Scores.csv", stringsAsFactors = F)
lsoa_imd = lsoa_imd[,-c(2,3,4)]

parkrun_events = read.csv("./raw_data/parkrun_data/event_info_scraped_10.12.18.csv", stringsAsFactors = F)[,-1]


lsoa_cntrds = spTransform(lsoa_cntrds,CRS("+proj=longlat"))
lsoa_cntrds_coord = coordinates(lsoa_cntrds)
lsoa_cntrds_coord_df = data.frame(code = lsoa_cntrds@data$code,
                                  centr_lng = lsoa_cntrds_coord[,1],
                                  centr_lat = lsoa_cntrds_coord[,2],
                                  stringsAsFactors = F
)

dist_M_full = geosphere::distm(lsoa_cntrds_coord_df[,2:3],parkrun_events[,2:3])
rownames(dist_M_full) = lsoa_cntrds$code
colnames(dist_M_full) = parkrun_events$course 
# distance to the narest event for each LSOA
lsoa_min_dist = apply(dist_M_full,1,FUN= function(x){round(min(x),0)} )
lsoa_distance = data.frame(lsoa_cntrds$name,mn_dstn = lsoa_min_dist / 1000)

  
# ethnicity

# density?

obs_period = 49 #weeks
