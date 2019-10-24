# generate and merge data 

#     rm(list=ls())

library(sp)
library(geosphere)
library(dplyr)


# LSOA centroids
lsoa_cntrds = raster::shapefile("./raw_data/England_lsoa_2011_centroids/england_lsoa_2011_centroids")
lsoa_cntrds = spTransform(lsoa_cntrds,CRS("+proj=longlat"))
lsoa_cntrds = data.frame(code = lsoa_cntrds$code, coordinates(lsoa_cntrds),stringsAsFactors = F)

# English parkrun event locations 2018
parkrun_events = read.csv("./raw_data/parkrun_data/event_info_scraped_10.12.18.csv", stringsAsFactors = F)[,-1]

# LSOA distance to nearest parkrun event
dist_M_full = geosphere::distm(lsoa_cntrds[,2:3],parkrun_events[,2:3])
lsoa_distance = apply(dist_M_full,1,FUN= function(x){round(min(x),0)} )
lsoa_distance = data.frame(code = lsoa_cntrds$code,mn_dstn = lsoa_distance / 1000) # in km
rm("dist_M_full","parkrun_events","lsoa_cntrds")

# LSOA parkrun participation
lsoa_participation = read.csv("./raw_data/parkrun_data/runs_per_lsoa_010117_101218.csv", stringsAsFactors = F)

# LSOA total population
lsoa_pop = read.csv("./raw_data/IoD2019_Population_Denominators.csv",stringsAsFactors = F)
lsoa_pop[,-c(1:4)] = data.frame(apply(lsoa_pop[,-c(1:4)],2,function(x){as.numeric(as.character(gsub(",","",x)))}),stringsAsFactors = F)
lsoa_pop = lsoa_pop[,c(1,5,7)]
names(lsoa_pop) = c("code","total_pop","perc_non_working_age")
lsoa_pop$perc_non_working_age = 1-(lsoa_pop$perc_non_working_age / lsoa_pop$total_pop)

# density
lsoa_density = read.csv("./raw_data/Mid-2017 Population Density.csv",stringsAsFactors = F)[,c(1,4)]
lsoa_density = rename(lsoa_density, code = Code)
lsoa_density = merge(lsoa_density,lsoa_pop[,1:2],by="code")
lsoa_density$pop_density = round(lsoa_density$total_pop / lsoa_density$Area.Sq.Km,2)
lsoa_density = lsoa_density[,c(1,4)]

# LSOA IMD score
lsoa_imd = read.csv("./raw_data/IoD2019_Scores.csv", stringsAsFactors = F)
lsoa_imd = lsoa_imd[,-c(2:4,13:20)]
names(lsoa_imd) = c("code","imd","d_income","d_employment","d_education","d_health","d_crime","d_housing","d_enviroment")


# ethnicity
lsoa_ethnicity = read.csv("raw_data/LSOA_Ethnicity.csv",stringsAsFactors = F)
lsoa_ethnicity = lsoa_ethnicity[,3:5]
lsoa_ethnicity = data.frame(code = lsoa_ethnicity$geography.code,
                            perc_bme = 1-lsoa_ethnicity$Sex..All.persons..Age..All.categories..Age..Ethnic.Group..White..Total..measures..Value/lsoa_ethnicity$Sex..All.persons..Age..All.categories..Age..Ethnic.Group..All.categories..Ethnic.group..measures..Value)
lsoa_ethnicity = lsoa_ethnicity[!(grepl("W",lsoa_ethnicity$code)),]                            
# density?

# merge everything
lsoa_df = Reduce(function(x, y) merge(x, y,by="code", all=TRUE), list(lsoa_participation,lsoa_distance, lsoa_imd, lsoa_pop,lsoa_density,lsoa_ethnicity))
lsoa_df$run_count[is.na(lsoa_df$run_count)] = 0
# observation period 1 Jan 2018-10 Dec 2018
obs_period = 49 # weeks


write.csv(lsoa_df,"./output/lsoa_df.csv",row.names = F)

