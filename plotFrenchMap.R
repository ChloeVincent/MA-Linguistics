library(tidyverse)

# First I build the map of France
# I then add the "participants life": where the participants of my survey have lived
# Finally I keep only places where they chose to comment on a local accent, and I plot the responses to the survey

# feel free to contact me if you want further informations on the code or the linguistics project


# map data from base R - match departments (which are annoyingly called region in R) with regions from INSEE

#data from INSEE for official correspondance between region/departments/codes
reg2018 <- read_csv(paste0(corpus_folder,"external_data/reg2018.txt"))
dep2018 <- read_csv(paste0(corpus_folder,"external_data/depts2018.txt"))
depReg <- merge(reg2018, dep2018, by="REGION", suffixes = c(".reg", ".dep"))%>%
  add_row(DEP="20", NCCENR.reg = "Corse")

map.data_fr <-map_data("france") %>% rename(department=region) %>% select(-subregion)
# subregion include islands in France (Grois, Oleron, Noirmoutier...)

map.data_fr$region <- plyr::mapvalues(map.data_fr$department,
                                      as.vector(depReg$NCCENR.dep),
                                      as.vector(depReg$NCCENR.reg))


# COLOR REGIONS
map_colorRegion <- map.data_fr%>% ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = region), alpha = 0.8) + #alpha is to help bring out the point that comes next
  scale_fill_viridis_d(option="plasma")+
  scale_x_discrete(NULL)+ # no need for the longitude and latitude info
  scale_y_discrete(NULL)+
  labs(fill="Regions", title = "Regions of France")

map_colorRegion

#####################
# Participants lives
#####################

# Initial Data Processing provide a table the with the different places participants have lived (postcode, period, coordinate longitude and latitude, corresponding department and region) - I used the official coordinate for the postal service (https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/)
part_life <-read_csv(paste0(corpus_folder,"participants_lives.csv")) %>%select(-X1)%>%filter(is.na(as.numeric(reg)))

p_mapPartLife <- map_colorRegion + geom_point(data=part_life, aes(x=coord_long, y=coord_lat, size=period))+labs(size="Period in years", title="Where participants have lived")

p_mapPartLife


##############################################
# 1 point per region commented per participant
##############################################
df <-read.csv(paste0(corpus_folder,"dataframeAfterImport.csv")) # survey results 

# mapping of commented region to coordinate correponding to postcode
placesDescribed <- df %>% select(ID, region1, meanLat, meanLong,emotionalJudgment,statusJudgment, lesFrancaisAiment, lesFrancaisAimeLaRegion, accentFort) %>% unique()
# I use mean latitude and longitude because some people have moved within a region and I want only 1 point per accent commented. To build the map I consider that they have commented only one accent for each region - which is slightly unaccurate, but negligeable.

map_placesDescribed <- map_colorRegion+geom_point(data= placesDescribed, aes(x=meanLong, y=meanLat))+
  labs(title = "Places described by participants")

map_placesDescribed

# DEPARTEMENTS MAP to display participants responses depending on their location
departmentmap <-  map.data_fr%>% ggplot() +
  geom_polygon(aes(long, lat, group = group),color = "black", fill="white")+
  scale_x_discrete(NULL)+
  scale_y_discrete(NULL)+
  labs(title= "Participants responses to the survey")

# Survey responses:
departmentmap+geom_jitter(data= placesDescribed, aes(x=meanLong, y=meanLat, color=emotionalJudgment), size=3, width=0.2, height = 0.2) + scale_color_viridis_c(option="plasma")+labs(color="Emotional\njudgment")

departmentmap+geom_jitter(data= placesDescribed, aes(x=meanLong, y=meanLat, color=statusJudgment), size=3, width=0.2, height = 0.2) + scale_color_viridis_c(option="plasma")+labs(color="Status\njudgment")

departmentmap+geom_jitter(data= placesDescribed, aes(x=meanLong, y=meanLat, color=lesFrancaisAiment), size=3, width=0.2, height = 0.2) + scale_color_viridis_c(option="plasma")+labs(color="French people\nlike the accent")

departmentmap+geom_jitter(data= placesDescribed, aes(x=meanLong, y=meanLat, color=lesFrancaisAimeLaRegion), size=3, width=0.2, height = 0.2) + scale_color_viridis_c(option="plasma")+labs(color="French people\nlike the region")

