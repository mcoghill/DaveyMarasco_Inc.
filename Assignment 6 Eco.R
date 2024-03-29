library(readxl)
library(sf)
library(tidyverse)
library(mapview)
library(bcdata)
library(bcmaps)
library(terra)

View(available_layers())


all_zone <- bec()


# mapview(all_zone)


lacdb <- bcdc_search("BC Parks Ecological Reserves Protected Areas")
View(lacdb)

lacdbprotected <- bcdc_get_data(record = "1130248f-f1a3-4956-8b2e-38d29d3e4af7")
View(lacdbprotected)
mapview(lacdbprotected)

LacDubois <- lacdbprotected %>%
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA")
mapview(LacDubois)

#########
## 4/4 ##
#########

BEC_MAP<-bec()
BEC_LacDuBois<- st_intersection(BEC_MAP,LacDubois)


BEC_LacDuBois$area_sqm <- st_area(BEC_LacDuBois)
BEC_LacDuBois$area_ha <- BEC_LacDuBois$area_sqm / 10000

#########
## 2/2 ##
#########

BEC_Zone_Area_Sum <- BEC_LacDuBois %>%
  summarise(Total_Area = sum(area_ha))

library(units)

BEC_LacDuBois$area_ha<- as.numeric(BEC_LacDuBois[["area_ha"]])
view(BEC_LacDuBois)

colnames(BEC_LacDuBois)[8] <- 'BEC_Zones'

ggplot(BEC_LacDuBois, aes(x = BEC_Zones, y = area_ha, fill = BEC_Zones)) +
  geom_bar(stat = "identity")+
  labs(title = "BEC Zones in the Lac du Bois Grasslands", x = "BEC Zones", y = "Area (ha)")+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  guides(fill = guide_legend(title = "BEC Zones"))

#########
## 5/5 ##
#########

#Question 2#
site_dem <- cded_terra(BEC_LacDuBois)
site_dem
plot(site_dem)

lac_dubois_vect <- vect(BEC_LacDuBois)
elevations <- extract(site_dem, lac_dubois_vect, fun = mean, na.rm = TRUE)
BEC_LacDuBois$Mean_Elevation <- elevations
view(BEC_LacDuBois)
mapview(BEC_LacDuBois)

###########
## 3.5/4 ##
###########

## On line 69, it originally was vect(LacDuBois) - this produced an incorrect
## result. Changing it to BEC_LacDuBois fixes that.

#3#
map <- mapview(BEC_LacDuBois, zcol = "SUBZONE", legend = TRUE)
print(map)

#########
## 2/2 ##
#########


## Total:

#############
## 16.5/17 ##
#############