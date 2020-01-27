library(osmdata)
library(tidyverse)
library(sf)
GreyPlot<-function(XMIN, XMAX, YMIN, YMAX){
  p<-ggplot() +
    geom_sf(data = streets$osm_lines,
            inherit.aes = FALSE,
            color = "#FFFFFF",
            size = .4,
            alpha = .8) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#F7F7F7",
            size = .2,
            alpha = .6) +
    # geom_sf(data = river$osm_lines,
    #         inherit.aes = FALSE,
    #         color = "#F7F7F7",
    #         size = .2,
    #         alpha = .5) +
    coord_sf(xlim = c(XMIN, XMAX), 
             ylim = c(YMIN, YMAX),
             expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#AAAAAA")
    )
  return(p)}

BlackPlot<-function(XMIN, XMAX, YMIN, YMAX){
  p<-ggplot() +
    geom_sf(data = streets$osm_lines,
            inherit.aes = FALSE,
            color = "#FFFFFF",
            size = .4,
            alpha = .8) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#F7F7F7",
            size = .2,
            alpha = .6) +
    # geom_sf(data = river$osm_lines,
    #         inherit.aes = FALSE,
    #         color = "#F7F7F7",
    #         size = .2,
    #         alpha = .5) +
    coord_sf(xlim = c(XMIN, XMAX), 
             ylim = c(YMIN, YMAX),
             expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#000000")
    )
  return(p)}

WhitePlot<-function(XMIN, XMAX, YMIN, YMAX){
  p<-ggplot() +
    geom_sf(data = streets$osm_lines,
            inherit.aes = FALSE,
            color = "#000000",
            size = .4,
            alpha = .8) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#545454",
            size = .2,
            alpha = .6) +
    # geom_sf(data = river$osm_lines,
    #         inherit.aes = FALSE,
    #         color = "#F7F7F7",
    #         size = .2,
    #         alpha = .5) +
    coord_sf(xlim = c(XMIN, XMAX), 
             ylim = c(YMIN, YMAX),
             expand = FALSE) +
    theme_void() 
  return(p)}


# library(devtools)
# devtools::install_github("CRAN/sf")
##STL----
CITY<- "St. Louis, Missouri USA"
getbb(CITY)
streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets


# small_streets <- getbb(CITY)%>%
#   opq()%>%
#   add_osm_feature(key = "highway", 
#                   value = c("residential", "living_street",
#                             "unclassified",
#                             "service", "footway")) %>%
#   osmdata_sf()

small_streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential")) %>%
  osmdata_sf()

river <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


#  ggplot() +
#    geom_sf(data = streets$osm_lines,
#            inherit.aes = FALSE,
#            color = "#7fc0ff",
#            size = .4,
#            alpha = .8) +
#    geom_sf(data = small_streets$osm_lines,
#            inherit.aes = FALSE,
#            color = "#ffbe7f",
#            size = .2,
#          alpha = .6) +
#    geom_sf(data = river$osm_lines,
#            inherit.aes = FALSE,
#            color = "#ffbe7f",
#            size = .2,
#            alpha = .5) +
#    coord_sf(xlim = c(-90.375, -90.16), 
#             ylim = c(38.55, 38.75),
#             expand = FALSE) +
#    theme_void() +
#    theme(
#      plot.background = element_rect(fill = "#282828")
#    )
# 
# p_stl<-ggplot() +
#   geom_sf(data = streets$osm_lines,
#           inherit.aes = FALSE,
#           color = "black",
#           size = .4,
#           alpha = .8) +
#   geom_sf(data = small_streets$osm_lines,
#           inherit.aes = FALSE,
#           color = "grey69",
#           size = .2,
#           alpha = .6) +
#   geom_sf(data = river$osm_lines,
#           inherit.aes = FALSE,
#           color = "white",
#           size = .2,
#           alpha = .5) +
#   coord_sf(xlim = c(-90.375, -90.16), 
#            ylim = c(38.55, 38.75),
#            expand = FALSE) +
#   theme_void()
p_stl<-GreyPlot(-90.352, -90.16, 38.57, 38.745)


#################################################
##Chicago----
CITY<- "Chicago, IL USA"
getbb(CITY)
streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential")) %>%
  osmdata_sf()

river <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


p_chi<-GreyPlot(-87.755, -87.545, 41.805, 41.95)
p_chi_black<-BlackPlot(-87.755, -87.545, 41.805, 41.95)

CITY<- "Grasse, FRANCE"
getbb(CITY)
streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential")) %>%
  osmdata_sf()

p_grasse<-BlackPlot(6.885914, 6.988485, 43.613312, 43.698645)


# p_chi<-ggplot() +
#   geom_sf(data = streets$osm_lines,
#           inherit.aes = FALSE,
#           color = "black",
#           size = .4,
#           alpha = .8) +
#   geom_sf(data = small_streets$osm_lines,
#           inherit.aes = FALSE,
#           color = "grey69",
#           size = .2,
#           alpha = .6) +
#   geom_sf(data = river$osm_lines,
#           inherit.aes = FALSE,
#           color = "white",
#           size = .2,
#           alpha = .5) +
#   coord_sf(xlim = c(-87.75, -87.55), 
#            ylim = c(41.8, 41.95),
#            expand = FALSE) +
#   theme_void()

#################################################
##Portsmouth----
CITY<- "Portsmouth, NH USA"
getbb(CITY)
streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                                                         "unclassified",
                                                         "service", "footway")) %>%
  osmdata_sf()

river <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()



p_nh_grey<- GreyPlot(-70.823, -70.72500, 43.01355, 43.12500)
  p_nh_white<-WhitePlot(-70.823, -70.72500, 43.01355, 43.12500)
  
  # ggplot() +
  # geom_sf(data = streets$osm_lines,
  #         inherit.aes = FALSE,
  #         color = "black",
  #         size = .4,
  #         alpha = .8) +
  # geom_sf(data = small_streets$osm_lines,
  #         inherit.aes = FALSE,
  #         color = "grey69",
  #         size = .2,
  #         alpha = .6) +
  # geom_sf(data = river$osm_lines,
  #         inherit.aes = FALSE,
  #         color = "white",
  #         size = .2,
  #         alpha = .5) +
  # coord_sf(xlim = c(-70.823, -70.72793), 
  #          ylim = c(43.01355, 43.09961),
  #          expand = FALSE) +
  # theme_void()

#################################################
##Chicago----
CITY<- "Lake Forest, IL USA"
getbb(CITY)
streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey69",
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(-87.90201, -87.81500), 
           ylim = c(42.19000, 42.29000),
           expand = FALSE) +
  theme_void()






p_lf<-GreyPlot(-87.90201, -87.81500, 42.19000, 42.29000)


CITY<- "Greenville, SC USA"
getbb(CITY)
streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb(CITY)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

p_SC<-GreyPlot(-82.43464, -82.29101, 34.77579, 34.89084)
p_SC_grey<-GreyPlot(-82.43464, -82.29101, 34.77579, 34.89084)
p_SC_white<-WhitePlot(-82.43464, -82.29101, 34.77579, 34.89084)


RES<-600
png("C:/Users/julie.wisch/Documents/Map-%d", width = 4.5*RES, height = 6.75*RES, res = RES)
# 2. Create the plot
p_stl
#p_nh
p_nh_grey
p_nh_white
p_chi
p_chi_black
p_grasse
p_lf
p_SC_grey
p_SC_white
# 3. Close the file
dev.off()

png("C:/Users/julie.wisch/Documents/NHMap-%d", width = 4.5*RES, height = 6.75*RES, res = RES)
p_nh_grey
p_nh_white
dev.off()

