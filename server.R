#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(shiny)
library(maps)
library(geosphere)
library(xlsx)
library(scales)
library(plyr)  
library(ggplot2)
library(sp)
library(rgeos)
library(reshape2)
library(maptools)
source("code/fort.R")
shinyServer(function(input, output) {
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Functions
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #Converting full name of state/teritories to abbreviations
  getAbbreviation <- function(x) {
    if (x == "Queensland") return("QLD")
    string <- strsplit(x, " ")[[1]]
    if (length(string) == 1) {return(toupper(substring(string, 1,3)))} else
      return(paste(toupper(substring(string, 1,1)), sep="", collapse=""))
  }
  #Select cities at random from a state/territory based on 
  # weighted probability
  getRandomCity <- function(xstate = "SA", xnum=1) {
    allCities <- places.au[places.au$state == xstate,]
    allCities <- allCities[order(allCities$pop),] #sort
    if (nrow(allCities) == 0) {return(data.frame(lon=rep(NA, xnum),
                                                 lat=rep(NA, xnum),
                                                 state=rep("XY", xnum)))}
    selection <- sample(c(1:nrow(allCities)), xnum, replace=TRUE, prob=allCities$pop)
    return(allCities[selection,])
  }
  ##usage: 
  # getRandomCity("SA", 4);
  # gets 4 cities' lat & long from South Australia
  #randomizing rows of a dataframe based on a grouping value
  shuffelRows <- function(xdf, xcol){
    v <- unique(xdf[,xcol])
    x <- sample(v, size=length(v), replace=FALSE)
    u <- merge(data.frame(xid = x), xdf, by.x = "xid",
               by.y = names(xdf[xcol]), all.y = TRUE)
    #   return(u[with(u, order(group, order)),])
    return(u)
  }
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Read and prepare data
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  dataloc <- "data/auInternal.xlsx"
  aumig <- read.xlsx2(dataloc, 1, startRow = 4, endRow = 15, header=FALSE,
                      colIndex = c(1:11),
                      colClasses = c("character", rep("numeric", 10)))
  #drop overseas, not stated and totals
  aumig <- aumig[aumig$X1 != "Overseas" & aumig$X1 != "Total" & aumig$X1 != "Not stated",
                 c(1:10)]
  
  aumig$X1 <- sapply(as.character(aumig$X1), getAbbreviation)
  names(aumig) <- c("SOURCE", aumig$X1)
  #read au shapefile obtained from gadm
  aumap <- readShapeSpatial("data/AUS_adm/AUS_adm1.shp")
  aumap.df <- fortify(aumap)
  places <- read.csv("data/cities1000.csv", header=FALSE, stringsAsFactors=FALSE)
  places <- places[places$V9 == "AU",]
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Data processing
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #melt data
  aumelt <- melt(aumig, c("SOURCE"), names(aumig)[2:10],
                 variable.name = "DESTINATION", value.name = "MOVEMENT")
  #remove non movements
  aumelt <- aumelt[aumelt$SOURCE != aumelt$DESTINATION, ]
  aumelt <- aumelt[aumelt$MOVEMENT > 0,] #Remove 0 movement pattern
  totmov <- sum(aumelt$MOVEMENT) #296983; seems individual lines possible ;)
  aumelt$DESTINATION <- as.character(aumelt$DESTINATION)
  aumelt$MOVEMENT <- round(aumelt$MOVEMENT/10,0)
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Get cities details
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  
  # head(places); tail(places)
  places.df <- data.frame (as.numeric(places$V6), as.numeric(places$V5),
                           places$V9, as.numeric(places$V15), places$V2)
  #rm(places)
  names(places.df) <- c('lon', "lat", "code", "pop", "name")
  places.df <- places.df[,c(1:4)]
  places.df <- places.df[complete.cases(places.df),]
  places.bk <- places.df
  # head(places.df)
  coordinates(places.df) <- c("lon", "lat")
  proj4string(places.df) <- proj4string(aumap)
  # inside <- !is.na(over(places.df, as(aumap.sa, "SpatialPolygons")))
  places.df$state <- over(places.df, aumap)$NAME_1
  # places.ff <- fortify.SpatialPointsDataFrame(places.df)
  places.au <- data.frame(lon = places.df$lon,
                          lat = places.df$lat,
                          state = places.df$state,
                          pop = places.df$pop)
  places.au <- places.au[complete.cases(places.au),]
  # head(places.au)
  # unique(places.au$state)
  places.au$state <- sapply(as.character(places.au$state),
                            getAbbreviation)
  nrow(places.au) #1072
  #summarize places for each available country
  cities.count <- ddply(places.au, c("state"), function(xdf) {
    return(data.frame(count = nrow(xdf)))
  })
  #inflate the table to include individual cases
  aumelt$id <- c(1:nrow(aumelt))
  aumelt.full <- ddply(aumelt, c("id"), function(ydf) {
    if (ydf$MOVEMENT == 1) {data <- ydf} else {
      data <- ydf
      for (i in 2: ydf$MOVEMENT) {
        data <- rbind(data, ydf)
      }
      return(data)
    }
  })
  # tail(m2013.final); head(m2013.final)
  #m2013.final2 <- replaceCities(m2013.final)
  #replace cities for loop
  count <- nrow(aumelt.full)
  head(aumelt.full); tail(aumelt.full)
  aumelt.full <- aumelt.full[,c(-3, -4)]
  aumelt.full <- aumelt.full[order(aumelt.full$SOURCE),]
  str(aumelt.full)
  source.replace <- ddply(aumelt.full, c("SOURCE"), function(xdf) {
    return(data.frame(getRandomCity(xdf$SOURCE[1], nrow(xdf))))
  })
  names(source.replace) <- c("source", "lon.s", "lat.s", "state", "pop")
  # nrow(source.replace) - nrow(aumelt.full) #should be 0 to work :)
  aumelt.full <- cbind(aumelt.full,
                       source.replace[,c("lon.s", "lat.s")])
  #sort according to destination
  aumelt.full <- aumelt.full[order(aumelt.full$DESTINATION),] #sort
  destination.replace <- ddply(aumelt.full, c("DESTINATION"),
                               function(xdf) {
                                 return(data.frame(getRandomCity(xdf$DESTINATION[1], nrow(xdf))))
                               })
  names(destination.replace) <- c("destination", "lon.d", "lat.d",
                                  "state", "pop")
  # nrow(destination.replace) - nrow(aumelt.full) #should be 0 to work :)
  aumelt.full <- cbind(aumelt.full,
                       destination.replace[,c("lon.d", "lat.d")])
  aumelt.full <- aumelt.full[complete.cases(aumelt.full),]
  # aumelt.full <- aumelt.full[order(aumelt.full$pop),]
  # head(aumelt.full)
  
  geosource <- matrix(c(aumelt.full$lon.s,
                        aumelt.full$lat.s), ncol=2)
  geodestination <- matrix(c(aumelt.full$lon.d,
                             aumelt.full$lat.d), ncol=2)
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Plot graph
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  output$mapPlot <- renderPlot({
    #adding curvature without great circle
    # start from geosource and destination source
    arc.nombre <- 50
    locdata <- data.frame(id = 1L:(length(geosource)/2),
                          lons = geosource[,1],
                          lats = geosource[,2],
                          lond = geodestination[,1],
                          latd = geodestination[,2])
    t <- seq(0, 2*pi, length.out = arc.nombre + 2) #2 pi for sine curve
    arc <- ddply(locdata, .(id), function(xdf) {
      lonlist <- seq(xdf$lons, xdf$lond, length.out = arc.nombre + 2)
      latlist <- seq(xdf$lats, xdf$latd, length.out = arc.nombre + 2)
      d <- sqrt((xdf$lons - xdf$lond)^2 + (xdf$lats - xdf$latd)^2)
      latlist <- latlist + (d/8) * sin(t)  #d/8 for sine curve
      return(data.frame(long = lonlist,
                        lat = latlist,
                        order = 1L:(arc.nombre +2),
                        piece = xdf$id,
                        group = xdf$id))
    })
    head(arc)
    arc$sort <- rep(sample(1:max(arc$piece)),
                    each = arc.nombre + 2)
    arc <- arc[order(arc$sort, arc$order), ]
    row.names(arc) <- 1:nrow(arc)
    
    source.couleur <- "green4" #"green4"
    destination.couleur <- "red1" #"red3"
    mid.couleur <- "steelblue4"
    backdrop.couleur <- "grey4" #"azure2" 
    outline.couleur <- "grey0" #"honeydew4"  #"slategrey" #"honeydew4" grey17
    landmass.couleur <- "ivory" #"gray7"
    text.couleur <- "mistyrose" #"mintcream" #"black"
    mapLabel <- "Note: Migration data from United Nations, Department of Economic and Social Affairs, \nPopulation Division (2013). Trends in International Migrant Stock: Migrants by \nDestination and Origin (United Nations database, POP/DB/MIG/Stock/Rev.2013). \nWorld map shapefile from NautralEarth.com. Location and population of cities from geonames.org"
    mapTitle <- "International migration flows based on UN stock migrants data 2013"
    alpha <- 0.04 #0.3 0.2
    size <- 0.003 #0.02 0.01
    
    #plot with added curvature
    gcmap <- ggplot() +
      geom_polygon(aes(long,lat, group=group), 
                   size = 0.02, colour = outline.couleur, fill = landmass.couleur,
                   data=aumap.df) + #state/territories outline
      geom_line(aes(long, lat, group=sort, col=order),
                data=arc, alpha = alpha, size= size) + #drawing great circle lines works .02,.03
      scale_colour_gradient2(high=destination.couleur,
                             low=source.couleur,
                             mid=mid.couleur,
                             midpoint=arc.nombre/2,
                             name="Flow legend",
                             labels = c("Migrant origin", "Migrant destination"),
                             #breaks=seq(min(np.data$Total), max(np.data$Total), by=1000)
                             breaks=c(1, arc.nombre),
                             guide="legend") +
      guides(alpha = "none") +
      theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank()
        #     ,panel.background = element_rect(fill=backdrop.couleur,
        #                                      colour="black")
        ,legend.position = "none"
        ,legend.title = element_text(size = 8, colour = text.couleur)
        ,axis.text.x  = element_blank()
        ,axis.text.y  = element_blank()
        ,axis.ticks  = element_blank()
        ,axis.title  = element_blank()
        ,axis.title  = element_blank()
      ) + xlim(112, 155) + ylim(-45, -7) + coord_equal()
    gcmap
  })
  
  
  
  })