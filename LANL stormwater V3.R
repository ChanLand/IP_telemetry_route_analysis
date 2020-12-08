## V2: trying out some new things with coordinate systems.  
# just don't want to mess up the first version when it's working
## V3: New route table 

library(rgdal)
lanl <- readOGR(dsn = "LANL IP Stormwater GIS data/PLAN_lanlarea_ply.shp") #load LANL boundary shapefile

head(lanl@data, n=2)

plot(lanl, col = "lightblue")

lanl@proj4string 
# CRS arguments:
#+proj=tmerc +lat_0=31 +lon_0=-106.25 +k=0.9999 +x_0=500000.0000000001 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80
#+towgs84=0,0,0 

# We are New Mexico Central (NAD83), UTM 13
# ESPG:2258

# load IP sampler data
isco <- readOGR(dsn = "LANL IP Stormwater GIS data/ip_sma_samplers.shp")
head(isco@data, n=2)

plot(isco)

isco_lanl <- isco[lanl,] # optional clipped data that only includes iscos within the lanl boundary.

# plot isco locations on top of LANL boundary
plot(lanl, col = "lightblue")

points(isco, pch = 19)

# This turned out to be way easier than I thought... 

# The ISCO shapefile contains all samplers, but only a subset have telemetry
# Need to isolate the group of ISCOs that are on telemetry

# Take list of telemetry wells and join with attribute data from isco file
telemetry <- read.csv("LANL IP Stormwater GIS data/telemetry_iscos.csv", stringsAsFactors = FALSE)

# Compare the name column in isco to name column in telemetry to see which rows match
isco$name %in% telemetry$name

# Return rows with do not match
isco$name[!isco$name %in% telemetry$name]

# look at the opposite
telemetry$name[!telemetry$name %in% isco$name] # Looks like none of the repeaters are included in this dataset. shoot.

library(dplyr)
isco@data <- left_join(isco@data, telemetry, by = c('name' = 'name'))
head(isco@data, n=2)

test <- !is.na(isco$rtu_num) # pick out just rows that have RTU numbers - meaning those that have telemetry
# looks good - 83 out of 299 have RTUs

plot(lanl, col = "lightgreen")
points(isco, pch = 1)
points(isco[test,], pch = 19, col = 'red') # Wow, this works!  Hurray! 

# Now need to add repeaters to the shape file
# just the repeaters, including lat/long
repeaters <- data.frame(read.csv("LANL IP Stormwater GIS data/repeater_coords.csv", stringsAsFactors = FALSE))

# Convert repeater coords to State Plane: WHOOOAAAAAA this actually WORKS!!!
rep_latlong <- repeaters[,8:9]
coordinates(rep_latlong) <- ~ x_long+y_lat
proj4string(rep_latlong) <- CRS("+init=epsg:4326")
rep_xy = data.frame(spTransform(rep_latlong, CRS("+init=epsg:2258")))
names(rep_xy) <- c('x_coord', 'y_coord')

# plug in converted values to repeaters dataframe
repeaters$x_coord <- rep_xy$x_coord
repeaters$y_coord <- rep_xy$y_coord

# join repeater data to isco data
#isco@data <- left_join(isco@data, rep_latlong@coords, by = c('x_long' = 'x_long')) # this doesn't work - seems to mess everything up

# path info
paths <- read.csv("LANL IP Stormwater GIS data/route_table_171024.csv", stringsAsFactors = FALSE)

# join path data to isco data
isco@data <- left_join(isco@data, paths, by = c('rtu_num' = 'Node'))


new_isco <- rbind(isco@data, repeaters) # this makes a dataframe - not really what I want... 


# Try just plotting it all in ggplot
library(ggplot2)
library(rgeos)

# Fortify data so that it is in a dataframe that can be read by ggplot
lanl_f <- fortify(lanl)
lanl$id <- row.names(lanl)
lanl_f <- left_join(lanl_f, lanl@data)

#ggplot(lanl_f, aes(long, lat), fill = "Blue")+
#geom_polygon() + coord_equal() +
# labs(x="Easting (m)", y= "Northing (m)")+
#geom_point(data = new_isco[test,], aes(x_long[test], y_lat[test]), col = 'red')


T_isco <- new_isco[!is.na(new_isco$rtu_num),]
test <- T_isco$x_long < 0 & T_isco$y_lat > 20  

T_plot <- ggplot() +
  geom_point(data = T_isco[test,], aes(x_long[test], y_lat[test]), col = 'red', size=2)

# Want to add paths on to the plot above - to do this, will need to create dataframes of coordinates for each node
for (i in 1:(nrow(paths)-1)) { # (-1) so that it doesn't start with the master node - do not need a path here
  tmp <- paths[i+1,]
  test <- !is.na(tmp)
  df <- data.frame(matrix(NA, nrow = sum(test), ncol = 3))
  names(df) <- c('num','x_coord','y_coord')
  node_info <- T_isco[T_isco$rtu_num == tmp$Node,]
  if (nrow(node_info) >1){
    node_info<- node_info[1,]
  }
  df[,1] <- node_info$name
  

  df[1,2] <- node_info$x_coord
  df[1,3] <- node_info$y_coord
  path <- T_isco[T_isco$rtu_num == tmp$Path1,]
  
  if (nrow(path) >1){
    path <- path[1,]
  }
  df[2,2] <- path$x_coord
  df[2,3] <- path$y_coord
  if (nrow(df) >= 3){
    path <- T_isco[T_isco$rtu_num == tmp$Path2,]
    if (nrow(path) >1){
      path <- path[1,]
    }
    df[3,2] <- path$x_coord
    df[3,3] <- path$y_coord
  }
  if (nrow(df) >= 4){
    path <- T_isco[T_isco$rtu_num == tmp$Path3,]
    if (nrow(path) >1){
      path <- path[1,]
    }
    df[4,2] <- path$x_coord
    df[4,3] <- path$y_coord
  }  
  if (nrow(df) >= 5){
    path <- T_isco[T_isco$rtu_num == tmp$Path4,]
    if (nrow(path) >1){
      path <- path[1,]
    }
    df[5,2] <- path$x_coord
    df[5,3] <- path$y_coord
  }

    if (nrow(df) >= 6){
      path <- T_isco[T_isco$rtu_num == tmp$Path5,]
      if (nrow(path) >1){
        path <- path[1,]
      }
      df[6,2] <- path$x_coord
      df[6,3] <- path$y_coord
    }

  ifelse(i==1, df_all <- df, df_all <- rbind(df_all,df)) # save to big dataframe
}

# This plot kind of works!!! Needs some refinement and map layer added behind - also would be good if paths that are used over and over again could be thicker
test <- T_isco$x_long < 0 & T_isco$y_lat > 20  
T_plot <- ggplot() +
  geom_polygon(data = lanl_f, aes(long, lat, group=group))+
  geom_point(data = T_isco[test,], aes(x_coord[test], y_coord[test]), col = 'red', size=2)+
  geom_path(data = df_all, aes(x_coord, y_coord, color = factor(num)))+
  coord_equal()+
  xlab(NULL)+ylab(NULL)



test <- T_isco$x_long < 0 & T_isco$y_lat > 20  
df_all[39,3] <- NA
T_plot2 <- ggplot() +
  geom_point(data = T_isco[test,], aes(x_long[test], y_lat[test]), col = 'red', size=2)+
  geom_path(data = df_all, aes(long, lat, color = factor(num)))

ggplot(lanl_f, aes(long, lat, group = group), col = 'Blue')+
  geom_polygon()


# Now we want a plot that shows how often a repeater gets used
unique(paths$Path1)
unique_path <- unique(paths[,2:6])
rep_count <- data.frame(matrix(NA, nrow=nrow(unique_path), ncol=2))
names(rep_count) <- c('node', 'count')
rep_count[,1] <- unique_path[,1]
for (i in 1:nrow(rep_count)){
  test <- rep_count[i,1] == paths[,2:6]
  rep_count[i,2] <- sum(test==T, na.rm=TRUE)
}

# Need to make a similar loop to the big loop above to pull out the coords associated with each value
for (i in 1:(nrow(rep_count))) { # (-1) so that it doesn't start with the master node - do not need a path here
  tmp <- rep_count[i,]
  df_count <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  names(df_count) <- c('name','x_coord','y_coord', 'count')
  node_info <- T_isco[T_isco$rtu_num == tmp$node,]
  if (nrow(node_info) >1){
    node_info<- node_info[1,]
  }
  df_count[,1] <- node_info$name
  df_count[,4] <- rep_count[i,2]
  df_count[1,2] <- node_info$x_coord
  df_count[1,3] <- node_info$y_coord
  ifelse(i==1, df_count_all <- df_count, df_count_all <- rbind(df_count_all,df_count)) # save to big dataframe
}

test <- T_isco$x_long < 0 & T_isco$y_lat > 20  
size_plot <- ggplot() +
  geom_polygon(data = lanl_f, aes(long, lat, group=group))+
  geom_point(data = T_isco[test,], aes(x_coord[test], y_coord[test]), col = 'red', size=2, pch=1)+
  geom_point(data=df_count_all, aes(x_coord, y_coord, size=count), col='blue')+
  coord_equal()+
  ylab(NULL) + xlab(NULL)

# add paths back in too
test <- T_isco$x_long < 0 & T_isco$y_lat > 20  
ggplot() +
  geom_polygon(data = lanl_f, aes(long, lat, group=group), color='black', fill='gray')+
  geom_point(data = T_isco[test,], aes(x_coord[test], y_coord[test]), col = 'red', size=2, pch=1)+
  geom_path(data = df_all, aes(x_coord, y_coord, color = factor(num)))+
  geom_text(data=df_count_all, aes(x_coord, y_coord, label=name), col='black', vjust=2)+
  geom_text(data=df_count_all, aes(x_coord, y_coord, label=ifelse(name=='HOME'|name=='PMT-02'|name=='LACanyonPENS'|name=='2-MilePENS',as.character(name),'')), col='black', vjust=2)+
  geom_point(data=df_count_all, aes(x_coord, y_coord, size=count), col='DodgerBlue')+
  coord_equal()+
  ylab(NULL) + xlab(NULL)+
  theme(legend.position="none") # removes legends

# Just repeater points and locations
test <- T_isco$x_long < 0 & T_isco$y_lat > 20  
ggplot() +
  geom_polygon(data = lanl_f, aes(long, lat, group=group))+
  geom_point(data = T_isco[test,], aes(x_coord[test], y_coord[test]), col = 'red', size=2, pch=1)+
  geom_text(data=df_count_all, aes(x_coord, y_coord, label=name), col='white', vjust=2)+
  geom_text(data=df_count_all, aes(x_coord, y_coord, label=ifelse(name=='HOME'|name=='PMT-02'|name=='LACanyonPENS'|name=='2-MilePENS',as.character(name),'')), col='black', vjust=2)+
  geom_point(data=df_count_all, aes(x_coord, y_coord, size=count), col='DodgerBlue')+
  coord_equal()+
  ylab(NULL) + xlab(NULL)

# Pull out subsets of data
test <- grepl("W-SMA", T_isco$name, fixed=TRUE)
isco_sub <- T_isco[test,]
test <- isco_sub$x_long < 0 & isco_sub$y_lat > 20  
test2 <- grepl("W-SMA", df_all$num, fixed=TRUE) # find subset of iscos

#test <- grepl("A-SMA", T_isco$name, fixed=TRUE)
#isco_sub <- T_isco[test,]
#test <- isco_sub$x_long < 0 & isco_sub$y_lat > 20  
#test2 <- grepl("A-SMA", df_all$num, fixed=TRUE) # find subset of iscos

ggplot() + 
  geom_polygon(data = lanl_f, aes(long, lat, group=group))+
  geom_point(data = isco_sub[test,], aes(x_coord[test], y_coord[test]), col = 'red', size=2, pch=1)+
  geom_path(data = df_all[test2,], aes(x_coord, y_coord, color = factor(num)))+
  coord_equal()+
  ylab(NULL) + xlab(NULL)

# first, find locations that have path info associated with them (only have a small subset at the moment)
#path_isco <- T_isco[!is.na(T_isco$Path1),]


test <- new_isco$x_long < 0 & new_isco$y_lat > 20
plot(new_isco$x_long[test], new_isco$y_lat[test])
# isco_2 <- merge(isco@data, repeaters) # Nope, doesn't work


# Just telemetry Locations
plot(lanl, col = "lightgreen")
points(isco[test,], pch = 19)



