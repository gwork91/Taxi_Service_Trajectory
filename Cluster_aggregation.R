# This approach uses Heatmaps implementation to aggregate the coordinates so as to reduce the dataset size

x<-c("sp","rgdal", "raster","rgeos","spatstat","maptools")
invisible(lapply(x, require , character.only = TRUE))
options(scipen=100, digits=10)
setwd("D:\\Personal\\D_Drive\\Palash\\Ather\\start_end_files")

# Packages use :
# sp : loads S4 spatial objects
# spatstat : point density field calculation, contour plot
# maptools : points to point pattern conversion 

	# Loading the start points of CALL_TYPE=’B’ and DAY_TYPE=’A’
ba_start <- read.csv("B_A_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ba_start) <- c("unique_cab","freq_val","lon","lat")
ba_start_ <- data.frame(ba_start)								# Matrix to DF
ba_start_ <- na.omit(ba_start_)									# Removing if any NA entries
		# Outlier removal!! Do a manual check as well. Helps in plotting
max_lat <- mean(ba_start_$lat)+10*sd(ba_start_$lat)			
min_lat <- mean(ba_start_$lat)-10*sd(ba_start_$lat)
ba_start_ <- ba_start_[ba_start_$lat <= max_lat,] 			# There were 5 entries in this dataset
ba_start_ <- ba_start_[ba_start_$lat >= min_lat,] 
ba_start__ <- ba_start_[ ,c("lon","lat")]					# selecting only lon-lat columns for SpatialPointsDataFrame input
		# Plotting the above coordinates
ba_start_spdf <- SpatialPointsDataFrame(coords = ba_start__, data = ba_start_, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(ba_start_spdf, pch=1, cex=0.5)
	
	# Convert points to pp class
sp_obj <- as(SpatialPoints(ba_start_spdf), "ppp") 
dense_obj <- density(sp_obj, adjust = 0.2)  			# create density object of class 'im' (Image)
plot(dense_obj)

	# Density information via contour plots
contour(density(sp_obj, adjust = 0.15), nlevels = 4) 		# adjust is used for smoothing bandwidth. Higher the value for 'adjust' -> more zoomed in

do_sgdf <- as(dense_obj, "SpatialGridDataFrame")  			# density object to spatialGridDF conversion
im_sgdf <- as.image.SpatialGridDataFrame(do_sgdf)  			# image conversion
con_lines <- contourLines(im_sgdf, nlevels = 9)  			# contour creation
con_sldf <- ContourLines2SLDF(con_lines, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))  # SpatialLinesDataFrame conversion
plot(con_sldf, col = terrain.colors(8))

multi_cluster <- gPolygonize(con_sldf[5,])
garea_mc <- gArea(multi_cluster, byid = T)/10000
multi_cluster <- SpatialPolygonsDataFrame(multi_cluster, data = data.frame(garea_mc), match.ID = F)
plot(multi_cluster)

ba_aggr <- aggregate(ba_start_spdf, by = multi_cluster, FUN = length)			# This will aggregate the spatial object
#  head(ba_aggr)#  unique_cab freq_val  lon  lat
#1  		     	1236     1236 	1236 1236
#2       			3097     3097 	3097 3097

plot(dense_obj, main = "")
plot(con_sldf, col = terrain.colors(8), add = T)
plot(ba_aggr, col = "red", border = "white", add = T)
graphics::text(coordinates(ba_aggr) + 1000, labels = ba_aggr$unique_cab)

	# Within and outside coordinates
coord_in <- ba_start_spdf[ba_aggr, ]  												# No. of points inside the clusters
coord_out <- ba_start_spdf[!row.names(ba_start_spdf) %in% row.names(coord_in), ]  		# No. of points outside the clusters
plot(coord_out, pch=1, cex=0.5)														# Sparse distribution of coordinates (or low density)
plot(ba_aggr, border = "red", lwd = 3, add = T)

nrow(coord_in)/nrow(ba_start_spdf)	
# 0.52497


# 'spatstat' package installation!!
# One needs to have R >=3.2.2 for spatstat. To perform that :
# library(installr)
# updateR()
# -> This shud do the rest with RStudio only, if it doesn’t it might show an error that file R-3.2.3 path not found
# Go and select the latest R version which has been installed as the default choice for RStudio:
# Tools -> Global Options -> General -> R version : Change the path to newly installed version

# I installed the library spatstat by using .tar.gz file : Downloaded it to Ather folder
install.packages('D:\\Personal\\D_Drive\\Palash\\Ather\\spatstat_1.44-1.tar', repos=NULL, type='source')

# Now, as the folder location has changed, so you have to copy the libraries installed from the previous version’s library folder to this version’s library folder. 
# This is pretty easy in R, and one has to just copy the folders and all the packages which will be installed in the updated version. I did it using Beyond compare -> Sync Folders.


