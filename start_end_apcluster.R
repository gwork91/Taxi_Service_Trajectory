# AP-Clustering is done here, and some beautiful maps have been plotted as well!

x<-c("sp","apcluster","leaflet")
invisible(lapply(x, require , character.only = TRUE))
options(scipen=100, digits=10)

setwd("D:\\Personal\\D_Drive\\Palash\\Ather\\start_end_files")

	# File with lat-lng, unique_cab, freq_val 
ba_start <- read.csv("B_A_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ba_start) <- c("unique_cab","freq_val","lon","lat")
ba_start <- data.frame(ba_start)
ba_start_ <- ba_start[, c('lon','lat')]
ba_start_ <- na.omit(ba_start_)						# removing if any entry has NA values to it
max_lat <- mean(ba_start_$lat)+10*sd(ba_start_$lat)
min_lat <- mean(ba_start_$lat)-10*sd(ba_start_$lat)
ba_start_ <- ba_start_[ba_start_$lat <= max_lat,] 			# There were 5 outliers in this dataset
ba_start_ <- ba_start_[ba_start_$lat >= min_lat,]
ba_start_ <- as.matrix(ba_start_)
# ba_start__ <- ba_start_
# n_row <- 1000
n_row <- 20000
ba_start__ <- ba_start_[1:n_row,]			# Set a varying number here

	# Matrix to form the similarity matrix, either of the full given matrix or a subset
spdist_func <- function(x, sel=NA){
if(any(is.na(sel)))
	s <- spDists(x,x, longlat=TRUE)									# specify longlat=TRUE for earth distance
else
	s <- spDists(x,x[sel,], longlat=TRUE)
as(s, "matrix")
}

# Different apclusters ran : 
# Assign the current apcluster to apres10 here : :D
apres1 <- apclusterL( s=spdist_func, ba_start__ , frac=0.1, sweeps=5, q=0.9)
apres10 <- apres1	# Done for other values as well
#apres2 <- apclusterL( s=spdist_func, ba_start__ , frac=0.1, sweeps=10, q=0.9)
#apres5 <- apclusterL( s=spdist_func, ba_start__ , frac=0.1, sweeps=5, q=0.9)
#apres10 <- apclusterL( s=spdist_func, ba_start__ , frac=0.1, sweeps=10, q=0.9)
#apres2 <- apclusterL( s=spdist_func, ba_start__ , frac=0.15, sweeps=10, q=0.99)		: this gave only 1 cluster
#apres2 <- apclusterL( s=spdist_func, ba_start__ , frac=0.3, sweeps=10, q=0.97)	
#apres2 <- apclusterL( s=spdist_func, ba_start__ , frac=0.02, sweeps=10, q=0.94)

	# For size of 20,000 : Done for following : These were run separately and were allotted to apres10 variable everytime!
apres559_2 <- apclusterL( s=spdist_func, ba_start__ , frac=0.05, sweeps=5, q=0.93)
apres10 <- apres559_2
apres51094 <- apclusterL( s=spdist_func, ba_start__ , frac=0.05, sweeps=10, q=0.94)
apres10 <- apres51094
apres51098 <- apclusterL( s=spdist_func, ba_start__ , frac=0.05, sweeps=10, q=0.98)
apres10 <- apres51098

	# All points along with their cluster labels
index_cluster <- data.frame(indexes = matrix(unlist(apres10@clusters), nrow = n_row, byrow = T), cluster_label= rep(1:length(apres10@clusters),  sapply(apres10@clusters, length)))
	# All points/clusters which have more than '10' points in them
index_cluster <- index_cluster[index_cluster$cluster_label %in% names(table(index_cluster$cluster_label))[table(index_cluster$cluster_label)>10],]
index_cluster <- index_cluster[with(index_cluster, order(indexes)), ]
rownames(index_cluster) <- NULL
index_cluster$cluster_label <- as.character(index_cluster$cluster_label)
uni_labels <- unique(index_cluster$cluster_label)

ba_start__ <- data.frame(ba_start__)
ba_start__$indexes <- seq(1, dim(ba_start__)[1], 1)
ba_start__ <- merge(x= ba_start__, y=index_cluster, by="indexes")
ba_start__<- ba_start__[,c('lat','lon','indexes','cluster_label')]
# DON'T DO ROWNAMES NULL ON THIS DF
		
		# This is DF of Exemplars
exemplar_df <- data.frame(exemp_ind = apres10@exemplars, exemp_label = 1:length(apres10@exemplars) )
exemplar_df$exemp_label <- as.character(exemplar_df$exemp_label)
exemplar_df <- exemplar_df[exemplar_df$exemp_label %in% uni_labels, ]
rownames(exemplar_df) <- NULL
exemplar_df$lon <- ba_start__[ba_start__$indexes %in% exemplar_df$exemp_ind,'lon']
exemplar_df$lat <- ba_start__[ba_start__$indexes %in% exemplar_df$exemp_ind,'lat']
	# File with the lat-lng, TAXI_ID, freq_val in them. Using this to find unique TAXI_ID for all the lat-lng pairs present within a cluster
ba_taxi_start <- read.csv("B_A_taxi_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ba_taxi_start) <- c("taxi_id","freq_val","lon_list","lat_list") 
exemplar_df$exemp_marker <- sapply(exemplar_df$exemp_label, function(x){ 
										uni_lat_lon <- ba_start__[ ba_start__$cluster_label==x , c('lat','lon')]
										taxi_count <- length(unique(ba_taxi_start$taxi_id[(ba_taxi_start$lat %in% uni_lat_lon$lat) & (ba_taxi_start$lon %in% uni_lat_lon$lon) ]))
										taxi_count	} )
exemplar_df$exemp_marker <- paste0("Unique TAXIs passing : ",exemplar_df$exemp_marker, sep="")			# column with labels : No. unique cab values in it

# Plotting the values on the map :
df <- sp::SpatialPointsDataFrame( cbind(ba_start__$lon , ba_start__$lat), data.frame(type=factor(ba_start__$cluster_label)))  
		# I have made colorFactor as a varying length element for colors
		# By default, the 'domain' values are assigned the first and last entry of the colors : blue/green/white/red, if the length(domain)<length(color)
pal <- colorFactor(c("blue","green","white","red"), domain = unique(ba_start__$cluster_label))
	# Benefit of layers! Entries from another DF could also be used here 
leaflet(df) %>% addTiles() %>%
	addMarkers(~exemplar_df$lon, ~exemplar_df$lat, popup = ~as.character(exemplar_df$exemp_marker), label= ~as.character(exemplar_df$exemp_marker), labelOptions = labelOptions(noHide = T) ) %>%
	addCircleMarkers( radius = 4, color = ~pal(type),	stroke = FALSE, fillOpacity = 0.5 , label= ~as.character(paste0(ba_start__$lat,"_", ba_start__$lon)) )
	
# To put new colors to the exemplar markers : Change the 'markerColor' here :
# icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'white', iconColor = 'black')
# icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', prefix='ion')
# leaflet() %>% addTiles() %>%
 #   addAwesomeMarkers(
 #       lng=-118.456554, lat=34.078039,
 #       label='This is a label',
 #       icon = icon.ion)
	

{ 'Files read command' : 'A_A_start.csv' .. 'C_A_end.csv'
aa_start <- read.csv("A_A_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(aa_start) <- c("unique_cab","freq_val","lon","lat")
ba_start <- read.csv("B_A_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ba_start) <- c("unique_cab","freq_val","lon","lat")
ca_start <- read.csv("C_A_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ca_start) <- c("unique_cab","freq_val","lon","lat")
aa_end <- read.csv("A_A_end.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(aa_end) <- c("unique_cab","freq_val","lon","lat")
ba_end <- read.csv("B_A_end.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ba_end) <- c("unique_cab","freq_val","lon","lat")
ca_end <- read.csv("C_A_end.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ca_end) <- c("unique_cab","freq_val","lon","lat")
}

{ 'Files read command' : 'A_A_taxi_start.csv' .. 'C_A_taxi_end.csv'
aa_taxi_start <- read.csv("A_A_taxi_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(aa_taxi_start) <- c("taxi_id","freq_val","lon_list","lat_list")
ba_taxi_start <- read.csv("B_A_taxi_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ba_taxi_start) <- c("taxi_id","freq_val","lon_list","lat_list")
ca_taxi_start <- read.csv("C_A_taxi_start.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ca_taxi_start) <- c("taxi_id","freq_val","lon_list","lat_list")
aa_taxi_end <- read.csv("A_A_taxi_end.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(aa_taxi_end) <- c("taxi_id","freq_val","lon_list","lat_list")
ba_taxi_end <- read.csv("B_A_taxi_end.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ba_taxi_end) <- c("taxi_id","freq_val","lon_list","lat_list")
ca_taxi_end <- read.csv("C_A_taxi_end.csv", header=TRUE, stringsAsFactor=FALSE, sep=',')
names(ca_taxi_end) <- c("taxi_id","freq_val","lon_list","lat_list")
}


	# Install leaflet package's new version. before doing this, remove the earlier version folder from the R/3.2.3/library folder, as this creates error
# Function required : 'makeAwesomeIcon' : to change the color of the markers 
devtools::install_github('bhaskarvk/leaflet')
library(leaflet)

	# AP-clustering
# This command is wrong, as only frac value is required, as it performs the 'sel' (which is a random value selection of 0.1 fraction out of total size) by itself on the overall data
# apclusterL( s=spdist_func, sel=c(40,50,60,80,100,200,300) ,x=ba_start__ , frac=0.1, sweeps=5, q=0.1)
