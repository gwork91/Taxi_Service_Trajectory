# R code to extract the start and end points of the rides for the above 3 files generated
	# Load libraries
x<-c("data.table","plyr", "dplyr","sp","splitstackshape","stringr","stringi")
invisible(lapply(x, require , character.only = TRUE))
options(scipen=100, digits=10)
		# Total no. of entries :  awk '{n+=1} END {print n}' train.csv			# 1710671

	# This function will give 2 types of files for different combination of CALL_TYPE and DAY_TYPE : 
	# 1. Unique Start and end points of the trip under the selected category, along with the no. of unique cabs through that coordinate and no. of trips through that coordinate
	# 2. Unique TAXI_ID, and Start/End points combination. This will help in getting all the unique cabs through a single charging station.
	
data_clean <- function(ini_index, fin_index, call_value, day_value){

call_value <- toupper(call_value)
day_value <- toupper(day_value)

file_name <- paste0(call_value,"_",day_value,"_",fin_index,".csv", sep="")

# ini_index <-1
# fin_index <- 10000
# call_value <- 'A'
# day_value <- 'A'
# file_name <- 'A_A_364769.csv'

coord_df <- fread(file_name, sep=',', stringsAsFactor=FALSE, header=FALSE, nrow=fin_index-1, skip= ini_index, colClasses=c('character','character','character','character','character'),na.strings=c("NA","","NULL"))
setnames(coord_df, c("TRIP_ID", "ORIGIN_STAND", "TAXI_ID", "TIMESTAMP", "POLYLINE") )
coord_df <- coord_df[,ORIGIN_STAND:= NULL]

coord_df <- coord_df[!duplicated(coord_df, by="TRIP_ID", fromLast=T)]										# Make selection of the unique TRIP_ID
coord_df <- coord_df[, TRIP_ID := stri_replace_all_fixed( coord_df[, TRIP_ID] , coord_df[, TAXI_ID],"") ]	# Removing the TAXI_ID from TRIP_ID
	# Adding the date and time columns : Not required as of now!
# coord_df <- coord_df[, trip_date := as.Date(sapply(coord_df[,TIMESTAMP], function(x) { as.Date(as.POSIXct(as.numeric(x), origin='1970-01-01')) }),origin='1970-01-01') ] 
# coord_df <- coord_df[, trip_hour := as.character(sapply(coord_df[,TIMESTAMP], function(x) { format(as.POSIXct(as.numeric(x), origin='1970-01-01'), '%H:%M:%S') } )) ] 
coord_df <- coord_df[ , TIMESTAMP:=NULL]
	# adding column with the coordinates
coord_df <- coord_df[, POLYLINE := substring( coord_df[,POLYLINE ], 3) ]
coord_df <- coord_df[, POLYLINE := substr(coord_df[,POLYLINE ],1, nchar(coord_df[,POLYLINE ])-2) ]
coord_df <- coord_df[, coord_leng := (str_count(coord_df[ , POLYLINE], "\\],\\[")+1) ]					# Now get the coord_leng using "],[" in the POLYLINE
coord_df <- coord_df[ coord_leng > 2 ]									# Removing all cases where the length of the trip is NULL, or less than 2 coordinates point
coord_df <- coord_df[ , coord_leng := NULL ]
coord_df <- coord_df[ ,start_point := gsub( "\\],\\[.*$", "", coord_df[ ,POLYLINE] ) ]
coord_df <- coord_df[ ,end_point   := gsub( "(.*)(\\],\\[)", "", coord_df[ ,POLYLINE] ) ]
setnames(coord_df, tolower(names(coord_df)))
setcolorder(coord_df, c("trip_id","taxi_id","polyline","start_point","end_point") )			# Arrange columns
save(x=coord_df, file=paste0(call_value,"_",day_value,"_coord.RData"))
# 4 data frames creation :
	# 1. coord_df : with initial details
	# 2. trip_cab_coord : with TRIP_ID, TAXI_ID, POLYLINE -extended with cSplit + unique lat-lng pair + unique TAXI_ID for each count
	# 3. trip_cab_start : 			# remove all those cases where the start_point value is NA or NULL. Don't select those cases
	# 4. trip_cab_end : 			# remove all those cases where the end_point value is NA or NULL. Don't select those cases

trip_cab_start <- coord_df[,.(trip_id, taxi_id, start_point)]						# For start points of the destination
trip_cab_end <- coord_df[,.(trip_id, taxi_id, end_point)]							# For end points of the destination
rm(coord_df)

		# For start points of the destination
trip_cab_start <- trip_cab_start[!duplicated(trip_cab_start, by=c("trip_id","taxi_id","start_point") )]
		# This DF will help in getting the unique no. of cabs across a single cluster
trip_cab_start_ <- data.table(trip_cab_start %>% group_by(start_point, taxi_id) %>% dplyr::summarize( freq_val =n() ))		
trip_cab_start_ <- trip_cab_start_[ ,lon_list := gsub( "(,)(.*)","", trip_cab_start_[, start_point]) ]
trip_cab_start_ <- trip_cab_start_[ ,lat_list := gsub( "(.*)(,)","", trip_cab_start_[, start_point]) ]
trip_cab_start_ <- trip_cab_start_[ , start_point:=NULL]
write.csv(x=trip_cab_start_, file=paste0(call_value,"_",day_value,"_taxi_start.csv",sep=""), row.names=FALSE)
rm(trip_cab_start_)
		# This DF will have data for unique start point only
trip_cab_start <- data.table(trip_cab_start %>% group_by(start_point) %>% dplyr::summarize(unique_cab = length(unique(taxi_id)), freq_val =n() ))
trip_cab_start <- trip_cab_start[ ,lon_list := gsub( "(,)(.*)","", trip_cab_start[, start_point]) ]
trip_cab_start <- trip_cab_start[ ,lat_list := gsub( "(.*)(,)","", trip_cab_start[, start_point]) ]
trip_cab_start <- trip_cab_start[ , start_point:=NULL]
write.csv(x=trip_cab_start, file=paste0(call_value,"_",day_value,"_start.csv",sep=""), row.names=FALSE)
rm(trip_cab_start)

	# For end points of the destination
trip_cab_end <- trip_cab_end[!duplicated(trip_cab_end, by=c("trip_id","taxi_id","end_point") )]
		# This DF will help in getting the unique no. of cabs across a single cluster
trip_cab_end_ <- data.table(trip_cab_end %>% group_by(end_point, taxi_id) %>% dplyr::summarize( freq_val =n() ))		
trip_cab_end_ <- trip_cab_end_[ ,lon_list := gsub( "(,)(.*)","", trip_cab_end_[, end_point]) ]
trip_cab_end_ <- trip_cab_end_[ ,lat_list := gsub( "(.*)(,)","", trip_cab_end_[, end_point]) ]
trip_cab_end_ <- trip_cab_end_[ , end_point:=NULL]
write.csv(x=trip_cab_end_, file=paste0(call_value,"_",day_value,"_taxi_end.csv",sep=""), row.names=FALSE)
rm(trip_cab_end_)
		# This DF will have data for unique end point only
trip_cab_end <- data.table(trip_cab_end %>% group_by(end_point) %>% dplyr::summarize(unique_cab = length(unique(taxi_id)), freq_val =n() ))
trip_cab_end <- trip_cab_end[ ,lon_list := gsub( "(,)(.*)","", trip_cab_end[, end_point]) ]
trip_cab_end <- trip_cab_end[ ,lat_list := gsub( "(.*)(,)","", trip_cab_end[, end_point]) ]
trip_cab_end <- trip_cab_end[ , end_point:=NULL]
write.csv(x=trip_cab_end, file=paste0(call_value,"_",day_value,"_end.csv",sep=""), row.names=FALSE)
rm(trip_cab_end)

# Names in order :  trip_cab_end : unique_cab freq_val  lon_list  lat_list; trip_cab_end_ : taxi_id freq_val lon_list  lat_list

}


# Pass your preferred CALL_TYPE and DAY_TYPE
data_clean(1, 364769, 'A', 'A' )	
# A_A_start.csv					296857					# A_A_end.csv					306657
# A_A_taxi_start.csv			358057					# A_A_taxi_end.csv				361083

data_clean(1, 817878, 'B', 'A' )
# B_A_start.csv					134582					# B_A_end.csv					678747
# B_A_taxi_start.csv			649743					# B_A_taxi_end.csv				805722

data_clean(1, 528013, 'C', 'A' )
# C_A_start.csv					359994					# C_A_end.csv					438274
# C_A_taxi_start.csv			481869					# C_A_taxi_end.csv				492859


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


# Non-optimized code :
# coord_df <- coord_df[, coord_list := sapply(coord_df[,POLYLINE], function(x){			
#									x <-  substring(x, 3)					# remove the 1st and last element
#									x <- substr(x,1, nchar(x)-2)
#									strsplit(x, '\\],\\[')					# list of coordinates
#									}) ]
# coord_df <- coord_df[ , POLYLINE:=NULL]			# removing the initial column with coordinates list
# coord_df <- coord_df[, coord_leng := as.numeric(sapply(coord_df[, coord_list], length)) ]		# putting the coordinates length

# coord_df <- coord_df[ ,start_point := as.character(sapply(coord_df[, coord_list] , '[[',1)) ]	# Destination Starting point # Lat-Lng stored together : Example : -8.618643,41.141412
# end_val <- function(list_vec,leng){  return(as.character(unlist(list_vec)[leng])) }
# coord_df <- coord_df[ ,end_point := as.character(mapply(end_val, coord_df[, coord_list], coord_df[, coord_leng] )) ]		# Destination Ending point
	# Creating the list for lon and lat
# lon_list_func <- function( list_vec ){ list(t(as.numeric(as.character(data.frame(t(matrix( unlist(strsplit(unlist(list_vec), split = ",")), ncol = length(unlist(list_vec)), nrow = 2)))[,1]))))  }
# lat_list_func <- function( list_vec ){ list(t(as.numeric(as.character(data.frame(t(matrix( unlist(strsplit(unlist(list_vec), split = ",")), ncol = length(unlist(list_vec)), nrow = 2)))[,2]))))  }
# coord_df <- coord_df[ ,lon_list := sapply( coord_df[, coord_list], lon_list_func ) ]
# coord_df <- coord_df[ ,lat_list := sapply( coord_df[, coord_list], lat_list_func ) ]

