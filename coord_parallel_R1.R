# R code to extract all the lat-lng points of the trip : 
	# For selected CALL_TYPE and DAY_TYPE it returns the "unique_cab","freq_val","lon_list","lat_list"
	# Done this for CALL_TYPE ='A' and DAY_TYPE='A' only, as too much data size

x<-c("data.table","plyr", "dplyr","sp","splitstackshape","stringr","stringi")
invisible(lapply(x, require , character.only = TRUE))
options(scipen=100, digits=10)
	
	# This will generate file for the selected CALL_TYPE and DAY_TYPE category : 
	# 1. All the unique coordinates by which the cabs under this category have passed, along with the no. of unique cabs for each coordinate and no. of trips through that coordinate
	
data_clean_full <- function(ini_index, fin_index, call_value, day_value){

# ini_index <-1
# fin_index <- 100
# call_value <- 'B'
# day_value <- 'A'

call_value <- toupper(call_value)
day_value <- toupper(day_value)

# file_name <- paste0(call_value,"_",day_value,"_",fin_index,".csv", sep="")

if(call_value=='B'){file_name <- 'B_A_817878.csv'}
else if(call_value=='A'){file_name <- 'A_A_364769.csv'}
else{ file_name <- 'C_A_528013.csv'}

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
coord_df <- coord_df[,.(TRIP_ID, TAXI_ID, POLYLINE)]			# Arrange columns

# This part is expansion of the routes : pick this later!!
coord_df <- coord_df[,POLYLINE := as.character(POLYLINE) ]				# it is factor
coord_df <- cSplit(coord_df, 'POLYLINE','],[', 'long')									# Now splitting the whole coord_df
coord_df <- coord_df[,POLYLINE := as.character(POLYLINE) ]				# it is factor
coord_df <- coord_df[!duplicated(coord_df, by=c("TRIP_ID","TAXI_ID","POLYLINE") )]				# only the unique entries for all the 3 columns
		# unique values of POLYLINE with unique count of cabs for that pair, and also the number of entries for 
coord_df <- data.table(coord_df %>% group_by(POLYLINE) %>% dplyr::summarize(unique_cab = length(unique(TAXI_ID)), freq_val =n() ))
coord_df <- coord_df[ ,lon_list := gsub( "(,)(.*)","", coord_df[, POLYLINE]) ]
coord_df <- coord_df[ ,lat_list := gsub( "(.*)(,)","", coord_df[, POLYLINE]) ]
coord_df <- coord_df[ , POLYLINE:=NULL]

write.csv(x=coord_df, file=paste0(call_value,"_",day_value,"_coord_df.csv",sep=""), row.names=FALSE)

}


data_clean_full(1, 364769, 'A', 'A' )	
data_clean_full(1, 817878, 'B', 'A' )
data_clean_full(1, 528013, 'C', 'A' )


tar -C /home/dsuser/palash/code/coord/ -czf /home/dsuser/palash/code/coord/aa_coord_df.tar.gz A_A_coord_df.csv
setwd("D:\\Personal\\D_Drive\\Palash\\Ather\\start_end_files")