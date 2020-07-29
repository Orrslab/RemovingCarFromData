#Date I need to start with: "Allthetags" and ""
# I will continue working on the data I made in the first script - 
#data I need to start with:
cat("\014")
#rm(AllDates)

#ListOfStart
#Allthetags

#Order it:
rm(All_Data,AllthetagsDet,d)
head(AllthetagsLoc)
AllthetagsLoc<-AllthetagsLoc[order(AllthetagsLoc$TAG,AllthetagsLoc$Location_Time),]

#Now I need to remove all the data before the lapwings were tagged (driving in the car),
# I need to subset ListOfStart to just tag, date_capture and start_hour:

TagStart <- ListOfStart[c("TAG", "date_capture", "start_hour")]

head(AllthetagsLoc)
str(AllthetagsLoc$DateLocation)
# I need to combine the date_capture and start_hour to one row and make it as time (DateTime POSIXct, format: "2020-03-31 06:05:10")

# I will create a list of the first time 

TagStart$DateLocation <- paste(TagStart$date_capture, TagStart$start_hour, sep = " ")
TagStart$DateLocation<- as.POSIXct(TagStart$DateLocation, format="%d/%m/%Y %H:%M:%S", tz="UTC")
str(TagStart) #Good


# Now i need to create a if function, that if the TAG column are the same, remove the data from
# that is smaller them DateTime

Lisoftags
FirstCleaning <- list()

for (i in Lisoftags){
  d <- subset(TagStart , TAG == i)
  tag <- subset(AllthetagsLoc, TAG == i)
  DF <- tag[!(as.POSIXlt(tag$DateLocation)< as.POSIXlt(d$DateLocation)),]
  FirstCleaning[[i]] <- DF
  print(head(DF,1))}
rm(tag,d,DF)


FirstTimeTaged <- do.call(rbind.data.frame, FirstCleaning)
rm(FirstCleaning,AllthetagsLoc)