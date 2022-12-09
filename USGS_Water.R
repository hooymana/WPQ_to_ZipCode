rm(list = ls())

library(dataRetrieval)
library(zipcodeR)

#Sites with tutorials for WPQ API
#https://www.waterqualitydata.us/webservices_documentation/
#https://waterdata.usgs.gov/blog/dataretrieval/

#PUT YOUR DATA HERE
# setwd("C:/Users/Andrew Hooyman/Documents/ADIallzip")
# myzip=read.csv("MyZip.csv")

#Algorithm to add 0 to zip codes with 4 digits
for(i in 1:dim(myzip)[1]){
  if(nchar(myzip$zip5[i])<5){
    myzip$zip5[i]=paste("0",myzip$zip5[i],sep="")
  }
}


#register API key with google cloud/geocode
#Register with a google cloud account to get key: https://cloud.google.com/maps-platform/
#register_google(key = "YOUR KEY")

#Get longitudinal and latitude data
for(i in 1:dim(myzip)[1]){
  #Get data from zipcodeR
  z=zipcodeR::reverse_zipcode(myzip$zip5[i])
  myzip$COUNTY[i]=z$county
  myzip$lat[i]=z$lat
  #Get latitude from geocoder
  #myzip$latg[i]=geocode(myzip$zip5[i])[2]
  myzip$lng[i]=z$lng
  #Get longitude from geocoder
  #myzip$long[i]=geocode(myzip$zip5[i])[1]
  myzip$medianincome[i]=z$median_household_income
  myzip$medianhouse[i]=z$median_home_value
  myzip$popdense[i]=z$population_density
  
  print(i)
}

#Add lead data from USGS
myzip$lead=NA
for(i in 1:dim(myzip)[1]){
  #tryCatch allows us to continue thorough the loop in the event of an error in the readWQPdata function
  tryCatch({
  if(!is.na(myzip$lat[i])){
  leaddata=readWQPdata(
    #statecode = c("MI"),
    #Find water quality measures within 80 decimal miles of the zip code latitude and longitude
  lat=myzip$lat[i], long=myzip$lng[i], within = 80,
  #Fine lead measurements specifically
  characteristicName = c("Lead"),
  #All measures starting from 2020
  startDateLo = "2020-01-01",
  convertType = TRUE
  #Sends query to waterqualitydata.us
  #Example query: https://www.waterqualitydata.us/data/Result/search?lat=26.3&long=-98.53&within=80&characteristicName=Lead&startDateLo=01-01-2020&zip=yes&mimeType=tsv

)
  #Remove missing values, values labeled ND, and values measured in mg/l
  leaddata=leaddata[!is.na(leaddata$ResultMeasureValue) & leaddata$ResultMeasureValue!="ND" &leaddata$ResultMeasure.MeasureUnitCode!="mg/l",]
  #convert to numeric
  leaddata$ResultMeasureValue=as.numeric(leaddata$ResultMeasureValue)
  #Get mean of lead values
  myzip$lead[i]=mean(leaddata$ResultMeasureValue)
  }
  },error=function(e){}
)
  #Track progress
  print(i)
}

sum(!is.na(myzip$lead))
hist(log(myzip$lead),breaks = 30)
hist(log(myzip$medianincome),breaks = 30)
hist(log(myzip$medianhouse),breaks = 30)
hist(log(myzip$popdense),breaks = 30)


ggplot(myzip,aes(y=log(lead),x=log(medianincome)))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(myzip,aes(y=log(lead),x=log(medianhouse)))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(myzip,aes(y=log(lead),x=log(popdense)))+
  geom_point()+
  geom_smooth(method = 'lm')

summary(lm(log(lead)~log(medianincome),myzip))

inc.agg=aggregate(medianincome~state,myzip,mean)
lead.agg=aggregate(lead~state,myzip,mean)

inclead=merge(inc.agg,lead.agg,by="state")

inclead$state=factor(inclead$state,levels = inclead$state[order(inclead$medianincome)])

coeff=800

ggplot(inclead,aes(x=state))+
  geom_col(aes(y=medianincome),fill="dodgerblue1",width = .5)+
  geom_line(aes(y=lead*coeff,group=1),size=2,color="darkorange1")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Median Household Income (USD)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Lead (ug/l)")
  )+
  xlab("US State")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=.5))


