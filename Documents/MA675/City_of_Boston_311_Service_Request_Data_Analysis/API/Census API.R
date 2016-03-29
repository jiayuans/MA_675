library(rjson)
library(plyr) 

# Didn't quite need this actually; was more for reference
key = "d83c7dae3c4a942529710984851840dc600c291e"

# Reads in from the JSON formatted URL. This request is set to get P0010001 and P0030001 (which are both total population) for every tract where state = 25 (MA) and county = 025 (Suffolk)
TractData <- fromJSON(file=url("http://api.census.gov/data/2010/sf1?get=P0010001,P0030002,P0030003,P0030004,P0030005,P0030006,P0030007,P0030008,P0040003,P0100001,P0120002,P0120026,P0130001&for=tract:*&in=state:25+county:025&key=d83c7dae3c4a942529710984851840dc600c291e"))
TractData2 <- ldply(TractData)[-1,]                
names(TractData2)<-ldply(TractData)[1,]
head(TractData2) 
 
