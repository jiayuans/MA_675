key<-"c1cb853cab5d1f080da0e055fa247d4118a310eb"
getCensusApi <- function(data_url,key, vars, region, numeric=TRUE){
  if(length(vars)>50){
    vars <- vecToChunk(vars) # Split vars into a list
    get <- lapply(vars, function(x) paste(x, sep='', collapse=","))
    data <- lapply(vars, function(x) getCensusApi2(data_url,key, x, region, numeric=TRUE))
  } else {
    get <- paste(vars, sep='', collapse=',')
    data <- list(getCensusApi2(data_url,key, get, region, numeric=TRUE))
  }
  # Format output.  If there were no errors, than paste the data together
  # If there is an error, just return the unformatted list.
  if(all(sapply(data, is.data.frame))){
    colnames <- unlist(lapply(data, names))
    data <- do.call(cbind,data)
    names(data) <- colnames
    # Prettify the output
    # If there are nonunique colums, remove them
    data <- data[,unique(colnames, fromLast=TRUE)]
    # Reorder columns so that numeric fields follow non-numeric fields
    data <- data[,c(which(sapply(data, class)!='numeric'), which(sapply(data, class)=='numeric'))]
    return(data)
  }else{
    print('unable to create single data.frame in getCensusApi')
    return(data)
  }
}
getCensusApi2 <- function(data_url,key, get, region, numeric=TRUE){
  if(length(get)>1) get <- paste(get, collapse=',', sep='')
  api_call <- paste(data_url, 
                    'key=', key, 
                    '&get=', get,
                    '&', region,
                    sep='')
  
  dat_raw <- try(readLines(api_call, warn="F"))
  if(class(dat_raw)=='try-error') {
    print(api_call)
    return}
  dat_df <- data.frame()
  
  #split the datastream into a list with each row as an element
  # Thanks to roodmichael on github
  tmp <- strsplit(gsub("[^[:alnum:], _]", '', dat_raw), "\\,")
  #dat_df <- rbind(dat_df, t(sapply(tmp, '[')))
  #names(dat_df) <- sapply(dat_df[1,], as.character)
  #dat_df <- dat_df[-1,]
  dat_df <- as.data.frame(do.call(rbind, tmp[-1]), stringsAsFactors=FALSE)
  names(dat_df) <- tmp[[1]]
  # convert to numeric
  # The fips should stay as character... so how to distinguish fips from data?
  # I think all of the data have numbers in the names, the fips do not
  #  Example: field names of B01001_001E vs state
  if(numeric==TRUE){
    value_cols <- grep("[0-9]", names(dat_df), value=TRUE)
    for(col in value_cols) dat_df[,col] <- as.numeric(as.character(dat_df[,col]))
  }
  return(dat_df)
}

sf1_2010_api <- 'http://api.census.gov/data/2010/sf1?'

# get the population_age_sex with zip code
vars <- c('P0010001', paste('P0120', sprintf('%03i', seq(1, 9)), sep=''), paste('P0120', sprintf('%03i', seq(10, 49)), sep=''))
zip_code <- getCensusApi(sf1_2010_api, key=key, vars=vars, region="for=zip+code+tabulation+area:*&in=state:25")

#get the population_age_sex with tract
# Set up the tract level API call
region_tract <- 'for=tract:*&in=state:25+county:025'
tract_df <- getCensusApi(sf1_2010_api, key=key, vars=vars, region=region_tract)

#get the population_age_sex with tract and block group
tract_list <- tract_df$tract
block_group <- NULL
for(t in tract_list){# For each tract
  #Construct the regions part of the API Call
  region = paste("for=block+group:*&in=state:25+county:025+tract:", t, sep='')
  # Pull data
  temp.df <- getCensusApi(sf1_2010_api, key=key, vars=vars, region=region)
  block_group <- rbind(block_group, temp.df)
}
rm(region,temp.df)


head(block_group)
head(tract_df)
