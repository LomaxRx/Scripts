library(RCurl)
library(digest)
library(RJSONIO)

geocode = function(address, client = "google_api_clientID_here",
  signature = "google_api_signature_here") {

	root = "http://maps.googleapis.com"
	
	a = paste(address$address,"+",address$city, "+", address$zip, sep="")
	url = paste("/maps/api/geocode/json?address=",a,"&sensor=false&client=",client,sep="")

	signature = base64Decode(signature)
	signature = hmac(signature, url, "sha1",FALSE,TRUE)
	signature = base64(signature)
	signature = gsub("+", "-", signature, fixed = TRUE)
	signature = gsub("/","_",signature, fixed = TRUE) 
	
	
	u = paste(root, url, "&signature=",signature,sep="")
	return(URLencode(u)) 

}

addCode = function(j, a){
	lat = j$results[[1]]$geometry$location$lat
	lng = j$results[[1]]$geometry$location$lng
	hood = j$results[[1]]$address_components
	a$lat = lat
	a$long = lng
	if(length(hood) >= 3){
		a$hood = hood[[3]]$long_name
    }

    return(a)
 }

gCode = function(address,verbose=FALSE){
	
    address[c("lat","long","hood")] = NA
    OQLs = 0
    errs = 0

	for (i in 1:nrow(address)) {
		thisAddress = address[i, ]     
		thisAddress$address = gsub(" ","+",thisAddress$address)
		thisAddress$address = gsub(",","",thisAddress$address)
		thisAddress$address = gsub("[.]","",thisAddress$address)
		thisAddress$city = gsub(" ","+",thisAddress$city)

		u = geocode(thisAddress)
		son = getURL(u)

		if(isValidJSON(son,asText=TRUE)){
			x = fromJSON(son,simplify=FALSE)
		
			if(x$status=="OK"){
				thisAddress = addCode(x, thisAddress)			
				

			} else if (x$status=="OVER_QUERY_LIMIT") {
			    success = FALSE
			    for (i in 1:3){
			    	OQLs = OQLs + 1
					Sys.sleep(2)
					son = getURL(u)
					
					if(isValidJSON(son,asText=TRUE)){
	            		x = fromJSON(son,simplify=FALSE)
	            		
	            		if(x$status == "OK"){
	            			thisAddress = addCode(x,thisAddress)
	            			success = TRUE
	            			break
	            		}
			   		}
	            } 

	            if (success == FALSE) { 
	            	cat("Geocoding stopped by OVER_QUERY_LIMIT error")
	            	break 
	            }      
			} else { errs = errs + 1 }
		}
    
    	address[i,]$lat = thisAddress$lat
    	address[i,]$long = thisAddress$long
    	address[i,]$hood = thisAddress$hood

    }

	cat("Errors:", errs, "\n")
    cat("OQLs: ", OQLs)
	write.csv(address, "H:/Projects/FY13\ Mapping/GeocodedAdds.csv")
	return(address)

}


