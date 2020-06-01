
Domo <- setRefClass("Domo",
	fields=list(client_id='ANY',secret='ANY',domain='ANY',scope='ANY',access_time='POSIXct',api_env='ANY'),
	methods=list(
		initialize=function(client_id=NA,secret=NA,domain=NA,scope=c('data'),access_time=as.POSIXct(NA)){
			if( is.na(client_id) & Sys.getenv("RDOMOAPI_CLIENTID") != '' ){
				client_id <<- Sys.getenv("RDOMOAPI_CLIENTID")
			}else if( !is.na(client_id) ) {
				client_id <<- client_id
			}else{
				stop('No Client ID Provided')
			}

			if( is.na(secret) & Sys.getenv("RDOMOAPI_SECRET") != '' ){
				secret <<- Sys.getenv("RDOMOAPI_SECRET")
			}else if( !is.na(secret) ){
				secret <<- secret
			}else{
				stop('No Secret Provided')
			}

			if( is.na(domain) & Sys.getenv('RDOMOAPI_DOMAIN') != '' ){
				domain <<- Sys.getenv('RDOMOAPI_DOMAIN')
			}else if( !is.na(domain) ){
				domain <<- domain
			}else{
				domain <<- 'api.domo.com'
			}
			scope <<- scope
			access_time <<- access_time
			api_env <<- new.env()
		}
	)
)

Domo$methods(set_access=function(){
	auth64 <- RCurl::base64(paste(.self$client_id,.self$secret,sep=':'))[[1]]
	my_headers <- httr::add_headers(c(Authorization=paste('Basic',auth64,sep=' ')))

	type <- 'data'
	access <- httr::content(httr::GET(paste('https://',domain,'/oauth/token',sep=''),my_headers,query=list(grant_type='client_credentials',scope=paste(.self$scope,collapse=' '))))

	return_value <- 0
	if( !is.null(access$access_token) ){
		assign('access', access$access_token, .self$api_env)
		access_time <<- Sys.time()
		return_value <- 1
	}

	return(access)

})

Domo$methods(get_access=function(){

	out <- 0

	if( is.na(.self$access_time) ){
		.self$set_access()
	}

	time_since <- as.numeric(difftime(Sys.time(),.self$access_time,units='mins'))

	if( time_since < 50 ){
		out <- get('access',.self$api_env)
	}else{
		.self$set_access()
		out <- get('access',.self$api_env)
	}

	return(out)
})

Domo$methods(ds_get=function(ds,r_friendly_names=TRUE,...){
	my_headers <- httr::add_headers(c(Authorization=paste('bearer',get_access(),sep=' ')))
	my_url <- paste('https://',.self$domain,'/v1/datasets/',ds,'/data',sep='')
	out <- httr::content((httr::GET(my_url,my_headers,query=list(includeHeader='true',fileName='bogus.csv'))),na=c('\\N'),...)

	if( r_friendly_names ){
		#need to check for duplicate names
		name_check <- data.frame(original=names(out),new=tolower(make.names(names(out))),stringsAsFactors=FALSE)
		dup_check <- dplyr::mutate(dplyr::group_by(name_check,new),rn=dplyr::row_number(),n=dplyr::n())
		new_name <- dplyr::mutate(dup_check,new_name=dplyr::if_else(rn > 1,paste(new,rn,sep='_'),new))

		if( max(dup_check$n) > 1 ){
			warning(paste0('Making friendly R names modified ',sum(new_name$rn > 1),' column names. Beware.'))
		}

		names(out) <- new_name$new_name
	}

	if( sum(class(out) == 'list') > 0 ){
		stop('Error: ',out$error,' -- ',out$error_description)
	}

	return(out)
})
