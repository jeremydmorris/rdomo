
Domo <- setRefClass("Domo",
	fields=list(client_id='ANY',secret='ANY',domain='ANY',scope='ANY',access_time='POSIXct',api_env='ANY'),
	methods=list(
		initialize=function(client_id=NA,secret=NA,domain='api.domo.com',scope=c('data'),access_time=as.POSIXct(NA)){
			if( is.na(client_id) & Sys.getenv("RDOMOAPI_CLIENTID") != '' ){
				client_id <<- Sys.getenv("RDOMOAPI_CLIENTID")
			}else{
				stop('No Client ID Provided')
			}

			if( is.na(secret) & Sys.getenv("RDOMOAPI_SECRET") != '' ){
				secret <<- Sys.getenv("RDOMOAPI_SECRET")
			}else{
				stop('No Secret Provided')
			}
			if( Sys.getenv('RDOMOAPI_DOMAIN') != '' ){
				domain <<- Sys.getenv('RDOMOAPI_DOMAIN')
			}else{
				domain <<- domain
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

Domo$methods(access_init=function(){

	return_value <- 0

	data_access <- set_access(client_id,secret,type='data')
	user_access <- set_access(client_id,secret,type='user')
	dashboard_access <- set_access(client_id,secret,type='dashboard')
	audit_access <- set_access(client_id,secret,type='audit')

	if( data_access == 1 ){
		assign("client_id",client_id,.rdomoapi_env)
		assign("secret",secret,.rdomoapi_env)
		cat('Data access success!',fill=TRUE)
		return_value <- 1
	}
	if( user_access == 1 ){
		assign("client_id",client_id,.rdomoapi_env)
		assign("secret",secret,.rdomoapi_env)
		cat('User access success!',fill=TRUE)
		return_value <- 1
	}
	if( dashboard_access == 1 ){
		assign("client_id",client_id,.rdomoapi_env)
		assign("secret",secret,.rdomoapi_env)
		cat('Dashboard access success!',fill=TRUE)
		return_value <- 1
	}
	if( audit_access == 1 ){
		assign("client_id",client_id,.rdomoapi_env)
		assign("secret",secret,.rdomoapi_env)
		cat('Audit access success!',fill=TRUE)
		return_value <- 1
	}
	if( max(data_access,user_access) == 0 ){
		cat('Invalid credentials. Please try again.',fill=TRUE)
	}


	return(return_value)
})


Domo$methods(get_access=function(type='data'){

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
	my_headers <- httr::add_headers(c(Authorization=paste('bearer',get_access(type='data'),sep=' ')))
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

	return(out)
})
