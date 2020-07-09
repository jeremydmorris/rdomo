
DomoUtilities <- setRefClass("DomoUtilities",
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
		},
		set_access=function(){
			auth64 <- RCurl::base64(paste(.self$client_id,.self$secret,sep=':'))[[1]]
			my_headers <- httr::add_headers(c(Authorization=paste('Basic',auth64,sep=' ')))

			type <- 'data'
			access <- httr::content(httr::GET(paste('https://',.self$domain,'/oauth/token',sep=''),my_headers,query=list(grant_type='client_credentials',scope=paste(.self$scope,collapse=' '))))

			return_value <- 0
			if( !is.null(access$access_token) ){
				assign('access', access$access_token, .self$api_env)
				access_time <<- Sys.time()
				return_value <- 1
			}

			return(access)

		},
		get_access=function(){

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
		},
		stream_create=function(df_up, name, description, updateMethod){

			dataframe_schema <- schema_definition(df_up)
			json <- list(dataSet=list(name=name, description=description, schema=list(columns=dataframe_schema$columns)), updateMethod=updateMethod)
			body <- rjson::toJSON(json)

			headers <- httr::add_headers(c("Content-Type"='application/json', Accept='application/json', Authorization=paste('bearer',get_access(), sep=' ')))
			url <- paste('https://',.self$domain,'/v1/streams', sep='')

			response <- httr::POST(url, headers, body=body)
			httr::stop_for_status(response)

			json <- httr::content(response)
			ds <- json$dataSet$id

			return (ds)
		},
		schema_definition=function (data) {
			schema <- schema_data(data)
			schema_def <- NULL
			schema_def$columns <- list()
			for (i in 1:length(schema$name)) {
				schema_def$columns[[i]] <- list()
				schema_def$columns[[i]]$name <- schema$name[i]
				schema_def$columns[[i]]$type <- schema$type[i]
			}
			return(schema_def)
		},
		schema_data=function(data) {
			schema <- list()
			if(!is.null(data)) {
				for (i in 1:ncol(data)) {
					t.name <- names(data)[i]
					t.type <- typeConversionText(data,i)
					schema$name[length(schema$name)+1] <- t.name
					schema$type[length(schema$type)+1] <- t.type
				}
			}
			return(schema)
		},
		util_ds_meta=function(ds){
			my_headers <- httr::add_headers(c(Authorization=paste('bearer',.self$get_access(),sep=' ')))
			my_url <- paste('https://',.self$domain,'/v1/datasets/',ds,sep='')
			out <- httr::content((httr::GET(my_url,my_headers)))
			return(out)
		},
		schema_domo=function(ds_id){

			response_content <- .self$util_ds_meta(ds_id)
			response_content$schema$objects <- NULL

			columns <- list()
			for( i in 1:length(response_content$schema$columns)){
				columns$name[length(columns$name)+1] <- response_content$schema$columns[[i]]$name
				columns$type[length(columns$type)+1] <- response_content$schema$columns[[i]]$type
			}
			return(columns)
		},
		typeConversionText=function(data, colindex) {
			result <- 'STRING' #default column type
			date_time <- convertDomoDateTime(data[,colindex])
			if(!is.na(date_time[1])){
				type <- class(date_time)[1]
				if(type == 'Date') result <- 'DATE'
				if(type == 'POSIXct') result <- 'DATETIME'
				if(type == 'POSIXlt') result <- 'DATETIME'
			}else{
				type <- class(data[,colindex])[1]
				if(type == 'character') result <- 'STRING'
				if(type == 'numeric') result <- 'DOUBLE'
				if(type == 'integer') result <- 'LONG'
				if(type == 'Date') result <- 'DATE'
				if(type == 'POSIXct') result <- 'DATETIME'
				if(type == 'factor') result <- 'STRING'
				if(type == 'ts') result <- 'DOUBLE'
			}
			return(result)
		},
		convertDomoDateTime=function(v) {

			date_time <- tryCatch({ as.POSIXct(strptime(v,"%Y-%m-%dT%H:%M:%S")) }, error = function(err) { NA })
			if (is.na(date_time[1]))
				date_time <- tryCatch({ as.POSIXct(strptime(v,"%Y-%m-%d %H:%M:%S")) }, error = function(err) { NA })
			if (is.na(date_time[1]))
				date_time <- tryCatch({ as.Date(v) }, error = function(err) { NA })

			return(date_time)
		},
		get_stream_id=function(ds_id) {

			headers <- httr::add_headers(c("Content-Type"='application/json', Accept='application/json', Authorization=paste('bearer',.self$get_access(), sep=' ')))
			url <- paste('https://',.self$domain,'/v1/streams/search?q=dataSource.id:', ds_id, sep='')

			response <- httr::GET(url, headers)
			httr::stop_for_status(response)

			json <- httr::content(response)

			if(length(json) < 1){
				stop(paste("Unable to find stream for", ds_id))
			}
			return (json[[1]]$id)
		},
		stream_upload=function(ds_id, df_up){

			domoSchema <- rjson::toJSON(list(columns=schema_domo(ds_id)))
			dataSchema <- rjson::toJSON(list(columns=schema_data(df_up)))

			stream_id <- get_stream_id(ds_id)

			if(!(identical(domoSchema,dataSchema))){
				dataframe_schema <- schema_definition(df_up)
				json <- list(schema=list(columns=dataframe_schema$columns))
				body <- rjson::toJSON(json)
				update_dataset(ds_id, body)
				warning('Schema changed')
			}

			exec_id <- start_execution(stream_id)

			total_rows <- nrow(df_up)

			CHUNKSZ <- estimate_rows(df_up)
			# cat(CHUNKSZ,fill=TRUE)
			start <- 1
			end <- total_rows
			part <- 1
			repeat {
				if (total_rows - end > CHUNKSZ) {
					end <- start + CHUNKSZ
				} else {
					end <- total_rows
				}
				data_frag <- df_up[start:end,]
				uploadPartStr (stream_id, exec_id, part, data_frag)
				part <- part + 1
				start <- end + 1
				if (start >= total_rows){
					break
				}
			}

			result <- commitStream(stream_id, exec_id)
		},
		uploadPartStr=function (stream_id, exec_id, part, data) {
			FNAME <- tempfile(pattern="domo", fileext=".gz")

			headers <- httr::add_headers(c('Content-Type'='text/csv', 'Content-Encoding'='gzip', Accept='application/json', Authorization=paste('bearer',.self$get_access(), sep=' ')))
			url <- paste('https://',.self$domain,"/v1/streams/", stream_id, "/executions/", exec_id, "/part/", part, sep='')

			z <- gzfile(FNAME, "wb")

			write.table(data, file=z, col.names=FALSE, row.names=FALSE, sep=',', na='\\N', qmethod="double")
			close(z)

			size <- file.info(FNAME)$size
			b <- readBin(f <- file(FNAME, "rb"), "raw", n=size)
			close(f)

			response <- httr::PUT(url, headers, body=b)
			unlink(FNAME)
			result <- httr::content(response)
			stopifnot(result$status == 200)
		},
		commitStream=function(stream_id, exec_id) {
			headers <- httr::add_headers(c(Accept='application/json', Authorization=paste('bearer',.self$get_access(), sep=' ')))
			url <- paste('https://',.self$domain, "/v1/streams/", stream_id, "/executions/", exec_id, "/commit", sep='')
			response <- httr::PUT(url, headers)
			return(httr::content(response))
		},
		start_execution=function(stream_id) {
			headers <- httr::add_headers(c("Content-Type"='application/json', Accept='application/json', Authorization=paste('bearer',.self$get_access(), sep=' ')))
			url <- paste('https://',.self$domain,"/v1/streams/", stream_id, "/executions", sep='')
			response <- httr::POST(url, headers)
			x <- httr::content(response)
			return(x$id)
		},
		estimate_rows=function (data, kbytes = 10000) {
			sz <- pryr::object_size(data)
			targetSize <- kbytes * 3 # compression factor
			if (sz / 1000 > targetSize)
				return(floor(nrow(data)*(targetSize) / (sz/1000)))
			return(nrow(data))
		},
		update_dataset=function(ds_id, body){
			headers <- httr::add_headers(c("Content-Type"='application/json', Accept='application/json', Authorization=paste('bearer',.self$get_access(), sep=' ')))
			url <- paste('https://',.self$domain,'/v1/datasets/', ds_id, sep='')
			response <- httr::PUT(url, headers, body=body)
			httr::stop_for_status(response)
		}
	)
)

#' Reference class containing functionality to interact with Domo.
#' 
#' Inherits fields from DomoUtilities
#' 
#' All methods documented separately via standard documentation methods.
Domo <- setRefClass("Domo",contains='DomoUtilities',
	methods=list(
		ds_get=function(ds,r_friendly_names=TRUE,...){
			"Download data from Domo into a dataframe (tibble)."
			my_headers <- httr::add_headers(c(Authorization=paste('bearer',.self$get_access(),sep=' ')))
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
		},
		ds_create=function(df_up,name,description='',update_method='REPLACE'){
			"Create a new data set."
			#creates a stream
			ds <- .self$stream_create(df_up, name, description, update_method)

			#upload
			.self$stream_upload(ds, df_up)
			return(ds)
		},
		ds_update=function(ds_id, df_up){
			"Update an existing data set."
			.self$stream_upload(ds_id, df_up)
		},
		ds_meta=function(...){
			"Get all meta data related to a data set."
			.self$util_ds_meta(...)
		},
		ds_list=function(limit=0,offset=0,df_output=TRUE){
			"List all data sets"
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))

			out <- -1

			if( limit < 1 ){
				n_ret <- 1
				my_batches <- list()
				i <- 1
				batch <- 50 #limit of what the API will return
				while( n_ret > 0 ){
					my_batches[[i]] <- httr::content(httr::GET(paste('https://',.self$domain,'/v1/datasets',sep=''),my_headers,query=list(sort='name',limit=batch,offset=((i-1)*batch))))
					n_ret <- ifelse(length(my_batches[[i]]) < batch,0,1)
					i <- i + 1
				}
				out <- unlist(my_batches,recursive=FALSE)
			}else{
				out <- httr::content(httr::GET(paste('https://',.self$domain,'/v1/datasets',sep=''),my_headers,query=list(sort='name',limit=limit,offset=offset)))
			}

			if( df_output ){
				out <- dplyr::bind_rows(lapply(out,tibble::as_tibble))
			}

			return(out)

		},
		ds_delete=function(ds,prompt_before_delete=TRUE){
			"Delete a data set."
			if( prompt_before_delete ){
				invisible(readline(prompt="Permanently deletes a DataSet from your Domo instance. This is destructive and cannot be reversed. Press enter to continue."))
			}
			my_headers <- httr::add_headers(c(Authorization=paste('bearer',.self$get_access(),sep=' ')))
			my_url <- paste('https://',.self$domain,'/v1/datasets/',ds,sep='')
			out <- (httr::DELETE(my_url,my_headers))
			out_status <- ifelse(out$status_code == 204,'Successfully deleted a dataset','Some Failure')
			return(out_status)
		},
		ds_query=function(ds,query,return_data=TRUE){
			"Evaluate a query against a data set."
			interpret_query <- function(x,col_names){
				out <- x
				names(out) <- col_names
				return(tibble::as_tibble(out))
			}
			my_headers <- httr::add_headers(c('Content-Type'='application/json','Accept'='application/json',Authorization=paste('bearer',.self$get_access(),sep=' ')))
			my_url <- paste('https://',.self$domain,'/v1/datasets/query/execute/',ds,sep='')
			query_body <- list(
				sql=query
			)
			out <- httr::content((httr::POST(my_url,my_headers,body=rjson::toJSON(query_body))))
			out_out <- out
			if( return_data ){
				out_out <- dplyr::bind_rows(lapply(out$rows,interpret_query,col_names=out$columns))
			}
			return(out_out)
		},
		ds_rename=function(ds,new_name,new_description=''){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			my_url <- paste('https://',.self$domain,'/v1/datasets/',ds,sep='')
			update <- list(name=new_name)
			if( new_description != '' ){
				update$description <- new_description
			}
			out <- (httr::PUT(my_url,my_headers,body=rjson::toJSON(update)))
			return(out)
		},
		streams_search=function(ds){
			my_headers <- httr::add_headers(c(Authorization=paste('bearer',.self$get_access(),sep=' ')))
			my_url <- paste0('https://',.self$domain,'/v1/streams/search')
			out <- unlist(httr::content((httr::GET(my_url,my_headers,query=list(q=paste0('dataSource.id:',ds),fields='all')))),recursive=FALSE)
			return(out)
		},
		streams_get=function(stream_id){
			my_headers <- httr::add_headers(c(Authorization=paste('bearer',.self$get_access(),sep=' ')))
			my_url <- paste0('https://',.self$domain,'/v1/streams/',stream_id)
			out <- httr::content(httr::GET(my_url,my_headers))
			return(out)
		},
		groups_add_users=function(group_id,users){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			local_user_list <- unlist(users)
			add_uu <- function(uu,grp_id){ httr::content(httr::PUT(paste('https://',.self$domain,'/v1/groups/',grp_id,'/users/',uu,sep=''),my_headers)) }
			out <- sapply(users,add_uu,grp_id=group_id)
			return(out)
		},
		groups_create=function(group_name,users=-1,active='true'){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			local_user_list <- unlist(users)
			my_body <- list(name=group_name,active=active)
			out <- httr::content(httr::POST(paste('https://',.self$domain,'/v1/groups/',sep=''),body=rjson::toJSON(my_body),my_headers))
			if( min(local_user_list) > 0 ){
				tmp <- .self$groups_add_users(out$id,local_user_list)
			}
			return(out)
		},
		groups_delete=function(group_id){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			out <- (httr::DELETE(paste('https://',.self$domain,'/v1/groups/',group_id,sep=''),my_headers))
			return(out)
		},
		groups_get=function(group_id){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			out <- httr::content(httr::GET(paste('https://',.self$domain,'/v1/groups/',group_id,sep=''),my_headers))
			return(out)
		},
		groups_list=function(df_output=TRUE){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			n_ret <- 1
			my_batches <- list()
			i <- 1
			batch <- 500
			while( n_ret > 0 ){
				my_batches[[i]] <- httr::content(httr::GET(paste('https://',.self$domain,'/v1/groups',sep=''),my_headers,query=list(limit=batch,offset=((i-1)*batch))))
				n_ret <- ifelse(length(my_batches[[i]]) < batch,0,1)
				i <- i + 1
			}
			out <- unlist(my_batches,recursive=FALSE)
			if( df_output ){
				out <- dplyr::bind_rows(lapply(out,as.data.frame))
			}
			return(out)
		},
		groups_list_users=function(group_id){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			n_ret <- 1
			my_batches <- list()
			i <- 1
			batch <- 500
			while( n_ret > 0 ){
				my_batches[[i]] <- httr::content(httr::GET(paste0('https://',.self$domain,'/v1/groups/',group_id,'/users'),my_headers,query=list(limit=batch,offset=((i-1)*batch))))
				n_ret <- ifelse(length(my_batches[[i]]) < batch,0,1)
				i <- i + 1
			}
			out <- unlist(my_batches,recursive=TRUE)
			return(out)
		},
		groups_remove_users=function(group_id,users){
			my_headers <- httr::add_headers(c(Accept="application/json","Content-Type"="application/json",Authorization=paste('bearer',.self$get_access(),sep=' ')))
			local_user_list <- unlist(users)
			remove_uu <- function(uu,grp_id){ httr::content(httr::DELETE(paste('https://',.self$domain,'/v1/groups/',grp_id,'/users/',uu,sep=''),my_headers)) }
			out <- sapply(users,remove_uu,grp_id=group_id)
			return(out)
		}
		
		
		
	)
)
