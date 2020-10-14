#' Get Dimensions db authorisation token
get_dimensions_token <- function(
  username = NULL, 
  password = NULL
){
  
  #establish login credentials
  credentials <- list(
    username = Sys.getenv("DIMENSIONS_USERNAME"), 
    password = Sys.getenv("DIMENSIONS_PASSWORD")
    )
  
  #retrieve authorisation
  credentials <- httr::POST(
    url = "https://app.dimensions.ai/api/auth.json",
    body = credentials,
    encode = "json"
  )
  
  if(httr::http_error(credentials)){
    stop("Error fetching credentials")
  }
  
  #return the token
  return(
    paste(
      "JWT", 
      httr::content(
        x = credentials,
        as = "parsed"
      )[["token"]]
    )
  )
}


#' Query the Dimensions database
query_dimensions <- function(query, token = NULL){
  
  if(is.null(token)){
    token <- get_dimensions_token()
  }
  
  post_query <- httr::POST(
    url = "https://app.dimensions.ai/api/dsl.json",
    config = httr::add_headers(
      Authorization = token
    ),
    body = query,
    httr::content_type_json(),
    encode = "raw"
  )
  
  if(httr::http_error(post_query)){
    stop(
      paste(
        "Query failed with status",
        httr::http_status(post_query)[["message"]]
      )
    )
  }
  
  return(post_query)
  
}


#' Parse query result
fetch_results <- function(query, token){
  df <- query_dimensions(query = query, token = token)
  df <- jsonlite::fromJSON(httr::content(df, "text", encoding = "UTF-8"))
  
  return(df)
}
