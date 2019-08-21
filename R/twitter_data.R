#' Returns representative twitter handles
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom  rvest html_text
#' @importFrom magrittr %>%
#' @export grab_representatives
grab_representatives <- function() {

  reps <- read_html(
    'https://resist.blog/us-house-representatives-twitter-list/'
  ) %>%
    html_nodes('#tablepress-1 a') %>%
    html_text()

  reps
}

#' Returns representative twitter handles
#' @importFrom configr read.config
#' @importFrom rtweet get_timelines
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export grab_timeline
grab_timeline <- function(handles = NULL, n_tweets = 50) {

  if(is.null(handles)) {
    stop('Must enter handles argument')
  }
  message("Grabbing up to ", n_tweets, " results.")

  config_data <- configr::read.config('config.yaml')$twitter

  ## authenticate via access token
  token <- create_token(
    app = "my_twitter_research_app",
    consumer_key = config_data$consumer_key,
    consumer_secret = config_data$consumer_secret,
    access_token = config_data$access_token,
    access_secret = config_data$access_secret)

  tmls <- get_timelines(handles, n = n_tweets) %>%
    mutate(
      update_time = Sys.time()
    )

  tmls
}

#' Stores rep tweets
#' @importFrom DBI dbConnect
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbExistsTable
#' @importFrom DBI dbGetQuery
#' @importFrom dbplyr sql
#' @importFrom stringr str_to_lower
#' @importFrom lubridate with_tz
#' @importFrom DBI dbWriteTable
#' @importFrom configr read.config
#' @export update_tweets
update_tweets <- function() {

  config_data <- configr::read.config('config.yaml')$postgres

  connection <- DBI::dbConnect(
    PostgreSQL(),
    dbname   = config_data$dbname,
    host     = config_data$host,
    port     = as.numeric(config_data$port),
    user     = config_data$user,
    password = config_data$password
  )
  on.exit({
    dbDisconnect(connection)
  })
  # dbRemoveTable(connection, 'tweets')
  reps <- grab_representatives()

  if(dbExistsTable(connection, 'tweets')) {
    response <- dbGetQuery(
      connection,
      sql(
        'select distinct handle, update_time
           from tweets
           where update_time >= current_timestamp - interval \'7 hour\'
          '
      )
    )
    reps <- reps[!str_to_lower(reps) %in% str_to_lower(response$handle)]
  }
  reps <- reps[!str_to_lower(reps) %in% str_to_lower(inactive_reps)]
  if(length(reps) == 0) {
    stop("Nothing to update")
  }

  df <- representatives::grab_timeline(handles = reps, n_tweets = 3200) %>%
    mutate(handle = paste0("@", screen_name),
           update_time = with_tz(Sys.time(), 'UTC'))
  message(print(df))
  dbWriteTable(conn = connection,
               name = 'tweets',
               value =  df,
               append = TRUE)

}
