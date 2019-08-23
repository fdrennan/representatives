#' Returns representative twitter handles
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
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
#' @importFrom rtweet create_token
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom DBI dbWriteTable
#' @importFrom magrittr %>%
#' @export grab_timeline
grab_timeline <- function(connection = NULL, handles = NULL, n_tweets = 50) {

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


  if(length(handles) > 50) {
    handle <-
      tibble(handles = handles) %>%
      mutate(split_handles = as.numeric(cut(as.numeric(as.factor(handles)), 50))) %>%
      split(.$split_handles)
  } else {
    handle <- handles
  }

  map(handle,
      function(x) {
        if(is.data.frame(x))
          ids = x$handles
        else
          ids = x

        message(paste0(ids, collapse = '\n'))

        tmls <-
          get_timelines(ids, n = n_tweets)
        if(nrow(tmls)==0)
          stop("Rate limit reached")
        else
          message(paste0(nrow(tmls), " tweets gathered!"))

        tmls <-
          tmls %>%
          mutate(handle = paste0("@", screen_name),
                 update_time = with_tz(Sys.time(), 'UTC'))

        dbWriteTable(conn = connection,
                     name = 'tweets',
                     value =  tmls,
                     append = TRUE)
      }
  )

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
#' @importFrom configr read.config
#' @export update_tweets
update_tweets <- function() {

  config_data <- configr::read.config('config.yaml')$postgres

  connection <- DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
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
  message(length(reps), " ids available for query.")
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
  if(length(reps) == 0)
    stop("Nothing to update")
  else
    message(length(reps), " ids staged to be gathered.")

  df <- grab_timeline(connection = connection, handles = reps, n_tweets = 3200)

}
