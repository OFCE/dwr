# fonctions pour mongo
# mongo login doit être une liste contenant username, password, host ainsi que database (dwrlog par défaut) et col (log) et col_count (count) pour le compteur
# les collections et database sont prédéfinies mais peuvent être changées

init_mongo <- function() {
  if(!file.exists(".secrets/mongo_id.r"))
    return(NULL)
  source(".secrets/mongo_id.r")
  mmg <- list(username = username,
              password = password,
              host = host,
              username = username,
              col = "log",
              col_count = "count",
              database = "dwrlog",
              counter = "dwrlog")
  db <- mongolite::mongo(mmg$col,
                         url = sprintf(
                           "mongodb+srv://%s:%s@%s/%s",
                           mmg$username,
                           mmg$password,
                           mmg$host,
                           mmg$database),
                         options = mongolite::ssl_options(weak_cert_validation = TRUE))
  return(mmg)
}

init_counter <- function(counter, mmg) {
  db <- mongolite::mongo(collection = mmg$col_count,
                         url = sprintf(
                           "mongodb+srv://%s:%s@%s/%s",
                           mmg$username,
                           mmg$password,
                           mmg$host,
                           mmg$database),
                         options = ssl_options(weak_cert_validation = TRUE))
  db$remove(query='{"name":"[counter]"}' |> glue::glue(.open="[", .close="]"))
  db$insert(tibble::tibble(name = counter, count=0L))
}

inc_counter <- function(counter, mmg) {
  db <- mongolite::mongo(collection = mmg$col_count,
                         url = sprintf(
                           "mongodb+srv://%s:%s@%s/%s",
                           mmg$username,
                           mmg$password,
                           mmg$host,
                           mmg$database),
                         options = mongolite::ssl_options(weak_cert_validation = TRUE))
  # FindAndModify returns a counter with concurrent accesses
  db$run('{"findAndModify":"count","query": {"name" : "[counter]"},"update":{"$inc":{"count":1}}, "new": "true"}' |> glue::glue(.open="[", .close="]"))$value$count
}

mongo_log <- function(ddf, mmg, no_log="") {
  
  if(is.null(mmg)) {
    if(file.exists("logs.rda"))
      logs <- qs::qread("logs.rda")
    else 
      logs <- tibble::tibble()
    uuid <- str_c("fak", getLetterID(nrow(logs)+1))
    ddf <- ddf |>
      dplyr::mutate(uuid=!!uuid) |> 
      dplyr::relocate(uuid)
    logs <- dplyr::bind_rows(logs, ddf)
    qs::qsave(logs, "logs.rda")
    return(uuid)
  }
  
  count <- inc_counter(counter = mmg$counter, mmg)
  if(no_log=="")
    uuid <- getLetterID(count)
  else 
    uuid <- stringr::str_c(no_log, count)
  
  ddf <- ddf |>
    dplyr::mutate(uuid = !!uuid) |>
    dplyr::relocate(uuid)
  
  db <- mongolite::mongo(collection = mmg$col,
                         url = sprintf(
                           "mongodb+srv://%s:%s@%s/%s",
                           mmg$username,
                           mmg$password,
                           mmg$host,
                           mmg$database),
                         options = mongolite::ssl_options(weak_cert_validation = TRUE))
  db$insert(ddf)
  # message("logged {uuid} to mongo, {count}" |> glue::glue())
  
  if(no_log=="")
    return(uuid)
  else 
    return(no_log)
}



mongo_get_param <- function(uuid, mmg) {
  if(is.null(mmg)) {
    p <- qs::qread("logs.rda")
    uuid_idx <- which(p[[1]] == uuid)[1]
    p <- lapply(p, `[[`, uuid_idx)
  }
  else {
    db <- mongolite::mongo(collection = mmg$col, url = sprintf("mongodb+srv://%s:%s@%s/%s", mmg$username,
                                                               mmg$password, mmg$host, mmg$database),
                           options = mongolite::ssl_options(weak_cert_validation = TRUE))
    p <- db$find(query='{"uuid" : "[uuid]" }' |> glue::glue(.open="[", .close="]"))
    if(nrow(p)!=1)
      return(NULL)
  }
  
  suppressWarnings(
    p <- p |>
      as.list() |>
      purrr::map(~ {
        stringr::str_split(.x, ",") |> unlist()
      }) |>
      purrr::map(~ {
        if (all(.x %in% c("TRUE", "FALSE"))) {
          as.logical(.x)
        } else
          if (any(is.na(as.numeric(.x)))) {
            .x
          } else {
            as.numeric(.x)
          }
      })
  )
  if(is.null(p$ameco))
    p$ameco <- "5/2021"
  return(p)
}

uuid_is_ok_raw <- function(uuid, mmg) {
  if(is.null(mmg)) {
    logs <- qs::qread("logs.rda")
    if(nrow(logs |> filter(uuid==!!uuid)))
      return(TRUE)
    else 
      return(FALSE)
  }
  
  db <- mongolite::mongo(collection = mmg$col, url = sprintf("mongodb+srv://%s:%s@%s/%s", mmg$username,
                                                             mmg$password, mmg$host, mmg$database),
                         options = mongolite::ssl_options(weak_cert_validation = TRUE))
  p <- db$find(query='{"uuid" : "[uuid]" }' |> glue::glue(.open="[", .close="]"),
               fields = '{"uuid":1}')
  if(nrow(p)==1)
    return(TRUE)
  
  return(FALSE)
}

uuid_is_ok <- memoise::memoise(uuid_is_ok_raw)

mongo_get_log <- function(mmg) {
  if(is.null(mmg)) {
    logs <- qs::qread("logs.rda")
  }
  else {
    db <- mongolite::mongo(collection = mmg$col, url = sprintf("mongodb+srv://%s:%s@%s/%s", mmg$username,
                                                               mmg$password, mmg$host, mmg$database),
                           options = mongolite::ssl_options(weak_cert_validation = TRUE))
    logs <- db$find(query='{}' )
  }
  return(logs)
}