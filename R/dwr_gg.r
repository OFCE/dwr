gs4_log <- function(ddf, gg) {
  # pas de connection on logge en local
  if(is.null(gg$gs4)) {
    if(file.exists("logs.rda"))
      logs <- qs::qread("logs.rda")
    else 
      logs <- tibble()
    uuid <- getLetterID(nrow(logs)+1)
    ddf <- ddf |>
      mutate(uuid=!!uuid) |> 
      relocate(uuid)
    logs <- bind_rows(logs, ddf)
    qs::qsave(logs, "logs.rda")
    return(uuid)
  }
  gs4_auth(email = gg$email, use_oob = TRUE, cache = ".secrets/")
  ssp <- sheet_properties(gg$gs4)
  old_index <- purrr::keep(ssp$name, ~ stringr::str_detect(.x, "^old")) |>
    stringr::str_remove("old") |>
    as.numeric()
  if (length(old_index) > 0) {
    old_index <- max(old_index)
  } else {
    old_index <- 0
  }
  
  if ("logs" %in% ssp$name) {
    safe_read_sheet <- safely(read_sheet)
    read <- safe_read_sheet(gg$gs4, sheet = "logs", range = cell_rows(1))
    if (!is.null(read$error)) {
      print(read$error)
      Sys.sleep(0.2)
      read <- safe_read_sheet(gg$gs4, sheet = "logs", range = cell_rows(1))
      if (!is.null(read$error))
        return(NULL)
    }
    cols_names <- read$result
  } else {
    cols_names <- NULL
  }
  
  if (!"logs" %in% ssp$name || (!setequal(c(names(ddf), "uuid"), cols_names)))
    # pas de feuille log ou avec d'autres colonnes on créé une feuille
  {
    if ("logs" %in% ssp$name) {
      sheet_rename(gg$gs4, sheet = "logs", new_name = str_c("old", old_index + 1))
    }
    sheet_add(ss = gg$gs4, sheet = "logs", .before = 1)
    uuid <- getLetterID(1)
    ddf <- ddf |>
      mutate(uuid = !!uuid) |>
      relocate(uuid)
    sheet_write(ss = gg$gs4, data = ddf, sheet = "logs")
  } else {
    uuid <- getLetterID(ssp |> filter(name == "logs") |> pull(grid_rows))
    ddf <- ddf |>
      mutate(uuid = !!uuid) |>
      relocate(uuid)
    sheet_append(ss = gg$gs4, data = ddf, sheet = "logs")
  }
  
  return(uuid)
}

gs4_get_param <- function(uuid, gg) {
  if(is.null(gg$gs4)) {
    logs <- qs::qread("logs.rda")
  }
  else {
    ssp <- sheet_properties(gg$gs4)
    if (!"logs" %in% ssp$name) {
      return(NULL)
    }
    logs <- read_sheet(gg$gs4, sheet = "logs")
  }
  p <- logs |> filter(uuid == !!uuid)
  if (nrow(p) != 1) {
    return(NULL)
  }
  suppressWarnings(
    p <- p |>
      as.list() |>
      map(~ {
        str_split(.x, ",") |> unlist()
      }) |>
      map(~ {
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
  return(p)
}
