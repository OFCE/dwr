actuel2scenarii <- function(record, scenarii, nom="", relatif=FALSE) {
  sim <- record$simulations |> dplyr::slice_tail() |> dplyr::pull(simulation) |> purrr::pluck(1)
  scenarii <- scenarii |> 
    dplyr::mutate(
      graph = ifelse(actuel, FALSE, graph),
      index = ifelse(actuel, max(index[is.finite(index)])+1, index),
      actuel = FALSE) 
  
  if(nom=="")
    nom <- NULL
  
  new <- tibble::tibble(
    uuid=record$uuid,
    nom_fr = nom %||% "Simulation actuelle",
    nom_en = nom %||% "Current simulation",
    comment_fr = "Simulé dans cette session",
    comment_en = "Simulation from this session",
    p = list(record$p),
    pwr = list(record$p),
    country = record$country,
    sim = list(!!sim),
    actuel = TRUE, 
    preset = FALSE,
    saved = FALSE,
    graph = TRUE, 
    relatif=relatif,
    index=Inf)
  dplyr::bind_rows(scenarii, new)
}

get_scenario_preset <- function(country, globals, cache=TRUE, start_year, periods, start_hist) {
  
  fncache <- "data/cache/scenarii_{country}.rda" |> glue::glue()
  
  if (file.exists(fncache)&&cache) {
    
    return(readRDS(fncache))
  }
  dp <- dataandparams(country = country, start_year = start_year, periods = periods, globals = globals)
  
  scn_def <- tibble(
    uuid ="",
    nom_fr = "paramètres par défaut",
    nom_en = "",
    comment_fr = "",
    comment_en = "",
    p = list(dp$params |>
               list_modify( 
                 !!!dp$p_init,
                 !!!dp$p_def,
                 start_year = start_year,
                 periods = periods,
                 start_hist = start_hist,
                 end_year = start_year+periods,
                 go_mc = TRUE,
                 seed = 1984L)
    ),
    pwr = NULL,
    country = country
  )
  
  fn <- "data/scenarii.xlsx"
  
  if (file.exists(fn) && country %in% readxl::excel_sheets("data/scenarii.xlsx"))
    scn <- readxl::read_xlsx(fn, sheet=country) |> 
    dplyr::mutate(
      p = purrr::map(uuid, ~ mongo_get_param(.x, globals$mmg)),
      country = country
    )
  else 
    scn <- tibble::tibble()
  
  scn <- scn_def |> 
    bind_rows(scn) |>
    dplyr::mutate(
      pwr = purrr::map(p, ~ {
        if(!is.null(.x))
          calc_rule_params(
            globals = globals, 
            params = .x, 
            draws = 500)
        else NULL})
    )
  
  saveRDS(scn, fncache)
  
  return(scn)
}

calc_presets <- function(country, globals, cache=TRUE, start_year= 2022, periods = 28, start_hist = 2007) {
  scn <- get_scenario_preset(
    country = country,
    globals = globals,
    cache=cache, 
    start_year = start_year,
    periods = periods,
    start_hist = start_hist)
  scn |>
    dplyr::mutate(
      preset = TRUE, 
      actuel = FALSE,
      saved = FALSE,
      graph = FALSE, 
      index = dplyr::row_number()
      ) |>
    recalc_scenarii(globals = globals)
}

recalc_scenarii <- function(scn, end_year = NULL, start_year = NULL, start_hist = NULL, globals) {
  sims <- purrr::map2(scn$pwr, scn$country, ~ {
    if(is.null(.x)) 
      return(NULL)
    sy <- start_year %||% .x$start_year
    sh <- start_hist %||% .x$start_hist
    ey <- end_year %||% .x$end_year
    des_inputs <- purrr::list_modify(.x, start_year = sy, end_year = ey, start_hist = sh, periods = ey - sy)
    ameco <- get_ameco(reset=FALSE, countries = globals$countries, variables = globals$variables, version = .x$ameco%||%"5/2021") |>
      dplyr::mutate(ccode = globals$ivariables[code])
    dandp <- dataandparams(
      country = .y, 
      start_year = sy, 
      periods = ey - sy,
      draws = .x$draws, 
      globals = globals,
      ameco = ameco
    )
    pp <- set_params(des_inputs, globals = globals, datas = dandp)
    calc_sim_dust(params = pp$p, model = globals$model, history = pp$h)
  })
  scn |> dplyr::mutate(sim = sims)
}


reset_presets_cache <- function(globals) {
  walk(globals$pays$id , ~{
    print(.x)
    presets <- calc_presets(country = .x, globals = globals, start_year=globals$start, start_hist=2007, cache = FALSE)
  })
}
