# quelques fonctions utiles et variees

log_params <- function(p, old_p, data, ts, ip) {
  nnow <- lubridate::now(tzone = "Europe/Paris")
  pp <- p
  call_param <- do.call(tibble::tibble, purrr::map(p$p, ~ ifelse(length(.x) > 1, list(.x), .x)))
  pt <- dplyr::bind_cols(list(country = data$country), call_param)
  pt <- pt |>
    dplyr::mutate(session_time = ts, ip = !!ip, called = nnow) |>
    dplyr::relocate(session_time, called, ip) |>
    dplyr::mutate(dplyr::across(where(is.list), function(l) purrr::map_chr(l, ~ stringr::str_c(.x, collapse = ","))))
  
  pp$previous_p <- old_p$p
  pp$country <- data$country
  pp$start_year <- data$start_year
  pp$start_hist <- data$years[[1]]
  pp$df <- dplyr::bind_rows(p$df, pt)
  
  return(pp)
}

check_params <- function(p) {
  if (is.null(p))
    return(FALSE)
  if (length(names(p))==0)
    return(FALSE)
  ok <- purrr::map_lgl(p, ~!is.null(.x))
  ok <- purrr::reduce(ok, `&`)
  return(ok)
}

f2si2 <- function(number, rounding = TRUE, digits = 1, unit = "median") {
  lut <- c(
    1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06,
    0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21,
    1e+24
  )
  pre <- c(
    "y", "z", "a", "f", "p", "n", "u", "m", "", "k",
    "M", "G", "T", "P", "E", "Z", "Y"
  )
  ix <- ifelse(number != 0, findInterval(number, lut), 9L)
  ix <- switch(unit,
               median = median(ix, na.rm = TRUE),
               max = max(ix, na.rm = TRUE),
               multi = ix
  )
  if (rounding == TRUE) {
    scaled_number <- round(number / lut[ix], digits)
  } else {
    scaled_number <- number / lut[ix]
  }
  
  sistring <- paste0(scaled_number, pre[ix])
  sistring[scaled_number == 0] <- "0"
  return(sistring)
}

# from https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(
      id = inputId,
      class = "mobile-element",
      type = "hidden"
    )
  )
}

listify <- function(tibble, name="id") {
  map(set_names(split(tibble, 1:nrow(tibble)), tibble[[name]]), as.list)
}

set_globals <- function(country = "FRA", version = "0.1", ameco_version="5/2021") {
  # les variables utilisées par le modèle
  # Attention à la concordance des noms (!!)
  variables <- c(
    pibpot = "OVGDP", og = "AVGDGP", ppib = "PVGD", vpib = "UVGD", qpib = "OVGD",
    dette = "UDGG", oneoff = "UDGGS", ci = "UYIGE", dep_prim = "UUCGI", rec_po = "UTTT",
    sprim = "UBLGI", solde = "UBLG", ira = "AYIGD", tcho = "ZUTN", tvnairu = "ZNAWRU", irl = "ILN", irs = "ISN"
  )
  icodes <- rlang::set_names(names(variables), variables)
  # on lit les paramètres par défaut (et la liste des pays)
  defaults <- readxl::read_xlsx("data/defaults.xlsx")
  fpanels <- readxl::read_xlsx("data/panels.xlsx")
  pays <- readxl::read_xlsx("data/pays.xlsx")
  fvars <- readxl::read_xlsx("data/vars.xlsx")
  fsliders <- readxl::read_xlsx("data/sliders.xlsx")
  sim_inputs <- fsliders$id
  countries <- purrr::keep(names(defaults), ~ stringr::str_detect(.x, "[:upper:]{3}"))
  # on lit les données (préchargées sinon, on les télécharge)
  ameco <- get_ameco(variables = variables, countries = countries, version = ameco_version)  |>
    dplyr::mutate(ccode = icodes[code])
  cn_year <- ameco |> filter(year == max(year[!is.prev])) |> distinct(year) |> pull(year)
  # on met en forme les données
  tmp <- list(ameco = ameco, defaults = defaults, pays = pays)
  dp <- dataandparams(country = country, start_year = max(ameco$year), periods = 2050-max(ameco$year), globals = tmp)
  
  panels <-  NULL
  panels$ids <- fpanels$id
  panels$names <- rlang::set_names(fpanels$name_fr, panels$id)
  panels$header <- rlang::set_names(fpanels$header, panels$id)
  panels$note <- rlang::set_names(fpanels$note, panels$id)
  panels$vars <- purrr::map2(
    fpanels$var_1,
    fpanels$var_2,
    ~ (if (is.na(..2)) ..1 else c(..1, ..2))
  )
  
  panels$collapsed <- rlang::set_names(fpanels$collapsed, panels$id)
  names(panels$vars) <- panels$ids
  
  bp <- paletteer::paletteer_d("miscpalettes::brightPastel")
  vars <- NULL
  vars$ids <- fvars$id
  vars$colors <- rlang::set_names(bp[fvars$color], vars$ids)
  vars$label <- rlang::set_names(fvars$long_name_fr, vars$ids)
  vars$short <- rlang::set_names(fvars$short_name_fr, vars$ids)
  
  # les sliders
  sliders <- listify(fsliders)
  
  # résultat final
  list(
    ts = lubridate::now("Europe/Paris"),
    variables = variables,
    ivariables = icodes,
    country = country,
    ameco = ameco,
    cn_year =cn_year,
    params = dp$params,
    p_def = dp$p_def,
    p_init = dp$p_init,
    history = dp$historical_data,
    init = dp$init,
    years = dp$years,
    start = dp$start_year,
    periods = dp$periods,
    countries = countries,
    pays = pays,
    flags = purrr::map_chr(
      set_names(
        countrycode::countrycode(countries, origin = "iso3c", destination = "iso2c"), 
        countries),
      ~ glue::glue("flags/{tolower(.x)}.png")
    ),
    code2cty = rlang::set_names(dp$countries, dp$countries_ln_fr),
    cty2code = rlang::set_names(dp$countries_ln_fr, dp$countries),
    version = version,
    defaults = defaults,
    vars = vars,
    panels = panels,
    sliders = sliders,
    sim_inputs = sim_inputs,
    mmg = init_mongo()
  )
}

download_ameco <- function(url) {
  download.file(url, destfile = "./data/ameco.zip")
  ameco <- unzip("data/ameco.zip", exdir = "./data/tmp")
  dt <- data.table::rbindlist(purrr::map(ameco, ~ {
    ff <- try(data.table::fread(.x), silent = TRUE)
    if ("try-error" %in% class(ff)) {
      print("Erreur en lisant {.x} de ameco" |> glue::glue())
      ff <- data.table::data.table()
    }
    ff
  }), fill = TRUE)
  file.remove("./data/ameco.zip")
  unlink("./data/tmp", recursive = TRUE)
  code_split <- stringr::str_split(dt$CODE, "\\.")
  code <- purrr::map_chr(code_split, ~ .x[[6]])
  unit <- purrr::map_chr(code_split, ~ .x[[4]])
  csplit2 <- purrr::map_chr(code_split, ~ .x[[2]])
  csplit3 <- purrr::map_chr(code_split, ~ .x[[3]])
  country <- purrr::map_chr(code_split, ~ .x[[1]])
  dt[, `:=`(
    code = (code),
    unit = (unit),
    country = (country),
    csplit2 = (csplit2),
    csplit3 = (csplit3),
    CODE = NULL
  )]
  dt <- tibble::as_tibble(dt)
  dates <- purrr::keep(names(dt), ~ stringr::str_detect(.x, "[:digit:]{4}"))
  dt <- dt |>
    dplyr::select(-dplyr::starts_with("V", ignore.case = FALSE)) |>
    tidyr::pivot_longer(cols = dplyr::any_of(dates), names_to = "year") |>
    dplyr::mutate(year = as.numeric(year)) |>
    tidyr::drop_na(value) |>
    dplyr::group_by(country) |>
    dplyr::mutate(is.prev = year >= max(year) - 1) |>
    dplyr::ungroup()
  dt
}

get_ameco <- function(reset = FALSE, countries = list("FRA"), variables = list("OVGD"), version = "5/2021") {
  version <- str_remove(version, "/")
  ameco_url <- c(
    "52021"  = "https://ec.europa.eu/info/sites/default/files/economy-finance/ameco_spring2021.zip",
    "112021" = "https://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco0.zip"
  )
  
  freset <- !file.exists("data/ameco_{version}.rds" |> glue())
  if (freset | reset) {
    dt <- download_ameco(url = ameco_url[version]) |>
      dplyr::filter(country %in% !!countries, code %in% !!variables)
    saveRDS(dt, file = "data/ameco_{version}.rds" |> glue())
  } else {
    dt <- readRDS("data/ameco_{version}.rds" |> glue())
    freset <- !(setequal(unique(dt$country), countries) && setequal(unique(dt$code), variables))
    if (freset) {
      dt <- download_ameco(url = ameco_url[version]) |>
        dplyr::filter(country %in% !!countries, code %in% !!variables)
      saveRDS(dt, file = "data/ameco_{version}.rds" |> glue())
    }
  }
  dt
}

get_ameco_date <- function(version = "5/2021") {
  if(is.null(version)) version <- "5/2021"
  version <- str_remove(version, "/")
  if(!file.exists("data/ameco_{version}.rds" |> glue()))
    return(NULL)
  dt <- readRDS("data/ameco_{version}.rds" |> glue())
  prev <- dt |> 
    filter(is.prev) |> 
    summarise(min = min(year, na.rm=TRUE),
              max = max(year, na.rm=TRUE))
  extremes <- dt |> 
    group_by(code) |> 
    summarise(min = min(year, na.rm=TRUE),
               max = max(year, na.rm=TRUE))
  return(list(min = max(extremes$min, na.rm=TRUE),
           max = max(extremes$max, na.rm=TRUE),
           cn = prev$min-1,
           prev = list(prev$min:prev$max)))
}

setSliderStyle <- function(sliderId, color = "grey50", bgcolor = "grey80", size = "12px", weight = "normal") {
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))
  is <- glue::glue(".js-irs-{sliderId-1}")
  fontsize <- glue::glue(
    "{{is}} .irs-single, {{is}} .irs-max, {{is}} .irs-min, {{is}} .irs-handle, {font-size: {{size}}; font-weight: {{weight}};}",
    .open = "{{", .close = "}}"
  )
  sliderCol <- glue::glue(
    "{{is}} .irs-single, {{is}} .irs-from, {{is}} .irs-to, {{is}} .irs-max, {{is}} .irs-min, {{is}} .irs-handle, {{is}} .irs-bar-edge, {{is}} .irs-bar {font-size: {{size}}; font-weight: {{weight}}; border-color: transparent; background: {{bgcolor}}; border-top: 1px solid {{bgcolor}}; border-bottom: 1px solid {{bgcolor}};}",
    .open = "{{", .close = "}}"
  )
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}

setSliderFont <- function(size = "12px", weight = "normal", sliderId) {
  is <- glue::glue(".js-irs-{i}")
  sliderCol <- glue::glue(
    "{{is}} .irs-single, {{is}} .irs-max, {{is}} .irs-min {font-size: {{size}}; font-weight: {{weight}};}",
    .open = "{{", .close = "}}"
  )
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}

slid <- function(label, size = "14px", weight = "normal") {
  tags$p(label, style = glue::glue("font-size: {size}; font-weight: {weight};"))
}

getIPcaller <- function(req) {
  div(
    style = "display: none;",
    textInput(
      "remote_addr", "remote_addr",
      if (!is.null(req[["HTTP_X_FORWARDED_FOR"]])) {
        req[["HTTP_X_FORWARDED_FOR"]]
      } else {
        req[["REMOTE_ADDR"]]
      }
    )
  )
}

countries_with_flag <- function(c2c, flags) {
  list(
    content = purrr::map(c2c, ~{
      HTML(paste(tags$img(src = flags[[.x]], width = 18, height = 12), set_names(names(c2c), c2c) [[.x]]))
    })
  )
}

icon_colored <- function(icon, color, bgcolor = "#FFFFFF00", size = "md", fa = "fa") {
  style <- "color: {color}; background-color:{bgcolor};" |> glue::glue()
  # sizes <- list(xs=9, sm=12, lg=24)
  # style <- glue::glue("{style} fontsize:{sizes[size]};")
  if (size != "md") {
    class <- "{fa} fa-{icon} fa-{size}}" |> glue::glue()
  } else {
    class <- "{fa} fa-{icon}" |> glue::glue()
  }
  htmltools::browsable(
    tags$i(class = class, style = style, role="presentation")
  )
}

socialButton_ws <- function(href, icon, size = NULL) {
  if (!is.null(icon)) {
    shinydashboardPlus:::tagAssert(icon, type = "i")
  }
  if (!is.null(size) && stringr::str_length(size) > 0) size <- stringr::str_c("-", size) else size <- ""
  name <- strsplit(icon$attribs$class, "-")[[1]][2]
  cl <- "btn{size} btn-social-icon btn-{name}" |> glue::glue()
  shiny::tags$a(
    href = href, target = "_blank", class = cl,
    icon
  )
}

letterID <- function(x, n = 6) {
  r <- rep("", length(x))
  rr <- numeric(length(x))
  col_i <- numeric(length(x))
  lx <- x
  for (i in n:1) {
    rr <- lx %/% 26^(i - 1)
    col_i <- letters[rr + 1]
    r <- stringr::str_c(r, col_i)
    lx <- lx - rr * 26^(i - 1)
  }
  r
}

getLetterID <- function(i, file = "data/UUIDletters.csv") {
  purrr::map_chr(i, ~ data.table::fread(file, skip = .x - 1, nrows = 1)[[1]])
}

# génère 500 000 identifiants uniques à 5 lettres
# uniqueletterids <- sample.int(26^5, size=26^5/2-1, useHash = TRUE) |> letterID(n=5)
# data.table::fwrite(uniqueletterids |> as.data.frame(), "data/UUIDletters.csv")
# Si ca ne suffit pas on ajoutera une lettre

textInput2 <- function(inputId, label, value = "", width = NULL, placeholder = NULL, style = NULL) {
  value <- restoreInput(id = inputId, default = value)
  div(
    class = "form-group shiny-input-container", style = htmltools::css(width = validateCssUnit(width), style = style),
    shiny:::shinyInputLabel(inputId, label), tags$input(
      id = inputId,
      type = "text", class = "form-control",
      value = value, placeholder = placeholder
    )
  )
}

help_text <- function(param, sliders) {
  sliders[[param]][["help_code"]]
}
