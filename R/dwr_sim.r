forme_history <- function(data, probs = c(0.025, 0.5, 0.975)) {
  fh <- data |> dplyr::transmute(
    year = year,
    og = og, dettep,
    txppib, tpo,
    tdep=tdeppp*pibpot/qpib,
    spp,
    ci = r_app*dettep,
    ib_po = - tpo + lag(tpo),
    ib_dep = tdeppp - dplyr::lag(tdeppp), 
    ib = ib_po + ib_dep,
    qpib, pibpot, tcho, tvnairu, gpib, r_app) |>
    tidyr::pivot_longer(cols = - year, names_to = "variable", values_to = "q0.5")
  se <- stringr::str_c("q", setdiff(probs, 0.5))
  dplyr::bind_cols(fh, dplyr::bind_cols(purrr::map(se, ~tibble::tibble(!!.x := fh$q0.5)))) |> 
    dplyr::arrange(year)
}

dataandparams <- function(country, start_year, periods = 2050-start_year, draws=100, globals) {
  
  country_list <- globals$ameco |> dplyr::distinct(country)
  
  countries <- set_names(globals$pays |> dplyr::pull(id))
  countries_ln_fr <- globals$pays |> dplyr::pull(nom_fr, name = id)
  
  data <- globals$ameco |> 
    dplyr::filter(country==!!country, unit==0) |>
    dplyr::rename(var = ccode) |> 
    dplyr::select(-code, -TITLE, -COUNTRY, -`SUB-CHAPTER`, -UNIT, -csplit2, -csplit3, -unit, -is.prev, -country) |> 
    tidyr::drop_na(value) |>  
    tidyr::pivot_wider(names_from = var, values_from = value) 
  if(!"tvnairu" %in% names(data)) 
    data <- data |>
    mutate(tvnairu = slider::slide_dbl(tcho, ~mean(.x), .before = 3 ))
  if(!"pibpot" %in% names(data)) 
    data <- data |>
    mutate(pibpot = slider::slide_dbl(qpib, ~mean(.x), .before = 10),
           og = qpib/pibpot-1)
  data <- data |> 
    dplyr::mutate(dplyr::across(.cols=c(irl, irs, tvnairu), .fns = ~ dplyr::if_else(is.na(.x), .x[year==max(year[!is.na(.x)])], .x))) |> 
    dplyr::mutate(
      ppib = vpib/qpib,
      gpot = pibpot/dplyr::lag(pibpot) - 1,
      txppib = ppib/dplyr::lag(ppib) - 1, 
      gpib = qpib/dplyr::lag(qpib) - 1, 
      og = qpib/pibpot-1,
      r_app = ci/dplyr::lag(dette),
      r_inst = irl/100,
      vpibpot = ppib*pibpot,
      tpo = rec_po/vpib,
      tpopp = rec_po/vpibpot,
      tdep = dep_prim/vpib,
      tdeppp = dep_prim/vpibpot,
      cip = ci/vpib,
      soldep=solde/vpib,
      spp=sprim/vpib,
      taut = spp - tpo + tdep,
      tcho = tcho/100,
      tvnairu = tvnairu/100,
      ec = irl/100 - ((ppib/dplyr::lag(ppib,3))^0.33 - 1)- ((pibpot/dplyr::lag(pibpot,3))^0.33 - 1) ) |> 
    tibble::as_tibble()
  
  full_historical_data <- data |> 
    dplyr::transmute(year,
                     dettep = dette/vpib, r_app,
                     spp, og, txppib, tpo, tdep, tdeppp, pibpot, qpib, tcho, tvnairu, gpib) |> 
    tidyr::drop_na(dettep)
  
  historical_data <- full_historical_data |> 
    dplyr::filter(year<=start_year) 
  
  years <- c(min(historical_data$year), max(data$year))
  
  init <- data |> 
    dplyr::arrange(year) |> 
    dplyr::mutate(lagog=dplyr::lag(og),
                  lag2og=dplyr::lag(og),
                  lagtcho=dplyr::lag(tcho),
                  lag2tcho=dplyr::lag(lagtcho),
                  lagtvnairu=dplyr::lag(tvnairu),
                  lagppib=dplyr::lag(ppib),
                  lagpibpot=dplyr::lag(pibpot),
                  lag2pibpot=dplyr::lag(lagpibpot),
                  lag2ppib=dplyr::lag(lagppib),
                  lagqpib = dplyr::lag(qpib),
                  lag2qpib = dplyr::lag(lagqpib),
                  lagtdeppp = dplyr::lag(tdeppp),
                  lag2tdeppp = dplyr::lag(lagtdeppp),
                  lagtpo =dplyr::lag(tpo),
                  lagci = dplyr::lag(ci),
                  lagr_inst = dplyr::lag(r_inst),
                  lagdette = dplyr::lag(dette),
                  lag2dette = dplyr::lag(lagdette),
                  lagdettep = dplyr::lag(dette/vpib),
                  lagspp = dplyr::lag(spp),
                  lag2spp = dplyr::lag(lagspp),
                  lagcip = dplyr::lag(cip),
                  lag2cip = dplyr::lag(lagcip)) |> 
    dplyr::filter(year==start_year)
  params <- list(
    periods = periods,
    draws = as.numeric(draws),  
    infstar = 0.0175,
    nairu = 0.07,
    dstar = init$dette/init$vpib
  )
  
  p_def <- globals$defaults |>
    dplyr::filter(type=="p") |>
    dplyr::select(!!country, id) |> 
    dplyr::mutate(!!country:=as.numeric(.data[[country]])) |>
    dplyr::pull(!!country, name=id) |> 
    as.list()
  
  dstar <- historical_data |>
    dplyr::filter(year==min(start_year, max(year)-3)) |> 
    dplyr::pull(dettep)
  
  if(is.null(p_def[["dstar"]]))
    p_def$dstar <- dstar
  
  nairu <- historical_data |>
    dplyr::filter(year==min(start_year, max(year))) |> 
    dplyr::pull(tcho)
  
  if(is.null(p_def[["nairu"]]))
    p_def$nairu <- nairu
  
  dci <- globals$defaults |>
    dplyr::filter(type==start_year) |> 
    dplyr::select(id, !!country) |> 
    dplyr::mutate(!!country:=as.numeric(.data[[country]])) |>
    dplyr::pull(name = id) |> 
    as.list()
  
  zn <- function(x) ifelse(is.null(x), 0, x)
  
  p_init <- list(
    i_ogm = init$og,
    i_lagog = init$og,
    i_lag2og = init$lagog,
    i_lagtcho = init$tcho,
    i_lagtvnairu = init$tvnairu,
    i_lagppib = init$ppib,
    i_lag2ppib = init$lagppib,
    i_lag3ppib = init$lag2ppib,
    i_lagqpib = init$qpib,
    i_lag2qpib = init$lagqpib,
    i_lag3qpib = init$lag2qpib,
    i_lagpibpot = init$pibpot,
    i_lag2pibpot = init$lagpibpot,
    i_lag2dette = init$lagdette,
    i_lagtpo = init$tpo + zn(dci$i_tpo),
    i_lagtdeppp = init$tdeppp + zn(dci$i_tdeppp),
    i_lag2tdeppp = init$lagtdeppp,
    i_lagr_app = init$r_app,
    i_lagr_inst = init$r_inst,
    i_lagdette = init$dette,
    i_lagspp = init$spp + zn(dci$i_tpo) - zn(dci$i_tdeppp)/(1+init$og),
    i_lag2spp = init$lagspp,
    i_lagcip = init$cip,
    i_lag2cip = init$lagcip,
    i_lagecart_c = init$ec,
    dette_oneoff = rep(0, periods),
    tx_deriv_dep = rep(0, periods),
    tx_deriv_po = rep(0, periods),
    gpot = rep(init$gpot, periods),
    taut = rep(init$taut, periods)
  )
  
  return(list(countries = countries, 
              countries_ln_fr = countries_ln_fr,
              params = params,
              p_init = p_init, 
              p_def = p_def,
              historical_data = historical_data, 
              full_historical_data = full_historical_data, 
              init = init,
              years = years,
              country=country,
              start_year=start_year, 
              periods=periods))
}

# dust_check_params
dust_fix_params <- function(params) {
  long_exo <- params |> purrr::keep(~length(.x)>1) |> names()
  # pas moyen de récupérer cette liste de l'objet dust model
  long_exo <- unique(c(long_exo, "dette_oneoff", "phi", "gpot", "taut", "tx_deriv_dep", "tx_deriv_po"))
  ss <- purrr::map(
    rlang::set_names(long_exo), 
    ~{
      if(!is.null(params[[.x]])) 
        trimortrick(params[[.x]], params$periods)
      else
        rep(0,params$periods)
    })
  purrr::list_modify(params, !!!ss)
}

# simulations ---------------

calc_sim_dust <- function(params, model, probs=c(0.025, 0.5, 0.975), history = NULL) {
  p <- params
  p <- dust_fix_params(p)
  m <- model$new(pars=p, step=1, n_particles = p$draws, n_threads = 2, seed = as.integer(p$seed))
  
  state_names <- names(m$info()$index)
  # les variables de sortie sont indicees par _o
  out_i <- which(stringr::str_detect(state_names, "_o$")) 
  m$set_index(out_i)
  out_names <- state_names[out_i] 
  # on simule une annee de plus
  res <- m$simulate(1:(params$periods+1))
  dimnames(res)[[1]] <- out_names
  # on drop le premier step qui ne contient pas d'info
  res <- res[,,-1, drop=FALSE]
  se <- stringr::str_c("q", setdiff(probs, 0.5))
  if(params$draws>1)
    sim <- purrr::map_dfr(out_names, ~{
      m <- matrixStats::colQuantiles(abind::adrop(res[.x,,,drop=FALSE], drop=1), probs=probs, drop=FALSE)
      colnames(m) <- stringr::str_c("q", probs)
      tibble::as_tibble(m)  |>  
        dplyr::mutate(variable = stringr::str_remove(.x, "_o$") , step = 1:nrow(m))
    })
  else 
  {
    rr <- tibble::as_tibble(t(abind::adrop(res, drop=2))) |> 
      dplyr::mutate(step = dplyr::row_number()) |> 
      tidyr::pivot_longer(cols = -step, names_to = "variable", values_to = "q0.5") |> 
      dplyr::mutate(variable = stringr::str_remove(variable,"_o$"))
    sim <- dplyr::bind_cols(rr, dplyr::bind_cols(purrr::map(se, ~tibble::tibble(!!.x := rr$q0.5))))
  }
  
  if(!is.null(history))
  {
    start_year <- max(history$year)
    sim <- sim |> 
      dplyr::mutate(year = start_year+step) |> 
      dplyr::bind_rows(history) |> 
      dplyr::arrange(year) |> 
      dplyr::select(-step)
  }
  else 
    sim <- sim |>
    dplyr::rename(year=step)
  sim
}

medianloss1 <- function(params, model, controls, n_threads = 2) {
  p <- params
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names=="loss_o")
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$run(params$periods+1)
  res <- matrixStats::rowMedians(res)
  lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
  return(log(res+lasso_loss))
}

medianloss2 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = as.integer(p$seed))
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("loss_nd_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    sim <-t(abind::adrop(res, drop=2)) |> as_tibble()
  
  loss_part1 <- sim$loss_nd_o[length(sim$loss_nd_o)]
  loss_part2 <- sum((sim$dettep_o[(params$loss_t+1):periods]-params$dstar)^2) * params$loss_d
  lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
  return(log(loss_part1 + loss_part2 + lasso_loss))
}

constrainedMedianLoss2 <- function(params, model, controls, ccont, n_threads = 2) {
  if(is.null(ccont))
    return(medianloss2(params, model, controls, n_threads))
  ccc <- purrr::map(
    rlang::set_names(names(ccont)),
    ~min(max(params[[.x]],ccont[[.x]][[1]]), ccont[[.x]][[2]])
  )
  params <- list_modify(params,!!!ccc)
  medianloss2(params, model, names(ccont), n_threads =2 )
}

meanloss2 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("loss_nd_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMeans2(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    sim <-t(abind::adrop(res, drop=2)) |> as_tibble()
  
  loss_part1 <- sim$loss_nd_o[length(sim$loss_nd_o)]
  loss_part2 <- sum((sim$dettep_o[(params$loss_t+1):periods]-params$dstar)^2) * params$loss_d
  lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
  return(log(loss_part1 + loss_part2 + lasso_loss))
}

medianloss3 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  p$draws <- NULL
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("og_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    res
  
  loss_part1 <- sum(sim$og_o^2/ (1+params$df)^(0:(periods-1)))
  loss_part2 <- sum((sim$dettep_o[(params$loss_t+1):periods]-params$dstar)^2)
  log(loss_part1 + params$loss_d * loss_part2)
}

lassomedianloss4 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  p$draws <- NULL
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("og_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    res
  og4l <- sim$og_o^2/ (1+params$loss_df)^(0:(periods-1))
  d4l <- sim$dettep_o[(params$loss_t+1):periods]
  loss_part1 <- sum(og4l[sim$og<0])
  loss_part2 <- sum((d4l[d4l>params$dstar]-params$dstar)^2) * params$loss_d 
  lasso_loss <- sum(purrr::as_vector(params[controls])^2)
  log(loss_part1 + loss_part2) + log(1+lasso_loss) * params$loss_lasso
}

lassomedianloss5 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  p$draws <- NULL
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("og_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    res
  og4l <- sim$og_o^2/ (1+params$loss_df)^(0:(periods-1))
  d4l <- sim$dettep_o[(params$loss_t+1):periods]
  loss_part1 <- sum(og4l)
  loss_part2 <- sum((d4l-params$dstar)^2) * params$loss_d 
  lasso_loss <- sum(purrr::as_vector(params[controls])^2)
  log(loss_part1 + loss_part2) + log(1+lasso_loss) * params$loss_lasso
}

add_previous <- function(ns, ps) {
  if(!purrr::is_empty(ps))
  {
    dplyr::left_join(ns, ps |> dplyr::select(year, variable, pq0.5 = q0.5), by=c("year", "variable"))
  }
  else 
    dplyr::mutate(ns, pq0.5 = NA_real_)
}

# applique le facteur pour l'affichage par exemple des %
# à renseigner dans le fichier sliders.xlsx
transform_params <- function(input, globals)
{
  from_input <- map(set_names(names(globals$sliders)), ~{
    if(globals$sliders[[.x]][["type"]]=="slider")
      return(input[[.x]]/globals$sliders[[.x]][["facteur"]])
    if(globals$sliders[[.x]][["type"]]=="pick")
      return(as.numeric(input[[.x]]))
    if(globals$sliders[[.x]][["type"]]=="date")
      return(lubridate::year(input[[.x]]))
    if(globals$sliders[[.x]][["type"]]=="check")
      return(as.logical(input[[.x]]))
  })
  return(from_input)
}

# inverse la transformation précédente
undo_transform <- function(p, globals) {
  to_undone <- purrr::map_dbl(
    rlang::set_names(names(globals$sliders)),
    ~globals$sliders[[.x]][["facteur"]])
  to_undone <- to_undone[!is.na(to_undone) & to_undone!=1]
  undone <- map(rlang::set_names(names(to_undone)), ~{
    p[[.x]]*to_undone[[.x]]
  })
  purrr::list_modify(p, !!!undone)
}

`%|||%` <- function (x, y) 
{
  if (purrr::is_empty(x)) 
    y
  else x
}

set_params <- function(inputs, globals, datas)
{
  s_y <- inputs$start_year
  periods <- inputs$end_year - s_y
  history <- NULL
  # les parametres
  p <- datas$p_def
  p <- purrr::list_modify(p, periods=periods)
  p <- purrr::list_modify(p, !!!datas$p_init)
  p <- purrr::list_modify(p, !!!inputs)
  # les ci calees
  # if(s_y>globals$years[[2]]-2)
  # {
  dtpo <- inputs$i_tpo%|||%0
  dtdeppp <- inputs$i_tdeppp%|||%0
  p$i_tpo <- NULL
  p$i_tdeppp <- NULL
  p$i_lagtdeppp <- p$i_lagtdeppp%|||%0 + dtdeppp 
  p$i_lagtpo <- p$i_lagtpo%|||%0 + dtpo
  # du coup on modifie l'historique
  history <- forme_history(
    datas$historical_data |> 
      dplyr::filter(year>=inputs$start_hist))
  full_history <- forme_history(datas$full_historical_data) |>
    select(-q0.025, -q0.975) |>
    rename(full_h = q0.5) 
  ses <- names(history |> dplyr::select(dplyr::starts_with("q0.", ignore.case = FALSE)) |> dplyr::select(-q0.5))
  names(ses) <- ses
  identities <- purrr::map(ses, ~ function(x) x)
  new <- history |> 
    dplyr::filter(year==s_y) |>
    dplyr::select(year, q0.5, variable) |> 
    tidyr::pivot_wider(names_from = variable, values_from = q0.5) |> 
    dplyr::mutate(
      spp = spp + dtpo - dtdeppp/(1+og),
      tpo = tpo + dtpo, 
      tdep = tdep + dtdeppp/(1+og),
      ib_dep = ib_dep + dtdeppp,
      ib_po = ib_po + dtpo) |> 
    tidyr::pivot_longer(cols=-year, values_to = "q0.5", names_to = "variable") |> 
    dplyr::mutate(dplyr::across(q0.5, .fns = identities, .names="{.fn}"))
  history <- history |> 
    dplyr::rows_update(new, by=c("year","variable"))
  # }
  # le potentiel
  p$gpot <- rep(inputs$gpot_sj,periods)
  # les depenses publiques et les po
  # dgdep <-  (1+inputs$gpot_sj) * ((1 + inputs$ddep/p$i_lagtdeppp)^0.1 - 1)
  # dgpo <-  (1+inputs$gpot_sj) * ((1 + inputs$dtpo/p$i_lagtpo)^0.1 - 1)
  
  p$tx_deriv_dep <- c(rep(inputs$ddep/10, min(10, periods)), rep(0, max(periods-10, 0)))
  p$tx_deriv_po <- c(rep(inputs$dtpo/10, min(10, periods)), rep(0, max(periods-10, 0)))
  
  # checke pas de montecarlo
  if(inputs$go_mc) {
    p$draws  <- inputs$draws
    p$ogn  <-  inputs$og_n
    p$ogn_sigma <- inputs$ogn_sigma
  } else {
    p$draws  <- 1
    p$ogn  <-  0
    p$ogn_sigma <- 0
  }
  
  # on change les vecteurs à la bonne longueur en les coupant ou étendant si nécessaire
  long_exo <- p |> purrr::keep(~length(.x)>1) |> names()
  ss <- purrr::map(rlang::set_names(long_exo), ~trimortrick(p[[.x]], periods))
  p <- purrr::list_modify(p, !!!ss)
  
  return(list(p=p, h=history, fh=full_history, start_year=s_y, periods=periods, end_year = s_y + periods, start_hist = globals$years[[1]]))
}

calc_rule_params <- function(globals, params, draws=5000) {
  # on calcule la regle optimale (lineaire) si le switch est frappe
  # pour les lag ajouter "tpo_1dstar", "tpo_1sstar", "tpo_1og"
  # pour les termes quadratiques ajouter "tpo_dstar2", "tpo_sstar2", "tpo_og2"
  # on sort une expression avec la regle budgetaire prete a etre affichee
  controls_c <- list(tpo_sstar = c(0.05 , 0.9), tpo_dstar = c(-0.2, -0.005), tpo_og = c(-1.5, 0))
  controls <- purrr::map(controls_c, mean)
  par_fr <- best_FR_par(
    params, globals$model,
    controls = controls, 
    constraints = controls_c,
    loss = medianloss2,
    draws = draws ,
    dt = 20)
  if(!is.null(par_fr$error))
    return(list(error = TRUE))
  p <- purrr::list_modify(params, !!!par_fr, error=FALSE)
  return(p)
}

do_rule_text <- function(tpo_og, tpo_sstar, tpo_dstar) {
  if(is.null(tpo_og)||is.null(tpo_dstar)||is.null(tpo_sstar))
    return("")
  c1 <- signif(tpo_og,2)
  c2 <- signif(tpo_sstar,2)
  c3 <- ifelse(tpo_dstar>0, stringr::str_c("+", signif(tpo_dstar,2)), signif(tpo_dstar,2))
  glue::glue(
    "ib = {{c1}} \\times og + {{c2}} \\times (s_p - s^*) \\\\ {{c3}} \\times (d-d^*)",
    .open="{{", .close="}}")
}


trimortrick <- function(s,t)
{
  if(length(s)>=t)
    return(s[1:t])
  return(c(s, rep(dplyr::last(s), t-length(s))))
}

best_FR_par <- function(p, model, controls, constraints = NULL, method="Nelder-Mead", loss = medianloss2, draws = 1000, dt = 20) {
  lp <- p
  periods <- lp$loss_t+dt
  if(!is.null(p$seed))
    seed <- p$seed + 1
  else 
    seed <- as.integer(floor(runif(1,1,2*10^9)))
  lp <- purrr::list_modify(lp, periods=periods, draws=draws, seed = seed)
  lp <- dust_fix_params(lp)
  ffo <- function(cont) {
    if(is.null(constraints))
      ccc <- cont
    else {
      ccc <- purrr::map(
        rlang::set_names(names(constraints)),
        ~{
          min(max(cont[.x], constraints[[.x]][[1]]), constraints[[.x]][[2]])
        }
      )
    }
    err <- sum((as_vector(ccc)-cont)^2)
    loss(
      params = purrr::list_modify(lp, !!!ccc), 
      model = model, 
      controls = names(controls),
      n_threads = 2
    ) + err
  }
  
  opt <- try(
    optimx::optimx(
      par=as_vector(controls), 
      fn=\(x) ffo(x),
      method=method,
      control = list(kkt=FALSE, maxit = 2000)
    )
  )
  
  if("try-error"%in%class(opt))
    return(list(error = TRUE))
  
  if(opt$convcode!=0)
  {
    mess <- case_when(
      opt$convcode == 1 ~ "maxit reached",
      opt$convcode == 10 ~ "Nelder-Mead simplex degerenate")
    message("optimisation code {opt$convcode} [{mess}]" |> glue::glue())
  }
  set_names(c(opt[names(controls)]), names(controls))
}

best_FR_par_with_phi <- function(p, model, controls, method="Nelder-Mead", loss=medianloss2, draws = 1000, dt = 40) {
  null_controls <- unique(purrr:::keep(names(p), ~stringr::str_starts(.x, "tpo")), names(controls))
  null_controls <- rlang::set_names(rep(0, length(null_controls)), null_controls)
  # on cherche les phi déteministes
  lp_det <- purrr::list_modify(p, periods = p$loss_t+dt, ogn_sigma = 0, ogn_ar = 0, loss_lasso = 0, loss_ib = 0, draws = 1, !!!null_controls)
  lp_det <- dust_fix_params(lp_det)
  
  det_with_phi <- function(cont) {
    medianloss1(purrr::list_modify(lp_det, phi = cont), model, "phi", 2) 
  }
  phic <- lp_det$phi
  det_opt <- optimx::optimx(
    par=phic, 
    fn=\(x) det_with_phi(x),
    method="nlm")
  phi <- unlist(det_opt[1:lp_det$periods])
  names(phi) <- NULL
  lp <- list_modify(p, phi = phi, periods = p$loss_t+dt, draws = draws)
  lp <- dust_fix_params(lp)
  ffo <- function(cont) {
    loss(
      params = purrr::list_modify(lp, !!!cont), 
      model = model, 
      controls = names(controls),
      n_threads = 4) 
  }
  opt <- optimx::optimx(par=controls, fn=\(x) ffo(x), method=method)
  opt <- set_names(c(opt[names(controls)]), names(controls))
  opt <- list_modify(as.list(null_controls), !!!as.list(opt))
  return(list_modify(list(phi = phi), !!!opt))
}

best_FR_par_with_phi_only <- function(p, model, method="nlm", loss=medianloss2, draws = 1000, dt = 40) {
  null_controls <- unique(purrr:::keep(names(p), ~stringr::str_starts(.x, "tpo")))
  null_controls <- rlang::set_names(rep(0, length(null_controls)), null_controls)
  lp_sto <- purrr::list_modify(p, periods = p$loss_t+dt, draws = draws, !!!null_controls)
  lp_sto <- dust_fix_params(lp_sto)
  phic <- lp_sto$phi
  with_phi <- function(cont) {
    medianloss1(
      params = purrr::list_modify(lp_sto, phi = cont), 
      model = model, 
      controls = "phi", 
      n_threads = 2) 
  }
  det_opt <- optimx::optimx(par = phic, 
                            fn = \(x) with_phi(x),
                            method= method)
  if(det_opt$convcode!=0)
    message("Convergence not achieved")
  phi <- unlist(det_opt[1:lp_sto$periods])
  names(phi) <- NULL
  list_modify(list(phi = phi), !!!null_controls)
}

