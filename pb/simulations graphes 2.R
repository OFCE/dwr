rm(list =ls())

library(tidyverse)
library(rlang)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(markdown)
library(future)
library(progressr)
library(odin.dust)
library(dust)
library(odin)
library(microbenchmark)
library(scales)
library(furrr)
library(tictoc)
library(colorspace)
library(R.filesets)


future::plan("multisession", nbrOfFreeWorkers()%/%2)
progressr::handlers(progressr::handler_progress(format = ":bar :percent :eta", width = 80))
purrr::walk(list.files("R/", "*.[r|R]"), ~ source("R/{.x}" |> glue::glue(), encoding="UTF-8"))

generate_sim_sto<-function(pp, cont, ccont, draws=100, bpdraws = 1000, seed=NULL){
  with_progress({
    pb <- progressr::progressor(steps = draws)
    sim_sto <- future_map_dfr(
      1:draws, 
      function(seed) {
        pb()
        m <- odin.dust::odin_dust("odin/dwrstochasticmodel.r")
        seed <- seed %||% floor(runif(1, 1,10^9))
        pp <- list_modify(pp, seed = seed)
        bp <- best_FR_par(pp, m, cont, ccont, draws=bpdraws) # on cherche les polbudg
        sim <- calc_sim_dust(list_modify(pp, !!!bp), m)
        sim |> mutate(seed = seed, f_r = list(bp))
      })
  })
  sim_sto
}

br<-paletteer::paletteer_d("miscpalettes::brightPastel")

# globals data 
init_cty <- "FRA"
init_sy <- 2022
globals <- set_globals(init_cty, version = "0.8.0")
globals$mmg <- init_mongo()

# equations 
globals$model <- odin.dust::odin_dust("odin/dwrstochasticmodel.r")
# to recalculate all presets
presets <- calc_presets(country = init_cty, globals = globals, cache = FALSE)
init_dp <- dataandparams(init_cty, init_sy, globals$ameco, defaults = globals$defaults, pays=globals$pays)

# définition des contrôles
cont <- c("tpo_dstar"=0, "tpo_sstar"=0, "tpo_og"=0)
ccont <- list(tpo_dstar = c(-0.1, 0.), tpo_sstar=c(0, 0.5), tpo_og=c(-1.5, 0))
# cont <- c("tpo_dstar"=0, "tpo_og"=0)

# Variantes ------------------------------

# on prend la France comme base
# pour changer de pays, c'est ici
dandp <- dataandparams(country = "FRA", start_year = 2022, globals = globals, periods = 30, draws = 1000)

# on met les paramètres par défaut de la France
p_def <- list_modify(dandp$params, !!!dandp$p_init, !!!dandp$p_def)


ccont0 <- list(tpo_dstar=c(-0.2,0), tpo_sstar=c(0,1), tpo_og = c(-1,0))
cont0 <- set_names(rep(0, length(ccont0)), names(ccont0))

# on change quelques valeurs
# le paramètre seed fait que les aléas sont tjrs les mêmes ! ca aide un peu parce que les différences ne viennent que des paramètres qu'on change
p <- list_modify(p_def,ec_mce=0.2, dstar=1, draws=1000, loss_t=20, loss_d=1, loss_ib = 0.1, loss_lasso = 0, ogn_sigma = 0.01, periods=30)
# on vérifie que ça fonctionne
sim1 <- calc_sim_dust(p, globals$model) 
# on caclule la politique budgétaire

bp <- best_FR_par(p, globals$model, controls=cont, constraints=ccont, draws=500)
sim1 <- calc_sim_dust(list_modify(p, !!!bp), globals$model)


tic()
sim_cible <- map_dfr(c(1.17, 1, 0.6), ~{
  pp <- list_modify(p, dstar  = .x) # met la valeur du multiplicateur à un élement de la liste
  # bp <- best_FR_par(pp, globals$model, controls=cont, constraints=ccont, draws=500) # on cherche les polbudg
  # sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim <- generate_sim_sto(pp, cont0, ccont0, draws=20,bpdraws=500)
  sim |> mutate(dstar = .x) # on ajoute la valeur du mulitiplicateur
})
toc()

saveRDS(sim_cible, file = "pb/sim_cible.rds")


tic()
sim_ec <- map_dfr(c(0, -0.02, 0.02), ~{
  pp <- list_modify(p, ecstar  = .x) # met la valeur du multiplicateur à un élement de la liste
  #bp <- best_FR_par(pp, globals$model, controls=cont, constraints=ccont, draws=500) # on cherche les polbudg
  #sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim <- generate_sim_sto(pp, cont0, ccont0, draws=20,bpdraws=500) 
  sim |> mutate(ecstar = .x) # on ajoute la valeur du mulitiplicateur
})
toc()

saveRDS(sim_ec, file = "pb/sim_ec.rds")


############# Effet de la maturité et 

tic()
sim_matur <- map_dfr(c(8, 12), ~{
  pp <- list_modify(p, r_mat = .x) # met la valeur du multiplicateur à un élement de la liste
  # bp <- best_FR_par(pp, globals$model, controls=cont, constraints=ccont, draws=500) # on cherche les polbudg
  # sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim <- generate_sim_sto(pp, cont0, ccont0, draws=20,bpdraws=500) 
  sim |> mutate(rmat = .x) # on ajoute la valeur du mulitiplicateur
})
toc()

saveRDS(sim_matur, file = "pb/sim_matur.rds")



########### Pour la variante qe, étrange que la charge d'intérêt ne diminue pas

####### Espace fiscal en fonction du pays



dandp_list <- map(c("FRA","ITA","DEU"),dataandparams, start_year = 2022, globals = globals, periods = 30, draws = 1000) 
names(dandp_list)<-c("FRA","ITA","DEU")

make_p_deflist<-function(x){
  p<-dandp_list[[x]][['params']]
  p<-list_modify(p,!!!dandp_list[[x]]$p_init, !!!dandp_list[[x]]$p_def)
  p<-list_modify(p, ec_mce=0.5,ecstar=0,loss_t=20, loss_d=1, loss_ib = 0.1, loss_lasso = 0, ogn_sigma = 0.01, periods=30, seed = 41)}
# on met les paramètres par défaut de la France
p_def_list <- map(c(1:3),make_p_deflist)
names(p_def_list)<-c("FRA","ITA","DEU")

p_def_list[["DEU"]][["tx_deriv_dep"]]<-c(-0.026,rep(0,29))
p_def_list[["ITA"]][["tx_deriv_dep"]]<-c(-0.019,rep(0,29))


sim_pays <- map_dfr(c(1:3), ~{
  pp <- p_def_list[[.x]] # met la valeur du multiplicateur à un élement de la liste
  # bp <- best_FR_par(pp, globals$model, controls=cont, constraints=ccont, draws=500) # on cherche les polbudg
  # sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim <- generate_sim_sto(pp, cont0, ccont0, draws=20,bpdraws=500) 
  sim |> mutate(pays = names(p_def_list)[[.x]]) # on ajoute la valeur du mulitiplicateur
})

saveRDS(sim_pays, file = "pb/sim_pays.rds")


