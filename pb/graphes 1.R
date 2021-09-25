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
library(patchwork)
br <-  paletteer::paletteer_d("miscpalettes::brightPastel")

future::plan("multisession", workers=4)
progressr::handlers(progressr::handler_progress(format = ":bar :percent :eta", width = 80))
purrr::walk(list.files("R/", "*.[r|R]"), ~ source("R/{.x}" |> glue::glue(), encoding="UTF-8"))

# globals data 
init_cty <- "FRA"
init_sy <- 2022
globals <- set_globals(init_cty, version = "0.8.0")
globals$mmg <- init_mongo()

# equations 
globals$model <- odin.dust::odin_dust("odin/dwrstochasticmodel.r")
# to recalculate all presets
presets <- calc_presets(country = init_cty, globals = globals, cache = FALSE)
init_dp <- dataandparams(init_cty, init_sy, globals = globals)

# si <- sessionInfo()
# print(si)

# globals data 
init_cty <- "FRA"
globals <- set_globals(init_cty, version="0.6.0")

# equations 
globals$model <- odin_dust("odin/dwrstochasticmodel.r")
presets <- calc_presets(country = init_cty, globals = globals, cache=FALSE)

# définition des contrôles
cont <- c("tpo_dstar"=0, "tpo_sstar"=0, "tpo_og"=0)
ccont <- list("tpo_dstar"=c(-0.2,0), "tpo_sstar"=c(0, 1), "tpo_og"=c(-1, 0))

# Variantes ------------------------------

# on prend la France comme base
# pour changer de pays, c'est ici
dandp <- dataandparams(country = "FRA", start_year = 2022, globals = globals, periods = 15, draws = 1000)

# on met les paramètres par défaut de la France
p_def <- list_modify(dandp$params, !!!dandp$p_init, !!!dandp$p_def)

# on change quelques valeurs
# le paramètre seed fait que les aléas sont tjrs les mêmes ! ca aide un peu parce que les différences ne viennent que des paramètres qu'on change
p <- list_modify(p_def, dstar=1, draws=1000, loss_t=20, loss_d=10, loss_ib = 1, loss_lasso = 1, 
                 ogn_sigma = 0.01, periods=8, seed = 1)

# on vérifie que ça fonctionne
sim1 <- calc_sim_dust(p, globals$model) 
# on caclule la politique budgétaire
bp <- best_FR_par(p, globals$model, cont,ccont, draws=500)
# et la simulation avec cette pb
sim1 <- calc_sim_dust(list_modify(p, !!!bp), globals$model)

col_4 <- scale_color_manual(values=br[c(6,2,3, 1)]) 
col_4_prog <-scale_color_manual(values=colorspace::sequential_hcl(n=4, palette = "Viridis"))


########################################################################
### Le graphique Dette en variant le nairu
#######################################################################
sim2 <- map_dfr(c(0.06,0.087,0.11), ~{
  pp <- list_modify(p, nairu  = .x) # met la valeur du multiplicateur à un élement de la liste
  bp <- best_FR_par(pp, globals$model, cont,ccont, draws=1000) # on cherche les polbudg
  sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim |> mutate(nairu = scales::label_percent(0.1)(.x)) # on ajoute la valeur du mulitiplicateur
})

sim2 <- sim2 |> mutate(nairu = factor(nairu, c("6.0%", "8.7%", "11.0%")))
g1 <- ggplot(sim2 |> filter(variable%in% c("dettep", "tpo")))+
  geom_line(aes(x=year+2022, y=q0.5, group = nairu, color = nairu ), size =1) + 
  theme_minimal(base_family = "Nunito", base_size = 8) +
  theme( legend.position = c(0.85,0.40), 
         strip.text = element_text(size=9, hjust=0.5, face="bold")) +
  labs(subtitle = "en % du PIB", col="Taux de chômage structurel\n[Nairu]", caption = "AMECO spring 2021, OFCE, simulation Debtwatch")+
  xlab("")+
  ylab("en % du PIB")+
  facet_wrap(vars(variable), scale="free", labeller = labeller(variable = c(dettep = "Dette publique", tpo ="Prélèvements Obligatoires"))) +
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2023, 2030), breaks = c(2023, seq(2023,2030,1)), minor_breaks = seq(2023, 2030)) +
  col_4
g1
ggsave("pb/graph_nairu.svg", width = 180, height = 100, units = "mm")




########################################################################
### Le graphique Dette en variant la cible d'inflation
#######################################################################

# on change quelques valeurs
# le paramètre seed fait que les aléas sont tjrs les mêmes ! ca aide un peu parce que les différences ne viennent que des paramètres qu'on change
p <- list_modify(p_def, dstar=1, draws=1000, loss_t=20, loss_d=10, loss_ib = 1, loss_lasso = 1, 
                 ogn_sigma = 0.01, periods=30, seed = 1)

# on vérifie que ça fonctionne
sim1 <- calc_sim_dust(p, globals$model) 
# on caclule la politique budgétaire
bp <- best_FR_par(p, globals$model, cont,ccont, draws=500)
# et la simulation avec cette pb
sim1 <- calc_sim_dust(list_modify(p, !!!bp), globals$model)

col_4 <- scale_color_manual(values=br[c(6,2,3, 1)]) 
col_4_prog <-scale_color_manual(values=colorspace::sequential_hcl(n=4, palette = "Viridis"))


sim2 <- map_dfr(c(0.01,0.02,0.03), ~{
  pp <- list_modify(p, infstar  = .x) # met la valeur du multiplicateur à un élement de la liste
  bp <- best_FR_par(pp, globals$model, cont,ccont, draws=1000) # on cherche les polbudg
  sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim |> mutate(infstar = scales::label_percent(0.1)(.x))
})

sim2 <- sim2 |> mutate(infstar = factor(infstar))

g2 <- ggplot(sim2 |> filter(variable%in% c("dettep", "tpo")))+
  geom_line(aes(x=year+2022, y=q0.5, group = infstar, color = infstar ), size =1) + 
  theme_minimal(base_family = "Nunito", base_size = 8) +
  theme( legend.position = c(0.90,0.80), 
         strip.text = element_text(size=9, hjust=0.5, face="bold")) +
  labs(subtitle = "en % du PIB", col="Cible d'inflation\n[infstar]", caption = "AMECO spring 2021, OFCE, simulation Debtwatch")+
  xlab("")+
  ylab("")+
  facet_wrap(vars(variable), scale="free", labeller = labeller(variable = c(dettep = "Dette publique", tpo ="Prélèvements Obligatoires"))) +
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2023, 2050), breaks = c(2023, seq(2025,2050,5)), minor_breaks = seq(2023, 2050)) +
  col_4
g2
ggsave("pb/graph_inflation.svg", width = 180, height = 100, units = "mm")


########################################################################
### Le graphique Dette en variant le multiplicateur
#######################################################################


sim2 <- map_dfr(c(0,1,2,3), ~{
  pp <- list_modify(p, og_po  = .x) # met la valeur du multiplicateur à un élement de la liste
  bp <- best_FR_par(pp, globals$model, cont,ccont, draws=1000) # on cherche les polbudg
  sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim |> mutate(og_po = .x) # on ajoute la valeur du mulitiplicateur
})
library(lubridate)
sim2 <- sim2 |> mutate(og_po = factor(og_po))

g3 <- ggplot(sim2 |> filter(variable%in% c("dettep", "tpo")))+
  geom_line(aes(x=year+2022, y=q0.5, group = og_po, color = og_po ), size =1) + 
  theme_minimal(base_family = "Nunito", base_size = 8) +
  theme( legend.position = c(0.95,0.75), 
         strip.text = element_text(size=9, hjust=0.5, face="bold")) +
  labs(subtitle = "en % du PIB", col="Multiplicateur\n[og_po]", caption = "AMECO spring 2021, OFCE, simulation Debtwatch")+
  xlab("")+
  ylab("")+
  facet_wrap(vars(variable), scale="free", labeller = labeller(variable = c(dettep = "Dette publique", tpo ="Prélèvements Obligatoires"))) +
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2023, 2050), breaks = c(2023, seq(2025,2050,5)), minor_breaks = seq(2023, 2050)) +
  col_4
g3
ggsave("pb/graph_multiplicateur.svg", width = 180, height = 100, units = "mm")

ggplot(sim2 |> filter(variable%in% c("tcho")))+
  geom_line(aes(x=year+2022, y=q0.5, group = og_po, color = og_po ), size =1) + 
  theme_minimal(base_family = "Nunito") +
  theme( legend.position = c(0.9,0.75), plot.subtitle=element_text(size=9, hjust=0)) +
  labs(subtitle = "en % de la population active", col="Multiplicateur\n[og_po]", caption = "AMECO spring 2021, OFCE, simulation Debtwatch")+
  xlab("")+
  ylab("")+
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2023, 2050), breaks = c(2023, seq(2025,2050,5)), minor_breaks = seq(2023, 2050)) +
  col_4
ggsave("pb/Chomage_multiplicateur.svg", width = 180, height = 100, units = "mm")


########################################################################
### Le graphique Dette en variant le gpot
#######################################################################
sim2 <- map_dfr(c(0.008,0.0125,0.015), ~{
  pp <- list_modify(p, gpot  = .x) # met la valeur du multiplicateur à un élement de la liste
  bp <- best_FR_par(pp, globals$model, cont, ccont, draws=1000) # on cherche les polbudg
  sim <- calc_sim_dust(list_modify(pp, !!!bp), globals$model)
  sim |> mutate(gpot = scales::label_percent(0.01)(.x))
})

sim2 <- sim2 |> mutate(gpot = factor(gpot))

g4 <- ggplot(sim2 |> filter(variable%in% c("dettep", "tpo")))+
  geom_line(aes(x=year+2022, y=q0.5, group = gpot, color = gpot ), size =1) + 
  theme_minimal(base_family = "Nunito", base_size = 8) +
  theme( legend.position = c(0.90,0.80), 
         strip.text = element_text(size=9, hjust=0.5, face="bold")) +
  labs(subtitle = "en % du PIB", col="Croissance potentielle\n[gpot]", caption = "AMECO spring 2021, OFCE, simulation Debtwatch")+
  xlab("")+
  ylab("")+
  facet_wrap(vars(variable), scale="free", labeller = labeller(variable = c(dettep = "Dette publique", tpo ="Prélèvements Obligatoires"))) +
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2023, 2050), breaks = c(2023, seq(2025,2050,5)), minor_breaks = seq(2023, 2050)) +
  col_4
g4
ggsave("pb/graph_gpot.svg", width = 180, height = 100, units = "mm")

ggplot(sim2 |> filter(variable%in% c("tcho")))+
  geom_line(aes(x=year+2022, y=q0.5, group = gpot, color = gpot ), size =1) + 
  theme_minimal(base_family = "Nunito") +
  theme( legend.position = c(0.9,0.75), plot.subtitle=element_text(size=9, hjust=0)) +
  labs(subtitle = "en % de la population active", col="Croissance potentielle\n[gpot]", caption = "AMECO spring 2021, OFCE, simulation Debtwatch")+
  xlab("")+
  ylab("")+
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2023, 2050), breaks = c(2023, seq(2025,2050,5)), minor_breaks = seq(2023, 2050)) +
  col_4
ggsave("pb/Chomage_gpot.svg", width = 180, height = 100, units = "mm")

