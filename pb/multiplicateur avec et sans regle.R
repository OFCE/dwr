# init --------------

library(tidyverse)
library(rlang)
library(future)
library(shiny)
library(progressr)
library(odin.dust)
library(dust)
library(odin)
library(microbenchmark)
library(scales)
library(furrr)
library(paletteer)
library(lubridate)
library(glue)

br <-  paletteer_d("miscpalettes::brightPastel")
col_4 <- scale_color_manual(values=br[c(6,2,3, 1)], aesthetics = c("color", "fill")) 
col_4_prog <-scale_color_manual(values=colorspace::sequential_hcl(n=4, palette = "Viridis"))

future::plan("multisession", workers=4)
progressr::handlers(progressr::handler_progress(format = ":bar :percent :eta", width = 80))
walk(list.files("R/", "*.[r|R]"), ~ source("R/{.x}" |> glue(), encoding="UTF-8"))

# globals data 
init_cty <- "FRA"
init_sy <- 2022
globals <- set_globals(init_cty, version = "x")

# equations 
globals$model <- odin.dust::odin_dust("odin/dwrstochasticmodel.r", options = odin_options(target="c", verbose = FALSE))

# définition des contrôles
ccont <- list("tpo_dstar"=c(-0.2,0), "tpo_sstar"=c(0, 1), "tpo_og"=c(-1, 0))
cont <- set_names(rep(0, length(ccont)), names(ccont))
# Variantes ------------------------------
# on prend la France comme base
# pour changer de pays, c'est ici

dandp <- dataandparams(country = "FRA", start_year = 2022, globals = globals, periods = 15, draws = 1000)

# on met les paramètres par défaut de la France
p_def <- list_modify(dandp$params, !!!dandp$p_init, !!!dandp$p_def)

# on change quelques valeurs
# le paramètre seed fait que les aléas sont tjrs les mêmes ! ca aide un peu parce que les différences ne viennent que des paramètres qu'on change
p <- list_modify(p_def, dstar=1, draws=1000,  loss_ib = 1, ogn_sigma = 0.01, periods=30, seed = 1)

# on caclule la politique budgétaire
bp <- best_FR_par(p, globals$model, cont,ccont, draws=500)
# et la simulation avec cette pb
sim1 <- calc_sim_dust(list_modify(p, !!!bp), globals$model)


# le graphique du multiplicateur sans règle

p1 <- list_modify(p_def, tpo_dstar = 0, tpo_sstar = 0, tpo_og = 0, draws=1000, loss_t=20,  loss_ib = 1, og_mce = 0.15, 
                  ogn_sigma = 0., periods=30, seed = 1)
p2 <- list_modify(p1, phi = c(1, -1, 0)) # un choc temporaire
p3 <- list_modify(p1, phi = c(1, 0, 0)) # un choc permanent

# avec règle
bp1 <- best_FR_par(p1, globals$model, cont, ccont, draws=500)
bp2 <- best_FR_par(p2, globals$model, cont, ccont, draws=500) # un choc temporaire
bp3 <- best_FR_par(p3, globals$model, cont, ccont, draws=500) # un choc permanent

s1 <- calc_sim_dust(p1, globals$model)
sr1 <- calc_sim_dust(list_modify(p1, !!!bp1), globals$model)
s2 <- calc_sim_dust(p2, globals$model)
sr2 <- calc_sim_dust(list_modify(p2, !!!bp2), globals$model)
s3 <- calc_sim_dust(p3, globals$model)
sr3 <- calc_sim_dust(list_modify(p3, !!!bp3), globals$model)


vs <- bind_rows(
  left_join(s1, s2, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc temporaire",
           rule = "Sans fonction de réaction"),
  left_join(s1, s3, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc permanent",
           rule = "Sans fonction de réaction"),
  left_join(sr1, sr2, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc temporaire",
           rule = "Avec fonction de réaction"),
  left_join(sr1, sr3, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc permanent",
           rule = "Avec fonction de réaction"))

# on recommence avec un og_mce plus grand
p1 <- list_modify(p_def, tpo_dstar = 0, tpo_sstar = 0, tpo_og = 0, draws=1000, loss_t=20,  loss_ib = 1, og_mce = 0.3,  
                  ogn_sigma = 0., periods=30, seed = 1)
p2 <- list_modify(p1, phi = c(1, -1, 0)) # un choc temporaire
p3 <- list_modify(p1, phi = c(1, 0, 0)) # un choc permanent

# avec règle
bp1 <- best_FR_par(p1, globals$model, cont, ccont, draws=500)
bp2 <- best_FR_par(p2, globals$model, cont, ccont, draws=500) # un choc temporaire
bp3 <- best_FR_par(p3, globals$model, cont, ccont, draws=500) # un choc permanent

s1 <- calc_sim_dust(p1, globals$model)
sr1 <- calc_sim_dust(list_modify(p1, !!!bp1), globals$model)
s2 <- calc_sim_dust(p2, globals$model)
sr2 <- calc_sim_dust(list_modify(p2, !!!bp2), globals$model)
s3 <- calc_sim_dust(p3, globals$model)
sr3 <- calc_sim_dust(list_modify(p3, !!!bp3), globals$model)


vs_rapide <- bind_rows(
  left_join(s1, s2, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc temporaire",
           rule = "Sans fonction de réaction"),
  left_join(s1, s3, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc permanent",
           rule = "Sans fonction de réaction"),
  left_join(sr1, sr2, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc temporaire",
           rule = "Avec fonction de réaction"),
  left_join(sr1, sr3, by=c("variable", "year"), suffix=c("", ".v")) |>
    mutate(dq = q0.5.v / q0.5 -1,
           choc = "Choc permanent",
           rule = "Avec fonction de réaction"))


vs <- bind_rows( vs |> mutate(og_mce = "Retour lent"),
                 vs_rapide |> mutate(og_mce = "Retour rapide"))
  
ggplot(vs |> filter(variable=="qpib")) + 
  geom_ribbon(aes(x=year+2022, ymax=dq*100, ymin = 0, fill = rule), alpha = 0.5 )  +
  geom_point(aes(x=year+2022, y=dq*100, col = rule), size = 1, alpha = 0.25, show.legend = FALSE )  +
  facet_grid(cols = vars(choc), rows=vars(og_mce)) +
  theme_minimal(base_family = "nunito", base_size = 8) +
  theme( legend.position = c(0.9,0.9), 
         strip.text.x=element_text(size=9, hjust=0.5, face = "bold"),
         strip.text.y=element_text(size=9, hjust=0.5, face = "bold")) +
  labs(subtitle = "", fill="", caption = "AMECO Spring 2021, OFCE, simulation Debtwatch\nLe retour rapide à l'équilibre est pour og_mce=0,3, le retour lent pour og_mce = 0,15\nLe multiplicateur de court terme og_dep vaut 0,7")+
  xlab("")+
  ylab("")+
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous()+
  scale_x_continuous(limits = c(2023, 2050), breaks = c(2023, seq(2025,2050,5)), minor_breaks = seq(2023, 2050)) +
  scale_color_manual(values=br[c(3, 1)], aesthetics = c("color", "fill")) 


ggsave("pb/EH/mult regle no regle.svg", width = 180, height = 100, units = "mm")
 