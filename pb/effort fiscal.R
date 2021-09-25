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
library(gt)

br <-  paletteer_d("miscpalettes::brightPastel")
col_4 <- scale_color_manual(values=br[c(6,2,3, 1)], aesthetics = c("color", "fill")) 
col_4_prog <-scale_color_manual(values=colorspace::sequential_hcl(n=4, palette = "Viridis"))

future::plan("multisession", workers=4)
progressr::handlers(progressr::handler_progress(format = ":bar :percent :eta", width = 80))
walk(list.files("R/", "*.[r|R]"), ~ source("R/{.x}" |> glue(), encoding="UTF-8"))

sim <- map_dfr(cross3(c("FRA", "ITA", "DEU"), c(-0.015, 0), c(0, 0.01)), ~{
  init_cty <- .x[[1]]
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
  
  dandp <- dataandparams(country = init_cty, start_year = init_sy, globals = globals, periods = 50, draws = 1000)
  
  # on change quelques valeurs
  # le paramètre seed fait que les aléas sont tjrs les mêmes ! ca aide un peu parce que les différences ne viennent que des paramètres qu'on change
  p <- list_modify(dandp$p_def,
                   start_year = init_sy,
                   start_hist = 2007,
                   end_year = 2050,
                   go_mc  = TRUE,
                   ecstar = .x[[2]],
                   gpot_sj = dandp$p_def$gpot_sj + .x[[3]],
                   dstar= globals$history |> filter( year==2022) |> pull(dettep), 
                   ddep = globals$history |> filter( year==2019) |> pull(tdep) - globals$history |> filter( year==2022) |> pull(tdep),
                   draws=1000, ibcap = 0.005, ogn_sigma = 0.01, periods=30, seed = 1)
  pp <- set_params(p, globals, dandp) 
  # on caclule la politique budgétaire
  bp <- best_FR_par(pp$p, globals$model, cont, ccont, draws=2000)
  # et la simulation avec cette pb
  calc_sim_dust(list_modify(pp$p, !!!bp), globals$model, history=pp$h) |> mutate(country = .x[[1]], ecstar = .x[[2]], dpot = .x[[3]])
  })

ecstarneg <- ggplot(sim |> filter(variable%in% c("dettep", "tpo", "tdep", "og"), ecstar == -0.015, dpot==0))+
  geom_line(data = ~.x |> filter(year>=2022), aes(x=year, y=q0.5, col=country), size =0.5, linetype="dotted") + 
  geom_line(data = ~.x |> filter(year<=2022), aes( x=year, y=q0.5, col=country), size = 0.5) + 
  geom_ribbon(aes( x=year, ymin=q0.025, ymax=q0.975, fill=country), alpha = 0.1, show.legend=FALSE) +
  theme_minimal(base_family = "nunito", base_size = 8) +
  theme( legend.position = c(0.9,0.65), 
         strip.text = element_text(size=9, hjust=0.5, face="bold")) +
  labs(subtitle = "en % du PIB", col = "Pays", caption = "AMECO 5/2021, OFCE, simulation Debtwatch à partir de 2023")+
  xlab("")+
  ylab("")+
  facet_wrap(vars(variable), scale="free", labeller=labeller(variable=c(dettep = "Dette publique", og = "Ecart de production", tdep = "Dépenses publiques", tpo = "Prélèvements obligatoires"))) +
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2007, 2050), breaks = c(2007, seq(2015,2050,5))) +
  col_4

(ecstarnull <- ggplot(sim |> filter(variable%in% c("dettep", "tpo", "tdep", "og"), ecstar == -0.015, dpot==0.0))+
  geom_line(data = ~.x |> filter(year>=2022), aes(x=year, y=q0.5, col=country), size =0.5, linetype="dotted") + 
  geom_line(data = ~.x |> filter(year<=2022), aes( x=year, y=q0.5, col=country), size = 0.5) + 
  geom_ribbon(aes( x=year, ymin=q0.025, ymax=q0.975, fill=country), alpha = 0.1, show.legend=FALSE) +
  theme_minimal(base_family = "nunito", base_size = 8) +
  theme( legend.position = c(0.9,0.65), 
         strip.text = element_text(size=9, hjust=0.5, face="bold")) +
  labs(subtitle = "en % du PIB", col = "Pays", caption = "AMECO 5/2021, OFCE, simulation Debtwatch")+
  xlab("")+
  ylab("")+
  facet_wrap(vars(variable), scale="free", labeller=labeller(variable=c(dettep = "Dette publique", og = "Ecart de production", tdep = "Dépenses publiques", tpo = "Prélèvements obligatoires"))) +
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_x_continuous(limits = c(2007, 2050), breaks = c(2007, seq(2010,2050,5)), minor_breaks = seq(2023, 2050)) +
  col_4)

ggsave(plot=ecstarneg, filename="pb/XT/FRA DEU ITA ecstar neg.svg", width = 180, height = 140, units = "mm")


sim |>
  select(-q0.025, -q0.975) |> 
  pivot_wider(names_from=variable, values_from = q0.5) |> 
  group_by(country, ecstar, dpot) |>  
  summarize(
    dtpo_max = tpo[year==2028] - tpo[year==2022]) |> 
  pivot_wider(names_from = c(ecstar, dpot), values_from=dtpo_max, names_glue = "ec={ecstar*100}% g*=+{dpot*100}") |> 
  ungroup() |> 
   gt(rowname_col = "country") |> 
  tab_spanner(label = "r-g", columns = -country) |> 
  fmt_percent(columns = -country, decimals=1)
