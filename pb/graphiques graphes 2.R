rm(list =ls())

#library(showtext)
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
#sysfonts::font_add_google("Nunito", "nunito")

purrr::walk(list.files("R/", "*.[r|R]"), ~ source("R/{.x}" |> glue::glue(), encoding="UTF-8"))

init_cty <- "FRA"
init_sy <- 2022
globals <- set_globals(init_cty, version = "0.8.0")

br<-paletteer::paletteer_d("miscpalettes::brightPastel")

make_graph_pb<-function(g){
    g+
    labs(col="",caption = "AMECO 5/2021, OFCE, simulation Debtwatch")+
    ylab("% du PIB ou de la population active")+
    xlab("")+
    theme_minimal(base_family='nunito',base_size = 8)+
    scale_y_continuous(labels=label_percent(1))+
    scale_x_continuous(limits = c(2005, 2050), breaks = c(2005, seq(2010,2050,10)), minor_breaks = seq(2005, 2050))+
    theme(legend.position="bottom",
          strip.text.x=element_text(size=9,hjust=0.5,face='bold'),
          strip.text.y=element_text(size=9,hjust=0.5,face='bold'),
          legend.text=element_text(size=9,hjust=0.5,face='bold'),
          # plot.title=element_text(size=12,hjust=0.5,face='bold'),
          axis.title.y=element_text(size=9,hjust=0.5,face='bold'))
    
    # theme(legend.position="bottom",panel.grid.major.y=element_line(colour = "grey",linetype='dotted',size=0.25),
    #       panel.grid.major.x=element_blank(),legend.text = element_text(size = 12),
    #       panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
    
  }


#############################
###########simul cible
############################

sim_cible<-loadRDS(file = "pb/sim_cible.rds")

sim_cible<-sim_cible |>select(-c(f_r)) |>  group_by(year,variable,dstar) |> summarise(q0.025=mean(0.025),q0.5=mean(q0.5),q0.975=mean(q0.975))
donnees_historiques<-globals$history%>%select(year,og,tcho,tpo,dettep)%>%filter(year>2004)%>%pivot_longer(!year,names_to="variable",values_to="q0.5")%>%mutate(q0.025=q0.5,q0.975=q0.5,dstar=1.17)
noms_de_variables<-c(og='Output Gap',tcho="Chômage",tpo="Prélèvements Obligatoires",dettep="Dette Publique")
graph_cible <- sim_cible|> filter(variable %in% c("og", "tcho","tpo","dettep"))|> mutate(year=2022+year)|> bind_rows(donnees_historiques) |>mutate(dstar = factor(100*dstar),q0.5=q0.5)

variante_cible<-ggplot(graph_cible)+
  geom_line(data = ~.x |> filter(year>=2022), aes(x=year, y=q0.5, col=dstar), size =1, linetype="dotted") + 
  geom_line(data = ~.x |> filter(year<=2022), aes( x=year, y=q0.5, col=dstar), size = 0.5) + 
  facet_wrap(vars(variable),scales="free",labeller=labeller(variable=noms_de_variables)) + 
  scale_color_manual(values=paletteer::paletteer_d("miscpalettes::brightPastel")[c(6,2,3)],labels=c("117%","100%", "60%"))
  
variante_cible<-make_graph_pb(variante_cible)

variante_cible

ggsave(file="pb/variante_cible.svg", plot=variante_cible, width=180, height=100, unit='mm')  

###############################
############simulation ec
##############################

sim_ec<-loadRDS(file = "pb/sim_ec.rds")

sim_ec<-sim_ec |>select(-c(f_r)) |>  group_by(year,variable,ecstar) |> summarise(q0.025=mean(0.025),q0.5=mean(q0.5),q0.975=mean(q0.975))
donnees_historiques<-globals$history%>%select(year,r_app,tcho,tpo,dettep)%>%filter(year>2004)%>%mutate(ci=r_app*dettep) |> select(-c("r_app"))%>%pivot_longer(!year,names_to="variable",values_to="q0.5")%>%mutate(q0.025=q0.5,q0.975=q0.5,ecstar=0)
noms_de_variables<-c(ci="Charge d'intérêt",tcho="Chômage",tpo="Prélèvements Obligatoires",dettep="Dette Publique")
graph_ec <- sim_ec|> filter(variable %in% c("ci", "tcho","tpo","dettep"))|> mutate(year=2022+year)|> bind_rows(donnees_historiques) |>mutate(ecstar = factor(100*ecstar),q0.5=q0.5)

variante_ec<-ggplot(graph_ec)+
  geom_line(data = ~.x |> filter(year>=2022), aes(x=year, y=q0.5, col=ecstar), size =1, linetype="dotted") + 
  geom_line(data = ~.x |> filter(year<=2022), aes( x=year, y=q0.5, col=ecstar), size = 0.5) + 
  facet_wrap(vars(variable),scales="free",labeller=labeller(variable=noms_de_variables)) +
  scale_color_manual(values=br[c(6,2,3)],labels=c("0%","-2%", "2%"))

variante_ec<-make_graph_pb(variante_ec)

variante_ec

ggsave(file="pb/variante_ec.svg", plot=variante_ec, width=180, height=100,unit='mm') 


###############################
############simulation maturité
##############################

sim_matur<-loadRDS(file = "pb/sim_matur.rds")

sim_matur<-sim_matur |>select(-c(f_r)) |>  group_by(year,variable,rmat) |> summarise(q0.025=mean(0.025),q0.5=mean(q0.5),q0.975=mean(q0.975))
donnees_historiques<-globals$history%>%select(r_app,year,tcho,dettep)%>%filter(year>2004)%>%mutate(ci=r_app*dettep) |> select(-c("r_app"))%>%pivot_longer(!year,names_to="variable",values_to="q0.5")%>%mutate(q0.025=q0.5,q0.975=q0.5,rmat=8)
noms_de_variables<-c(ci="Charge d'intérêt",tcho="Chômage",dettep="Dette Publique")
graph_matur <- sim_matur|> filter(variable %in% c( "ci","tcho","dettep"))|> mutate(year=2022+year)|> bind_rows(donnees_historiques) |>mutate(rmat = factor(rmat),q0.5=q0.5)

variante_matur<-ggplot(graph_matur)+
  geom_line(data = ~.x |> filter(year>=2022), aes(x=year, y=q0.5, col=rmat), size =1, linetype="dotted") + 
  geom_line(data = ~.x |> filter(year<=2022), aes( x=year, y=q0.5, col=rmat), size = 0.5) + 
  facet_grid(vars(variable),scales="free",labeller=labeller(variable=noms_de_variables)) + 
  scale_color_manual(values=br[c(6,2)], labels=c( '8 ans', '12 ans'))
  #theme_minimal(base_family = "Source Sans Pro") +
variante_matur<-make_graph_pb(variante_matur)

variante_matur

ggsave(file="pb/variante_matur.svg", plot=variante_matur, width=180, height=100,unit='mm') 

##########################
############simulation pays
######################
dandp_list <- map(c("FRA","ITA","DEU"),dataandparams, start_year = 2022, globals = globals, periods = 30, draws = 1000) 
names(dandp_list)<-c("FRA","ITA","DEU")

sim_pays<-loadRDS(file = "pb/sim_pays.rds")

sim_pays<-sim_pays |>select(-c(f_r)) |>  group_by(year,variable,pays) |> summarise(q0.025=mean(q0.025),q0.5=mean(q0.5),q0.975=mean(q0.975)) |> mutate(datatype='simulation')
donnees_historiques<-bind_rows(map_dfr(c("FRA","ITA","DEU"),~{
  donnees_pays<-dandp_list[[.x]][['historical_data']] %>%select(year,tdep,dettep,spp,tpo)%>%filter(year>2004)%>%mutate(pays=.x)}))|> 
  pivot_longer(!c('year','pays'),names_to="variable",values_to="q0.5")%>%mutate(q0.025=q0.5,q0.975=q0.5)
donnees_historiques<-donnees_historiques |> mutate(datatype='historique')
noms_de_variables<-c(spp="Surplus Primaire",tdep="Dépenses publiques",tpo="Prélèvements Obligatoires",dettep="Dette Publique")
graph_pays <- sim_pays|> filter(variable %in% c( "spp","tdep","dettep","tpo"))|> mutate(year=2022+year)|> bind_rows(donnees_historiques) |>mutate(pays= factor(pays),datatype=factor(datatype),q0.5=q0.5)



variante_pays<-ggplot(graph_pays)+
  geom_line(data = ~.x |> filter(year>=2022), aes(x=year, y=q0.5, col=pays), size =1, linetype="dotted") + 
  geom_line(data = ~.x |> filter(year<=2022), aes( x=year, y=q0.5, col=pays), size = 0.5) + 
  geom_ribbon(aes( x=year, ymin=q0.025, ymax=q0.975, fill=pays), alpha = 0.1, show.legend=FALSE) +
  #geom_ribbon(aes(x=year,ymin=q0.025, ymax=q0.975,y=q0.5, color = pays, linetype=datatype), alpha=0.1)+
  facet_wrap(vars(variable),scales="free",labeller=labeller(variable=noms_de_variables)) + 
  scale_color_manual(values=br[c(6,2,3)],labels=c( 'Allemagne', 'France','Italie'),name="",aesthetics = c("color", "fill"))
variante_pays<-make_graph_pb(variante_pays)+ylab("% du PIB")+
  labs(col="",caption = "AMECO 5/2021, OFCE, simulation Debtwatch \n Les parties transparentes correspondent à l'intervalle de confiance à 95%")
 
variante_pays

ggsave(file="pb/variante_pays.svg", plot=variante_pays, width=180, height=100,unit='mm')
