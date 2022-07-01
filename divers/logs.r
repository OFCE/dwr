library(lubridate)
library(tidyverse)
library(stringr)
library(shiny)
purrr::walk(list.files("R/", "*.[r|R]"), ~ source("R/{.x}" |> glue::glue(), encoding = "UTF-8"))

init_cty <- "FRA"
init_sy <- 2022
globals <- set_globals(init_cty, version = "1.0.0")
mg <- globals$mmg


log <- mongo_get_log(mg) |>
  filter(ymd_hms(session_time)>=ymd_hms("2021 10 11 9h00m00s", tz="Europe/Paris"))

hcounter <- log |>
  mutate(
    called=ymd_hms(called, tz="Europe/Paris"),
    dh = interval(ymd_hms("2021 10 11 9h00m00s", tz="Europe/Paris"), called)/hours(1) , 
    dhr = ceiling(dh),
    heure = ymd_hms("2021 10 11 9h00m00s", tz="Europe/Paris") + hours(dhr)) |> 
  arrange(dh) |> 
  group_by(dhr) |> 
  summarise(
    heure = first(heure),
    duree = (min(called)%--%max(called))/hours(1),
    local = sum(ip == "127.0.0.1"),
    ouvre = sum(str_length(uuid)>5&!ip == "127.0.0.1")/duree,
    change = sum(str_length(uuid)==5&!ip == "127.0.0.1")/duree
  ) |> 
  pivot_longer(cols = c(ouvre, change, local))

dcounter <- log |> 
  mutate(
    called=ymd_hms(called, tz="Europe/Paris"),
    dd = interval(ymd_hms("2021 10 11 9h00m00s", tz="Europe/Paris"), called)/days(1) , 
    ddr = ceiling(dd),
    jour = ymd_hms("2021 10 11 9h00m00s", tz="Europe/Paris") + days(ddr)) |> 
  arrange(dd) |> 
  group_by(ddr) |> 
  summarise(
    jour = first(jour),
    duree = (min(called)%--%max(called))/days(1),
    local = sum(ip == "127.0.0.1"),
    ouvre = sum(str_length(uuid)>5&!ip == "127.0.0.1"),
    change = sum(str_length(uuid)==5&!ip == "127.0.0.1")
  ) |> 
  pivot_longer(cols = c(ouvre, change, local))

ggplot(hcounter |> filter(dhr>0)) +
  geom_col(aes(x=heure, y=value, fill=name, width=3600*duree*0.9)) +
  scale_x_datetime() +
  scale_y_continuous(name="nombre de simulations par heure", limits=c(0,250))+
  xlab("")
ggplot(dcounter |> filter(ddr>0)) + 
  geom_col(aes(x=ddr, y=value, fill=name))  + 
  scale_y_continuous(name="nombre de simulations par jour")+xlab("")

sum(hcounter$value * hcounter$duree, na.rm=TRUE)/sum(hcounter$duree, na.rm=TRUE)
round(sum(hcounter$value * hcounter$duree, na.rm=TRUE))