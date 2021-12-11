# écriture de stochastic_model sans array
# avec output true

# modèle simplifié

# écart de production -------------------
og <- lagog + dog
dog <- og_lag * (lagog - lag2og) +
  - og_mce * (lagog - 0) + 
  og_k * ib + og_noise

update(lagog) <- og
update(lag2og) <- lagog
update(og_noise) <- ogn_ar * og_noise + rnorm(0, ogn_sigma)

# dette ------------
dette <- lagdette + ddette
update(lagdette) <- dette
update(lag2dette) <- lagdette
ddette <- lagrho*lagdette - surplus

# rho écart critique
rho <- lagrho + drho
update(lagrho) <- rho
drho <- -rho_mce*(lagrho - rhostar) + rho_d*(lagdette - dstarr)

# surplus
surplus <- lagsurplus + sur_og * (og - lagog) - ib
update(lagsurplus) <- surplus
update(lag2surplus) <- lagsurplus

# règle budgétaire --------------
sstar <- rhostar*dstar

ib <- tpo_sstar * (lagsurplus - sstar ) + tpo_1sstar * (lagsurplus - lag2surplus) + tpo_sstar2 * (lagsurplus - sstar)^2 +
      tpo_dstar * (lagdette - dstar) + tpo_1dstar * (lagdette - lag2dette) + tpo_dstar2 * (lagdette - dstar)^2 +
      tpo_og*(lagog) + tpo_og2*(lagog)^2 + tpo_1og*(lagog - lag2og) +
      tpo_rho*(rho - rhostar) +
      phi[step]

# fonction de perte ----------------------
# utilisée pour la partie optimisation
update(loss_d_only) <- loss_d_only + loss_d * (step >= loss_t)*(lagdette - dstar)^2
initial(loss_d_only) <- 0
update(loss_no_d) <- loss_no_d + og^2/(1+loss_df)^(step-1) + loss_dog * (og-lagog)^2 + loss_ib * (ib)^2
initial(loss_no_d) <- 0

# sorties ------
 
update(dettep_o) <- dette 
update(og_o) <- og
update(surplus_o) <- surplus
update(ib_o) <- ib
update(rho_o) <- rho
update(loss_o) <- loss_no_d + loss_d_only
update(loss_nd_o) <- loss_no_d

initial(surplus_o) <- 0
initial(og_o) <- 0
initial(dettep_o) <- 0
initial(ib_o) <- 0
initial(rho_o) <- 0
initial(loss_o) <- 0
initial(loss_nd_o) <- 0

# init ----------------
i_lagog <- user(0)
i_lag2og <- user(0)
i_lagrho <- user(0)
i_lagdette <- user(1)
i_lagsurplus <- user(-0.01)
i_lag2surplus <- user(-0.01)
i_lag2dette <- user(1)

## Initial conditions
# initial(og) <- init_og
initial(lagog) <- i_lagog
initial(lag2og) <- i_lag2og
initial(og_noise) <- i_ogn
initial(lagrho) <- i_lagrho
initial(lagdette) <- i_lagdette
initial(lag2dette) <- i_lag2dette
initial(lagsurplus) <- i_lagsurplus
initial(lag2surplus) <- i_lag2surplus

# params -------------
periods <- user(50)

## parameters
og_lag   <- user(0)
og_mce   <- user(0.15)
og_k <- user(0.7)
rho_d <- user(0.01)
rho_mce <- user(0.15)
dstar <- user(0.6)
dstarr <- user(0.6)
tpo_sstar <- user(0.4)
tpo_dstar <- user(-0.1)
tpo_sstar2 <- user(0)
tpo_1sstar <- user(0)
tpo_dstar2 <- user(0)
tpo_1dstar <- user(0)
tpo_1og <- user(0)
tpo_og <- user(0)
tpo_og2 <- user(0)
tpo_rho <- user(0)
loss_df <- user(0.02)
loss_ib <- user(0)
ogn_ar <- user(0)
ogn_sigma <- user(0.005)
rhostar <- user(-0.015)
i_ogn <- user(0.005)
sur_og <- user(0.5)

# loss
loss_dog <- user(0)
loss_d <- user(0.5)
loss_t <- user(20)

# controls&exogenes -----------
phi[] <- user(0)

dim(phi) <- periods