# dwr - debtwatchR

**debtwatch** est accessible à : <https://ofce.shinyapps.io/debtwatchr>. Ici se trouve le code source nécessaire pour faire fonctionner **debtwatch** en local, modifier le modèle ou encore l'interface.

## objectifs

Le but est de proposer un simulateur macroéconomique des évolutions de la dette publique.

La dette dépend de la politique budgétaire (dépenses, recettes), de l'évolution du taux d'intérêt souverain (charge d'intérêt), de l'activité (puisque c'est le ratio dette sur pib qui est simulé, mais aussi parce que les recettes et les dépenses dépendent de l'activité directement ou indirectement à travers la règle budgétaire).

La plupart des relations sont comptables et une grande partie de la dynamique de la dette est l'accumulation d'un solde. Plus précisemment, il existe (dans le modèle) un solde primaire stabilisant la dette, qui dépend de l'écart critique (taux souverain moins taux de croissance de l'activité). Cependant quelques mécanismes "comportementaux" sont introduits dans le modèle. Les principaux sont :

-   une relation de "Philips" qui fait dépendre l'inflation de l'écart de production.

-   une fermeture de l'écart de croissance "visqueuse" qui résume des frictions diverses. Le modèle intègre une croissance potentielle exogène et l'écart de croissance converge toujours vers le potentiel en l'absence de perturbations.

-   un politique budgétaire calculée pour atteindre la cible de dette de façon "optimale". On définit une cible de dette qui résume l'objectif de la règle budgétaire. On n'inclue pas de lien entre dette cible et croissance potentielle, mais rien n'interdit de considérer des scénarios où ces deux données sont corrélées.

-   une règle de politique monétaire, synthètique, qui garantie le retour de l'inflation à sa cible mais peut également représenter des spreads souverains.

## données

Le modèle est basé sur les données d'AMECO de la Commission Européenne. En l'occurence, il s'agit de la dernière version publiée (5/2021) et donc on peut commencer la simulation après ou avant la prévision de la commission (2020 est observé, 2021 est prévu mais intègre des données conjoncturelles et les budgets votés), 2022 est construit à politique votée (ce qui n'intègre que peu de mesures, y.c. annoncées).

Les simulations sont conduites avec une perturbation aléatoire (bruit autocorrélé sur l'écart de production) et les résultats sont extraits en prenant la médiane et les quantiles haut et bas à 95%. Les équations ne sont pas linéaires (à cause de r.d entre autres)

## structure des fichiers et des répertoires

Le répertoire R contient les programmes R qui font fonctionner l'application. Le coeur de calcul est assuré dans le fichier `dwr_sim.r` mais également dans la fonction reactive de `app.r`. Dans le répertoire `odin`, le fichier `dwrstochasticmodel.r` contient le code du modèle, lisible par `{odin_dust}`. Cela ressemble à du R, mais ce n'en est pas, c'est un DSL. Il faut installer la dernière version disponible du package `devtools::install_github("mrc-ide/odin.dust")` ainsi que le solver d'équations différentielles `devtools::install_github("mrc-ide/dust")`. `{dust}` compile le modèle, ce qui permet une performance très élevée et permet de faire du montecarlo en temps réel. Un défaut de ce package est qu'il suppose que la réccurence ets résolue. Cela contraint la modélisation. Les principales dépendances sont `{shiny}`, `{shinyWidgets}`, `{tidyverse}`, `{ggplot2}`, `{dust}`, `{odin_dust}`, `{matrixStats}`, `{abind}`, `{matrixStats}`, `{mongolite}`, `{markdown}`, `{tidyverse}`, `{optimx}`, `{shiny.i18n}`.

Le répertoire `www` contient les éléments habituels css, js etc... Le répertoire `help` contient les document md pour l'aide. Le répertoire `data` contient différents fichiers de paramètres (principalement xlsx) ainsi que les traductions. Les répertoires `rsconnect` et `.secrets` contiennent des éléments de connection (à shinyapps.io et mongoDB Atlas), et ne sont pas sur le github. La journalisation est donc faite en local et les uuid ne sont pas transférables à d'autres. Le répertoire `pb` contient les codes R pour reproduire les graphiques et tableaux du Policy Brief associé.

la plupart des commentaires des codes sont en français et ne seront pas traduits.

## licence

Le logiciel est sous licence de logiciel libre CeCILL-B ([LICENCE](LICENCE)), licence permissive avec fort devoir de citation.

Debtwatch ©2021 [Timbeau](mailto:xavier.timbeau@sciencespo.fr) (auteur principal), [Heyer](mailto:eric.heyert@sciencespo.fr) (contributeur), [Aurissergues](mailto:elliot.aurissergues@sciencespo.fr) (contributeur) sous licence CeCIIL-B

Citation suggérée :

Timbeau, X., E. Aurissergues, E. Heyer, 2021, Debtwatch, un simulateur de dette publique pour le XXIe siècle, ofce.shinyapps.io/debtwatchr, github.com/OFCE/dwr
