# bien installer les dernières version de github pour shiny
# devtools::install_github("mrc-ide/dust")
# devtools::install_github("mrc-ide/odin")
# devtools::install_github("mrc-ide/odin.dust")
# devtools::install_github("EmilHvitfeldt/quickpalette")
# devtools::install_github("dreamRs/shinyWidgets")

library(tidyverse)
library(rlang)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(markdown)
library(glue)
library(stringr)
library(purrr)
library(svglite)
library(ragg)
library(showtext)

options(shiny.useragg = TRUE)
# les google fonts sont dans www, s'ils sont effacés passer les deux lignes suivantes
# gfonts::setup_font("source-code-pro", output_dir="www")
# gfonts::setup_font("source-sans-pro", output_dir="www")
# gfonts::setup_font("roboto", output_dir="www")
# purrr::walk(list.files("R/", "*.[r|R]"), ~ source("R/{.x}" |> glue::glue(), encoding = "UTF-8"))
# options(shiny.reactlog = TRUE)

# fonts 
font_add_google("Roboto", "roboto")
# # sysfonts::font_add_google("Source Sans Pro", "source-sans-pro")
showtext_auto()

# Translation files
# # read from xlsx data describing and documenting panels, variables, sliders, and so on
trad_scn <- map_dfr(
  readxl::excel_sheets("data/scenarii.xlsx"),
  ~ bind_rows(
    readxl::read_xlsx("data/scenarii.xlsx") |> select(fr = nom_fr, en = nom_en),
    readxl::read_xlsx("data/scenarii.xlsx") |> select(fr = comment_fr, en = comment_en)
  ))
trad <- bind_rows(
  trad_scn,
  readxl::read_xlsx("data/traductions-autres.xlsx"),
  readxl::read_xlsx("data/sliders.xlsx") |> select(fr = label, en = label_en),
  readxl::read_xlsx("data/vars.xlsx")    |> select(fr = long_name_fr, en = long_name_en),
  readxl::read_xlsx("data/vars.xlsx")    |> select(fr = short_name_fr, en = short_name_en),
  readxl::read_xlsx("data/pays.xlsx")    |> select(fr = nom_fr, en = nom_en),
  readxl::read_xlsx("data/vars.xlsx")    |> select(fr = long_name_fr, en = long_name_en),
  readxl::read_xlsx("data/panels.xlsx")  |> select(fr = name_fr, en = name_en),
  readxl::read_xlsx("data/panels.xlsx")  |> select(fr = header, en = header_en),
  readxl::read_xlsx("data/panels.xlsx")  |> select(fr = note, en = note_en))

write.csv(
  dplyr::distinct(trad,fr,.keep_all = TRUE), 
  "data/translations/translation_en.csv", 
  fileEncoding = "UTF-8",
  row.names=FALSE)  

i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "data/translations"))
i18n$set_translation_language("fr")
translation_en <- read.csv("data/translations/translation_en.csv", fileEncoding = "UTF-8")

# globals data -----------------------
init_cty <- "FRA"
init_sy <- 2022
ameco_version <-  "5/2022"
ameco_versions <- c("5/2021", "11/2021", "5/2022")
globals <- set_globals(init_cty, version = "1.0.6", ameco_version=ameco_version)

# equations -------------------
# equations should be located in odin folder
# syntax is odin rule
globals$model <- odin.dust::odin_dust(
  "odin/dwrstochasticmodel.r", 
  options = odin::odin_options(target = "c", verbose =FALSE))
# to recalculate all presets
# reset_presets_cache(globals)
g_presets <- calc_presets(country = init_cty, globals = globals)
# init_dp <- dataandparams(country = init_cty, start_year = init_sy, globals = globals)

katex_equations <- HTML(paste(readLines("help/equation-katex.html"), collapse="\n"))

# shiny -----------------

## ui -------------

ui <- function(req) {
  fluidPage(
    # ------ <HEAD> ------------------------------------------------------------
    shiny.i18n::usei18n(i18n),
    shinyjs::useShinyjs(),
    includeCSS("www/css/sass.css"),
    includeCSS("www/css/collapse.css"),
    includeScript("www/js/js-cookie.js"),
    includeScript("www/js/get_cookie.js"),
    shinybusy::add_busy_bar(timeout=500, color="#B80000"),
    gfonts::use_font("roboto", "www/css/roboto.css"),
    gfonts::use_font("source-code-pro", "www/css/source-code-pro.css"),
    gfonts::use_font("source-sans-pro", "www/css/source-sans-pro.css"),
    title = "debtwatchr",
    
    tags$head(
      includeHTML("www/googleanalytics.html"),
      tags$style(type = "text/css", "text {font-family: sans-serif}"),
      tags$script(
        id = "Cookiebot",
        src = "https://consent.cookiebot.com/uc.js",
        `data-cbid` = "d47ae39c-39ee-46f3-9b10-478e2f19cdf2",
        `data-blockingmode` = "auto",
        type = "text/javascript"
      ),
      # Meta tags to share on social media
      # Facebook & LinkedIn
      tags$meta(property = "og:title", content = "Debtwatch"),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:image", content = "https://gyazo.com/db673e54f188093da535f492bb8eb38f.png"),
      tags$meta(property = "og:url", content = "https://ofce.shinyapps.io/debtwatchr/"),
      tags$meta(property = "og:description", content = "Debtwatch, un simulateur de dette pour le XXIe siècle"),
      # Twitter
      tags$meta(property = "twitter:card", content = "app"),
      tags$meta(property = "twitter:title", content = "Debtwatch"),
      tags$meta(property = "twitter:description", content = "Debtwatch, un simulateur de dette pour le XXIe siècle"),
      tags$meta(property = "twitter:image", content = "https://gyazo.com/db673e54f188093da535f492bb8eb38f.png")
    ),
    theme = bslib::bs_theme(
      version = 4,
      base_font = "Source Sans Pro",
      code_font = "Source Code Pro"
    ),
    includeCSS("www/dwrstyle.css"),
    includeCSS("www/katex.css"),
    getIPcaller(req),
    # ------ TITLE PANEL -------------------------------------------------------
    fluidRow(
      id = "title_panel",
      column(
        width = 1,
        class = "logo",
        img(src='ofce.png', align = "center", width = 142*.5, heigth = 45*.5)
      ),
      column(
        width = 2,
        class = "title",
        h2("DebtWatch")
      ),
      column(
        width = 5,
        class = "subtitle",
        div(
          id = "toggle_sidebar_wrapper",
          actionButton(
            inputId = "toggle_sidebar",
            label = NULL,
            icon = icon("bars")
          )
        ),
        h3(i18n$t("Observatoire de la dette publique"))
      ),
      column(
        width = 2,
        class = "languages",
        div(
          class = "language-item",
          actionLink(
            inputId = "language_fr",
            label = "FR"
          )
        ),
        div(
          class = "language-item",
          actionLink(
            inputId = "language_en",
            label = "EN"
          )
        )
      ),
      column(
        width = 2,
        class = "social-buttons",
        # ------ * Boutons sociaux ---------------------------------------------
        # Use Share Link Generator to update the text to share:
        # http://www.sharelinkgenerator.com/
        a(
          href = "https://twitter.com/intent/tweet?text=Debtwatch,%20la%20dette%20publique%20au%20XXIe%20si%C3%A8cle%20%3A%20https%3A//ofce.shinyapps.io/debtwatchr%0Avia%20%40XTimbeau%20%40HeyerEric%20%40EllAurissergues%20%40OFCEParis",
          icon("twitter"),
          target = "_blank" # To open a new tab
        ),
        a(
          href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//ofce.shinyapps.io/debtwatchr/",
          icon("facebook"),
          target = "_blank"
        ),
        a(
          href = "https://github.com/OFCE/dwr",
          icon("github"),
          target = "_blank"
        ),
        a(
          href = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//ofce.shinyapps.io/debtwatchr/&title=Le%20debtwatchr%20!&summary=Venez%20essayer%20le%20debtwatchr%20comme%20moi.&source=",
          icon("linkedin"),
          target = "_blank"
        ),
        includeHTML("www/cookiebutton.html"),
        span(stringr::str_c("v", globals$version), style = "font-size:9px;")
      )
    ),
    fluidRow(
      id = "body",
      column(
        id = "sidebar_panel",
        width = 3,
        # ------ SIDEBAR -------------------------------------------------------
        div(
          # ------ * Paramètres principaux -------------------------------------
          class = "parametres-principaux",
          div(
            class = "sidebar-title",
            h4(i18n$t("Paramètres principaux"))
          ),
          div(
            class = "country-wrapper",
            pickerInput(
              inputId = "country",
              label = NULL,
              choices = globals$code2cty,
              selected = init_cty,
              choicesOpt = countries_with_flag(globals$code2cty, globals$flags)
            )
          ),
          div(
            class = "parametres-wrapper",
            collapseUI(
              id = "politique_budgetaire",
              title = h6(i18n$t("Politique budgétaire")),
              content = div(
                dwr_slider("dstar", globals, i18n),
                dwr_slider("loss_t", globals, i18n),
                dwr_slider("ibcap", globals, i18n),
                dwr_slider("pcpo", globals, i18n),
                dwr_slider("ddep", globals, i18n),
                dwr_slider("dtpo", globals, i18n)
              )
            ),
            collapseUI(
              id = "politique_monetaire",
              title = h6(i18n$t("Politique monétaire")),
              content = div(
                dwr_slider("infstar", globals, i18n),
                dwr_slider("ecstar", globals, i18n),
                dwr_slider("dettebc", globals, i18n),
                dwr_slider("r_bc", globals, i18n)
              )
            ),
            collapseUI(
              id = "hypotheses-simulation",
              title = h6(i18n$t("Hypothèses simulation")),
              content = div(
                dwr_slider("gpot_sj", globals, i18n),
                dwr_slider("nairu", globals, i18n),
                selectInputWrapper(
                  inputId = "ameco",
                  label = i18n$t("Version d’AMECO"),
                  help_text = help_text("ameco", globals$sliders),
                  value = ameco_version,
                  choices = ameco_versions,
                  selected = ameco_version
                )
              )
            )
          )
        ), # div paramètres principaux
        div(
          # ------ * scenarii -----------------------------------
          class = "parametres-secondaires",
          style = "margin-top: 40px",
          div(
            class = "sidebar-title",
            h3(i18n$t("Scénarios")),
            tags$button(
              id = "scenarii_more",
              href = "#",
              class = "action-button section-more",
              icon("caret-up")
            )
          ),
          div(
            id="scenarii_content",
            class="section-content",
            div(
              class = "scenarii-wrapper",
              scenarii_panel(globals, i18n)
            ) 
          )
        ), # div scenarios
        div(
          class = "parametres-secondaires",
          div(
            # ------ * Paramètres secondaires -----------------------------------
            class = "sidebar-title",
            h3(i18n$t("Paramètres avancés")),
            tags$button(
              id = "parametres_secondaires_more",
              href = "#",
              class = "action-button section-more",
              icon("caret-up")
            )
          ),
          div(
            id = "parametres_secondaires_content",
            class = "section-content",
            div(
              class = "parametres-wrapper",
              collapseUI(
                id = "dates_periodes",
                title = h6(i18n$t("Dates et périodes")),
                content = div(
                  dwr_slider("start_hist", globals, i18n),
                  dwr_slider("start_year", globals, i18n),
                  dwr_slider("end_year", globals, i18n)
                )
              )
            ),
            div(
              class = "parametres-wrapper",
              collapseUI(
                id = "multiplicateurs",
                title = h6(i18n$t("Multiplicateurs et réaction budgétaire")),
                content = div(
                  dwr_slider("og_dep", globals, i18n),
                  dwr_slider("og_po", globals, i18n),
                  dwr_slider("loss_d", globals, i18n),
                  dwr_slider("loss_df", globals, i18n),
                  hr(),
                  splitLayout(
                    prettyCheckbox(
                      inputId = "autorule",
                      label = i18n$t("Réaction budgétaire"),
                      value = TRUE, 
                      status = "success"
                    ),
                    actionBttn(
                      inputId = "refreshrule",
                      style = "minimal",
                      color = "default",
                      label = NULL,
                      icon = icon_colored("refresh", color = "#222222", size ="xs"),
                      size ="xs"
                    ),
                    cellWidths = c("80%", "20%"),
                    cellArgs = list(style ='font-size : 15px !important;')
                  ),
                  uiOutput("rule")
                )
              )
            ),
            div(
              class = "parametres-wrapper",
              collapseUI(
                id = "dette_monnaie",
                title = h6(i18n$t("Dette et monnaie")),
                content = div(
                  dwr_slider("r_dette", globals, i18n),
                  dwr_slider("dstarr", globals, i18n),
                  dwr_slider("r_mat", globals, i18n),
                  dwr_slider("pot_r", globals, i18n),
                  dwr_slider("ec_mce", globals, i18n),
                  dwr_slider("infdep_mce", globals, i18n),
                  dwr_slider("infdep_star", globals, i18n),
                  dwr_slider("potdep_mce", globals, i18n),
                  dwr_slider("potdep_star", globals, i18n)
                )
              )
            ),
            div(
              class = "parametres-wrapper",
              collapseUI(
                id = "parametres-estimes",
                title = h6(i18n$t("Ajustements dynamiques")),
                content = div(
                  dwr_slider("og_mce", globals, i18n),
                  dwr_slider("og_lag", globals, i18n),
                  dwr_slider("tcho_okun", globals, i18n),
                  dwr_slider("tcho_mce", globals, i18n),
                  dwr_slider("tvnairu_mce", globals, i18n),
                  dwr_slider("inf_tcho", globals, i18n),
                  dwr_slider("inf_lag", globals, i18n),
                  dwr_slider("inf_mce", globals, i18n)
                )
              )
            ),
            div(
              class = "parametres-wrapper",
              collapseUI(
                id = "conditions-initiales",
                title = h6(i18n$t("Conditions initiales")),
                content = div(
                  dwr_slider("i_tdeppp", globals, i18n),
                  dwr_slider("i_tpo", globals, i18n)
                )
              )
            ),
            div(
              class = "parametres-wrapper",
              collapseUI(
                id = "montecarlo",
                title = h6(i18n$t("Montecarlo")),
                content = div(
                  checkboxInput(
                    inputId = "go_mc",
                    label = i18n$t("Montecarlo"),
                    value = TRUE
                  ),
                  selectInputWrapper(
                    inputId = "draws",
                    label = i18n$t("Nombre de tirages"),
                    help_text = help_text("draws", globals$sliders),
                    value = 5000,
                    choices = c(100, 1000, 5000, 10000),
                    selected = 5000
                  ),
                  dwr_slider("ogn_ar", globals, i18n),
                  dwr_slider("ogn_sigma", globals, i18n)
                )
              )
            )
          )
        ) # div paramètres secondaires
      ), # column siberbar panel
      # ------ MAIN ------------------------------------------------------------
      column(
        id = "main_panel",
        width = 9,
        div(
          class = "introduction",
          div(
            class = "first",
            div(
              class = "intro-content-wrapper",
              p(
                class = "intro-content-header",
                "Debtwatch permet de calculer la politique budgétaire pour atteindre une cible de dette.", 
                tags$br(),
                tags$a(tags$img(src="pdf-download.png"), 
                       tags$small("fr"),
                       href="https://www.ofce.sciences-po.fr/pdf/pbrief/2021/OFCEpbrief93.pdf", 
                       download="OFCE policy brief 93 la dette publique au XXIe siecle.pdf"),
                tags$a(tags$small("en"), 
                       href= "https://www.ofce.sciences-po.fr/pdf/pbrief/2021/OFCEpbrief96.pdf",
                       download = "OFCE policy brief 96 Public Debt in the XXIst century.pdf"),
                tags$small("Policy Brief de l'OFCE n°93, la dette publique au XXIe siècle"),
                tags$br(),
                tags$img(src="new.png", width = 25, height = 25), tags$small("données AMECO printemps 2022 (04/05/2022)"),
                tags$br(),
                tags$small("Debtwatch ©2021-22 Timbeau, Heyer, Aurissergues sous licence CeCILL-B")
              ),
              tags$hr(),
              p(
                class = "intro-more",
                a(
                  id = "intro_more",
                  href = "#",
                  class = "section-more",
                  "Lire plus"
                )
              )
            )
          ),
          div(
            id = "intro_content",
            class = "intro-content section-content",
            tabsetPanel(
              id="intro-tabset",
              tabPanel(
                id = "intro-intro",
                title = "premiers pas",
                includeMarkdown(
                  path = "help/introduction.md"
                )
              ),
              tabPanel(
                id = "intro-faq",
                title = "faq",
                includeMarkdown(
                  path = "help/faq.md"
                )
              ),
              tabPanel(
                id = "intro-equa",
                title="équations",
                div(
                  includeMarkdown(
                    path = "help/equations.md"
                  ),
                  katex_equations
                )
              ),
              tabPanel(
                title = "références",
                id = "intro-ref",
                includeMarkdown(
                  path = "help/references.md"
                )
              ),
              tabPanel(
                id = "intro-credit",
                title = "crédits",
                includeMarkdown(
                  path = "help/credits.md"
                )
              ),
              tabPanel(
                id = "intro-legal",
                title = "mentions légales",
                includeMarkdown(
                  path = "help/legal.md"
                )
              )
            )
          )
        ),
        fluidRow(
          class = "graphiques",
          lapply(globals$panels$ids, function(panel) {
            graphique_ui(
              id = panel,
              title = globals$panels$names[[panel]],
              header = globals$panels$header[[panel]],
              note = globals$panels$note[[panel]],
              collapsed = globals$panels$collapsed[[panel]],
              i18n = i18n
            )
          })
        )
      ),
      # Must be at the end to load HTML first
      includeScript("www/js/slider_collapse.js"),
      includeScript("www/js/collapse.js"),
      includeScript("www/js/tooltip.js"),
      includeScript("www/js/widgetwrappers.js"),
      includeScript("www/js/copy_graph.js")
    ) # fluidRow body
  ) # fluidpage
} # function ui

## server -----------
server <- function(input, output, session) {
  observers <- reactiveVal(list())
  sim_counter <- reactiveVal(0)
  
  # ------ * Collapse sidebar --------------------------------------------------
  observeEvent(input$toggle_sidebar, {
    shinyjs::toggle(
      id = "sidebar_panel",
      anim = TRUE,
      time = 1,
      animType = "fade"
    )
  })
  
  # reactive values and val -----------------------
  scenarii <- reactiveVal(tibble::tibble())
  scenarii_saved <- reactiveVal(tibble::tibble())
  presets <- reactiveVal(g_presets)
  
  # ------ * Feed scenarii_saved() with cookies --------------------------------
  # input$scenarii is created from www/js/get_cookies.js
  observeEvent(input$scenarii, {
    all_scenarii <- strsplit(input$scenarii, ",")[[1]]
    for (uuid in all_scenarii) {
      p_found <- mongo_get_param(uuid, globals$mmg)
      found <- tibble::tibble(
        uuid = uuid,
        nom_fr = str_c(p_found$country, " (log)"),
        nom_en = "",
        comment_fr = "",
        comment_en = "",
        country = p_found$country,
        p = list(p_found),
        pwr = list(p_found),
        preset = FALSE,
        actuel = FALSE,
        saved = TRUE,
        graph = FALSE,
        index = 0,
        sim = NULL
      )
      scn_s <- scenarii_saved()
      scenarii_saved(bind_rows(scn_s, found))
    }
  })
  
  # ------ * Cookies: Load language --------------------------------------------
  # input$lang is created from www/js/get_cookies.js
  observeEvent(input$lang, {
    if (input$lang == "en") {
      shinyjs::click(id = "language_en")
    }
  })
  
  querycount <- reactiveVal(0)
  lang_chgt <- reactiveVal(FALSE)
  force_uuid <- reactiveVal(
    list(
      uuid = "",
      params = NULL,
      country = NULL,
      precalc = FALSE,
      ruleok =FALSE
    )
  )
  
  record <- reactiveVal(
    list(
      p = NULL,
      previous_p = NULL,
      simulations = tibble::tibble(simulation = list()),
      h = NULL,
      df = tibble::tibble(),
      start_year = globals$start,
      periods = globals$periods,
      country = "FRA",
      uuid = "",
      rule_text = "",
      no_log = "",
      is_precalc = FALSE,
      seed = NULL,
      uuid_found = NULL,
      p_found = NULL
    )
  )
  
  refresh_rule_click <- reactiveVal(0)
  
  refresh_rule <- reactiveVal(TRUE)
  
  country_set <- reactiveValues(
    countries = globals$countries,
    countries_ln_fr = globals$cty2code,
    params = NULL,
    p_init = NULL,
    p_def = NULL,
    historical_data = NULL,
    init = NULL,
    years = NULL,
    country = NULL,
    start_year = NULL,
    periods = NULL
  )
  # observe Event ----------------------------
  ## ------ * Conditions initiales (UI) -----------------------------------------
  # observeEvent(input$start_year, {
  #   shinyjs::html(
  #     selector = "#widget-label-i_tdeppp span",
  #     html = HTML(paste(i18n$t("Calage DP/DIB"), input$start_year))
  #   )
  #   shinyjs::html(
  #     selector = "#widget-label-i_tpo span",
  #     html = HTML(paste(i18n$t("Calage DP/DIB"), input$start_year))
  #   )
  # })
  
  observeEvent(input$refreshrule, {
    dfr <- refresh_rule_click() + 1
    refresh_rule_click(dfr)
    refresh_rule(TRUE)
  },
  priority = 1
  )
  
  ## country_set ----------------------------
  observeEvent(list(input$country, input$ameco, querycount()), {
    country <- input$country
    start <- req(input$start_year)
    hist <- req(input$start_hist)
    periods <- req(input$end_year) - start
    draws <- req(input$draws)
    scn <- isolate(scenarii())
    if(is.null(country_set$country) || country_set$country != country) {
      ameco <- get_ameco(reset=FALSE, countries = globals$countries, variables = globals$variables, input$ameco) |>
        dplyr::mutate(ccode = globals$ivariables[code])
      dandp <- dataandparams(
        country = country,
        start_year = start,
        periods = periods,
        draws = draws,
        globals = globals,
        ameco = ameco)
      
      country_set$p_init <- dandp$p_init
      country_set$p_def <- dandp$p_def
      country_set$historical_data <- dandp$historical_data
      country_set$init <- dandp$init
      country_set$years <- dandp$years
      country_set$country <- dandp$country
      country_set$start_year <- dandp$start_year
      country_set$periods <- dandp$periods

      new_presets <- calc_presets(country=country, globals, start_year = start, periods = periods) 
      if(nrow(scn)>0) {
        scn <- scn |> 
          filter(!preset) |>
          mutate(
            actuel = FALSE,
            graph = FALSE, 
            relatif = FALSE, 
            index = dplyr::row_number()
          )
        updatePrettyToggle(session=session, inputId = "relatif_actuel", value=TRUE)
        iscn <- max(scn$index)
      } else {
        iscn <- 0
      }
      
      new_presets <- new_presets |>
        mutate(index = iscn + dplyr::row_number())
      presets(new_presets)
      
      scn <- bind_rows(scn, new_presets)
      scenarii(scn)
      
      scn_s <- scenarii_saved()
      scn_s <- scn_s |> 
        mutate(graph=FALSE)

      scenarii_saved(scn_s)
      
      if(force_uuid()$uuid=="") {
        uuid_ameco <- scn |>
          dplyr::filter(nom_fr == str_c("AMECO ", ameco_version) & country==!!country) |>
          dplyr::pull(uuid) |>
          purrr::pluck(1)
        
        if(!is.null(uuid_ameco)) {
          prms <- scn |>
            dplyr::filter(nom_fr == str_c("AMECO ", ameco_version) & country==!!country) |>
            dplyr::pull(pwr) |>
            purrr::pluck(1)
          updateTextInput(session=session, "nom_actuel", value = str_c("AMECO ", ameco_version))
          force_uuid(
            list(
              uuid = uuid_ameco,
              params = prms,
              country = country,
              precalc = TRUE,
              ruleok = TRUE
            )
          )
        }
      }
    }
    
    if(force_uuid()$uuid=="")
      prms <- country_set$p_def
    else
      prms <- force_uuid()$params
    set_dwrsliders(prms, globals, session, input)
    
    country_set$params <- prms
  })
  
  # dates et périodes ----------------------------------------------
  observeEvent(list(input$start_hist, input$start_year), priority = 3, {
    # dans ce cas freezer les values bloque tout
    updateSliderInput2(session=session, "start_year", min=input$start_hist+1)
    updateSliderInput2(session=session, "start_hist",  max=input$start_year-1)
    
    shinyjs::html(
      selector = "#widget-label-i_tdeppp span",
      html = HTML(paste(i18n$t("Calage DP/DIB"), input$start_year))
    )
    shinyjs::html(
      selector = "#widget-label-i_tpo span",
      html = HTML(paste(i18n$t("Calage DP/DIB"), input$start_year))
    )
    
  })
  
  observeEvent(list(input$ameco), priority = 3, {
    dates <- get_ameco_date(input$ameco)
    updateSliderInput2(session=session, "start_hist",  max=dates[["max"]]-1, min = dates[["min"]])
    updateSliderInput2(session=session, "start_year", min=input$start_hist+1, value = dates[["max"]], max = dates[["max"]])
    updateSliderInput2(session=session, "start_hist",  max=dates[["max"]]-1)
    
  })
  
  # observeEvent(input$end_year, priority = 0, {
  #   freezeReactiveValue(input, "start_year")
  #   updateSliderInput2(session=session, "start_year", max=min(input$end_year-1, 2022))
  # })
  
  # simulation ----------------------------
  simulation <- reactive({
    
    req(isolate(country_set$country))
    tictoc::tic()
    les_inputs <- transform_params(req(input), globals)
    irr <- input$refreshrule
    isolate({ # on isolate à partir de là comme ça pas de mauvaise surprise
      ar <- les_inputs$autorule
      tt <- i18n$t
      old_record <- record()
      f_uuid <- force_uuid()
      # if preset simulation is probably cached
      scns <- scenarii()
      scns <- scns |>
        dplyr::mutate(graph = dplyr::if_else(saved & actuel, FALSE, graph))
      ameco <- get_ameco(reset=FALSE, countries = globals$countries, variables = globals$variables, les_inputs$ameco) |>
        dplyr::mutate(ccode = globals$ivariables[code])
      c_s <- dataandparams(
        country = input$country, 
        start_year = input$start_year, 
        periods = input$end_year-input$start_year,
        draws=input$draws,
        globals = globals,
        ameco = ameco
      )
      rrefresh <- irr != old_record$irr || is.null(old_record$p$tpo_og) || ar
      pp <- set_params(les_inputs, globals, c_s)
      if(is.null(old_record$seed))
        seed <- as.integer(runif(1, 1, 10^9))
      else 
        seed <- old_record$seed
      pp$p$seed <- seed
      
      if (rrefresh & !f_uuid$ruleok) {
        # la politique budgétaire
        showNotification(tt("Calcul en cours"), type = "warning", duration = 3)
        pp$p <- calc_rule_params(globals = globals, params = pp$p, draws = 1000)
      } else {
        if(f_uuid$ruleok)
        {
          pp$p$tpo_dstar <- f_uuid$params$tpo_dstar
          pp$p$tpo_sstar <- f_uuid$params$tpo_sstar
          pp$p$tpo_og <- f_uuid$params$tpo_og  
        } else {
          pp$p$tpo_dstar <- old_record$p$tpo_dstar
          pp$p$tpo_sstar <- old_record$p$tpo_sstar
          pp$p$tpo_og <- old_record$p$tpo_og
        }
      }
      if(!is.null(pp$p$error)&&pp$p$error) { 
        showNotification(tt("Echec de la simulation"), duration=15, type="error")
        return(NULL)
      }
      new_record <- log_params(
        pp,
        old_record, 
        c_s,
        globals$ts,
        input$remote_addr
      )
      
      if (f_uuid$uuid == "") {
        new_record$uuid <- mongo_log(
          new_record$df |> dplyr::slice_tail(), 
          globals$mmg)
      } else {
        new_record$uuid <- mongo_log(
          new_record$df |>
            dplyr::slice_tail() |>
            dplyr::select(1:4) |>
            mutate(ameco = les_inputs$ameco),
          globals$mmg,
          no_log = f_uuid$uuid)
      }
      
      new_record$irr <- irr
      
      new_record$p <- pp$p
      new_record$seed <- NULL # il sera donc retiré la prochaine fois
      
      if (!check_params(pp$p)) {
        return(NULL)
      }
      time <- tictoc::toc(quiet = TRUE)
      tictoc1 <- time$toc - time$tic
      tictoc::tic()
      # la simulation
      if (!f_uuid$precalc) {
        sim <- calc_sim_dust(pp$p, globals$model, history = pp$h)
        sim_counter(isolate(sim_counter()) + 1)
        message_sim <- tt("réussie")
      } else {
        sim <- scns |>
          dplyr::filter(uuid == f_uuid$uuid) |>
          dplyr::pull(sim) |>
          purrr::pluck(1)
        message_sim <- tt("en cache")
      }
      prev_sim <- old_record$simulations |>
        dplyr::slice_tail() |>
        dplyr::pull(simulation) |>
        purrr::pluck(1)
      new_record$simulations <- dplyr::bind_rows(
        old_record$simulations, 
        tibble::tibble(simulation = list(sim)))
      new_record$ruletext <- do_rule_text(
        tpo_dstar = pp$p$tpo_dstar,
        tpo_sstar = pp$p$tpo_sstar,
        tpo_og = pp$p$tpo_og)
      new_record$data <- add_previous(sim, prev_sim)
      nom <- input$nom_actuel
      chgt_sim <- !f_uuid$precalc &
        (!is.null(nom) && nom %in% (scns |> dplyr::filter(preset) |> dplyr::pull(nom_fr)))
      
      no_name <- is.null(nom) || stringr::str_detect(nom, "[:upper:]{3} #[:digit:]*")
      
      if (chgt_sim | no_name) {
        freezeReactiveValue(input, "nom_actuel")
        nom <- stringr::str_c(c_s$country, " #", sim_counter())
        updateTextInput(session = session, inputId = "nom_actuel", value = nom)
      }
      new_record$is_precalc <- FALSE
      new_record$country <- c_s$country
      new_scenarii <- actuel2scenarii(new_record, scns, nom = nom, relatif = !input$relatif_actuel)
      scn_s <- scenarii_saved()
      if(nrow(scn_s)>0) {
        if(any(scn_s$actuel))
          scenarii_saved(scn_s |> mutate(actuel = FALSE))
        if(new_record$uuid%in%scn_s$uuid)
          scenarii_saved(scn_s |> mutate(actuel = if_else(uuid == new_record$uuid, TRUE, FALSE)))
      }
      updatePrettyCheckbox(session, "graph_actuel", label = NULL, value = TRUE)
      time <- tictoc::toc(quiet = TRUE)
      tictoc2 <- time$toc - time$tic
      showNotification(
        ui = "Simulation {new_record$uuid} {message_sim} {str_c(signif(tictoc1+tictoc2, 2),'s (', signif(tictoc2, 1), 's)')}" |> glue::glue(),
        type = "message"
      )
    }) # isolate 
    force_uuid(list(uuid = "", params = NULL, country = NULL, precalc = FALSE, ruleok=FALSE))
    scenarii(new_scenarii)
    record(new_record)
    return(new_record)
  })
  
  # dynamic UI --------------------
  # output$ci_tdeppp <- renderUI({
  #   ciy <- input$start_year
  #   if (ciy > globals$years[[2]] - 2) {
  #     tagList(sliderInput("i_tdeppp",
  #                         label = "Calage DP/PIB {ciy}" |> glue::glue() |> slid(),
  #                         min = -0.05, max = 0.05, value = 0, ticks = FALSE
  #     ))
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # output$ci_tpo <- renderUI({
  #   ciy <- input$start_year
  #   if (ciy > globals$years[[2]] - 2) {
  #     tagList(sliderInput("i_tpo",
  #                         label = "Calage TPO/PIB {ciy}" |> glue::glue() |> slid(),
  #                         min = -0.05, max = 0.05, value = 0, ticks = FALSE
  #     ))
  #   } else {
  #     NULL
  #   }
  # })
  
  # scenarii panel ---------------------
  scenarii_server(
    input,
    output,
    session,
    scenarii,
    scenarii_saved,
    record,
    simulation,
    presets,
    observers,
    force_uuid,
    querycount,
    globals,
    i18n)

  # go plots -------------------
  pre_plot <- reactive(pre_plot_sim(scenarii(), simulation(), gl = globals, i18n = i18n))
  
  # ------ * Graphiques --------------------------------------------------------
  lapply(
    globals$panels$ids,
    function(panel) {
      graphique_server(
        id = panel,
        pre_plot = reactive(pre_plot()),
        vvars = globals$panels$vars[[panel]],
        reactive(simulation()),
        globals,
        i18n
      )
    }
  )
  
  output$rule <- renderUI(
    HTML(
      katex::katex_html(
        record()$ruletext,
        displayMode = FALSE,
        preview = FALSE,
        output = "html"
      )
    )
  )
  
  # uuid quand appelé, si il y en a --------------------
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query$id))
      return(NULL)
    uuid <- query$id
    p <- mongo_get_param(uuid, globals$mmg)
    if(is.null(p))
      return(NULL)
    querycount(isolate(querycount())+1)  
    
    rec <- isolate(record())
    rec <- purrr::list_modify(rec, seed = p$seed, no_log="", uuid = uuid, is_precalc = FALSE)
    record(rec)
    nom <- glue::glue("{p$country} (url)")
    tt <- isolate(i18n$t)
    force_uuid(list(
      uuid = uuid,
      params = p,
      country = p$country,
      precalc = FALSE,
      ruleok = TRUE
    ))
    
    message <- "{tt('Scénario')} {uuid}, '{nom}' {tt('requis par url')}" |> glue::glue()
    showNotification(message, type="message")
    # on fait ça pour être sûr que le pays est bien dans le sélecteur
    freezeReactiveValue(input, "country")
    updatePickerInput(session, "country", selected = p$country )
    updateTextInput(session, "nom_actuel", value = nom)
  }
  )
  
  # ------ * Languages ---------------------------------------------------------
  observeEvent(input$language_fr, {
    shiny.i18n::update_lang(session, "fr")
    lang_chgt(TRUE)
    # Translate countries
    
    updatePickerInput(
      session = session,
      inputId = "country",
      choices = globals$code2cty,
      selected = input$country,
      choicesOpt = countries_with_flag(globals$code2cty, globals$flags)
    )
    
    # Translate intro
    shinyjs::html(
      selector = ".intro-content-header",
      html = as.character(
        p(
          class = "intro-content-header",
          "Debtwatch permet de calculer la politique budgétaire pour atteindre une cible de dette.", 
          tags$br(),
          tags$a(tags$img(src="pdf-download.png"), 
                 tags$small("fr"),
                 href="https://www.ofce.sciences-po.fr/pdf/pbrief/2021/OFCEpbrief93.pdf", 
                 download="OFCE policy brief 93 la dette publique au XXIe siecle.pdf"),
          tags$a(tags$small("en"), 
                 href= "https://www.ofce.sciences-po.fr/pdf/pbrief/2021/OFCEpbrief96.pdf",
                 download = "OFCE policy brief 96 Public Debt in the XXIst century.pdf"),
          tags$small("Policy Brief de l'OFCE n°93, la dette publique au XXIe siècle"), 
          tags$br(),
          tags$img(src="new.png", width = 20, height = 20), tags$small("données AMECO hiver 2021 (11/11/2021)"),
          tags$br(),
          tags$small("Debtwatch ©2021 Timbeau, Heyer, Aurissergues sous licence CeCILL-B")
        )
      )
    )
    
    # Translation intro content
    
    shinyjs::html(
      id = "intro_content",
      html = as.character(
        div(
          class = "intro-content section-content",
          tabsetPanel(
            id="intro-tabset",
            tabPanel(
              title = "premier pas",
              includeMarkdown(
                path = "help/introduction.md"
              )
            ),
            tabPanel(
              title = "faq",
              includeMarkdown(
                path = "help/faq.md"
              )
            ),
            tabPanel(
              id = "intro-equa",
              title="équations",
              div(
                includeMarkdown(
                  path = "help/equations.md"
                ),
                katex_equations
              )
            ),
            tabPanel(
              title = "références",
              includeMarkdown(
                path = "help/references.md"
              )
            ),
            tabPanel(
              title = "crédits",
              includeMarkdown(
                path = "help/credits.md"
              )
            ),
            tabPanel(
              id = "intro-legal",
              title = "mentions légales",
              includeMarkdown(
                path = "help/legal.md"
              )
            )
          )
        )
      )
    )
    
    # Translate "Tout lire"
    shinyjs::html(
      id = "intro_more",
      html = "Tout lire"
    )
    
    # Translate tooltips
    for (id in names(globals$sliders)) {
      title <- globals$sliders[[id]][["help_code"]]
      if (is.na(title))
        title <- ""
      shinyjs::runjs(glue::glue(
        "$('#{id}_tooltip').attr('data-original-title', '{title}')"
      ))
    }
    
    # Set cookies
    shinyjs::runjs("Cookies.set('lang', 'fr', { expires: 365 })")
  })
  
  observeEvent(input$language_en, {
    shiny.i18n::update_lang(session, "en")
    lang_chgt(TRUE)
    # Translate countries
    choices <- globals$code2cty
    names(choices) <- translation_en[translation_en$fr %in% names(choices), "en"]
    updatePickerInput(
      session = session,
      inputId = "country",
      choices = choices,
      selected = input$country,
      choicesOpt = countries_with_flag(choices, globals$flags)
    )
    
    # Translate intro
    shinyjs::html(
      selector = ".intro-content-header",
      html = as.character(
        p(
          class = "intro-content-header",
          "Debtwatch computes fiscal policy to meet a public debt target.", 
          tags$br(),
          tags$a(tags$img(src="pdf-download.png"), 
                 tags$small("fr"),
                 href="https://www.ofce.sciences-po.fr/pdf/pbrief/2021/OFCEpbrief93.pdf", 
                 download="OFCE policy brief 93 la dette publique au XXIe siecle.pdf"),
          tags$a(tags$small("en"), 
                 href= "https://www.ofce.sciences-po.fr/pdf/pbrief/2021/OFCEpbrief96.pdf",
                 download = "OFCE policy brief 96 Public Debt in the XXIst century.pdf"),
          tags$small("OFCE Policy Brief #96, Public debt in the XXIst century"), 
          tags$br(),
          tags$img(src="new.png", width = 20, height = 20), tags$small("data from AMECO winter 2021 (11/11/2021)"),
          tags$br(),
          tags$small("Debtwatch ©2021 Timbeau, Heyer, Aurissergues under CeCILL-B licence")
        )
      )
    )
    
    # Translation intro content
    
    shinyjs::html(
      id = "intro_content",
      html = as.character(
        div(
          class = "intro-content section-content",
          tabsetPanel(
            id="intro-tabset",
            tabPanel(
              title = "first steps",
              includeMarkdown(
                path = "help/introduction-en.md"
              )
            ),
            tabPanel(
              title = "faq",
              includeMarkdown(
                path = "help/faq-en.md"
              )
            ),
            tabPanel(
              id = "intro-equa",
              title="equations",
              div(
                includeMarkdown(
                  path = "help/equations-en.md"
                ),
                katex_equations
              )
            ),
            tabPanel(
              title = "references",
              includeMarkdown(
                path = "help/references-en.md"
              )
            ),
            tabPanel(
              title = "credits",
              includeMarkdown(
                path = "help/credits-en.md"
              )
            ),
            tabPanel(
              id = "intro-legal",
              title = "legals",
              includeMarkdown(
                path = "help/legal.md"
              )
            )
          )
        )
      )
    )
    
    # Translate "Tout lire"
    shinyjs::html(
      id = "intro_more",
      html = "Read everything"
    )
    
    # Translate tooltips
    for (id in names(globals$sliders)) {
      title <- globals$sliders[[id]][["help_code_en"]]
      if (is.na(title)) 
        title <- ""
      shinyjs::runjs(glue::glue(
        "$('#{id}_tooltip').attr('data-original-title', '{title}')"
      ))
    }

    # Set cookies
    shinyjs::runjs("Cookies.set('lang', 'en', { expires: 365 })")
  })
}

# shinyApp ---------------------------------
shinyApp(ui = ui, server = server)
