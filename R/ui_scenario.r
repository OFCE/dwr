# helpers ----------------------------------

tag5col <- tags$colgroup(
  tags$col(width = "64%"),
  tags$col(width = "12%", align = "center"),
  tags$col(width = "8%", align = "center"),
  tags$col(width = "8%", align = "center"),
  tags$col(width = "8%", align = "center")
)

small_eye <- icon_colored("eye", "#111111", size = "xs")
small_noeye <- icon_colored("eye-slash", "#111111", size = "xs")
small_anchor <- icon_colored("anchor", "#111111", size = "xs")
small_line <- icon_colored("chart-line", "#111111", size = "xs")
small_trash <- icon_colored("trash", "#111111", size = "xs")
small_sliders <- icon_colored("sliders-h", "#111111", size = "xs")

smalldownloadBttn <- function(outputId, style = "minimal",
                              color = "primary", block = FALSE,
                              no_outline = TRUE) {
  bttn <- shinyWidgets::actionBttn(
    inputId = paste0(outputId, "_bttn"),
    label = tags$a(
      id = outputId,
      class = "shiny-download-link",
      href = "",
      target = "_blank",
      download = NA
    ),
    color = color,
    style = style,
    size = "xs",
    block = block,
    no_outline = no_outline,
    icon = icon_colored("download", "#111111", size = "xs")
  )
  htmltools::tagAppendAttributes(
    bttn,
    onclick = sprintf("getElementById('%s').click()", outputId)
  )
}

set_dwrsliders <- function(params, globals, session, input) {

  params <- undo_transform(params, globals)
  params$autorule <- TRUE
  params$country <- NULL # attention, le pays ne sera pas modifié donc
  inputs_list <- names(isolate(reactiveValuesToList(input)))
  
  for (slider in intersect(names(params), inputs_list)) {
    freezeReactiveValue(input, slider)
    updateSliderInput2(session = session, inputId = slider, value = params[[slider]])
  }
}

# ui ----------------------

scenarii_panel <- function(presets, globals, i18n) {
  div(
    class = "parametres-wrapper",
    tags$style(
      type = "text/css",
      "input[type=text] {font-size:12px; height:26px}
                 input[type=text] {border: none; outline: none; border-width: 0; box-shadow: none;}}"
    ),
    ## table actuel -----------------------
    tags$table(
      style = "width:100%; table-layout:fixed; font-size:12px;border-spacing: 0",
      tag5col,
      tags$tr(
        tags$th(i18n$t("Scénario actuel"), style = "padding-top:1em")
      ),
      tags$tr(
        tags$td(
          textInput(
            "nom_actuel",
            label = NULL,
            value = "AMECO printemps 2021",
            placeholder = "nom du scénario..."
          )
        ),
        tags$td(
          textOutput("uuid"),
          style = "font-size:12px;"
        ),
        tags$td(
          shinyWidgets::prettyToggle(
            inputId = "relatif_actuel", value = TRUE, inline = TRUE, label_on = NULL, label_off = NULL,
            outline = FALSE,
            plain = TRUE,
            icon_on = small_line,
            icon_off = small_anchor,
            fill = FALSE,
            status_on = "default",
            status_off = "default"
          )
        ),
        tags$td(),
        tags$td(
          shinyWidgets::actionBttn(
            "save_actuel",
            label = NULL,
            icon = icon_colored("save", "#111111", size = "xs"),
            size = "xs",
            style = "minimal"
          )
        )
      ),
      tags$tr(
        tags$th(
          i18n$t("Scénarios prédéfinis"),
          style = "padding-top:0.5em"
        )
      ),
      ## presets -----------------------
      tagList(
        purrr::map(
          1:nrow(presets),
          ~ tags$tr(
            style = "padding-top:0em",
            tags$td(
              i18n$t(presets[.x, ]$nom_fr),
              style = "font-size:12px;"
            ),
            tags$td(
              presets[.x, ]$uuid,
              style = "font-size:12px;"
            ),
            tags$td(
              shinyWidgets::prettyToggle(
                inputId = stringr::str_c("graph_preset_", .x),
                value = FALSE, inline = TRUE,
                label_on = NULL,
                label_off = NULL,
                outline = FALSE,
                plain = TRUE,
                icon_on = small_eye,
                icon_off = small_noeye,
                status_on = "default",
                status_off = "default"
              )
            ),
            tags$td(
              shinyWidgets::actionBttn(
                stringr::str_c("upload_preset_", .x),
                label = NULL,
                icon = icon_colored("sliders-h", "#111111", size = "xs"),
                size = "xs",
                style = "minimal"
              )
            )
          )
        )
      )
    ),
    tags$table(
      style = "width:100%; table-layout:fixed; font-size:12px;border-spacing: 0",
      tag5col,
      tags$tr(
        tags$th(i18n$t("Scénarios enregistrés"),
                style = "padding-top:0.5em"
        )
      )
    ),
    ## saved --------------------------
    uiOutput("panel_saved"),
    ## find -------------------
    tags$table(
      style = "width:100%; table-layout:fixed; font-size:12px;border-spacing: 0",
      tags$colgroup(
        tags$col(width = "40%"),
        tags$col(width = "12%", align = "center"),
        tags$col(width = "12%", align = "center"),
        tags$col(width = "12%", align = "center"),
        tags$col(width = "12%", align = "center"),
        tags$col(width = "12%", align = "center")
      ),
      tags$tr(
        tags$th(i18n$t("Chercher un scénario"), style = "padding-top:1em")
      ),
      tags$tr(
        tags$td(
          textInput(
            "find_uuid",
            label = NULL,
            value = "",
            placeholder = "uuid..."
          )
        ),
        tags$td(
          shinyWidgets::prettyToggle(
            "okfound",
            label_on = NULL,
            label_off = NULL,
            icon_on = icon_colored("check", color = "#00FF00"),
            icon_off = icon_colored("times", color = "#FF0000"),
            plain = TRUE,
            outline = FALSE,
            fill = FALSE,
            inline = TRUE,
            status_off = "default",
            status_on = "default"
          ) |> shinyjs::hidden() |> shinyjs::disabled()
        ),
        tags$td(
          shinyWidgets::actionBttn(
            "go_find",
            label = NULL,
            icon = icon_colored("search", "#111111", size = "xs"),
            size = "xs",
            style = "minimal"
          )
        ),
        tags$td(
          shinyWidgets::actionBttn(
            "upload_found",
            label = NULL,
            icon = icon_colored("sliders-h", "#111111", size = "xs"),
            size = "xs",
            style = "minimal"
          ) |> shinyjs::hidden(),
          tags$td(
            shinyWidgets::actionBttn(
              "save_found",
              label = NULL,
              icon = icon_colored("save", "#111111", size = "xs"),
              size = "xs",
              style = "minimal"
            ) |> shinyjs::hidden()
          )
        )
      )
    ),
    ## down ---------------------------
    hr(),
    tags$div(
      style = "font-size:12px;",
      smalldownloadBttn(
        "dwn_scn_csv",
        style = "bordered",
        color = "default"
      ) |> shinyjs::hidden(),
      i18n$t("tous les scénarios en csv")
    )
  )
}


# server part ------------------------
scenarii_server <- function(input, output, session, scenarii, scenarii_saved,
                            record, sim, presets, observers, force_uuid, querycount, globals, i18n) {
  # uuid dans la ligne actuel
  tt <- i18n$t
  output$uuid <- renderText(sim()$uuid)
  outputOptions(output, "uuid", suspendWhenHidden = FALSE)
  # panel_saved ---------------------------
  output$panel_saved <- renderUI({
    scn_s <- scenarii_saved()
    
    obss <- isolate(observers())
    if (nrow(scn_s) > 0) {
      rows_HTML <-
        tagList(
          purrr::map(
            1:nrow(scn_s),
            ~ tags$tr(
              style = "padding-top:0em",
              tags$td(
                tt(scn_s[.x, ]$nom_fr),
                style = "font-size:12px;"
              ),
              tags$td(
                scn_s[.x, ]$uuid,
                style = "font-family:'Source Sans Pro'; font-size:12px;"
              ),
              tags$td(
                if (!scn_s[.x, ]$actuel) {
                  shinyWidgets::prettyToggle(
                    inputId = stringr::str_c("graph_saved_", scn_s[.x, ]$uuid),
                    value = scn_s[.x, ]$graph,
                    inline = TRUE,
                    label_on = NULL,
                    label_off = NULL,
                    outline = FALSE,
                    plain = TRUE,
                    icon_on = small_eye,
                    icon_off = small_noeye,
                    status_on = "default",
                    status_off = "default"
                  )
                },
                tags$td(
                  shinyWidgets::actionBttn(
                    inputId = stringr::str_c("upload_saved_", scn_s[.x, ]$uuid),
                    label = NULL,
                    icon = small_sliders,
                    size = "xs",
                    style = "minimal"
                  )
                ),
                tags$td(
                  shinyWidgets::actionBttn(
                    inputId = stringr::str_c("trash_saved_", scn_s[.x, ]$uuid),
                    label = NULL,
                    icon = small_trash,
                    size = "xs",
                    style = "minimal"
                  )
                )
              )
            )
          )
        )
      purrr::walk(
        1:nrow(scn_s),
        function(i) {
          uuid_s <- scn_s |>
            dplyr::slice(i) |>
            dplyr::pull(uuid)
          if (is.null(obss[[str_c(uuid_s, "_graph")]])) {
            obss[[str_c(uuid_s, "_graph")]] <<- observeEvent(input[[stringr::str_c("graph_saved_", uuid_s)]], {
              scn <- scenarii()
              scn_s <- scenarii_saved()
              ss <- scn_s |> filter(uuid==uuid_s)
              if(!uuid_s %in% scn$uuid) {
                p <- ss |> pull(pwr) |> pluck(1)
                dandp <- dataandparams(
                  country = ss |> pull(country) |> pluck(1), 
                  start_year = p$start_year, 
                  periods = input$end_year - p$start_year,
                  draws = p$draws, 
                  globals = globals
                )
                pp <- set_params(inputs = p, globals = globals,datas = dandp)
                sim <- calc_sim_dust(params=pp$p, model=globals$model, history=pp$h )
                scn <- bind_rows(
                  scn,
                  scn_s |> filter(uuid==uuid_s) |> mutate(sim = list(sim))
                )
              }
              scn <- scn |>
                dplyr::mutate(
                  graph = dplyr::if_else(uuid == uuid_s, input[[stringr::str_c("graph_saved_", uuid_s)]], graph)
                )
              if(nrow(scn |> filter(graph))<2) {
                scn |> mutate(relatif=FALSE)
                updatePrettyToggle(session=session, inputId = "relatif_actuel", value=TRUE)
              }
              scenarii(scn)
              scn_s <- scenarii_saved()
              scn_s <- scn_s |>
                dplyr::mutate(
                  graph = dplyr::if_else(uuid == uuid_s, input[[stringr::str_c("graph_saved_", uuid_s)]], graph)
                )
              scenarii_saved(scn_s)
            })
          }
          if (is.null(obss[[str_c(uuid_s, "_trash")]])) {
            obss[[str_c(uuid_s, "_trash")]] <<- observeEvent(input[[stringr::str_c("trash_saved_", uuid_s)]], {
              scn <- scenarii()
              scn_s <- scenarii_saved()
              is_actuel <- scn_s |> filter(uuid == uuid_s) |> pull(actuel) |> pluck(1)
              is_graphed <- scn |> filter(uuid == uuid_s) |> pull(graph) |> pluck(1) %||% FALSE
              if (!is_actuel&is_graphed) {
                scn <- scn |>
                  dplyr::mutate(graph = dplyr::if_else(uuid == uuid_s, FALSE, graph))
                if(nrow(scn |> filter(graph))<2) {
                  scn |> mutate(relatif=FALSE)
                  updatePrettyToggle(session=session, inputId = "relatif_actuel", value=TRUE)
                }
                scenarii(scn)
              }
              is_saved <- scn |> filter(uuid == uuid_s) |> pull(saved) |> pluck(1) %||% FALSE
              if (is_saved) {
                scn <- scn |>
                  dplyr::mutate(saved = dplyr::if_else(uuid == uuid_s, FALSE, saved))
                if(nrow(scn |> filter(graph))<2) {
                  scn |> mutate(relatif=FALSE)
                  updatePrettyToggle(session=session, inputId = "relatif_actuel", value=TRUE)
                }
                scenarii(scn)
              }
              scn_s <- scn_s |> dplyr::filter(uuid != uuid_s)
              scenarii_saved(scn_s)
              obss[[str_c(uuid_s, "_trash")]] <<- NULL
              obss[[str_c(uuid_s, "_graph")]] <<- NULL
              obss[[str_c(uuid_s, "_upload")]] <<- NULL
            }
            )
          }
          if (is.null(obss[[str_c(uuid_s, "_upload")]])) {
            obss[[str_c(uuid_s, "_upload")]] <<- observeEvent(input[[stringr::str_c("upload_saved_", uuid_s)]], {
              if (record()$uuid == uuid_s) 
                return(NULL)
              scn_s <- scenarii_saved()
              le_scn <- scn_s |> filter(uuid == uuid_s)
              le_nom <- le_scn$nom_fr
              # on charge le scénario dans actuel
              p_actuel <- le_scn$pwr |>
                purrr::flatten()
              p_actuel <- purrr::list_modify(
                p_actuel,
                uuid = purrr::zap(),
                ip = purrr::zap(),
                called = purrr::zap(),
                session_time = purrr::zap()
              )
              # on met les paramètres dans force_uuid
              force_uuid(
                list(
                  uuid = uuid_s,
                  params = p_actuel,
                  country = p_actuel$country,
                  precalc = TRUE,
                  ruleok = TRUE
                )
              )
              tt <- i18n$t
              message <- "{tt('Scénario')} {uuid_s}, '{le_nom}' {tt('chargé dans les sliders')}" |> glue::glue()
              
              querycount(querycount() + 1)
              
              updateTextInput(inputId = "nom_actuel", value = tt(le_nom))
              showNotification(message, type = "message")
              rec <- record()
              record(
                purrr::list_modify(
                  rec,
                  uuid = uuid_s,
                  no_log = uuid_s,
                  is_precalc = force_uuid()$precalc,
                  seed = p_actuel$seed
                )
              )
            }
            )
          }
        }
      )
      observers(obss)

    } else {
      rows_HTML <- tags$tr(tags$td(tt("pas de scénarii enregistrés")))
    }
    
    tags$table(
      style = "width:100%; table-layout:fixed; font-size:12px;border-spacing: 0",
      tag5col,
      rows_HTML
    )
  })
  
  outputOptions(output, "panel_saved", suspendWhenHidden = FALSE)
  
  # relatif ou non ---------------
  observeEvent(input$relatif_actuel, ignoreInit = TRUE, {
    scn <- scenarii()
    if(sum(scn$graph|scn$actuel)>1) {
      scn <- scn |> dplyr::mutate(relatif = dplyr::if_else(actuel, !input$relatif_actuel, FALSE))
      scenarii(scn)
    }
    else 
      shinyWidgets::updatePrettyToggle(
        session = session,
        inputId = "relatif_actuel", 
        value = TRUE
      )
  })
  # save actuel --------------
  observeEvent(input$save_actuel, ignoreInit = TRUE, {
    scn <- scenarii()
    to_save <- scn |>
      filter(actuel) |>
      mutate(saved = TRUE, graph = FALSE, actuel = TRUE)
    
    if(to_save$nom_fr=="AMECO 5/2021")
      to_save <- to_save |> mutate(nom_fr = str_c("AMECO 5/2021 (", to_save$country, ")"), saved = TRUE)
    
    scn_s <- scenarii_saved()
    if (to_save$uuid %in% scn_s$uuid) {
      return(NULL)
    }
    
    scn_s <- bind_rows(scn_s, to_save)
    scenarii_saved(scn_s)
    showNotification(
      ui = tt("Scénario sauvegardé"),
      session = session, type = "message", duration = 5
    )
  })
  # change le nom -------------------
  observeEvent(input$nom_actuel, ignoreInit = TRUE, {
    scn <- scenarii()
    scn <- scn |> dplyr::mutate(nom_fr = dplyr::if_else(actuel, input$nom_actuel, nom_fr))
    scenarii(scn)
  })
  
  # les presets ---------------
  purrr::walk(1:nrow(presets), ~ {
    observeEvent(input[[stringr::str_c("graph_preset_", .x)]], ignoreInit = TRUE, {
      scn <- scenarii()
      scn <- scn |>
        dplyr::mutate(graph = dplyr::if_else(uuid == presets[.x, ]$uuid & preset, input[[stringr::str_c("graph_preset_", .x)]], graph))
      if(nrow(scn |> filter(graph))<2) {
        scn |> mutate(relatif=FALSE)
        updatePrettyToggle(session = session, inputId = "relatif_actuel", value=TRUE)
      }
      scenarii(scn)

    })
    ## upload presets -------------------
    observeEvent(input[[stringr::str_c("upload_preset_", .x)]], ignoreInit = TRUE, {
      scn <- scenarii()
      le_uuid <- presets[.x, ]$uuid
      if (record()$uuid == le_uuid) {
        return(NULL)
      }
      le_nom <- presets[.x, ]$nom_fr
      
      # on charge le scénario dans actuel
      p_actuel <- scn |>
        dplyr::filter(uuid == le_uuid) |>
        dplyr::pull(pwr) |>
        purrr::flatten()
      p_actuel <- purrr::list_modify(
        p_actuel,
        uuid = purrr::zap(),
        ip = purrr::zap(),
        called = purrr::zap(),
        session_time = purrr::zap()
      )
      # on met les paramètres dans force_uuid
      tt <- i18n$t
      if (uuid_is_ok(le_uuid, globals$mmg)) {
        force_uuid(
          list(
            uuid = le_uuid,
            params = p_actuel,
            country = p_actuel$country,
            precalc = TRUE,
            ruleok = TRUE
          )
        )
        message <- "{tt('Scénario')} {le_uuid}, '{le_nom}' {tt('chargé dans les sliders')}" |> glue::glue()
      } else {
        force_uuid(
          list(
            uuid = "",
            params = p_actuel,
            country = presets[.x, ]$country,
            precalc = FALSE,
            ruleok = FALSE
          )
        )
        message <- "{tt(le_nom)} {tt('chargé dans les sliders')}" |> glue::glue()
      }
      querycount(querycount() + 1)
      shinyWidgets::updatePrettyToggle(
        session = session,
        inputId = stringr::str_c("graph_preset_", .x),
        label = NULL,
        value = FALSE
      )
      shinyWidgets::updatePrettyToggle(
        session = session,
        inputId = stringr::str_c("graph_actuel_", .x),
        label = NULL,
        value = TRUE
      )
      
      updateTextInput(inputId = "nom_actuel", value = tt(le_nom))
      showNotification(message, type = "message")
      rec <- record()
      record(
        purrr::list_modify(
          rec,
          uuid = le_uuid,
          no_log = le_uuid,
          is_precalc = force_uuid()$precalc,
          seed = p_actuel$seed
        )
      )
    })
  })
  
  ## find -------------------
  go_find_trigger <- reactiveVal(0)
  
  observeEvent(input$find_uuid, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if (!uuid_is_ok(input$find_uuid, globals$mmg)) {
      shinyjs::hide(id = "okfound")
      shinyjs::hide(id = "upload_found")
      shinyjs::hide(id = "save_found")
      scenario_found(tibble::tibble())
    } else {
      go_find_trigger(go_find_trigger() + 1)
    }
  })
  
  scenario_found <- reactiveVal(tibble::tibble())
  
  observeEvent(list(input$go_find, go_find_trigger()), ignoreInit = TRUE, {
    if (!uuid_is_ok(input$find_uuid, globals$mmg)) {
      shinyjs::hide(id = "upload_found")
      shinyjs::hide(id = "save_found")
      shinyjs::show(id = "okfound")
      shinyWidgets::updatePrettyToggle(session, "okfound", value = FALSE)
      scenario_found(tibble::tibble())
      return(NULL)
    }
    
    shinyjs::show(id = "okfound")
    shinyWidgets::updatePrettyToggle(session, "okfound", value = TRUE)
    shinyjs::show(id = "upload_found")
    shinyjs::show(id = "save_found")
    
    p_found <- mongo_get_param(input$find_uuid, globals$mmg)
    
    found <- tibble::tibble(
      uuid = input$find_uuid,
      nom_fr = str_c(p_found$country, " (log)"),
      nom_en = "",
      comment_fr = "",
      comment_en = "",
      country = p_found$country,
      p = list(p_found),
      pwr = list(p_found),
      preset = FALSE,
      actuel = FALSE,
      saved = FALSE,
      graph = FALSE,
      index = 0,
      sim = NULL
    )
    
    scenario_found(found)
  })
  
  ## save found -------------
  observeEvent(input$save_found, ignoreInit = TRUE, {
    if (nrow(scenario_found()) != 1) {
      return(NULL)
    }
    scn_s <- scenarii_saved()
    if (!scenario_found()$uuid %in% scn_s$uuid) {
      scenarii_saved(bind_rows(scn_s, scenario_found() |> mutate(saved = TRUE)))
    }
    scenario_found(tibble::tibble())
    updateTextInput(inputId = "find_uuid", value = "", placeholder = "...uuid")
    shinyjs::hide(id = "upload_found")
    shinyjs::hide(id = "save_found")
    shinyjs::hide(id = "okfound")
    
    showNotification(
      ui = "{uuid} i18n$t(sauvegardé)",
      session = session, type = "message", duration = 5
    )
  })
  
  ## upload found ------------------------
  observeEvent(input$upload_found, ignoreInit = TRUE, {
    le_uuid <- scenario_found() |> pull(uuid)
    if (record()$uuid == le_uuid) {
      return(NULL)
    }
    
    le_nom <- ""
    
    # on charge le scénario dans actuel
    p_actuel <- scenario_found() |>
      pull(pwr) |>
      pluck(1)
    # on met les paramètres dans force_uuid
    force_uuid(
      list(
        uuid = le_uuid,
        params = p_actuel,
        country = p_actuel$country,
        precalc = FALSE,
        ruleok = FALSE
      )
    )
    # et dans record()$seed
    record(
      list_modify(
        record(),
        uuid_found = le_uuid,
        p_found = p_actuel,
        seed = p_actuel$seed,
        p = p_actuel
      )
    )
    tt <- i18n$t
    message <- "{tt('Scénario')} {le_uuid} {tt('chargé dans les sliders')}" |> glue::glue()
    
    querycount(querycount() + 1)
    freezeReactiveValue(input, "country")
    updatePickerInput(session, "country", selected = p_actuel$country)
    updateTextInput(session, "nom_actuel", value = p_actuel$country)
    
    updateTextInput(inputId = "find_uuid", value = "", placeholder = "...uuid")
    scenario_found(tibble::tibble())
    
    showNotification(message, type = "message")
  })
  
  observe({
    if (nrow(scenarii()) == 0)
      return(NULL)
    shinyjs::show(id = "dwn_scn_csv_bttn")
  })
  
  ## le bouton de download -----------
  output$dwn_scn_csv <- downloadHandler(
    filename = function() {
      uuids <- scenarii() |>
        filter(saved | actuel | preset, uuid != "") |>
        distinct(uuid) |>
        pull(uuid)
      fn <- dwr_file_name(ext = "csv", "dwr_scenarios", uuids)
      return(fn)
    },
    content = function(file) {
      scn <- scenarii() |>
        dplyr::filter(saved | actuel | preset, uuid != "") |>
        dplyr::distinct(uuid, .keep_all = TRUE)
      
      data <- purrr::pmap_dfr(
        list(scn$sim, scn$uuid, scn$country, scn$nom_fr),
        ~ {
          ..1 |> dplyr::mutate(uuid = ..2, country = ..3, name = ..4)
        }
      ) |>
        dplyr::arrange(uuid, country, variable, year) |>
        dplyr::select(uuid, country, variable, year, q0.5, q0.025, q0.975) |>
        dplyr::filter(variable %in% names(globals$vars$label)) |>
        dplyr::mutate(label = map_chr(variable, ~ i18n$t(globals$vars$label[[.x]])))
      readr::write_csv(data, file = file)
    }
  )
}
