pre_plot_sim <- function(scn, sim, gl, i18n) {
  if (is.null(scn) || is.null(sim) || sim$uuid == "") {
    return(NULL)
  }
  scn <- scn |>
    dplyr::filter(graph == TRUE | actuel) |>
    dplyr::filter(!purrr::map_lgl(sim, is.null))
  if (nrow(scn) == 0) {
    return(NULL)
  }
  sim_principale <- scn |>
    dplyr::filter(index == max(index)) |>
    unlist(recursive = FALSE)
  autre_sim <- scn |>
    dplyr::filter(index != max(index))
  cn_year <- get_ameco_date(sim_principale$p$ameco)$cn
  start_year <- sim_principale$p$start_year
  start_hist <- sim_principale$p$start_hist
  year_dette <- sim_principale$p$loss_t + start_year
  end_year <- sim_principale$p$end_year
  annot_year <- (end_year - start_year) %/% 2 + start_year
  sim_p  <- sim_principale$sim |>
    left_join(sim$fh, by=c("variable", "year")) |> 
    dplyr::mutate(
      cn_year = !!cn_year,
      start_year = !!start_year,
      year_dette = !!year_dette,
      nom_fr = sim_principale$nom_fr,
      scn_index = 1,
      label = i18n$t(gl$vars$label[variable]),
      valeur = stringr::str_c(signif(q0.5, 3) * 100, "%"),
      vr = ifelse(
        q0.025 == q0.975,
        "",
        glue::glue("95%\u2208[{signif(q0.025,3)}, {signif(q0.975,3)}]")
      ),
      valeur = ifelse(
        variable %in% c("qpib", "pibpot", "pibpot_dep"),
        stringr::str_c(signif(q0.5, 3), "m€"),
        valeur
      ),
      tip1 = stringr::str_c(valeur, " ", i18n$t("en"), " ", year),
      tip2 = vr,
      tip3 = case_when(
        year <= cn_year ~ i18n$t("Données historiques"),
        year <= start_year ~ i18n$t("Prévisions (AMECO, possiblement amendées)"),
        year < year_dette ~ i18n$t("Simulation debwatch"),
        year >= year_dette ~ i18n$t("Simulation debwatch, avec contrainte de dette")
      ),
      scn_index = 1,
      scn = dplyr::if_else( 
        year == annot_year - 5,
        stringr::str_c(i18n$t(gl$vars$short[variable]), " - ", i18n$t(sim_principale$nom_fr)),
        NA_character_
      ),
      uuid = sim_principale$uuid
    )
  # la distribution des simulations
  se_names <- setdiff(
    purrr::keep(
      names(sim_p),
      ~ stringr::str_starts(.x, "q")
    ),
    c("year", "variable", "q0.5", "pq0.5")
  ) |> sort()
  se <- length(se_names) > 0
  
  alt_sim_i <- autre_sim$index

  sim_alt <- purrr::pmap(
    list(
      autre_sim$nom_fr,
      autre_sim$sim,
      autre_sim$index,
      autre_sim$uuid,
      map(autre_sim$p, function(p) get_ameco_date(p[["ameco"]]%||%"5/2021")),
      map(autre_sim$p, "start_year"),
      map(autre_sim$p, "loss_t")
    ),
    ~ dplyr::mutate(
      .data = ..2,
      nom_fr = ..1,
      index = ..3,
      uuid = ..4,
      cn_year = ..5$cn,
      start_year = ..6,
      year_dette = ..6 + ..7
      
    )
  ) |>
    dplyr::bind_rows()
  if (nrow(sim_alt) > 0) {
    sim_alt <- sim_alt |>
      dplyr::arrange(index) |>
      dplyr::group_by(index) |>
      dplyr::mutate(
        scn_index = dplyr::cur_group_id() + 1,
        scn = NA_character_
      ) |>
      dplyr::ungroup()
  }
  
  sim_all <- dplyr::bind_rows(sim_p |> mutate(index = 0), sim_alt)
  en_variante <- nrow(sim_alt)>0&sim_principale$relatif
  
  if(en_variante)
  {
    p_uuid <- sim_principale$uuid
    sim_all <- sim_all |>
      group_by(variable, year) |>
      arrange(index) |> 
      transmute(
        index = index,
        uuid = uuid,
        q0.025 = q0.025 - q0.5[index==0],
        q0.975 = q0.975 - q0.5[index==0],
        q0.5 = q0.5 - q0.5[index==0],
        scn = str_c(scn, " [//", p_uuid, "]"),
        nom_fr = str_c(nom_fr, " [//", p_uuid, "]"),
        scn_index = scn_index - 1,
        label = label,
        valeur = stringr::str_c(signif(q0.5, 3) * 100, "%"),
        tip1 = "Ecart entre {uuid} et {p_uuid} de {valeur}" |> glue::glue(),
        tip2 = case_when(
          year <= cn_year ~ i18n$t("Données historiques"),
          year <= start_year ~ i18n$t("Prévisions (AMECO, possiblement amendées)"),
          year < year_dette ~ i18n$t("Simulation debwatch"),
          year >= year_dette ~ i18n$t("Simulation debwatch, avec contrainte de dette")
        ),
        tip3 = ""
      ) |> 
      ungroup() |> 
      filter(index!=0)
  }
  
  # Un graphique generique
  
  gen <- function(var) {
    lty <- c(l1 = "solid", l2 = "dashed", l3 = "dotted", l4 = "dotdash", l5 = "longdash", l6 = "twodash")
    sim_all <- sim_all |>
      dplyr::arrange(dplyr::desc(scn_index)) |>
      dplyr::filter(variable %in% var) |>
      dplyr::mutate(
        scn = dplyr::if_else(
          scn_index > 1 & year == annot_year + 5 * (scn_index - 2), 
          nom_fr,
          scn),
        linetype = stringr::str_c("l", scn_index)
      )
    indexes <- sim_all |>
      dplyr::group_by(scn_index, variable) |>
      dplyr::summarize(qq = q0.5[year == annot_year], .groups = "drop") |>
      dplyr::arrange(dplyr::desc(qq)) |>
      dplyr::mutate(
        couleur = colorspace::lighten(gl$vars$colors[variable], scn_index / 5),
        ic = stringr::str_c("c", dplyr::row_number()),
        ii = dplyr::row_number()
      )
    color_scale <- indexes |> dplyr::pull(couleur, name = ic)
    sim_all <- sim_all |>
      merge(indexes, by = c("scn_index", "variable")) |>
      mutate(
        ic = factor(ic),
        ic = forcats::fct_reorder(ic, scn_index, median, .desc = TRUE)
      )
    data_p_lims <- sim_all |>
      dplyr::filter(year > start_year, variable %in% var) |>
      dplyr::select(q0.5)
    data_p_lims_se <- sim_all |>
      dplyr::filter(year > start_year, variable %in% var, scn_index == 1) |>
      dplyr::select(q0.025, q0.975)
    ylim_max <- max(
      max(data_p_lims, na.rm = TRUE),
      max(data_p_lims_se, na.rm = TRUE)
    )
    ylim_min <- min(
      min(data_p_lims, na.rm = TRUE),
      min(data_p_lims_se, na.rm = TRUE)
    )
    
    if(start_year<max(sim$fh$year))
      graph_fh <- ggplot2::geom_line(
        data = ~ dplyr::filter(.x, scn_index == 1, year>=start_year),
        mapping = ggplot2::aes(x = year, y = full_h, color=ic), 
        linetype = "solid",
        size = 0.5,
        show.legend = FALSE,
        na.rm = TRUE
      ) 
    else
      graph_fh <- NULL
    
    graph_line <- ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = q0.5, color = ic, linetype = linetype),
      size = 1.5,
      show.legend = FALSE,
      na.rm = TRUE
    )
    if (se) {
      graph_se <- ggplot2::geom_ribbon(
        data = ~ dplyr::filter(.x, scn_index == 1),
        mapping = ggplot2::aes(x = year, ymin = q0.025, ymax = q0.975, fill = ic),
        alpha = 0.1,
        col = NA,
        show.legend = FALSE,
        na.rm = TRUE
      )
    } else {
      graph_se <- NULL
    }
    
    graph_point <- list(
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, scn_index == 1),
        mapping = ggplot2::aes(x = year, y = q0.5, color = ic),
        size = 3,
        shape = 16,
        show.legend = FALSE,
        na.rm = TRUE
      ),
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, year > cn_year, scn_index == 1),
        mapping = ggplot2::aes(x = year, y = q0.5),
        col = "white",
        fill = "white",
        size = 0.5,
        shape = 16,
        show.legend = FALSE,
        na.rm = TRUE
      ),
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, year > start_year, scn_index == 1),
        mapping = ggplot2::aes(x = year, y = q0.5),
        size = 2,
        col = "white",
        shape = 16,
        show.legend = FALSE,
        na.rm = TRUE
      ),
      ggplot2::geom_point(
        data = ~ dplyr::filter(.x, year >= year_dette, scn_index == 1),
        mapping = ggplot2::aes(x = year, y = q0.5, color = ic),
        size = 1,
        shape = 16,
        show.legend = FALSE,
        na.rm = TRUE
      )
    )
    graph_text_up <- ggrepel::geom_text_repel(
      data = ~ dplyr::filter(.x, ii %% 2 == 1),
      mapping = ggplot2::aes(x = year, y = q0.5, label = scn, color = ic),
      ylim = c(ylim_max, NA),
      size = 3.5,
      show.legend = FALSE,
      na.rm = TRUE
    )
    graph_text_down <- ggrepel::geom_text_repel(
      data = ~ dplyr::filter(.x, ii %% 2 == 0),
      mapping = ggplot2::aes(x = year, y = q0.5, label = scn, color = ic),
      ylim = c(NA, ylim_min),
      size = 3.5,
      show.legend = FALSE,
      na.rm = TRUE
    )
    # gfonts::use_font("source-sans-pro", "www/css/source-sans-pro.css")
    ggplot2::ggplot(sim_all) +
      graph_se +
      graph_text_up +
      graph_text_down +
      graph_line +
      graph_point +
      graph_fh +
      ggplot2::scale_colour_manual(values = color_scale, aesthetics = c("colour", "fill")) +
      ggplot2::scale_linetype_manual(values = lty) +
      ggplot2::theme_minimal(base_family = "roboto") +
      ggplot2::scale_x_continuous(breaks=sort(unique(c(start_hist, start_year, seq(round(start_hist/10)*10,end_year, 10)))))+
      ggplot2::xlab("") +
      ggplot2::ylab("")
  }
  
  # uniquement pour les images enregistrées
  amecs <-  str_c("AMECO ", str_c(map(scn$p, "ameco"), collapse=", "))
  uuids <- str_c(scn$uuid, collapse=", ")
  decoration <- function(title, pays) {
    ggplot2::labs(
      title = stringr::str_c(title, ", ", pays),
      caption = stringr::str_c(
        "Source: ",
        amecs,
        " OFCE Debtwatch, ofce.shinyapps.io/debtwatchr\nuuid:",
        uuids
      )
    )
  }
  
  minyear <- min(sim_p$year)
  maxyear <- max(sim_p$year)
  dstar <- sim_principale$p$dstar
  infstar <- sim_principale$p$infstar
  nairu <- sim_principale$p$nairu
  
  spstar <- sim_principale$sim |>
    dplyr::filter(
      variable == "spstar",
      year == max(year)
    ) |>
    dplyr::pull(q0.5)
  
  gstar <- sim_principale$sim |>
    dplyr::filter(
      variable == "gpot",
      year == max(year)
    ) |>
    dplyr::pull(q0.5)
  
  return(list(
    data = sim_p,
    data_alt = sim_all,
    gen_graph = gen,
    minyear = minyear,
    maxyear = maxyear,
    dstar = dstar,
    spstar = spstar,
    gstar = gstar,
    infstar = infstar,
    nairu = nairu,
    decoration = decoration,
    anchor = en_variante
  ))
}

post_plot_sim <- function(gen, var, title = NULL, pays = NULL, uuid = NULL, box_collapsed = FALSE) {
  if (box_collapsed) {
    return(NULL)
  }
  if (is.null(gen)) {
    return(NULL)
  }
  g <- gen$gen_graph(var)
  annote <- !gen$anchor
  add_g <- ggplot2::scale_y_continuous(labels = scales::label_percent(1))
  if ("dettep" %in% var) {
    add_g <- list(
      ggplot2::scale_y_continuous(labels = scales::label_percent(1)),
      
      if(annote) list(
        ggplot2::geom_hline(yintercept = gen$dstar, col = "black", size = 0.5, linetype = "dotted"),
        ggplot2::annotate(
          "text",
          x = gen$minyear + 1,
          y = gen$dstar*1.025,
          label = glue::glue("d*={signif(gen$dstar*100,3)}%"),
          hjust = 0,
          size = 3.5
        ))
      else NULL
    )
  }
  if ("og" %in% var) {
    add_g <- list(
      ggplot2::geom_hline(yintercept = 0, col = "black", size = 0.5, linetype = "dotted"),
      ggplot2::scale_y_continuous(labels = scales::label_percent(1))
    )
  }
  if ("tcho" %in% var) {
    add_g <- list(
      ggplot2::scale_y_continuous(labels = scales::label_percent(1)),
      if(annote) list(
        ggplot2::annotate(
          "text",
          x = gen$minyear + 0.5,
          y = gen$nairu+0.0025,
          label = glue::glue("Nairu={signif(gen$nairu*100,2)}%"),
          hjust = 0,
          size = 3.5
        ),
        ggplot2::geom_hline(yintercept = gen$nairu, col = "black", size = 0.5, linetype = "dotted")
      )
      else NULL
    )
  }
  if ("spp" %in% var) {
    add_g <- list(
      ggplot2::scale_y_continuous(labels = scales::label_percent(1)),
      
      if(annote) list(
        ggplot2::geom_hline(yintercept = gen$spstar, col = "black", size = 0.5, linetype = "dotted"),
        ggplot2::annotate(
          "text",
          x = gen$minyear + 1,
          y = gen$spstar + 0.01,
          label = glue::glue("s*={signif(gen$spstar*100,3)}%"),
          hjust = 0,
          size = 3.5
        )
      )
      else NULL
    )
  }
  if (all(purrr::map_lgl(var, ~ .x %in% c("ib_dep", "ib_po")))) {
    add_g <- list(ggplot2::scale_y_continuous(labels = scales::label_percent(1)))
  }
  if (all(purrr::map_lgl(var, ~ .x %in% c("tpo", "tdep")))) {
    add_g <- list(ggplot2::scale_y_continuous(labels = scales::label_percent(1)))
  }
  if (all(purrr::map_lgl(var, ~ .x %in% c("qpib", "pibpot", "pibpot_dep")))) {
    add_g <- list(ggplot2::scale_y_continuous(labels = f2si2))
  }
  if ("txppib" %in% var) {
    add_g <- list(
      ggplot2::scale_y_continuous(labels = scales::label_percent(0.1)),
      
      if(annote)
        list(
          ggplot2::geom_hline(yintercept = gen$infstar, col = "black", size = 0.5, linetype = "dotted"),    
          ggplot2::annotate(
            "text",
            x = gen$maxyear,
            y = gen$infstar + 0.0025,
            label = glue::glue("i*={signif(gen$infstar*100,3)}%"),
            hjust = 1,
            size = 3.5
          )
        )
      else
        NULL
    )
  }
  if ("gpib" %in% var) {
    add_g <- list(
      ggplot2::scale_y_continuous(labels = scales::label_percent(1)),
      
      if(annote) list(
        ggplot2::geom_hline(
          yintercept = gen$gstar,
          col = "black",
          size = 0.5,
          linetype = "dotted"
        ),
        ggplot2::annotate(
          "text",
          x = gen$maxyear,
          y = gen$gstar + 0.015,
          label = glue::glue("g*={signif(gen$gstar*100,3)}%"),
          hjust = 1,
          size = 3.5
        )
      )
      else NULL
    )
  }

  g <- g + add_g
  if (!is.null(uuid)) {
    g <- g + gen$decoration(title = title, pays = pays)
  }
  if(gen$anchor)
    g <-  g +  ggplot2::scale_y_continuous(labels = scales::label_percent(0.1))
  
  return(g)
}

hover_on_plot <- function(gen, var, hover, box_collapsed = FALSE) {
  if (box_collapsed) {
    return(NULL)
  }
  if (is.null(gen)) {
    return(NULL)
  }
  data <- gen$data |>
    dplyr::filter(variable %in% var, scn_index == 1) |>
    dplyr::select(q0.5, year, label, tip1, tip2, tip3)
  
  point <- nearPoints(
    data,
    hover,
    xvar = "year",
    yvar = "q0.5",
    threshold = 10,
    maxpoints = 1,
    addDist = TRUE
  )
  
  if (nrow(point) == 0) {
    return(NULL)
  }
  
  left_px <- hover$coords_css$x
  top_px <- hover$coords_css$y - 500
  
  # je ne sais pas pourquoi il faut enlever 400 du coup
  # c'est la taille d'un panel, il se prend les pieds dans le relatif...
  
  style <- paste0(
    "position:absolute; z-index:100; font-size: 80%; background-color: rgba(255, 235, 200, 0.75); ",
    "left:", left_px + 5, "px; top:", top_px + 15, "px;"
  )
  div(
    class = "well well-sm",
    style = style,
    p(HTML(glue::glue("<b> {point$label} </b> <br/> {point$tip1} <br/> {point$tip2} <br/> {point$tip3}")))
  )
}

hggplot_col <- function(id, panels) {
  bsCollapsePanel(
    panels[[id]],
    div(
      style = "position:relative",
      plotOutput(stringr::str_c("plot_", id),
                 hover = hoverOpts(
                   stringr::str_c("h_", id),
                   delay = 100,
                   delayType = "debounce",
                   nullOutside = TRUE
                 )
      ),
      uiOutput(stringr::str_c("h_", id))
    )
  )
}

hggplot_col_cb <- function(id, name, color) {
  div(
    style = "position:relative",
    plotOutput(
      str_c("plot_", id),
      hover = hoverOpts(
        str_c("h_", id),
        delay = 100,
        delayType = "debounce",
        nullOutside = TRUE
      )
    ),
    uiOutput(str_c("h_", id)),
    shinyWidgets::downloadBttn(
      stringr::str_c("dwn_svg_", id),
      label = "svg", style = "minimal", size = "xs", color = "succes"
    ) |>
      shinyjs::hidden(),
    shinyWidgets::downloadBttn(
      stringr::str_c("dwn_png_", id),
      label = "png", style = "minimal", size = "xs", color = "succes"
    ) |>
      shinyjs::hidden(),
    shinyWidgets::downloadBttn(
      stringr::str_c("dwn_csv_", id),
      label = "csv", style = "minimal", size = "xs", color = "succes"
    ) |>
      shinyjs::hidden(),
  )
}

hggplot_box <- function(id, name, color, collapsed = TRUE) {
  box(
    div(
      style = "position:relative",
      plotOutput
      (stringr::str_c("plot_", id),
        hover = hoverOpts(
          stringr::str_c("h_", id),
          delay = 100,
          delayType = "debounce",
          nullOutside = TRUE
        )
      ),
      uiOutput(stringr::str_c("h_", id))
    ),
    shinyWidgets::downloadBttn(
      stringr::str_c("dwn_svg_", id),
      label = "svg", style = "minimal",
      size = "xs",
      color = "succes"
    ) |>
      shinyjs::hidden(),
    shinyWidgets::downloadBttn(
      stringr::str_c("dwn_png_", id),
      label = "png",
      style = "minimal",
      size = "xs",
      color = "succes"
    ) |>
      shinyjs::hidden(),
    shinyWidgets::downloadBttn(
      stringr::str_c("dwn_csv_", id),
      label = "csv",
      style = "minimal",
      size = "xs",
      color = "succes"
    ) |>
      shinyjs::hidden(),
    id = stringr::str_c("box_", id),
    title = actionLink(
      inputId = stringr::str_c(id, "_tgglbox"),
      label = name,
      icon = icon_colored("chart-line", color)
    ),
    width = 12,
    collapsible = TRUE,
    collapsed = collapsed
  )
}
