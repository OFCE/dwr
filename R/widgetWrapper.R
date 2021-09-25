
#' Wrapper to create a section with a sliderInput
#' 
#' @param inputId input ID
#' @param label Label of the sliderInput
#' @param help_text Help text that is displayed in the tooltip
#' @param min min of the slider
#' @param max max of the slider
#' @param value default value of the slider
#' @param step step of the slider

dwr_slider <- function(id, globals, i18n) {
  slider <- globals$sliders[[id]]
  sliderInput2(
    inputId = id,
    label = tagList(
      span(i18n$t(slider[["label"]]), class = "parametres-label"),
      div(
        id = paste0(id, "_value"), 
        class = "parametres-value",
        tags$output(class = "widget-value", slider[["initial"]])
      ),
      if (!is.null(slider[["help"]])) {
        div(
          id = paste0(id, "_tooltip"),
          `data-toggle` = "tooltip",
          `data-html` = "true",
          `data-placement` = "right",
          title = i18n$t(slider[["help_code"]]),
          tags$i(class = "far fa-question-circle")
        )
      } else {
        div(style = "width: 15px")
      }
    ),
    min = slider[["min"]],
    max = slider[["max"]],
    value = slider[["initial"]],
    step = slider[["step"]]
  )
}


sliderWrapper <- function(inputId, label, help_text, min, max, value, step = NULL, ...) {
  sliderInput2(
    inputId = inputId,
    label = tagList(
      span(label, class = "parametres-label"),
      div(
        id = paste0(inputId, "_value"), 
        class = "parametres-value",
        tags$output(class = "widget-value", value)
      ),
      if (!is.null(help_text)) {
        div(
          id = paste0(inputId, "_tooltip"),
          `data-toggle` = "tooltip",
          `data-html` = "true",
          `data-placement` = "right",
          title = help_text,
          tags$i(class = "far fa-question-circle")
        )
      } else {
        div(style = "width: 15px")
      }
    ),
    min = min, max = max, value = value, step = step, ...
  )
}

airYearpickerWrapper <- function(inputId, label, help_text, value, maxDate, minDate, ...) {
  div(
    class = "widget-wrapper",
    id = paste0("widget-wrapper-", inputId),
    airYearpickerInput(
      inputId = inputId, 
      label = div(
        class = "widget-label",
        span(label, class = "parametres-label"),
        div(
          id = paste0(inputId, "_value"),
          class = "parametres-value",
          tags$output(class = "widget-value", value)
        ),
        if (!is.null(help_text)) {
          div(
            id = paste0(inputId, "_tooltip"),
            `data-toggle` = "tooltip",
            `data-html` = "true",
            `data-placement` = "right",
            title = help_text,
            tags$i(class = "far fa-question-circle")
          )
        } else {
          div(style = "width: 15px")
        }
      ),
      value = value, 
      maxDate = maxDate, 
      minDate = minDate,
      ...
    )
  )
}

selectInputWrapper <- function(inputId, label, help_text, value, choices, ...) {
  div(
    class = "widget-wrapper",
    id = paste0("widget-wrapper-", inputId),
    selectInput(
      inputId = inputId,
      label = div(
        class = "widget-label",
        span(label, class = "parametres-label"),
        div(
          id = paste0(inputId, "_value"),
          class = "parametres-value",
          tags$output(class = "widget-value", value)
        ),
        if (!is.null(help_text)) {
          div(
            id = paste0(inputId, "_tooltip"),
            `data-toggle` = "tooltip",
            `data-html` = "true",
            `data-placement` = "right",
            title = help_text,
            tags$i(class = "far fa-question-circle")
          )
        } else {
          div(style = "width: 15px")
        }
      ),
      choices = choices,
      selectize = FALSE,
      ...
    )
  )
        
}

