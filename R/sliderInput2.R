
#' New slider input widget
#' 
#' @param inputId input ID
#' @param label Label of the sliderInput
#' @param min min of the slider
#' @param max max of the slider
#' @param value default value of the slider
#' @param step step of the slider
#' @param class CSS class

sliderInput2 <- function(
  inputId,
  label,
  min,
  max,
  value,
  step = NULL,
  class = ""
) {
  
  tagList(
    
    singleton(
      tags$head(
        tags$script(src = "js/sliderInput2.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "css/sliderInput2.css")
      )
    ),
    
    div(
      class = paste("form-group shiny-input-container widget-wrapper slider-wrapper", class),
      id = paste0("widget-wrapper-", inputId),
      tabindex = "-1",
      tags$label(
        `for` = inputId,
        div(
          class = "widget-label",
          id = paste0("widget-label-", inputId),
          tabindex = "-1",
          label
        )
      ),
      div(
        class = "input-group slider-widget",
        id = paste0("slider-widget-", inputId),
        span(class = "slider-min", min),
        tags$input(
          type = "range",
          class = "slider",
          min = min,
          max = max,
          step = step,
          value = value,
          id = inputId
        ),
        span(class = "slider-max", max),
        tags$output(
          class = "slider-value"
        )
      )
    )
    
  )
}

#' Function to update a `sliderInput2`
#' 
#' @param session session object passed to function given to shinyServer.
#' @param inputId id of the input object
#' @param label label to set for the input object
#' @param min minimum value that can be selected
#' @param max maximum value that can be selected
#' @param value initial value of the slider
#' @param step interval between each selectable value on the slider

updateSliderInput2 <- function(
  session = getDefaultReactiveDomain(),
  inputId,
  label = NULL,
  min = NULL,
  max = NULL,
  value = NULL,
  step = NULL
) {
  shiny:::validate_session_object(session)
  message <- shiny:::dropNulls(
    list(
      label = label,
      min = shiny:::formatNoSci(min),
      max = shiny:::formatNoSci(max),
      value = shiny:::formatNoSci(value),
      step = shiny:::formatNoSci(step)
    )
  )
  session$sendInputMessage(inputId, message)
}
