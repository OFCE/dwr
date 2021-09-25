

#' UI-side to collapse a div section in Shiny
#' 
#' @param id Root ID of the section.
#' @param title Title of the section
#' @param content HTML content of the section
#' @param icon_name Fontawesome name of the icon

collapseUI <- function(id, title, content, collapsed = TRUE, class = "section") {
  if (collapsed == TRUE) {
    icon_name <- "caret-up"
    collapsed_class <- "hidden"
  } else {
    icon_name <- "caret-down"
    collapsed_class <- "show"
  }
  div(
    class = class,
    div(
      class = paste0(class, "-title"),
      title,
      tags$button(
        id = paste0(id, "_more"),
        href = "#",
        class = "action-button section-more",
        icon(icon_name)
      )
    ),
    div(
      id = paste0(id, "_content"),
      class = paste0(class, "-content ", collapsed_class),
      content
    )
  )
}
