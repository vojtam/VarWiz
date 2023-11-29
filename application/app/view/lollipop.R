box::use(
  shiny[moduleServer, NS, tagList],
  bs4Dash[box]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      status = "navy",
      solidHeader = TRUE,
      title = "Lollipop plot",
      id = "lollipop_plot_box"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
