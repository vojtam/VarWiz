box::use(
  shiny[moduleServer, NS, div, observeEvent, req, reactive, textOutput, renderText, uiOutput, renderUI],
  shinyWidgets[awesomeCheckboxGroup, pickerInput, pickerOptions],
  bs4Dash[box]

)

#' @export
ui <- function(id) {
  ns <- NS(id)
  box(
    width = 12,
    title = "Plot options",
    status = "navy",
    maximizable = TRUE,
    solidHeader = TRUE,
    div(
      class = "flex-centered, flex-col",
      uiOutput(ns("filter_var_type_UI")),
      uiOutput(ns("filter_consequence_UI"))
    )
  )
}

#' @export
server <- function(id, var_types, conseq_types) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # so that we put the renderui inputs in the same namespace as the session
    
    output$filter_var_type_UI <- renderUI({
      awesomeCheckboxGroup(
        inputId = ns("pick_var_type"),
        label = "Filter variant type",
        choices = var_types,
        selected = var_types,
        inline = TRUE
      )
    })
    
    output$filter_consequence_UI <- renderUI({
      labels <- gsub(",", ", ", gsub("_", " ", conseq_types))
      names(conseq_types) <- labels
      pickerInput(
        inputId = ns("pick_consequence"),
        label = "Select consequence types", 
        choices = conseq_types,
        selected = conseq_types,
        multiple = TRUE,
        options = pickerOptions(container = "body", 
                                actionsBox = TRUE),
        width = "100%"
      )
    })
    
    
    return(reactive(list(input$pick_var_type, input$pick_consequence)))
  })
}
