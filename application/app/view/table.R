box::use(
  shiny[moduleServer, req, NS, observeEvent, tagList, reactiveVal, h3, div, reactive, HTML],
  bs4Dash[actionButton, box],
  DT[renderDataTable, dataTableOutput]
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      solidHeader = TRUE,
      title = div(
        class = "header-btns",
        bs4Dash::actionButton(ns("reset_btn"),
                                        label = "Reset selection",
                                        status = "secondary"
        ),
        HTML(
          '
                  <div id="feat-radio" class="feat-radio-container">
                    <h4>Gene</h4>
                    <div class="toggle-switch-feat">
                      <input  name="checkbox-feature" class="toggle-input-feat" id="toggle-feat" type="checkbox">
                      <label class="toggle-label-feat" for="toggle-feat"></label>
                    </div>
                    <h4>Pathway</h4>
                  </div>

                  <script>

                  var checkbox = document.querySelector("input[name=checkbox-feature]");
                  checkbox.addEventListener("change", function() {
                    if (this.checked) {
                      Shiny.setInputValue("table-feature_radio", "pathway", {priority: "event"});
                    } else {
                      Shiny.setInputValue("table-feature_radio", "gene", {priority: "event"});
                    }
                  });

                  </script>

              '
        )
      ),
      collapsible = TRUE,
      status = "primary",
      width = 12,
      dataTableOutput(ns("table"), height = "40%")
    )
  )

}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    is_path_tab_active <- reactiveVal(TRUE)
    selected_pathways <- reactiveVal(NULL)
    selected_genes <- reactiveVal(NULL)
    
    pathways_tab <- reactive({
      unique(data[, .(kegg_paths_name)])
    })
    
    gene_tab <- reactive({
      unique(data[, .(gene_name)])
    })
    
    observeEvent(pathways_tab(),{
      req(pathways_tab())
      print(input)
      output$table <- renderDataTable({pathways_tab()})
    })

    
    observeEvent(input$feature_radio, {
      print("Triggered")
      if (input$feature_radio == "gene") {
        if (is_path_tab_active()) {
          is_path_tab_active(FALSE)
          output$table <- render_gene_table(
            gene_tab(),
            selected_genes())
        }

      } else if (input$feature_radio == "pathway") {
        if (!(is_path_tab_active())) {
          is_path_tab_active(TRUE)
          output$analysis_table <- render_pathway_table(
            pathways_tab(),
            selected)
          
        }
      }
    })
    
  })
}



render_gene_table <- function(gene_tab, selected = NULL) {
  print(selected) # it must be some kind of black magic but including the print makes the bug where the first click on any row of the table is not registered go away
  DT::renderDT({
    gene_tab[, link := paste0(
      '<a onmousedown="event.preventDefault(); event.stopPropagation(); return false;" target=_blank href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=',
      feature_id, '">', '<i class="feature-link fa fa-link"></i>', "</a>"
    )]
    gene_tab[, pathways := sprintf(
      '<button id="%s" type="button" class="btn action-button btn-secondary" onmousedown="event.preventDefault(); event.stopPropagation(); return false;" onclick="%s">Show</button>', 
      feature_id, "Shiny.setInputValue('select_features-gene_pathways_btn', this.id);")]
    
    DT::datatable(
      gene_tab,
      escape = FALSE,
      selection = list(
        mode = "multiple",
        selected = selected,
        target = "row"
      ),
      options = list(
        pageLength = 8
      )
    )
  })
}


render_pathway_table <- function(all_pathways_tab, selected = NULL) {
  all_pathways_tab[, genes := sprintf(
    '<button id="%s" type="button" class="btn action-button btn-secondary" onmousedown="event.preventDefault(); event.stopPropagation(); return false;" onclick="%s">Show</button>', 
    pathway, "Shiny.setInputValue('table-pathway_genes_btn', this.id);")]
  DT::renderDT({
    tab <- DT::datatable(
      all_pathways_tab,
      escape = FALSE,
      selection = list(
        mode = "multiple",
        selected = selected,
        target = "row"
      ),
      callback = htmlwidgets::JS('
                                  table.on("mouseover", "td", function() {
                                  var index = table.cell(this).index();
                                  Shiny.setInputValue("table-hover_pathway_genes", index, {priority: "event"});
                                   })'),
      options = list(pageLength = 8)
    )

    return(tab)
  })
}
