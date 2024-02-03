box::use(
  shiny[moduleServer, req, NS, observeEvent, tagList, reactiveVal, h3, div, reactive, HTML],
  bs4Dash[actionButton, useAutoColor, box],
  DT[renderDataTable, dataTableOutput],
  gargoyle[trigger]
)

box::use(
  app/logic/render_table[render_pathways_table, render_genes_table]
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      solidHeader = TRUE,
      title = div(
        class = "header-btns",
        bs4Dash::actionButton(
          ns("reset_btn"),
          label = "Reset selection",
          status = "secondary"
        ),
        HTML(
          '
                  <div id="feat-radio" style="margin-left: 15px" class="feat-radio-container">
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
                      Shiny.setInputValue("app-table-feature_radio", "pathway", {priority: "event"});
                    } else {
                      Shiny.setInputValue("app-table-feature_radio", "gene", {priority: "event"});
                    }
                  });

                  </script>

              '
        )
      ),
      collapsible = TRUE,
      status = "navy",
      width = 12,
      dataTableOutput(ns("table"), height = "40%")
    )
  )

}

#' @export
server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    is_path_tab_active <- reactiveVal(FALSE)
    selected_pathways <- reactiveVal(NULL)
    selected_genes <- reactiveVal(NULL)
    
    pathways_tab <- reactive({
      tab <- unique(data[, .(kegg_paths_name)])
      colnames(tab) <- "pathway"
      return(tab)
    })
    
    genes_tab <- reactive({
      unique(data[, .(gene_name)])
    })
    
    observeEvent(genes_tab(),{
      req(genes_tab())
      output$table <- render_genes_table(
        genes_tab(),
        selected_genes()
      )
    })

    
    observeEvent(input$feature_radio, {
      if (input$feature_radio == "gene") {
        if (is_path_tab_active()) {
          is_path_tab_active(FALSE)
          output$table <- render_genes_table(
            genes_tab(),
            selected_genes()$row_i
          )
        }

      } else if (input$feature_radio == "pathway") {
        if (!(is_path_tab_active())) {
          is_path_tab_active(TRUE)
          output$table <- render_pathways_table(
            pathways_tab(),
            selected_pathways()$row_i
          )
          
        }
      }
      
    })
    
    
    observeEvent(input$reset_btn, {
      if ((is_path_tab_active())) {
        selected_pathways(NULL)
        output$table <- render_pathways_table(
          pathways_tab()
        )
        trigger("reset_sankey")
        
      }
      else {
        selected_genes(NULL)
        output$table <- render_genes_table(
          genes_tab()
        )
      }
      
      
    })
    
    
    ############# FEATURE SELECTION ##########
    observeEvent(input$table_cell_clicked, {
      gene <- input$table_cell_clicked[["value"]]
      row <- input$table_cell_clicked[["row"]]
      req(gene)
      if (is.null(row)) {
        return(NULL)
      }
      
      rows <- input$table_rows_selected
      if (is_path_tab_active()) {
        features <- pathways_tab()[rows]
        features[, row_i := rows]
        selected_pathways(features)
      } else {
        features <- genes_tab()[rows]
        features[, row_i := rows]
        selected_genes(features)
      }
    })
    
    return(reactiveVal(list(selected_pathways, selected_genes, is_path_tab_active)))
  })
}



