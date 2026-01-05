#code for draggable rows from here: https://laustep.github.io/stlahblog/posts/DTcallbacks.html

selectableDT_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(".ui-selectable-helper { display: none !important; }")) #
    ),
    DTOutput(NS(id, "table")),
    br(),
    verbatimTextOutput(NS(id, "selectedPreview"))
  )
}

selectableDT_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    # JS callback for drag-select + persistent selection
    callback <- c(
      "function distinct(value, index, self){ return self.indexOf(value) === index; }",
      "var dt = table.table().node();",
      "var tblID = $(dt).closest('.datatables').attr('id');",
      "var inputName = tblID + '_rows_selected2';",
      "var selected = [];",

      "function makeSelectable(){",
      "  $(dt).find('tbody').selectable({",
      "    distance: 10,",
      "    filter: 'tr',",
      "    selecting: function(evt, ui){",
      "      $(this).find('tr').each(function(i){",
      "        if($(this).hasClass('ui-selecting')){",
      "          var row = table.row(this);",
      "          row.select();",
      "          var rowIndex = parseInt(row.id().split('-')[1]);",
      "          selected.push(rowIndex);",
      "          selected = selected.filter(distinct);",
      "          Shiny.setInputValue(inputName, selected);",
      "        }",
      "      });",
      "    }",
      "  }).on('dblclick', function(){ table.rows().deselect(); });",
      "}",

      "makeSelectable();",
      "table.on('draw.dt', function(){ makeSelectable(); });",
      "table.on('click', 'tr', function(){",
      "  var row = table.row(this);",
      "  if(!$(this).hasClass('selected')){",
      "    var rowIndex = parseInt(row.id().split('-')[1]);",
      "    var index = selected.indexOf(rowIndex);",
      "    if(index > -1){ selected.splice(index, 1); }",
      "  }",
      "  Shiny.setInputValue(inputName, selected);",
      "});"
    )

      output$table <- renderDT({
        dat <- df()
        if (nrow(dat) > 0) {
          dat$ROWID <- paste0("row-", seq_len(nrow(dat)))
        } else {
          dat$ROWID <- character(0)
        }
        rowNames <- TRUE
        colIndex <- as.integer(rowNames)

        suppressWarnings(dtable <- datatable(
          dat, rownames = rowNames,
          extensions = "Select",
          callback = JS(callback),
          selection = "multiple",
          options = list(
            rowId = JS(sprintf("function(data){return data[%d];}", ncol(dat)-1L+colIndex)),
            columnDefs = list(list(visible = FALSE, targets = ncol(dat)-1L+colIndex))
          )
        ))

        dep <- htmltools::htmlDependency(
          "jqueryui", "1.12.1", "www/shared/jqueryui",
          script = "jquery-ui.min.js",
          package = "shiny"
        )
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        dtable
      }, server = FALSE)

      # Combine DT selections into a single reactive
      selectedRows <- reactive({
        req(df())
        unique(c(input[["table_rows_selected"]], input[["table_rows_selected2"]]))
      })

      return(selectedRows)


  })
}
