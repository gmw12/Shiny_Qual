cat(file = stderr(), "Shiny_Tables.R", "\n")
#--------------------------------------------------------------------------------------------------------------------

#load design table
create_data_table <- function(session, input, output, type){
  cat(file = stderr(), "Function create_data_table...", "\n")
  require('DT')
  require('shinyjs')

  options <- list(
    selection = 'single',
    #dom = 'Bfrtipl',
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(0),
        visibile = TRUE,
        "width" = '30',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '20',
        className = 'dt-center'
      ),
      list(
        targets = c(1),
        width = '250',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 35 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
          "}"
        )
      ),
      list(
        targets = c(3),
        width = '100',
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 20 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
          "}"
        )
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 10,
    lengthMenu = c(10, 50, 100, 200)
    #formatRound(columns = c(sample_col_numbers + 1), digits = 0)
  )
  
  if (type == "peptide"){
    
    df <- filter_peptide_table(session, input, output)
    
    peptide_table_DT <-  DT::datatable(df, rownames = FALSE, options = options)
    output$peptide_table <- DT::renderDataTable(peptide_table_DT, rownames = FALSE, extensions = c("FixedColumns"), 
                                                selection = 'single', options=options,
                                                callback = DT::JS('table.page(3).draw(false);'))
  }
  
  if (type == "protein"){
    
    df <- filter_protein_table(session, input, output)
    
    protein_table_DT <-  DT::datatable(df, rownames = FALSE, extensions = "FixedColumns", options = options)
    output$protein_table <- DT::renderDataTable(protein_table_DT) 
  }
  
}

#--------------------------------

#load design table
create_data_table2 <- function(session, input, output, type){
  cat(file = stderr(), "Function create_data_table...", "\n")
  require('DT')
  require('shinyjs')
  
  options <- list(
    selection = 'single',
    #autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(1),
        visibile = TRUE,
        "width" = '50',
        className = 'dt-center'
      ),
      list(
        targets = c(2),
        visible = TRUE,
        "width" = '50',
        className = 'dt-center'
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 10,
    lengthMenu = c(10, 50, 100, 200)
  )
  
  if (type == "peptide"){
    
    df <- filter_peptide_table(session, input, output)
    
    peptide_table_DT <-  DT::datatable(df, rownames = FALSE, options = options)
    output$peptide_table <- DT::renderDataTable(peptide_table_DT) 
  }
  
  if (type == "protein"){
    
    df <- filter_protein_table(session, input, output)
    
    protein_table_DT <-  DT::datatable(df, rownames = FALSE, extensions = "FixedColumns", options = options)
    output$protein_table <- DT::renderDataTable(protein_table_DT) 
  }
  
}


#--------------------------------------------------------------------------------------------------------------------

#load design table
create_meta_table <- function(session, input, output){
  cat(file = stderr(), "Function create_meta_table...", "\n")
  require('DT')
  require('shinyjs')
  
  options <- list(
    selection = 'single',
    #dom = 'Bfrtipl',
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = 500,
    scrollCollapse = TRUE,
    columnDefs = list(
      list(
        targets = c(0),
        visibile = TRUE,
        "width" = '30',
        className = 'dt-center'
      )
    ),
    ordering = TRUE,
    orderClasses = TRUE,
    fixedColumns = list(leftColumns = 1),
    pageLength = 10,
    lengthMenu = c(10, 50, 100, 200)
    #formatRound(columns = c(sample_col_numbers + 1), digits = 0)
  )
  
  df <- data.frame(t(df_meta))
  colnames(df) <- "Data"
    
  meta_table_DT <-  DT::datatable(df, rownames = TRUE, extensions = "FixedColumns", options = options)
  output$meta_table <- DT::renderDataTable(meta_table_DT) 

  
}