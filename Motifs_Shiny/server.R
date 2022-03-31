## Where objects are created.
## input : things you re getting from the user
## output :

function(input, output, session) {
  
  # function to read txt : 
  
  read_txt <- reactive({

    if (is.null(input$files_imported))
      return(NULL)     
    
    inFile <- input$files_imported
    
    df <- vroom(inFile$datapath,
                id = "FileName",
                delim = "\n",
                col_names = "words",
                progress = F)
    
    return(df)

  })
  
  # output : 
  
  output$import_text <- renderDT(
    df
    )
  
  output$plot <- renderPlot({
    if(!is.null(read_txt()))
    plot(nrow(read_txt()), ncol(read_txt()))
  } 
  )
  
  output$data_tagged <- renderTable({
    datatable(Etiquetage_UDPipe())
    
  })
  
  output$DownloadEtiquetage <-
    downloadHandler(
      filename = "UDPipe_corpus_complet.csv",
      content = function(file)
      {
        write.csv(corpus_cols_annotate,
                  "UDPipe_corpus_complet.csv",
                  fileEncoding = "UTF-8")
      }
    )
  
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## TF-IDF :

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##




function(input, output) {
  #
  
  output$tfidf_table <- renderTable({
    req(input$csv_viz) # To avoid error of no file when launching the app.
    
    df_idf <-
      read.csv(
        input$csv_viz$datapath,
        header = input$header,
        sep = input$sep
      )
    
    return(datatable(df_idf))
    
  })
  
}



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Function to render the datatable to go back to the texts :

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

function(input, output) {
  #
  output$data2 <- renderTable({
    req(input$file_retours_aux_textes) # To avoid error of no file when launching the app.
    
    df <-
      read.csv(
        input$file_retours_aux_textes$datapath,
        header = input$header,
        sep = input$sep
      )
    
    return(datatable(df))
    
  })
  
}
