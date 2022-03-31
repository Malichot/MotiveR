## References :
# https://stackoverflow.com/questions/31585191/performing-operations-after-uploading-a-csv-file-in-shiny-r

bootstrapPage(
  navbarPage("MotiveR",inverse = T,
    tabPanel("Import",
             sidebarLayout(
               sidebarPanel(
                 # Upload csv file :
                 fileInput(
                   inputId = "files_imported",
                   label = "Import",
                   multiple = TRUE,
                   accept = c(".txt"),
                   buttonLabel = "Download your .txt",
                   placeholder = "*.txt"
                 ), downloadButton(outputId = "downloadData", label = "Download"),
                 # actionButton("EtiquetageUDpipe", label = "Étiquetage !"),
                 # downloadButton(outputId = "DownloadEtiquetage", label = "Download the labelled corpus"),
               ),
               mainPanel(DTOutput('import_text')) # To display the table
             ), ),
    
    
    tabPanel("Transformation en motifs"),
    
    
    tabPanel("TF-IDF", sidebarPanel(
      # Upload csv file :
      fileInput(
        inputId = "csv_viz",
        label = "TF-IDF",
        multiple = FALSE,
        accept = c(".csv"),
        buttonLabel = "Télécharger votre csv",
        placeholder = "Tf-idf_test.csv"
      )
    ), mainPanel(DTOutput(outputId = "tfidf_table"), plotOutput("tfidf_viz"))),
    
    
    tabPanel("ACP"),
    
    
    tabPanel("Calcul de spécificité"),
    
    
    tabPanel(
      "Retours aux textes",
      sidebarLayout(
        sidebarPanel(
          # Upload csv file :
          fileInput(
            inputId = "file_retours_aux_textes",
            label = "Retours_aux_textes",
            multiple = FALSE,
            accept = c(".csv"),
            buttonLabel = "Télécharger",
            placeholder = "corpus_motifs_grams.csv"
          ),
          textInput("sep", label = "Entrer le séparateur utilisé dans le csv", value = ","),
          checkboxInput("header", label = "Le fichier contient un header", value = TRUE),
          
          # all the select inputs the user can choose :
          textInput(
            inputId = "MotifSelector",
            label = "Sélection du motif d'intérêt:",
            placeholder = "le NC de le NC"
          ),
          textInput(
            inputId = "Freq_Motif_Selector",
            label = "Sélection de la fréquence:",
            placeholder = "x"
          ),
          actionButton("RefreshSelection", label = "Refresh")
        ),
        
        mainPanel(DTOutput("data2")) # To display dataframe.
      ),
    ),
    
  )
)
