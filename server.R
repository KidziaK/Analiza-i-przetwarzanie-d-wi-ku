server <- function(input, output, session) {

  source("functions.R", local = TRUE)
  
  output$files <- renderTable(input$upload)
  wav_files <- NULL
  selected_wav <- NULL
  
  # UI for upload tab
  output$upload_ui <- renderUI({
    box(
      title = span(icon("upload"), "Upload Data"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 12,
          fileInput(
            "upload", 
            NULL, 
            accept = "audio/*",
            buttonLabel = "Select files...", 
            multiple = TRUE
          )
        )
      ),
      
      #TODO make columns shorter so that they dont come out of bounds
      tableOutput("files")
    )
  })
  
  # UI for parameters tab
  output$parameters_ui <- renderUI({
    box(
      title = span(icon("chart-pie"), "Basic parameters"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fluidRow(

        valueBoxOutput("volume"),
        valueBoxOutput("STE"),
        valueBoxOutput("ZCR")
    
      ),
      fluidRow(
        column(
          width = 3,
          uiOutput("parameters_choose_ui")
        ),
        column(
          width = 9,
          uiOutput("parameters_plot_ui")
        )
      )
    )
  })
  
  # UI for Volume box
  output$volume <- renderValueBox({
    req(input$select_file)
    
    valueBox(calculate_volume(selected_wav@left - mean(selected_wav@left)), 
             "Volume", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # UI for STE box
  output$STE <- renderValueBox({
    req(input$select_file)
    
    valueBox(calculate_STE(selected_wav@left - mean(selected_wav@left)), 
             "Short time energy", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # UI for ZCR box
  output$ZCR <- renderValueBox({
    req(input$select_file)
    
    valueBox(calculate_ZCR(selected_wav@left - mean(selected_wav@left), selected_wav@samp.rate), 
             "Zero crossing rate", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # UI for paramters chooser
  output$parameters_choose_ui <- renderUI({
    box(
      title = span(icon("list-alt"), "Choose"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      uiOutput("audio_file_selecter")
    )
  })
  
  # UI for plot box
  output$parameters_plot_ui <-  renderUI({
    box(
      title = span(icon("chart-line"), "Plots"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
     
      plotOutput("plot")
    )
  })
  
  output$plot <- renderPlot({

    req(input$select_file)
    
    snd <- selected_wav@left - mean(selected_wav@left)
    
    plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
  })
  
  # Data upload event handler
  observeEvent(input$upload, {
    wav_files <<- lapply(input$upload$datapath, readWave)
  })
  
  # UI for audio file selector
  output$audio_file_selecter <- renderUI({
    selectInput("select_file", label = h5("Select audio"), 
                choices = input$upload$name, 
                selected = 1)
  })
  
  # Change audio file event handler
  observeEvent(input$select_file, {

    req(input$upload)
    req(wav_files)

    selected_wav <<- wav_files[which(input$upload$name == input$select_file)][[1]]
  })
  
  
}