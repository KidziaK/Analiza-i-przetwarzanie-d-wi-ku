server <- function(input, output, session) {

  source("functions.R", local = TRUE)
  
  output$files <- renderTable(input$upload)
  wav_files <- NULL
  selected_wav <- NULL
  selected_frame <- 1

  
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
          width = 6,
          uiOutput("parameters_plot_ui")
        ),
        column(
          width = 3,
          uiOutput("current_frame_plot_ui")
        )
      )
    )
  })
  
  # UI for Volume box
  output$volume <- renderValueBox({
    req(input$current_frame_number)
    
    valueBox(calculate_volume(selected_frame - mean(selected_frame)), 
             "Volume", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # Slider for STE box
  output$STE <- renderValueBox({
    req(input$current_frame_number)
    
    valueBox(calculate_STE(selected_frame - mean(selected_frame)), 
             "Short time energy", 
             icon = icon("bolt"), 
             color = "green")
  })
  
  # Slider for ZCR box
  output$ZCR <- renderValueBox({
    req(input$current_frame_number)
    
    valueBox(calculate_ZCR(selected_frame - mean(selected_frame), ifelse(is.null(selected_wav), 1, selected_wav@samp.rate)), 
             "Zero crossing rate", 
             icon = icon("times"), 
             color = "green")
  })
  
  # Slider for paramters chooser
  output$parameters_choose_ui <- renderUI({
    box(
      title = span(icon("list-alt"), "Choose"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      uiOutput("audio_file_selecter"),
      sliderInput("frames", "Number of frames:",
                  min = 1, max = 1000, value = 500
      ),
      sliderInput("overlap", "Size of overlap:",
                  min = 0, max = 100, value = 0
      ),
      uiOutput("current_frame_number")
    )
  })
  
  # UI for current frame slider
  output$current_frame_number <- renderUI({
    req(input$frames)
    
    sliderInput("current_frame_number", "Current frame:",
                min = 1, max = input$frames, value = 1
    )
  })
  
  # UI for plot box
  output$parameters_plot_ui <-  renderUI({
    box(
      title = span(icon("chart-line"), "Signal Plot"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
     
      plotOutput("plot")
    )
  })
  
  output$current_frame_plot_ui <-  renderUI({
    box(
      title = span(icon("chart-line"), "Current Frame"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      plotOutput("frame_plot")
    )
  })
  
  output$plot <- renderPlot({

    req(input$select_file)
    
    snd <- selected_wav@left - mean(selected_wav@left)
    
    n <- length(snd)
    
    p <- ggplot(data.frame(Time = 1:n,  Amplitude = snd),
                aes(x = Time, y = Amplitude)) +
      geom_line() 

    
    p
  })
  
  # UI for current frame plot
  output$frame_plot <-  renderPlot({
    req(input$select_file)
    req(input$current_frame_number)
    
    frame_size <- length(selected_wav@left)/input$frames
    
    selected_frame <<- selected_wav@left[
      (1 + (input$current_frame_number - 1)*(frame_size)):(1 + input$current_frame_number*frame_size)] 

    snd <- selected_frame - mean(selected_frame)
    
    p <- ggplot(data.frame(Time = (1 + (input$current_frame_number - 1)*(frame_size)):(1 + input$current_frame_number*frame_size),  
                           Amplitude = snd),
                aes(x = Time, y = Amplitude)) +
      geom_line() 
    
    p
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
  

  
  observeEvent(input$current_frame_number, {
    req(selected_wav)
    req(input$frames)

    frame_size <- round(length(selected_wav@left)/input$frames)
    
    selected_frame <<- selected_wav@left[
      (1 + (input$current_frame_number - 1)*(frame_size)):(1 + input$current_frame_number*frame_size)] 
  })
  
  
  
  
}