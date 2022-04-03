server <- function(input, output, session) {

  source("functions.R", local = TRUE)
  
  #################
  #################
  # Globals
  #################
  #################
  
  output$files <- renderTable(input$upload)
  wav_loaded <- reactiveVal(0)
  wav_files <- NULL
  selected_wav <- reactiveVal(0)
  selected_frame <- NULL
  volume_vector <- reactiveVal(0)
  frame_count <- reactiveVal(0)
  zcr_vector <- reactiveVal(0)
  ste_vector <- reactiveVal(0)

  #################
  #################
  # Main UI
  #################
  #################
  
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
      
      tableOutput("files")
    )
  })
  
  # UI for parameters tab
  output$parameters_ui <- renderUI({

    req(wav_loaded() == 1)
    fluidPage(
      box(
        title = span(icon("chart-pie"), "Basic parameters"),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          
          valueBoxOutput("VDR"),
          valueBoxOutput("LSTER"),
          valueBoxOutput("VSTD")
          
        ),
        fluidRow(
          
          valueBoxOutput("HZCRR"),
          valueBoxOutput("Entropy")
          
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
      ),
      
      fluidRow(
        column(
          width = 6,
          uiOutput("volume_plot_ui")
        ),
        column(
          width = 6,
          uiOutput("ZCR_plot_ui")
        )
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("STE_plot_ui")
          
        ),
        column(
          width = 6,
          uiOutput("AC_plot_ui")
        )
      )
      
    )
    
    
  })
  
  # UI for audio file selector
  output$audio_file_selecter <- renderUI({
    selectInput("select_file", label = h5("Select audio"), 
                choices = input$upload$name, 
                selected = 1)
  })
  
  #################
  #################
  # Boxes
  #################
  #################
  
  # UI for Volume box
  output$volume <- renderValueBox({
    
    valueBox(calculate_volume(selected_frame - mean(selected_frame)), 
             "Volume", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # Slider for STE box
  output$STE <- renderValueBox({
    
    valueBox(calculate_STE(selected_frame - mean(selected_frame)), 
             "Short time energy", 
             icon = icon("bolt"), 
             color = "green")
  })
  
  # Slider for ZCR box
  output$ZCR <- renderValueBox({
    
    valueBox(calculate_ZCR(selected_frame - mean(selected_frame), selected_wav()@samp.rate), 
             "Zero crossing rate", 
             icon = icon("times"), 
             color = "green")
  })
  
  # UI for SR box
  output$SR <- renderValueBox({
    
    SR <- calculate_SR(selected_frame - mean(selected_frame),ifelse(is.null(selected_wav()), 1, selected_wav()@samp.rate), 50, 0.02)
    
    valueBox(1*SR, 
             "SR", 
             icon = icon("volume-up"), 
             color = ifelse(SR == TRUE, "green", "red"))
  })
  
  # UI for VSTD box
  output$VSTD <- renderValueBox({
    
    valueBox(calculate_VSTD(selected_wav()@left - mean(selected_wav()@left), frame_count()),
             "VSTD", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # UI for VDR box
  output$VDR <- renderValueBox({
    
    valueBox(calculate_VDR(selected_wav()@left - mean(selected_wav()@left), frame_count()),
             "VDR", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # UI for LSTER box
  output$LSTER <- renderValueBox({
    valueBox(calculate_LSTER(selected_wav()@left - mean(selected_wav()@left), frame_count()), 
             "LSTER", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # UI for HZCRR box
  output$HZCRR <- renderValueBox({
    valueBox(calculate_HZCRR(selected_wav()@left - mean(selected_wav()@left), frame_count()), 
             "HZCRR", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  # UI for Entropy box
  output$Entropy <- renderValueBox({
    valueBox(calculate_entropy(selected_wav()@left - mean(selected_wav()@left), frame_count()), 
             "Entropy", 
             icon = icon("volume-up"), 
             color = "green")
  })
  
  #################
  #################
  # Sliders
  #################
  #################
  
  # Slider for paramters chooser
  output$parameters_choose_ui <- renderUI({
    box(
      title = span(icon("list-alt"), "Choose"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      uiOutput("audio_file_selecter"),
      uiOutput("frames"),
      uiOutput("current_frame_number")
    )
  })
  
  # UI for frame size dlider
  output$frames <- renderUI({
    sliderInput("frames", "Size of a frame:",
                min = 2, max = 1000, value = 10
    )
  })
  
  
  # UI for current frame slider
  output$current_frame_number <- renderUI({

    sliderInput("current_frame_number", "Current frame:",
                min = 1, max = frame_count(), value = 1
    )
  })
  
  #################
  #################
  # Plots
  #################
  #################
  
  # UI for plot box
  output$parameters_plot_ui <-  renderUI({
    req(input$select_file)
    box(
      title = span(icon("chart-line"), "Signal Plot"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
     
      plotOutput("plot")
    )
  })
  
  # UI for current frame plot
  output$current_frame_plot_ui <-  renderUI({
    req(input$select_file)
    box(
      title = span(icon("chart-line"), "Current Frame"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      plotOutput("frame_plot")
    )
  })
  
  
  # UI for volume plot
  output$volume_plot_ui <-  renderUI({
    req(input$select_file)
    box(
      title = span(icon("chart-line"), "Volume Plot"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      plotOutput("volume_plot")
    )
  })
  
  # UI for ZCR plot
  output$ZCR_plot_ui <-  renderUI({
    req(input$select_file)
    box(
      title = span(icon("chart-line"), "ZCR Plot"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      plotOutput("zcr_plot")
    )
  })
  
  # UI for STE plot
  output$STE_plot_ui <-  renderUI({
    req(input$select_file)
    box(
      title = span(icon("chart-line"), "STE Plot"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      plotOutput("ste_plot")
    )
  })
  
  # UI for Autocorelation plot
  output$AC_plot_ui <-  renderUI({
    box(
      title = span(icon("chart-line"), "Autocorrelation Plot"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      
      plotOutput("AC_plot")
    )
  })
  
  # UI for signal plot
  output$plot <- renderPlot({
    
    snd <- selected_wav()@left - mean(selected_wav()@left)
    
    n <- length(snd)
    
    p <- ggplot(data.frame(Time = 1:n,  Amplitude = snd),
                aes(x = Time, y = Amplitude)) +
      geom_line() 

    
    p
  })
  
  # UI for volume plot
  output$volume_plot <-  renderPlot({
    
    p <- ggplot(data.frame(Frame = 1:length(volume_vector()), Volume = volume_vector()),
                aes(x = Frame, y = Volume)) +
      geom_line() +
      geom_hline(yintercept=20, linetype="dashed", color = "red")
    
    p
  })
  
  # UI for ZCR plot
  output$zcr_plot <-  renderPlot({
    
    p <- ggplot(data.frame(Frame = 1:length(zcr_vector()), Volume = zcr_vector()),
                aes(x = Frame, y = Volume)) +
      geom_line() +
      geom_hline(yintercept=0.02, linetype="dashed", color = "red")
    
    p
  })
  
  # UI for STE plot
  output$ste_plot <-  renderPlot({
    
    p <- ggplot(data.frame(Frame = 1:length(ste_vector()), Volume = ste_vector()),
                aes(x = Frame, y = Volume)) +
      geom_line() 
    
    p
  })
  
  # UI for AC plot
  output$AC_plot <-  renderPlot({
    
    ac <- calculate_autocorelation(selected_wav()@left)
    
    p <- plot(ac)
    
    p
  })
  
  # UI for current frame plot
  output$frame_plot <-  renderPlot({
    req(input$frames)
    frame_size <- input$frames
    
    selected_frame <<- selected_wav()@left[
      (1 + (input$current_frame_number - 1)*(frame_size)):(1 + input$current_frame_number*frame_size)] 
    
    snd <- selected_frame - mean(selected_frame)
    
    p <- ggplot(data.frame(Time = (1 + (input$current_frame_number - 1)*(frame_size)):(1 + input$current_frame_number*frame_size),  
                           Amplitude = snd),
                aes(x = Time, y = Amplitude)) +
      geom_line() 
    
    p
  })
  
  #################
  #################
  # Event handlers
  #################
  #################
  
  # Data upload event handler
  observeEvent(input$upload, {
    wav_files <<- lapply(input$upload$datapath, readWave)
    selected_wav(wav_files[[1]])
    selected_frame <<- 1
    frame_count(1000)
    volume_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_volume))))
    zcr_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_ZCR))))
    ste_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_STE))))
    
    wav_loaded(1)
  })
  
  observeEvent(input$frames, {
    frame_count(length(selected_wav()@left) %/% input$frames)

    volume_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_volume))))
    zcr_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_ZCR))))
    ste_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_STE))))

  })
  
  # Change audio file event handler
  observeEvent(input$select_file, {

    selected_wav(wav_files[which(input$upload$name == input$select_file)][[1]])
    frame_count(length(selected_wav()@left) %/% input$frames)
    volume_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_volume))))
    zcr_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_ZCR))))
    ste_vector(suppressWarnings(unlist(lapply(split(selected_wav()@left, seq(1, frame_count())), FUN = calculate_STE))))
  })
  

  
  observeEvent(input$current_frame_number, {

    
    selected_frame <<- selected_wav()@left[
      (1 + (input$current_frame_number - 1)*(input$frames)):(1 + input$current_frame_number*input$frames)] 
    
    
  })
  
}