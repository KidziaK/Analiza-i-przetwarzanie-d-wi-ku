server <- function(input, output) {
  
  output$files <- renderTable(input$upload)
  
  output$upload_ui <- renderUI({
    box(
      title = span(icon("upload"), "Upload Data"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fileInput(
        "upload", 
        NULL, 
        accept = "audio/*",
        buttonLabel = "Upload...", 
        multiple = TRUE
        ),
      #TODO make columns shorter so that they dont come out of bounds
      tableOutput("files")
    )
  })
}