

ui <- dashboardPage(
  skin = "yellow",
  
  dashboardHeader(
    title = "Sound Analysis"
    ),

  dashboardSidebar(

    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      menuItem( "Upload Data",
                tabName = "upload",
                badgeColor = "green",
                icon = icon("upload")
                ),
      menuItem( "Parameters",
                tabName = "test",
                badgeColor = "green",
                icon = icon("chart-line")
      )
      
    )
    
  ),
  dashboardBody(
    conditionalPanel(
      'input.sidebar == "upload"',
      uiOutput("upload_ui")
    ),
    conditionalPanel(
      'input.sidebar == "test"',
      uiOutput("parameters_ui"),
      
    )
    
  )
)