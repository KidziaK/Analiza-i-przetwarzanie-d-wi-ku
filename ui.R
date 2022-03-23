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
      menuItem( "No Upload Data",
                tabName = "test",
                badgeColor = "green",
                icon = icon("upload")
      )
      
    )
    
  ),
  dashboardBody(
    conditionalPanel(
      'input.sidebar == "upload"',
      "Upload Data"
    ),
    conditionalPanel(
      'input.sidebar == "test"',
      "test"
    )
    
  )
)