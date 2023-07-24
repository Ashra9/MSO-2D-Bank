source("usePackages.R")
#source("database/database.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","bs4Dash", "plotly", "fresh", "RMySQL", "imola")
loadPkgs(pkgnames)
#Define your custom theme
my_theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF",
    text_light = "#272c30"
  ),
  bs4dash_layout(
    main_bg = "#353c42"
  ),
  bs4dash_sidebar_light(
    bg = "#272c30",
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30",
    submenu_color = "#FFF",
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
  ),
  bs4dash_color(
    gray_900 = "#FFF", white = "#272c30"
  ),
  bs4dash_font(
    weight_bold = 900
  )
)
# Define UI function for the module
dashboardUI <- function(id) {
  ns <- NS(id)
  useShinyjs()
  fluidPage(
    fluidPage(
      ),
      tagList(
        dashboardPage(
          # freshTheme = my_theme,
          header = dashboardHeader(
            h2("Game of Loans", style="color:#ff851b"),
            title = dashboardBrand(
              title = "Retail Bank Simulator",
              color = "success",
              href = NULL,
              image = "Team logo.png"
            ),
            sidebarIcon = shiny::icon("bars"),
            controlbarIcon = shiny::icon("table-cells")
            #rightUi = uiOutput(ns("nextButton"))
          ),
          sidebar = bs4DashSidebar(
            collapsed = TRUE,
            sidebarUserPanel(
              name = "No Bank Runs!"
            ),
            uiOutput("sidebarmenu"),
            bs4SidebarMenu(
              id=ns("sidebar"),
              bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
              bs4SidebarMenuItem("Tutorial", tabName = "tutorial", icon = icon("dashboard")),
              bs4SidebarMenuItem("Game", tabName = "game", icon = icon("gamepad")),
              bs4SidebarMenuItem("Leaderboard", tabName = "leaderboard", icon = icon("trophy"))
            )
          ),
          title = "Game of Loans Simulator",
          body = dashboardBody(
            bs4TabItems(
              bs4TabItem(
                tabName = "game",
                gridPanel(
                  template = "sidebar-right",
                  bs4Card(
                    title = "Current Stats",
                    width = 12,
                    height = NULL,
                    descriptionBlock(
                      header = "1200", 
                      text = "Total Cash",
                      rightBorder = FALSE,
                      marginBottom = FALSE
                    ),
                    descriptionBlock(
                      number = "18%", 
                      numberColor = "secondary", 
                      numberIcon = icon("caret-down"),
                      header = "1200", 
                      text = "GOAL COMPLETION", 
                      rightBorder = FALSE,
                      marginBottom = FALSE
                    )
                  ),
                  actionButton(
                    ns("nextmonth"), 
                    "Next Month",
                    status = "primary", 
                    outline = TRUE,
                    flat = TRUE,
                    size = "lg",
                    class = "nxtbtn"
                  ),
                  tags$style(".nxtbtn {height: 100px;}")
                ),
                fluidRow(
                  box(
                    title = "Deposits and Withdrawals",
                    width = 4,
                    height = "100px",
                    "Welcome to the dashboard!"
                  ),
                  box(
                    title = "Loan Purchasing",
                    width = 4,
                    #height = "100px",
                    "Welcome to the dashboard!",
                    numericInput(ns("loan1"), label = "Loan 1", value = 0, min=0),
                    numericInput(ns("loan2"), label = "Loan 2", value = 0, min=0),
                    numericInput(ns("loan3"), label = "Loan 3", value = 0, min=0)
                  ),
                  box(
                    title = "Hello, Shiny!",
                    width = 4,
                    height = "100px",
                    "Welcome to the dashboard!"
                  ),
                  box(
                    title = "State of each inventory",
                    width = 12,
                    height = "100px",
                    "Welcome to the dashboard!"
                  )
                )
              ),
              bs4TabItem(
                tabName = "home",
                # Login page UI
                div(
                  class = "login-page",
                  div(
                    class = "form",
                    h2("Login"),
                    textInput("usernameInput", "Username"),
                    passwordInput("passwordInput", "Password"),
                    actionButton("loginButton", "Login")
                  )
                )
            ),
            bs4TabItem(
              tabName = "tutorial",
              tags$div(
                class = "tab-content",  # Optional class for styling
                tags$img(src = "Operational Concept.png", width = "100%", height = "100%")
              )
            )
            # Add your dashboard content here
          )
        )
      )
    )
  )
  
}

# Define server function for the module
dashboardServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Add any server logic here
      ns <- session$ns
      
      # reactiveValues object for storing items like the user password
      vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL, current_month=2)
      
      output$nextButton <- renderUI({
        req(input$sidebar == "game")
        tags$li(class = "dropdown", actionButton("nextmonth", "Next Month"))
      })
      
      # Check observation of next month
      observeEvent(input$nextmonth,{
        print("Current month:")
        print(vals$current_month)
        loanData <- getloanData(vals$current_month)
        print(loanData)
        
        # Enact withdrawals and ensure demand is met
        # ......
        
        # Update loans purchased
        # Check cash balance first.......
        purchase_list = list(type=c(1,2,3), num=c(input$loan1,input$loan2,input$loan3))
        print("Purchase List")
        print(purchase_list)
        #updateLoansPurchased(purchase_list, current_month=vals$current_month)
        
        # Update loans that reached maturity
        loanData <- subset(loanData, loanData$durationToMaturity!=0)
        print("Loan Maturity")
        print(loanData)
        #loanID_left_in_query <- generate_loanID_left_in_query(loanData)
        #updateLoansRemoved(loanID_left_in_query, defaulted=0, liquidated=0, current_month=3)
        
        # Update loans defaulted on
        # ......
        
        # Update new month
        vals$current_month <- vals$current_month + 1
      })
      # output$sidebarmenu <- renderUI({
      #   sidebarMenuItems <- list()
      #   observeEvent(input$nextmonth, {
      #     sidebarMenuItems[[1]] <- menuItem("User Info", tabName = "userInfoTab", icon=icon("user"))
      #     sidebarMenuItems[[2]] <- menuItem("Main Page", tabName = "game", icon = icon("gamepad"))
      #     sidebarMenuItems[[3]] <- menuItem("Analysis Page", tabName = "analysisTab", icon = icon("dashboard"))
      #     sidebarMenuItems[[4]] <- menuItem("Leaderboard", tabName = "leaderTab", icon = icon("star"))
      #     sidebarMenuItems[[5]] <- menuItem("Tutorial", tabName = "tutorialTab", icon = icon("person-chalkboard"))
      #   })
      #   return(sidebarMenu(id=ns("tabs"), .list=sidebarMenuItems))
      #   
      # })
    }
  )
}


# Use the module in an app

# Define UI
ui <- dashboardUI("mydashboard")

# Define server
server <- function(input, output, session) {
  # Call the server function of the module
  dashboardServer("mydashboard")
}

# Run the app
shinyApp(ui, server)
