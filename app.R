source("usePackages.R")
source("database/database.R")
source("routes/HelperServerFunctions.R")
source("routes/WithdrawalandLiquidateHelperfunctionsforshow.R")
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
                  box(
                    title = "Current Stats",
                    width = 12,
                    
                    fluidRow(
                      bs4Card(
                        background = "maroon",
                        title = "Title",
                        width = 4,
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
                      bs4Card(
                        background = "lime",
                        title = "Title",
                        width = 4,
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
                      bs4Card(
                        background = "info",
                        title = "Title",
                        width = 4,
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
                      )
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
                    "Select No. of each type of loan!",
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
                    
                    uiOutput(ns("progressTrackers"))
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
                    textInput(ns("usernameInput"), "Username"),
                    passwordInput(ns("passwordInput"), "Password"),
                    actionButton(ns("loginButton"), "Login"),
                    actionButton(ns("registerButton"), "Register")
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
      
      # Initialise cash on hand with deposits
      gamestate <- getGameState(1)
      deposits <- randomiser(gamestate$depositsMean, gamestate$depositsSTD)
      print(paste("Deposits amount:", deposits))
      
      # reactiveValues object for storing items like the user password
      vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL, current_month=1, cashOnHand=deposits, deposits=deposits, withdrawals=NULL, loanPayout=NULL)

      # when registering
      observeEvent(input$registerButton,{
        showModal(passwordModal())
      })
      
      #check if the login is successfull, then go to tutorial for instructions
      login_checker(input,output, session)
      
      # Check observation of next month
      loan_select(input,output,session, vals)
      
      #select loans to liquidate modal
      selectLoansLiquidateModal()
      
      #liquidate loans event
      LiquidateLoans(cashbalance=1400, withdrawalamount=1860, 
                     loanData=data.frame(loanID = c(1,2,3,4,5), 
                                         loanType=c(1,2,3,2,2), 
                                         loanValue = c(200, 300, 600, 300, 300), 
                                         durationToMaturity = c(3,1,2,2,3)), 
                     loansselected=SelectLoans(c(2,1),c(2,1)), percentage=0.7)
      #for the progress trackers
      loanData <- data.frame(
        loanID = c(1, 2),
        loanType = c(1, 2),
        loanValue = c(1000, 2000),
        loanmaturity = c(2, 3),
        loan_risk = c(0.03, 0.05)
      )
      #rendering the UI for progress tracker
      output$progressTrackers <- renderUI({
        stateofProgressUI(session)
      })
      #render the progress tracker logic
      serverProgressTracker(input,output,loanData)
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
