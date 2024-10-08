#Ezra, Yu Liang, Xing Jun, Yixiu contributed to app.R 
source("usePackages.R")
source("database/database.R")
source("routes/HelperServerFunctions.R")
source("routes/Progress Tracker.R")
source("routes/WithdrawalandLiquidateHelperfunctionsforshow.R")
source("routes/Leaderboard Helper S&UI.R")
source("routes/Game UI.R")
source("routes/Graphs.R")

pkgnames <- c("tidyverse","shiny", "shinyjs","DBI","jsonlite","bs4Dash", "plotly", "fresh", "RMySQL")
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
    text_dark = "#272c30",
    text_light = "#FFF"
  ),
  bs4dash_layout(
    main_bg = "seagreen"
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
    primary = "lightblue", danger = "lightblue", light = "#272c30"
  ),
  bs4dash_font(
    weight_bold = 900
  )
)


# Define UI function for the module
dashboardUI <- function(id) {
  ns <- NS(id)
  useShinyjs()
      tagList(
        dashboardPage(
          freshTheme = my_theme,
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
                tags$head(tags$style(HTML("
                @import url('https://fonts.googleapis.com/css?family=Titillium%20Web');
                                          * {font-family: 'Titillium Web'};"))),
                uiOutput(ns("ingame"))
              ),
              bs4TabItem(
                tabName = "home",
                # Login page UI
                tags$style(HTML("
                /* Custom styles for inverse color scheme */
                .login-page {
                  background-color: #555; /* Dark background color */
                  color: #fff; /* Light text color */
                }
                .form {
                  background-color: #555; /* Dark background color for the form container */
                  color: #fff; /* Text color for input fields
                }
              ")),
                div(
                  class = "login-page",
                  div(
                    class = "form",
                    h2("Login"),
                    textInput(ns("usernameInput"), "Username"),
                    passwordInput(ns("passwordInput"), "Password"),
                    actionButton(ns("loginButton"), "Login"),
                    actionButton(ns("registerButton"), "Register"),
                    uiOutput(ns("loggedInAs"))
                  )
                )
            ),
            bs4TabItem(
              tabName = "tutorial",
              div(
                class = "carousel slide",  # Optional class for styling
                carousel(
                  id = ns("mycarousel"),
                  indicators = TRUE,
                  carouselItem(
                    img(src = "1.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "2.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "3.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "4.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "5.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "6.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "7.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "8.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "9.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "10.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "11.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "12.png", width = "100%", height = "100%")
                  ),
                  carouselItem(
                    img(src = "13.png", width = "100%", height = "100%")
                  )
                ),
                actionButton(ns("startGame"), "Play",status = "warning", size = "lg" )
              )
            ),
            bs4TabItem(
              tabName = "leaderboard",
              uiOutput(ns("ldbrd"))
            )
            # Add your dashboard content here
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
      

      # Clear database
      start_game_clear_tables()
      
      # Initialise cash on hand with deposits
      gamestate <- getGameState(1)
      deposits <- randomiser(gamestate$depositsMean, gamestate$depositsSTD)
      print(paste("Deposits amount:", deposits))
      
      # reactiveValues object for storing items like the user password
      vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL, gamevariantid=1, current_month=1, cashOnHand=deposits, deposits=deposits, withdrawals=0, loanPayout=0, loanData = NULL,
                            completedLoansReachMaturity = NULL, completedLoansDefaulted = NULL, completedLoansLiquidated = NULL,
                            gamestate = gamestate,
                            cashInventory = NULL,
                            numberofeachtypeofloan=NULL,
                            percentage=0.7,
                            endgame="F")


      #after reading instructions and clicking the play button
      observeEvent(input$startGame,{
        updateTabItems(session, "sidebar", selected = "game")
      })
      
      #display game UI
      endgameServer(input,output,session,vals)

      
      # Check observation of next month
      next_button(input,output,session, vals)
      after_withdrawal(input, output, session, vals)
      
      #rendering the UI for progress tracker
      output$progressTrackers <- renderUI({
        stateofProgressUI(session)
      })
      
      #render the progress tracker logic
      serverProgressTracker(input,output,vals)
      
      #render completed loans table
      completedLoansTracker(input, output, vals)
      
      #for updating the display cards
      output$totalCash <- renderUI(vals$cashOnHand)
      
      #for updating the month no.
      output$currMonth <- renderUI(paste0("Current Month: ", vals$current_month, "/36"))
      
      # plot graph
      plotCashGraph(input, output, vals)
      
      #Login and register###################
      # when registering
      observeEvent(input$registerButton,{
        showModal(passwordModal(session = session))
      })
      #check if the login is successfull, then go to tutorial for instructions
      # login_checker(input,output, session)
      
      # Fire some code if the user clicks the passwordok button
      observeEvent(input$passwordok, {
        # Check that password1 exists and it matches password2
        if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
          #store the password and close the dialog
          vals$password <- input$password1
          # print(vals$password) # for debugging
          vals$playername = registerPlayer(vals$password)
          if (!is.null(vals$playername)){
            vals$playerid <- getPlayerID(vals$playername,vals$password)
          }
          # print(vals$playerid) # for debugging
          removeModal()
        } else {
          showModal(passwordModal(failed = TRUE, session))
        }
      })
      
      # Fire some code if the user clicks the login button
      observeEvent(input$loginButton, {
        # Get the playerID and check if it is valid
        playerid <- getPlayerID(input$usernameInput,input$passwordInput)
        if (playerid>0) {
          #store the playerid and playername and close the dialog
          vals$playerid <- playerid
          print(vals$playerid) # for debugging
          vals$playername <- input$usernameInput
          print(vals$playername) # for debugging
          #goes to instructions page
          updateTabItems(session, "sidebar", selected = "tutorial")
        } else {
          #show alert
          print("Wrong credentials")
        }
      })
      
      # React to successful login
      output$loggedInAs <- renderUI({
        if (is.null(vals$playername))
          "Not logged in yet."
        else
          vals$playername
      })
      
      #Leaderboard####################
      
      #for displaying leaderboard in leaderboard tab
      output$ldbrd <- renderUI({
        req(vals$cashOnHand,vals$playerid) # if vals$score is NULL, the controls will not be visible
        tagList(
          tableOutput(ns("leadboard2"))
        )
      })
      output$leadboard2 <- renderTable({
        numclicks <- input$publishscore #to force a refresh whenever one of these buttons is clicked
        leadboard2 <- getLeaderBoard(vals$gamevariantid)
        leadboard2}
      )
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
