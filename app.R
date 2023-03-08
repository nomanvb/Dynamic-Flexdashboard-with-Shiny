
library(shiny)
library(googlesheets4)
library(DT)

# Oth section
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)
  
  #googledrive::drive_auth()  # run one time to get auth token in local. use gmail account r.shiny.go@gmail.com  pass: ssgGDtPgmdueE$4

  #googlesheets4::gs4_auth()   # run one time to get auth token in local. use gmail account r.shiny.go@gmail.com  pass: ssgGDtPgmdueE$4




# Define UI for application that draws a histogram
ui <- fluidPage(

  shinyjs::useShinyjs(),
  # Application title
  titlePanel("Automatically read google sheet and refresh"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  #gs4_auth(cache=".secrets", email="r.shiny.go@gmail.com ")

  rt_data <- reactive({
    invalidateLater(20000)
    googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1u5J002c-jq1UGDhhwlRRHm1UrBAWuXj9IuG5XKpLCP8/edit#gid=0")
  })
  
  proxy <- dataTableProxy("distPlot")
  observe({
    replaceData(
      proxy,
      rt_data(),
      resetPaging=F,
      clearSelection=F
    )
  })

  shinyjs::runjs(
    "function reload_page() {
    window.location.reload();
    setTimeout(reload_page, 20000);}
    setTimeout(reload_page, 20000);")
      
      output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- isolate(rt_data())$Count
        class(x)
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
