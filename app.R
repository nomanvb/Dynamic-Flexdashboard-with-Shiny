
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
googledrive::drive_auth()

googlesheets4::gs4_auth()




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

  gs4_auth(cache=".secrets", email="ariful.ambia@gmail.com")

  rt_data <- reactive({
    invalidateLater(20000)
    googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1_1e_WXmDuCDkcP-b9DfEDl2c0fA5rFNg0IjjfmaURk4/edit?usp=share_link")
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
        x    <- isolate(rt_data())$B
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
