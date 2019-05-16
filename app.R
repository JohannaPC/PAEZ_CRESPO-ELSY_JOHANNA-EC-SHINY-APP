#SHINY TUTORIAL
#Every “app.R” script has three components:
#a user interface object (called ui)
#a server function
#a call to the shinyApp function

#The user interface (ui) object controls the layout and appearance of your app. 
#The server function contains the instructions that your computer needs to build 
#your app. Finally the shinyApp() function creates Shiny app objects from an 
#explicit UI/server pair.

library(shiny)
library(DT) #visualize data in shiny
library(tidyverse)
library(curl)
f <- curl("https://raw.githubusercontent.com/JohannaPC/PAEZ_CRESPO-ELSY_JOHANNA-EC-SHINY-APP/master/VocalizacionesChurucos.csv")
z <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = TRUE)
c <- select(z, AveFreqMax, VarFreqMax, AveFreqMin, VarFreqMin, AveDur, VarDur)
v <- names(c)
a <- select(z, vocalname, ID, age, sex)
b <- names(a)
# Define the UI ----
ui <- fluidPage(
  titlePanel(h1("Woolly Monkeys Vocalizations")),
    sidebarLayout(
      sidebarPanel(
        selectInput(
        "predictors",
        label = "Monkeys' name, sex or age / vocal name?",
        choices = c("",b) # NOTE: the "" is needed to allow NO VARIABLE to be the default value
      ),
      br(),
      selectInput(
        "response",
        label = "Choose a variable ... ",
        choices = c("",v)
      ),
      br(),
      tableOutput("Vocalization results"),
      style="text-align:center"
    ),
    mainPanel(
      dataTableOutput("datatable"),
      plotOutput("plot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$datatable <- renderDataTable(z, options = list(paging = TRUE, lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')), pageLength = 10))
  
  m <- reactive({
    mod <- NULL
    if (input$response=="" | length(input$predictors)==0){return(mod)}
    mod <- paste0(input$response," ~ ", input$predictors[1])
    if (length(input$predictors) > 1){
      for (i in 2:length(input$predictors)){
        mod <- paste0(mod, " + ", input$predictors[i])
      }
    }
    return(mod)
  })
  
  output$modelresults <- renderTable({
    if (!is.null(m())){
      res <- lm(data=z,formula=m())
      res <- as.data.frame(coefficients(res))
      res
    }
  }, width="100%", rownames = TRUE, striped=TRUE, spacing="s", bordered=TRUE, align="c", digits = 3)
  
  output$plot <- renderPlot({
    if (!is.null(m()) & length(input$predictors==1)){
      y <- input$response
      x <- input$predictors
      if (class(z[[x]])!="factor"){
        p <- ggplot(data=z,aes(x=z[[x]],y=z[[y]])) + geom_point() + geom_smooth(method=lm)
      } else {
        p <- ggplot(data=z,aes(x=z[[x]],y=z[[y]])) + geom_boxplot()
      }
      p <- p + xlab(x) + ylab(y)
      p
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)