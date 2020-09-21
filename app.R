# https://rdrr.io/cran/prevalence/man/betaPERT.html

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Beta Parameter getter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Enter your PERT parameters to get Beta shape parameters"),
        numericInput("pert_min", p("Min"), value=0),
        numericInput("pert_ml", p("ML"), value=3000),
        numericInput("pert_max", p("Max"), value=10000),
        h2("Quick reference"),
        p("How far along the range is the most likely?"),
        tableOutput("cheat_sheet")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h2("Calculated values"),
        textOutput("mean"),
        textOutput("sd"),
        textOutput("alpha"),
        textOutput("beta"),
        h2("Plot"),
        plotOutput("distPlot"),
        h2("Excel formula"),
        p("Paste this into a cell to get a random number from this distribution"),
        textOutput("excel_formula"),
        h2("References & Source Code"),
        p("Calculation: https://rdrr.io/cran/prevalence/man/betaPERT.html"),
        p("Calculation: https://rdrr.io/github/n8thangreen/treeSimR/src/R/sample_distributions.R"),
        p("Calculation: https://www.vosesoftware.com/riskwiki/PERTdistribution.php"),
        p("Source code: https://github.com/blackfist/beta_maker")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  lambda <- 4 # Lambad can be adjusted for confidence
  calculate_mean <- reactive({
    (input$pert_min + (input$pert_ml * lambda) + input$pert_max)/(lambda +  2)
  })
  
  calculate_sd <- reactive({
    (input$pert_max-input$pert_min)/(lambda+2)
  })
  
  calculate_alpha <- reactive({
    mean <- calculate_mean()
    sd <- calculate_sd()
    if (mean == input$pert_ml) {
      return((lambda/2)+1)
    } else {
      return(
        ((mean-input$pert_min)*(2*input$pert_ml-input$pert_min-input$pert_max))/((input$pert_ml-mean)*(input$pert_max-input$pert_min)) 
      )
    }
  })
  
  calculate_beta <- reactive({
    alpha <- calculate_alpha()
    mean <- calculate_mean()
    max <- input$pert_max
    min <- input$pert_min
   ( alpha*(max-mean))/(mean-min)
  })
  
  
   output$mean <- renderText(paste0("Mean: ", calculate_mean()))
   output$sd <- renderText(paste0("standard deviation: ", calculate_sd()))
   output$alpha <- renderText(paste0("alpha: ", calculate_alpha()))
   output$beta <- renderText(paste0("beta: ",calculate_beta()))
   
   output$distPlot <- renderPlot({
     alpha <- calculate_alpha()
     beta <- calculate_beta()
     x_decimal <- seq(0,1,.001)
     y <- dbeta(x_decimal, alpha, beta)
     
     x <- (x_decimal * (input$pert_max - input$pert_min)) + input$pert_min
     
     ggplot(data=data.frame(x=x,y=y), aes(x,y)) +
       geom_line() +
       labs(title="Beta distribution of PERT estimate",
            subtitle=paste0("min=",input$pert_min,", most likely=",input$pert_ml,", max=",input$pert_max),
            caption=paste0("beta parameters: alpha=",alpha,", beta=",beta)) +
       theme_bw()
   })
     
   distance <- c("Bottom","1/10","1/5","1/4","3/10","1/3","2/5","1/2","3/5","2/3","7/10","3/4","4/5","9/10","Top")
   alpha <- c(1,1.4,1.8,2,2.2,2.3332,2.6,3,3.4,3.6664,3.8,4,4.2,4.6,5)
   beta <- c(5,4.6,4.2,4,3.8,3.6668,3.4,3,2.6,2.3336,2.2,2,1.8,1.4,1)
   output$cheat_sheet <- renderTable(data.frame(distance=distance,alpha=alpha,beta=beta), striped = TRUE,bordered = TRUE)
   
   output$excel_formula <- renderText({
     alpha <- calculate_alpha()
     beta <- calculate_beta()
     
     paste0("=BETA.INV(RAND(),",alpha,",",beta,",",input$pert_min,",",input$pert_max, ")")
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

