library(shiny)
library(metafor)
library(brms)
library(tidyverse)

data <- read.csv("updated_mock.csv")

##############################
ui <- fluidPage(
  # Application title
  titlePanel("Bayesian Meta-Analysis"),
  
  # Sidebar with a choices input dropdown
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId =  "prior",
                  label = "Choose prior for Tau2",
                  choices = c("Cauchy" = "cauchy", 
                              "Student-t" = "student_t", 
                              "normal" = "normal")),
      selectInput(inputId =  "prior_es",
                  label = "Choose prior for Intercept",
                  choices = c("Cauchy" = "cauchy", 
                              "Student-t" = "student_t", 
                              "normal" = "normal")),
      selectInput(inputId =  "design",
                  label = "Select design",
                  choices = c("All", unique(data$study_design))),
      checkboxGroupInput(inputId = "rob", 
                         label = "Select risk of bias",
                         c(unique(data$rob)),
                         selected = (unique(data$rob))),
      numericInput(inputId = "warmup",
                   label = "Choose number of practice iterations",
                   value = 2000,
                   min = 1000,
                   max = 10000,
                   step = 1000),
      numericInput(inputId = "iterations",
                   label = "Choose number of iterations (number can't be lower than practice iterations)",
                   value = 5000,
                   min = 1000,
                   max = 20000,
                   step = 1000)
      ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Posterior distribution",
                           plotOutput("Plot1")),
                  tabPanel("Summary", textOutput("Summary")),
                  tabPanel("Table", tableOutput("Table"))
      )
    )
  )
)

##############################
server <- function(input, output) {
  

  robfilter <- reactive({data %>% filter(
    rob %in% input$rob) 
  })
  
  
  effectsinput <- reactive({                #### Create reactive dataframe based on selected study design and grade
    
    if  (any(input$design == "All")) {
      robfilter()
    } else {
      robfilter() %>% filter(
        study_design == input$design)}
    
    
  })
  
  
  #############################################
  
  ef <- reactive({escalc("SMD", data = effectsinput(), m1i = m1i, n1i = N1i, sd1i = sd1i,
               m2i = m2i, n2i = N2i, sd2i = sd2i)})
  
  
  ##############################

  #TODO: for now distribution values are hard coded, but maybe make it interactive as well?  
  prior <- reactive({
          c(prior_string(paste0(input$prior_es,"(0,1)"), class = "Intercept"),
             prior_string(paste0(input$prior,"(0,0.05)"), class = "sd"))})
  
  
  ma <- reactive({brm(yi|se(vi) ~ 1 + (1|study_ID),
                       data = ef(),
                       prior = prior(),
                       warmup = input$warmup, iter = input$iterations)
#    #show progress of running the model
#    withProgress(message = 'Calculation in progress',
#                 detail = 'Higher number of iterations takes longer to run...',
#                 {for (i in seq_len(input$iterations)) {
#                   incProgress(100/ length(input$iterations))
#                   Sys.sleep(0.5)      
#                 }
#                 })
})
  
  output$Plot1 <- renderPlot({pp_check(ma())

  })
  
  output$Summary <- renderPrint(summary(ma()))
  
}

#############################
################## run the app ####################
shinyApp(ui, server)
