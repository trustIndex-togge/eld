library(shiny)
library(brms)
library(tidyverse)

csv <- read.csv2("Bakken_ID.csv")

##############################
ui <- fluidPage(
  # Application title
  titlePanel("Bayesian Meta-Analysis"),
  
  # Sidebar with a choices input dropdown
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId =  "prior",
                  label = "Choose prior for Tau2",
                  choices = c("Half-Cauchy" = "HC", 
                              "Student-t" = "ST", 
                              "Inverse-gamma" = "IG")),
      selectInput(inputId =  "design",
                  label = "Select design",
                  choices = c("All", unique(data$study_design))),
      selectInput(inputId = "grade",
                  label = "Select grade",
                  choices = c("All", unique(data$Grade))),
      checkboxGroupInput(inputId = "rob", 
                         label = "Select risk of bias",
                         c(unique(data$rob)),
                         selected = (unique(data$rob)))
      ),
    ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Posterior distribution",
                           plotOutput("Plot1", hover = hoverOpts(id ="hover_plot1")),
                           verbatimTextOutput("hover_info")),
                  tabPanel("Summary", textOutput("Summary")),
                  tabPanel("Table", tableOutput("Table"))
      )
    )
  )


##############################
server <- function(input, output) {
  

  robfilter <- reactive({data() %>% filter(
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
  
  
  
}

#############################

