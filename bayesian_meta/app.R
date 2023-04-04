library(shiny)
library(metafor)
library(brms)
library(tidyverse)
#library(tidybayes)

d <- read.csv("data.csv")
data <- d %>% filter(!is.na(study_ID))

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
                   min = 500,
                   max = 10000,
                   step = 500),
      numericInput(inputId = "iterations",
                   label = "Choose number of iterations (number can't be lower than practice iterations)",
                   value = 5000,
                   min = 1000,
                   max = 10000,
                   step = 500),
      actionButton(inputId = "go",
                   label = "Run model")
      ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Posterior distribution",
                           plotOutput("Plot1")),
                  tabPanel("Summary", verbatimTextOutput("Summary")),
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
  
  
  ma <- eventReactive(input$go, {
    
#    withProgress(
#      
#      message='Model is running',
#      detail='More iterations can take longer to run',
#      min = 0,
#      max = input$iterations,
#      
#      {for (i in seq_len(input$iterations)) 
#            { 
#             incProgress(1 / length(input$iterations), detail = paste("iteration", i))
#            }
#        }
      
      brm(yi|se(vi) ~ 1 + (1|study_ID),
          data = ef(),
          prior = prior(),
          warmup = input$warmup, iter = input$iterations)  
#    )
    
   }
)
  
##################################
#prepare data for the density plot
  
  post.samples <- reactive({
    ma() %>% posterior::as_draws_df(., c("^b_", "^sd_"), regex = TRUE) %>% 
      rename("smd" = "b_Intercept", "tau" = "sd_study_ID__Intercept")
  })
  
  output$Plot1 <- renderPlot({
    
    ggplot(aes(x = smd), data = post.samples()) +
      geom_density(fill = "lightblue",                # set the color
                   color = "lightblue", alpha = 0.7) +  
      geom_point(y = 0,                               # add point at mean
                 x = mean(post.samples()$smd)) +
      labs(x = expression(italic(SMD)),
           y = element_blank()) +
      theme_minimal()
    
   #ggplot(aes(x = tau), data = post.samples()) +
   #  geom_density(fill = "lightgreen",               # set the color
   #               color = "lightgreen", alpha = 0.7) +  
   #  geom_point(y = 0, 
   #             x = mean(post.samples()$tau)) +        # add point at mean
   #  labs(x = expression(tau),
   #       y = element_blank()) +
   #  theme_minimal()
    #pp_check(ma())
    })
  
  output$Summary <- renderPrint({
    summary(ma())
    })
  
  output$Table <- renderTable({
    ef()
    })
  
}

#############################
################## run the app ####################
shinyApp(ui, server)
