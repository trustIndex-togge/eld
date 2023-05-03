library(shiny)
library(metafor)
library(brms)
library(tidyverse)
#library(tidybayes)

#set where the datasets are TODO: change this to the actual repository url once we have it
data_dir <- "../csv_files"

# list the CSV files 
data_files <- list.files(path = data_dir, pattern = ".csv")

datasets <- setNames(lapply(data_files, function(file) {
  data <- read_csv(file.path(data_dir, file)) %>%
    mutate(rob_either = ifelse(is.na(rob), robins, rob)) %>% 
    filter(!is.na(study_id))
  return(data)
}
), 
tools::file_path_sans_ext(data_files))

##############################
ui <- fluidPage(
  # Application title
  titlePanel("Bayesian Meta-Analysis"),
  
  # Sidebar with a choices input dropdown
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset", choices = names(datasets)),
      downloadButton(outputId = "download",
                     label = "Download the dataset"),
      selectInput(inputId =  "prior",
                  label = "Choose prior for Tau2",
                  choices = c("Half-Cauchy" = "cauchy", 
                              "Student-t" = "student_t", 
                              "normal" = "normal")),
      selectInput(inputId =  "prior_es",
                  label = "Choose prior for Intercept",
                  choices = c("Cauchy" = "cauchy", 
                              "Student-t" = "student_t", 
                              "normal" = "normal")),
      selectInput(inputId =  "design",
                  label = "Select design",
                  choices = c("All", unique(datasets[[1]]$study_design))),
      selectInput(inputId =  "outcomes",
                  label = "Select outcomes",
                  choices = c("All", unique(datasets[[1]]$outcome_full))),
      selectInput(inputId = "grade",
                  label = "Select grade",
                  choices = c("All", unique(unique(unlist(strsplit(unique(as.character(datasets[[1]]$grade)), ":")))))),
      checkboxGroupInput(inputId = "rob", 
                         label = "Select risk of bias",
                         choices = unique(datasets[[1]]$rob_either),
                         selected = unique(datasets[[1]]$rob_either)),
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
server <- function(input, output, session) {
  
  #set up updating input boxes based on changed datasets
  unique_designs <- reactive({
    unique(datasets[[input$dataset]]$study_design)
  })
  
  unique_outcomes <- reactive({
    unique(datasets[[input$dataset]]$outcome_full)
  })
  
  unique_grades <- reactive({
    unique(unlist(strsplit(unique(as.character(datasets[[input$dataset]]$grade)), ":")))
  })
  
  unique_robs <- reactive({
    unique(datasets[[input$dataset]]$rob_either)
  })
  
  observe({
    updateSelectInput(session, "design",
                      label = "Select design",
                      choices = c("All", unique_designs()),
                      selected = "All")
    
    updateSelectInput(session, "outcomes",
                      label = "Select outcomes",
                      choices = c("All", unique_outcomes()),
                      selected = "All")
    
    updateSelectInput(session, "grade",
                      label = "Select grade",
                      choices = c("All", unique_grades()),
                      selected = "All")
    
    updateCheckboxGroupInput(session, "rob",
                             label = "Select risk of bias",
                             choices = unique_robs(),
                             selected = unique_robs())
  })
  
  # create the reactive dataset
  effectsinput <- reactive({
    data <- datasets[[input$dataset]]
    
    if (input$design != "All") {
      data <- data %>% filter(study_design == input$design)
    }
    
    if (input$grade != "All") {
      data <- data %>% filter(str_detect(grade, input$grade))
    }
    
    data <- data %>% filter(rob_either %in% input$rob)
    
    return(data)
  })
  
  #############################################
  
  ef <- reactive({escalc("SMD", data = effectsinput(), m1i = m1i, n1i = n1i, sd1i = sd1i,
               m2i = m2i, n2i = n2i, sd2i = sd2i)})
  
  
  ##############################

  #TODO: for now distribution values are hard coded, but maybe make it interactive as well?  
  prior <- reactive({
          c(prior_string(paste0(input$prior_es,"(0,1)"), class = "Intercept"),
             prior_string(paste0(input$prior,"(0,0.05)"), class = "sd"))})
  
  
  ma <- eventReactive(input$go, {
    
      brm(yi|se(vi) ~ 1 + (1|study_ID),
          data = ef(),
          prior = prior(),
          warmup = input$warmup, iter = input$iterations)  
    
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
    
    })
  
  output$Summary <- renderPrint({
    summary(ma())
    })
  
  output$Table <- renderTable({
    ef()
    })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
}

#############################
################## run the app ####################
shinyApp(ui, server)
