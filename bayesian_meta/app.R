library(shiny)
library(metafor)
library(brms)
library(tidybayes)
library(ggridges)
library(glue)
library(ggdist)
library(tidyverse)
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
                  label = "Choose prior for Tau\u00B2",
                  choices = c("Half-Cauchy" = "cauchy", 
                              #"Student-t" = "student_t", 
                              "normal" = "normal"),
                  selected = "cauchy"),
      sliderInput(inputId = "prior_sd_slide", # let users set their own sd prior for tau
                  label = "Set \u03c3 for the tau\u00B2 distribution. Note that x_0 = 0", 
                  min = 0,
                  max = 5,
                  value = 0.5, # set it to be sd 1
                  step = 0.1),
      selectInput(inputId =  "prior_es",
                  label = "Choose prior for Intercept",
                  choices = c("Cauchy" = "cauchy", 
                             # "Student-t" = "student_t", 
                              "normal" = "normal"),
                  selected = "normal"),
      sliderInput(inputId = "prior_es_sd_slide", # let users set their own sd priors for intercept
                  label = "Set \u03c3 for the distribution of the intercept. Note that \u03BC = 0", 
                  min = 0,
                  max = 5,
                  value = 1, # set it to be mean 0
                  step = 0.1),
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
                  tabPanel("Forest plot",
                           plotOutput("Plot_forest")),
                  tabPanel("Posterior distribution",
                           plotOutput("Plot_posterior")),
                  tabPanel("Convergence Diagnostics and Model Validity",
                           h3("Model summary"),
                           verbatimTextOutput("Summary"),
                           h3("Posterior predictive distribution"),
                           plotOutput("Plot2")),
                  tabPanel("Result Summary", htmlOutput("Results")),
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
    
    if (input$outcomes != "All") {
      data <- data %>% filter(outcome_full %in% input$outcomes)
    }
    
    if (input$grade != "All") {
      data <- data %>% filter(str_detect(grade, input$grade))
    }
    
    data <- data %>% filter(rob_either %in% input$rob)
    
    return(data)
  })
  
  #############################################
  
  ef <- reactive({
    
    if (!any(is.na(effectsinput()$m1i), is.na(effectsinput()$n1i), is.na(effectsinput()$sd1i), is.na(effectsinput()$m2i), is.na(effectsinput()$sd2i), is.na(effectsinput()$n2i))) {
      
      escalc("SMD", data = effectsinput(), m1i = m1i, n1i = n1i, sd1i = sd1i,
             m2i = m2i, n2i = n2i, sd2i = sd2i)
      
    } else if (!any(is.na(effectsinput()$ni), is.na(effectsinput()$ri))) {
      
      escalc("ZCOR", data = effectsinput(), ni = ni, ri = ri)

    } else {
      stop("Required data columns contain NAs.")
    }
  
})  
  
  
  ##############################

  # make priors interactive
  prior <- reactive({
          c(prior_string(paste0(input$prior_es,"(0,",input$prior_es_sd_slide,")"), class = "Intercept"),
             prior_string(paste0(input$prior,"(0,",input$prior_sd_slide,")"), class = "sd"))
    })
  
  # bayesian meta model
  ma <- eventReactive(input$go, {
    
      brm(yi|se(vi) ~ 1 + (1|study_id),
          data = ef(),
          prior = prior(),
          warmup = input$warmup, 
          iter = input$iterations)  
    
   }
)
  
################################## Posterior distribution ####################
#prepare data for the density plot
  # code from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/bayesian-ma.html#priors
  
  post.samples <- reactive({
    ma() %>% posterior::as_draws_df(., c("^b_", "^sd_"), regex = TRUE) %>% 
      rename("es" = "b_Intercept", "tau" = "sd_study_id__Intercept")
  })
  
  # effect size of each study for the forest plot
  study.draws <- reactive({
  tidybayes::spread_draws(ma(), r_study_id[study_id,], b_Intercept) %>% 
      mutate(b_Intercept = r_study_id + b_Intercept)
  })
  
  # add the effect size estimate at the end of forest plot
  pooled.effect.draws <- reactive({
    tidybayes::spread_draws(ma(), b_Intercept) %>% 
    mutate(study_id = "Pooled Effect")
  })
  
  # bind the data for the fp
  forest.data <- reactive({
    bind_rows(study.draws(), pooled.effect.draws()) %>% 
    ungroup() %>%
    mutate(study_id = reorder(study_id, b_Intercept))
    
  })
  
  # effect size and credible interval for each study
  forest.data.summary <- reactive({
    group_by(forest.data(), study_id) %>% 
    ggdist::mean_qi(b_Intercept)
  })
  
  
# plots start here  
  output$Plot_posterior <- renderPlot({
    
    ggplot(aes(x = es), data = post.samples()) +
      geom_density(fill = "lightblue",                # set the color
                   color = "lightblue", alpha = 0.7) +  
      geom_point(y = 0,                               # add point at mean
                 x = mean(post.samples()$es)) +
      labs(x = expression(italic("Effect size")),
           y = element_blank()) +
      theme_minimal()
    
    })
  
  output$Plot_forest <- renderPlot({
    ggplot(aes(b_Intercept, 
               relevel(study_id, "Pooled Effect", 
                       after = Inf)), 
           data = forest.data()) +
      
      # Add vertical lines for pooled effect and CI
      geom_vline(xintercept = fixef(ma())[1, 1], 
                 color = "grey", linewidth = 1) +
      geom_vline(xintercept = fixef(ma())[1, 3:4], 
                 color = "grey", linetype = 2) +
      
      # Add densities
      ggridges::geom_density_ridges(fill = "red", 
                          rel_min_height = 0.01, 
                          col = NA, scale = 1,
                          alpha = 0.8) +
      #geom_pointinterval(data = forest.data.summary(), 
      #                    linewidth = 1,
      #                   xmin = min(b_Intercept), xmax = max(b_Intercept)) +
      
      # Add text and labels
      geom_text(data = mutate_if(forest.data.summary(), 
                                 is.numeric, round, 2),
                aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                    x = (max(b_Intercept) + 4), hjust = "inward")) +
      labs(x = "Effect size", # summary measure
           y = element_blank()) +
      theme_minimal()
  })

############### Results #####################  
  
  #ranef(ma())
  # text summary of the effect
  output$Results <- renderUI({

    # Extract posterior samples and summarize effect size and tau
    post_samples <- post.samples()
    es_mean <- mean(post_samples$es)
    es_lower <- quantile(post_samples$es, 0.025)
    es_upper <- quantile(post_samples$es, 0.975)
    tau_mean <- mean(post_samples$tau)
    tau_lower <- quantile(post_samples$tau, 0.025)
    tau_upper <- quantile(post_samples$tau, 0.975)
    
    HTML(paste0("The results show that the estimated pooled effect size is ", round(es_mean, 2),
                " with a 95% credible interval of [", round(es_lower, 2), ", ", round(es_upper, 2), "]. ",
                "The estimated between-study heterogeneity (tau) is ", round(tau_mean, 2),
                " with a 95% credible interval of [", round(tau_lower, 2), ", ", round(tau_upper, 2), "]. ",
                "A larger tau value indicates greater between-study variability in the effect sizes."))
    
  
})
 
  
############ Diagnostics ################## 
  
  # print the whole model to see Rhat values
  output$Summary <- renderPrint({
        summary(ma())
  })

  # posterior predictive check
  output$Plot2 <- renderPlot(
    pp_check(ma())
  )
  
  
############ Table ######################
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
