library(shiny)
library(metafor)
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

############### user interface ####################
ui <- fluidPage(
  
    # website prep
    tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")),
    theme = bslib::bs_theme(version = 4),
    # Application title
    titlePanel("Frequentist Meta-analysis"),
    
    # Sidebar with a choices input dropdown
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset", "Select Dataset", choices = names(datasets)),
            downloadButton(outputId = "download",
                           label = "Download the dataset"),
            tags$div(
            selectInput(inputId =  "effect_type",
                        label = HTML("Select effect <i class='fas fa-question-circle'></i>"),
                        choices = c(#"Odds ratio" = "OR", 
                                    #"Relative risk" = "RR", 
                                    "Hedge's g" = "SMD", 
                                    "Fisher's z" = "ZCOR"),
                        selected = "SMD"),
                        title = "Effect size is the magnitude of a relationship or difference between measure points or groups. \n Depending on type of data, we have difference effect size measures. Most common one for measuring the difference between two groups is Hedge's g, which is a standardized mean difference. \n  For correlational studies, we use Fisher's z, which is a z-transformatio of the Pearson's corelation coefficient (r).",
                        `data-toggle` = "tooltip"),
            selectInput(inputId = "ma_model",
                        label = "Select meta-analysis model",
                        choices = c("Fixed-effects" = "FE", 
                                    "Restricted Maximum likelihood Estimator" = "REML",
                                    "DerSimonian-Laird" = "DL",
                                    "Paule-Mandel estimator" = "PM"),
                        selected = "REML"),
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
            numericInput(inputId = "conf",
                         label = "Select confidence",
                         value = 0.05,
                         min = 0.001,
                         max = 0.1,
                         step = 0.001
            ),
        ), 
       # main panel with output plots 
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plots",
                                 plotOutput("Plot1", hover = hoverOpts(id ="hover_plot1")),
                                 plotOutput("Plot2", hover = hoverOpts(id ="hover_plot2")),
                                 verbatimTextOutput("hover_info")),
                        tabPanel("Summary", textOutput("Summary")),
                        tabPanel("Table", tableOutput("Table"))
            )
        )
    )
)

############### server ############################

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
  ################## meta-analysis part #############################  
    MA_res <- reactive({rma(ai = ai, bi = bi, ci = ci, di = di,
                            m1i = m1i, sd1i = sd1i, n1i = n1i,
                            m2i = m2i, sd2i = sd2i, n2i = n2i,
                            ni = ni, ri = ri,
                            method = input$ma_model, measure = input$effect_type, 
                            data = effectsinput(), slab = short_cite)}) ## create reactive ma dependent on input
    
    forest_es <- reactive({rbind(data.frame(es=MA_res()$yi, se=sqrt(MA_res()$vi), #effect size and sd
                                            study=as_factor(MA_res()$slab), #study label
                                            #calculate lower and upper bound 95% conf interval
                                            lo.ci = MA_res()$yi - qnorm(1-(input$conf/2)) * sqrt(MA_res()$vi),  
                                            up.ci = MA_res()$yi + qnorm(1-(input$conf/2)) * sqrt(MA_res()$vi))
                                 )}) 
    

    
    ###########################
    ### Models print text summary   
    output$Summary <- renderPrint({
        print(paste("This meta-analysis was conducted using the ", MA_res()$method , " tau estimator, and the average effect of the intervention was ", 
                    MA_res()$b, " , (", (1-input$conf)*100, " % CI [ ", MA_res()$ci.lb, MA_res()$ci.ub, " ], 95% PI [ ", 
                    MA_res()$pi.lb, MA_res()$pi.ub, " ], z = ", MA_res()$zval, " , p = ", 
                    round(MA_res()$pval, digits = 10), " Total variance was tau2 = ", 
                    round(MA_res()$tau2, digits = 2),  " (SD of the true effects across studies was tau = ", 
                    round(sqrt(MA_res()$tau2), digits = 2),"), and the heterogeneity was I2 = ", MA_res()$I2, "%."))
        
    })    
    
    
    ############## forest plots #############
    
    output$Plot1 <- renderPlot({
        
        robcol <- c("high"="maroon4", "moderate"="darkgoldenrod4", "low"="royalblue4")
      
        ggplot2::ggplot(data = forest_es(), aes(x = es, y=study,
                                                xmin = min(lo.ci) - 3, xmax = max(up.ci) + 3)) +
            scale_x_continuous(breaks = c(round(1.1*min(forest_es()$lo.ci), 0), 
                                          round(0.5*min(forest_es()$lo.ci), 0), 
                                          0, 
                                          round(MA_res()$b, 2),
                                          round(0.5*max(forest_es()$up.ci), 0), 
                                          round(1.1*max(forest_es()$up.ci), 0))) +
            geom_pointrange(aes(x = MA_res()$b, xmin = MA_res()$ci.lb, xmax = MA_res()$ci.ub), colour = "grey") + #add average effects estimate
            #scale_x_log10() + TODO: what should be transformed and what not
            geom_point(aes(colour=factor(effectsinput()$rob_either))) +#add effect sizes
            geom_linerange(aes(xmin = lo.ci, xmax = up.ci, colour=factor(effectsinput()$rob_either))) + #add conf intervals
            #geom_vline(xintercept = MA_res()$b, colour = "blue") + #intercept line based on average effect
            scale_color_manual(values = robcol) +
            labs(
                x = "Effect size and CI",
                y = "Study",
                colour = "Risk of Bias",
                title = "Meta Analaysis of dataset", #add variables that change depending on dataset,
                subtitle = "Link to dataset" #add dataset url
            ) +
            theme_classic()
        
        
    })
    

    
    output$Plot2 <- renderPlot({
      
      #create the contour line (code by jksakaluk https://tinyurl.com/3bdznjav)
      contour.seq <- seq(0, max(forest_es()$se), 0.001)
      lb.90 <- as.vector(MA_res()$b) - (1.645 * contour.seq)
      ub.90 <- as.vector(MA_res()$b) + (1.645 * contour.seq)
      lb.95 <- as.vector(MA_res()$b) - (1.96 * contour.seq)
      ub.95 <- as.vector(MA_res()$b) + (1.96 * contour.seq)
      lb.99 <- as.vector(MA_res()$b) - (2.56 * contour.seq)
      ub.99 <- as.vector(MA_res()$b) + (2.56 * contour.seq)
      
      
      funn_df <- data.frame(contour.seq, lb.90,lb.90, ub.90, lb.95, ub.95,lb.99,
                            ub.99)
      
      #metafor::funnel(MA_res())
      ggplot2::ggplot(data = forest_es(), aes(x = es, y = se)) +
        #add studies
        geom_point() +
        #add summary effect line
        #geom_linerange(aes(x = MA_res()$b, ymin = min(forest_es()$es), ymax = max(forest_es()$es)), color = "grey") +
        #add contour lines for diff CI
        geom_line(data = funn_df, aes(x = lb.90 , y = contour.seq ), linetype = 'dotted') +
        geom_line(data = funn_df, aes(x = ub.90 , y = contour.seq ), linetype = 'dotted') +
        geom_line(data = funn_df, aes(x = lb.95 , y = contour.seq ), linetype = 'dashed') +
        geom_line(data = funn_df, aes(x = ub.95 , y = contour.seq ), linetype = 'dashed') +
        geom_line(data = funn_df, aes(x = lb.99 , y = contour.seq )) +
        geom_line(data = funn_df, aes(x = ub.99 , y = contour.seq )) +
        scale_y_reverse() +
        labs(
          x = "Effect size",
          y = "Standard Error",
          title = "Funnel Plot"
        ) +
        theme_classic()
        
      
    })
    
    #create a hover text showing which Study and crossref to funnel
    output$hover_info <- renderPrint({
      req(input$hover_plot2)
      nearPoints(forest_es(), input$hover_plot2, xvar = "es", yvar = "se")
    })
    
    
    output$Table <- renderTable({
        # TODO: make it nicer
        effectsinput()
        
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



################## run the app ####################
shinyApp(ui, server)