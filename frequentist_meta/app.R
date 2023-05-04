library(shiny)
library(metafor)
library(rmarkdown)
library(knitr)
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
           checkboxGroupInput(inputId =  "outcomes",
                       label = "Select outcomes",
                       choices = c("All", unique(datasets[[1]]$outcome_full))),
           checkboxGroupInput(inputId = "grade",
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
                        tabPanel("Forest plot",
                                 wellPanel(style = "padding: 5px; border: solid 1px #337ab7; border-radius: 1px;",
                                           p("Pooled effect", style = "color: #337ab7;"),
                                           textOutput("effect")),
                                 plotOutput("Plot1")),
                        tabPanel("Publication bias",
                                 h3("Egger's regression test"),
                                 verbatimTextOutput("regtest"),
                                 h3("Funnel plot"),
                                 plotOutput("Plot2", hover = hoverOpts(id ="hover_plot2")),
                                 verbatimTextOutput("hover_info2")),
                        tabPanel("Summary", htmlOutput("Summary")),
                        tabPanel("Table", tableOutput("Table"))
                        
            )
        )
    ) #sidebarlayout
) #fluidpage

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
    
    updateCheckboxGroupInput(session, "outcomes",
                      label = "Select outcomes",
                      choices = c("All", unique_outcomes()),
                      selected = "All")
    
    updateCheckboxGroupInput(session, "grade",
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
      data <- data %>% filter(study_design %in% input$design)
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
    
    pred <- reactive({
      predict(MA_res())
    })
    
    ###########################
    ### Models print text summary   
    output$Summary <- renderUI({
      
    
        # explain different effect sizes depending on effect chosen
        e_size <- if (abs(MA_res()$b) <= 0.2) {
          "small effect size that could have practical meaning for interventions that solve highly influential problems. For example, improvement of this size might be considered meaningful for students that struggle academically but improve with better peer support"
        } else if (abs(MA_res()$b) > 0.2 & MA_res()$b <= 0.8) {
          "moderate effect size, which on its own already presents practical implications, as long as the costs of the intervention don't overshadow the benefit, for example, implementing the Letter Reading intervention might provide a moderate effect size on reading, but the cost of implementation might not justify it"
        } else if (abs(MA_res()$b) > 0.8) {
          "large effect size which shows clear practical implications and justify its implementations in classrooms if possible"
        }
        conditional_text <- if (input$effect_type == "SMD") {
          paste0("<br><br> Hedge's g, similar to Cohen's d, is a standardised mean difference between two groups. 
                 Common interpretations of magnitude of this effect is: small effect size (g = 0.2), moderate (0.5), and large (> 0.8). 
                 <br><br> These are rough guidelines, and should usually be interpreted in context of other variables. The effect size in this meta-analysis g = ",
                 round(MA_res()$b, digits = 3), " could be interpreted as a ", e_size, ". <br><br> Note that the sign of the effect implies direction, with the negative sign implying worse performance by the intervention group, and positive a better performance for the intervention group.")
        } else if (input$effect_type == "ZCOR") {
          paste0("<br><br> Fisher's z transformation is used to transform Pearson's correlation coefficients to a normally distributed variable, and is used in meta-analyses as an effect size of a relationship between two variables. An example of that relationship might be the attitudes about school and age. Larger z-scores imply a stronger relationship between the variables, and the sign represents a negative (decrease in one variable implies increase in other) or a positive relationship (both variables increase or decrease).
                 As a rule of thumb, we can interpret z-scores larger than 0.55 as strong relationship, with the z-score of this meta-analysis being z = ",
                 round(MA_res()$b, digits = 3), ".")
        }
        
        
          
        text <- paste0(# first summarize the results of the analysis
          "This meta-analysis was conducted using the ", 
          strong(MA_res()$method), 
          " tau estimator, and the average effect of the intervention was ", 
          strong(if (is.numeric(MA_res()$b)) round(MA_res()$b, digits = 2) else MA_res()$b), 
          ", ", 
          strong((1-input$conf)*100), 
          " % CI [ ", 
          strong(if (is.numeric(MA_res()$ci.lb)) round(MA_res()$ci.lb, digits = 2) else MA_res()$ci.lb), 
          ", ",
          strong(if (is.numeric(MA_res()$ci.ub)) round(MA_res()$ci.ub, digits = 2) else MA_res()$ci.ub), 
          " ], 95% PI [ ", 
          strong(if (is.numeric(pred()$pi.lb)) round(pred()$pi.lb, digits = 2) else pred()$pi.lb), 
          ", ",
          strong(if (is.numeric(pred()$pi.ub)) round(pred()$pi.ub, digits = 2) else pred()$pi.ub), 
          " ], z = ", 
          strong(if (is.numeric(MA_res()$zval)) round(MA_res()$zval, digits = 2) else MA_res()$zval), 
          " , p = ", 
          strong(if (is.numeric(MA_res()$pval)) round(MA_res()$pval, digits = 10) else MA_res()$pval),
          " Total variance was tau\u00B2 = ", 
          strong(if (is.numeric(MA_res()$tau2)) round(MA_res()$tau2, digits = 2) else MA_res()$tau2),
          " (SD of the true effects across studies was tau = ", 
          strong(if (is.numeric(MA_res()$tau2)) round(sqrt(MA_res()$tau2), digits = 2) else sqrt(MA_res()$tau2)),
          "), and the heterogeneity was I\u00B2 = ", 
          strong(if (is.numeric(MA_res()$I2)) round(MA_res()$I2, digits = 2) else MA_res()$I2), 
          "%. <br><br>",
                     
                     # now comes explanation of used moderators if any
                     "This analysis was done on the studies belonging to the following subgroups: <br>
                     Studies that implemented designs: ",
                     paste(unique(input$design), collapse = ", "), "<br>",
                     "Studies that measured these outcomes: ",
                     paste(unique(input$outcomes), collapse = ", "), "<br>",
                     "Studies that included participants attending these grades: ",
                     paste(unique(input$grade), collapse = ", "), " <br>",
                     "Studies with the following risk of bias: ",
                     paste(unique(input$rob), collapse = ", "), ".",
                     
                     
                     # now explain meaning
                     conditional_text
                     
                     )
        HTML(text)
        
    })    
    
    # from reporter() in metafor
    report_path <- tempfile(fileext = ".Rmd")
    file.copy("report.Rmd", report_path, overwrite = TRUE)
    
    render_report <- function(input, output, params) {
      rmarkdown::render(input,
                        output_file = output,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
    
    ############## forest plots #############
    output$effect <- renderText({
      paste0("ES = ", round(MA_res()$b, digits = 2), ", 95% CI (", round(MA_res()$ci.lb, digits = 2), ", ", round(MA_res()$ci.ub, digits = 2), ")",
             ", k = ", MA_res()$k)
    })
    
    
    height <- function() {
      if (nrow(forest_es()) < 5) 
        400
       else if (nrow(forest_es()) >= 5 & nrow(forest_es()) < 15) 
        600
       else if (nrow(forest_es()) >= 15 & nrow(forest_es()) < 25) 
        850
      else if (nrow(forest_es()) >= 25) 
        950
    }
    
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
                title = paste0("Meta Analysis of ", input$dataset)
            ) +
            theme_classic() +
            theme(axis.text = element_text(face="bold"))
        
        
    }, height = height)

    
############### Publication bias ##########################

    
    output$regtest <- renderPrint({
      eggers <- regtest(MA_res())
      eggers
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
    output$hover_info2 <- renderPrint({
      req(input$hover_plot2)
      nearPoints(forest_es(), input$hover_plot2, xvar = "es", yvar = "se")
    })
    
 

################ rest #########################   
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