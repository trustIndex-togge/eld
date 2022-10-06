library(shiny)
library(metafor)
library(tidyverse)

data <- read_csv("updated_mock.csv")

############### user interface ####################
ui <- fluidPage(
    # Application title
    titlePanel("ELD Meta-analysis"),
    
    # Sidebar with a choices input dropdown
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId =  "effect_type",
                        label = "Select effect",
                        choices = c("Odds ratio" = "OR", 
                                    "Relative risk" = "RR", 
                                    "Hedge's g" = "SMD", 
                                    "Fisher's z" = "ZCOR")),
            selectInput(inputId = "ma_model",
                        label = "Select meta-analysis model",
                        choices = c("Fixed-effects (inverse variance)" = "FE", 
                                    "Restricted Maximum likelihood Estimator" = "REML",
                                    "DerSimonian-Laird" = "DL",
                                    "Paule-Mandel estimator" = "PM")),
            selectInput(inputId =  "design",
                        label = "Select design",
                        choices = c("All", unique(data$study_design))),
            selectInput(inputId = "grade",
                        label = "Select grade",
                        choices = c("All", unique(data$Grade))),
            checkboxGroupInput(inputId = "rob", 
                               label = "Select risk of bias",
                               c(unique(data$rob)),
                               selected = (unique(data$rob))),
            numericInput(inputId = "conf",
                         label = "Select confidence",
                         value = 0.05,
                         min = 0.001,
                         max = 0.1,
                         step = 0.001
            ),
        ),
        
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("Plot")),
                        tabPanel("Summary", textOutput("Summary")),
                        tabPanel("Table", tableOutput("Table"))
            )
        )
    )
)

############### server ############################

server <- function(input, output) {
    
    ########### sub-grouping function #############
    
    datagrade <- reactive({                 #### Create reactive dataframe based on selected grade
        
        if (any(input$grade == "All")) {
            data
        } else {
            data %>% filter(
                Grade == input$grade)}
    })
    
    
    robfilter <- reactive({datagrade() %>% filter(
        rob %in% input$rob) 
    })
    
    
    effectsinput <- reactive({                #### Create reactive dataframe based on selected study design and grade
        
        if  (any(input$design == "All")) {
            robfilter()
        } else {
            robfilter() %>% filter(
                study_design == input$design)}
        
        
    })
    
    MA_res <- reactive({rma(ai = ai, bi = bi, ci = ci, di = di,
                            m1i = m1i, sd1i = sd1i, n1i = N1i,
                            m2i = m2i, sd2i = sd2i, n2i = N2i,
                            ni = ni, ri = ri,
                            method = input$ma_model, measure = input$effect_type, 
                            data = effectsinput(), slab = short_cite)}) ## create reactive ma dependent on input
    
    forest_es <- reactive({rbind(data.frame(es=MA_res()$yi, se=sqrt(MA_res()$vi), #effect size and sd
                                            study=as_factor(MA_res()$slab), #study label
                                            #calculate lower and upper bound 95% conf interval
                                            #TODO:change 1.96 into a criterion variable so conf int can be chosen
                                            lo.ci = MA_res()$yi - qnorm(1-(input$conf/2)) * sqrt(MA_res()$vi),  
                                            up.ci = MA_res()$yi + qnorm(1-(input$conf/2)) * sqrt(MA_res()$vi)))}) 
    #############################
    ### ma based on effect types
    #############################
    
    ###########################
    ### Models print text summary
    
    output$Summary <- renderPrint({
        
        # name all possible arguments to calculate effects
        # filter the method and the effect based on inputs
        
        ## TODO: have pred function after to create predictive intervals and exp logs
        # MA_res <- reactive({rma(ai = ai, bi = bi, ci = ci, di = di,
        #                         m1i = m1i, sd1i = sd1i, n1i = N1i,
        #                         m2i = m2i, sd2i = sd2i, n2i = N2i,
        #                         ni = ni, ri = ri,
        #                         method = input$ma_model, measure = input$effect_type, 
        #                         data = effectsinput(), slab = short_cite)}) ## create reactive ma dependent on input
        # 
        # add ifelse function to exponentiate log results - TODO
        print(paste("This meta-analysis was conducted using the ", MA_res()$method , " tau estimator, and the average effect of the intervention was ", 
                    MA_res()$b, " , (", (1-input$conf)*100, " % CI [ ", MA_res()$ci.lb, MA_res()$ci.ub, " ], 95% PI [ ", 
                    MA_res()$pi.lb, MA_res()$pi.ub, " ], z = ", MA_res()$zval, " , p = ", 
                    round(MA_res()$pval, digits = 10), " Total variance was tau2 = ", 
                    round(MA_res()$tau2, digits = 2),  " (SD of the true effects across studies was tau = ", 
                    round(sqrt(MA_res()$tau2), digits = 2),"), and the heterogeneity was I2 = ", MA_res()$I2, "%."))
        
    })    
    
    
    ####### forest plots
    
    output$Plot <- renderPlot({
        
        # MA_res <- reactive({rma(ai = ai, bi = bi, ci = ci, di = di,
        #                         m1i = m1i, sd1i = sd1i, n1i = N1i,
        #                         m2i = m2i, sd2i = sd2i, n2i = N2i,
        #                         ni = ni, ri = ri,
        #                         method = input$ma_model, measure = input$effect_type, 
        #                         data = effectsinput(), slab = short_cite)}) ## create reactive ma dependent on input
        # 
        # forest_es <- reactive({rbind(data.frame(es=MA_res()$yi, se=sqrt(MA_res()$vi), #effect size and sd
        #                                         study=as_factor(MA_res()$slab), #study label
        #                                         #calculate lower and upper bound 95% conf interval
        #                                         #TODO:change 1.96 into a criterion variable so conf int can be chosen
        #                                         lo.ci = MA_res()$yi - 1.96 * sqrt(MA_res()$vi),  
        #                                         up.ci = MA_res()$yi + 1.96 * sqrt(MA_res()$vi)))}) 
        
        
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
            geom_point(aes(colour=factor(effectsinput()$rob))) +#add effect sizes
            geom_linerange(aes(xmin = lo.ci, xmax = up.ci, colour=factor(effectsinput()$rob))) + #add conf intervals
            #geom_vline(xintercept = MA_res()$b, colour = "blue") + #intercept line based on average effect
            labs(
                x = "Effect size and CI",
                y = "Study",
                colour = "Risk of Bias",
                title = "Meta Analaysis of dataset", #add variables that change depending on dataset,
                subtitle = "Link to dataset" #add dataset url
            ) +
            theme_classic()
        
        
    })
    
    
    output$Table <- renderTable({
        # TODO: make it nicer
        effectsinput()
        
    })
    
}



################## run the app ####################
shinyApp(ui, server)