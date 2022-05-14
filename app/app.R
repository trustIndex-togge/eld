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
                  choices = c("Fixed-effects" = "FE", 
                              "Random-effects" = "REML",
                              "Equal-effects" = "EE")),
      selectInput(inputId =  "design",
                  label = "Select design",
                  choices = c("All", unique(data$study_design))),
      selectInput(inputId = "grade",
                  label = "Select grade",
                  choices = c("All", unique(data$Grade))),
      checkboxGroupInput(inputId = "rob", 
                  label = "Select risk of bias",
                  c(unique(data$rob)),
                  selected = (unique(data$rob)))),
    mainPanel(plotOutput('Plot'),
              textOutput("print"))
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
      rob == input$rob) ## check this out because it doesn't respond properly in the app
    })
    
    
    effectsinput <- reactive({                #### Create reactive dataframe based on selected study design and grade
      
      if  (any(input$design == "All")) {
          robfilter()
      } else {
      robfilter() %>% filter(
             study_design == input$design)}
 
                         
  })
  
    

 

  #############################
  ### ma based on effect types
  #############################
  
  ###########################
  ### Models print text summary
  
  output$print <- renderPrint({
      
      # name all possible arguments to calculate effects
      # filter the method and the effect based on inputs
      MA_res <- reactive({rma(ai = ai, bi = bi, ci = ci, di = di,
                              m1i = m1i, sd1i = sd1i, n1i = N1i,
                              m2i = m2i, sd2i = sd2i, n2i = N2i,
                              ni = ni, ri = ri,
                              method = input$ma_model, measure = input$effect_type, 
                              data = effectsinput(), slab = short_cite)}) ## create reactive ma dependent on input
      print(MA_res())
      

})    
  
  
  ####### forest plots
  
  output$Plot <- renderPlot({
 
      MA_res <- reactive({rma(ai = ai, bi = bi, ci = ci, di = di,
                              m1i = m1i, sd1i = sd1i, n1i = N1i,
                              m2i = m2i, sd2i = sd2i, n2i = N2i,
                              ni = ni, ri = ri,
                              method = input$ma_model, measure = input$effect_type, 
                              data = effectsinput(), slab = short_cite)}) ## create reactive ma dependent on input
        
        #forest plots for ratios, exponentiate graphs, present 2x2 frequencies
        if (input$effect_type == "OR" | input$effect_type == "RR") {
          
          forest(MA_res(), xlim=c(-15,9), ilab = cbind(ai, bi, ci, di, rob), 
                 atransf = exp, at=log(c(.10, 1, 10)),
                 ilab.xpos = c(-9.5, -8.5, -7.5, -6.5, -4),
                 header="Author(s) and Year", mlab="",
                 )
          
        }
        
        else if (input$effect_type == "SMD") {
          
          forest(MA_res(), xlim=c(-12,7), ilab = cbind(N1i, N2i, rob), 
                 at=(c(-1.5, 0, 1.5)),
                 ilab.xpos = c(-7, -6, -4),
                 header="Author(s) and Year", mlab="")
        }
        
        
        else if (input$effect_type == "ZCOR") {
          
          forest(MA_res(), xlim=c(-12,7), ilab = cbind(ni, rob), 
                 at=c(-1, 0, 1),
                 ilab.xpos = c(-6, -4),
                 header="Author(s) and Year", mlab="")
          
        }

  
})  

}



################## run the app ####################
shinyApp(ui, server)


