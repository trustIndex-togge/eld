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
                  choices = c("Odds ratio", "Relative risk", "Hedge's g", "Pearson r")),
      selectInput(inputId = "ma_model",
                  label = "Select meta-analysis model",
                  choices = c("Fixed-effects", "Random-effects", "Equal-effects")),
      selectInput(inputId =  "design",
                  label = "Select design",
                  choices = c("All", unique(data$study_design))),
      selectInput(inputId = "grade",
                  label = "Select grade",
                  choices = c("All", unique(data$Grade)))),
    mainPanel(plotOutput('Plot'),
              textOutput("print"))
  )
)

############### server ############################

server <- function(input, output) {
  
  #### sub-grouping function
  effectsinput <- reactive({
    data %>% filter(study_design == input$design,
                    Grade == input$grade) ## add reactive dataframe based on input
  })
  
  
  #############################
  ### ma based on effect types
  #############################
  
  ###########################
  ### Models print text summary
  
  output$print <- renderPrint({
    
    
    ###### Fixed-effects
    if (input$ma_model == "Fixed-effects") {
      
      if (input$effect_type == "Odds ratio") {
        
        #data <- escalc("OR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        FE_res <- reactive({rma(ai, bi, ci, di, 
                                method = "FE", measure = "OR", 
                                data = effectsinput(), slab = short_cite)}) ## create reactive ma dependent on input
      }
      
      else if (input$effect_type ==  "Relative risk") {
        
        #data <- escalc("RR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        FE_res <- reactive({rma(ai = ai, n1i = N1i, 
                                ci = ci, n2i = N2i, 
                                method = "FE", measure = "OR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Hedge's g") {
        
        #data <- escalc("SMD", m1i = m1i, sd1i = sd1i, n1i = N1i,
        #                      m2i = m2i, sd2i = sd2i, n2i = N2i,
        #               data = data)
        
        FE_res <- reactive({rma(m1i = m1i, sd1i = sd1i, n1i = N1i,
                                m2i = m2i, sd2i = sd2i, n2i = N2i,
                                method = "FE", measure = "SMD", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Pearson r") {
        
        #data <- escalc("ZCOR", ri, ni
        #               data = data)
        
        FE_res <- reactive({rma(ri = ri, ni = ni,
                                method = "FE", measure = "ZCOR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      print(FE_res()) ## added () in order for r to understand its an reactive frame
      
    }
    
    
    ####### Random-effects
    else if (input$ma_model == "Random-effects") {
      
      if (input$effect_type == "Odds ratio") {
        
        #data <- escalc("OR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        RE_res <- reactive({rma(ai, bi, ci, di, 
                                method = "REML", measure = "OR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Relative risk") {
        
        #data <- escalc("RR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        RE_res <- reactive({rma(ai = ai, n1i = N1i, 
                                ci = ci, n2i = N2i, 
                                method = "REML", measure = "OR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Hedge's g") {
        
        #data <- escalc("SMD", m1i = m1i, sd1i = sd1i, n1i = N1i,
        #                      m2i = m2i, sd2i = sd2i, n2i = N2i,
        #               data = data)
        
        RE_res <- reactive({rma(m1i = m1i, sd1i = sd1i, n1i = N1i,
                                m2i = m2i, sd2i = sd2i, n2i = N2i,
                                method = "REML", measure = "SMD", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Pearson r") {
        
        #data <- escalc("ZCOR", ri, ni
        #               data = data)
        
        RE_res <- reactive({rma(ri = ri, ni = ni,
                                method = "REML", measure = "ZCOR", 
                                data = data, slab = short_cite)})
      }
      
      print(RE_res())
      
    }
    
    ##### Equal-effects
    else if (input$ma_model == "Equal-effects") {
      
      if (input$effect_type == "Odds ratio") {
        
        #data <- escalc("OR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        EE_res <- reactive({rma(ai, bi, ci, di, 
                                method = "EE", measure = "OR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Relative risk") {
        
        #data <- escalc("RR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        EE_res <- reactive({rma(ai = ai, n1i = N1i, 
                                ci = ci, n2i = N2i, 
                                method = "EE", measure = "OR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Hedge's g") {
        
        #data <- escalc("SMD", m1i = m1i, sd1i = sd1i, n1i = N1i,
        #                      m2i = m2i, sd2i = sd2i, n2i = N2i,
        #               data = data)
        
        EE_res <- reactive({rma(m1i = m1i, sd1i = sd1i, n1i = N1i,
                                m2i = m2i, sd2i = sd2i, n2i = N2i,
                                method = "EE", measure = "SMD", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Pearson r") {
        
        #data <- escalc("ZCOR", ri, ni
        #               data = data)
        
        EE_res <- reactive({rma(ri = ri, ni = ni,
                                method = "EE", measure = "ZCOR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      print(EE_res())
      
    }
    
  })    
  
  
  ####### forest plots
  
  output$Plot <- renderPlot({
    
    
    ###### Fixed-effects
    if (input$ma_model == "Fixed-effects") {
      
      if (input$effect_type == "Odds ratio") {
        
        #data <- escalc("OR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        FE_res <- reactive({rma(ai, bi, ci, di, 
                                method = "FE", measure = "OR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Relative risk") {
        
        #data <- escalc("RR", ai = ai, bi = bi,
        #               ci = ci, di = di,
        #               data = data)
        
        FE_res <- reactive({rma(ai = ai, n1i = N1i, 
                                ci = ci, n2i = N2i, 
                                method = "FE", measure = "OR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Hedge's g") {
        
        #data <- escalc("SMD", m1i = m1i, sd1i = sd1i, n1i = N1i,
        #                      m2i = m2i, sd2i = sd2i, n2i = N2i,
        #               data = data)
        
        FE_res <- reactive({rma(m1i = m1i, sd1i = sd1i, n1i = N1i,
                                m2i = m2i, sd2i = sd2i, n2i = N2i,
                                method = "FE", measure = "SMD", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      else if (input$effect_type ==  "Pearson r") {
        
        #data <- escalc("ZCOR", ri, ni
        #               data = data)
        
        FE_res <- reactive({rma(ri = ri, ni = ni,
                                method = "FE", measure = "ZCOR", 
                                data = effectsinput(), slab = short_cite)})
      }
      
      forest(FE_res(), xlim=c(-16,6), ilab=cbind(N1i, N2i), ilab.xpos=c(-8, -6),
             header="Author(s) and Year", mlab="")
      
    }
    
    
    #      ####### Random-effects
    #      if (input$ma_model == "Random-effects") {
    #        
    #        if (input$effect_type == "Odds ratio") {
    #          
    #          #data <- escalc("OR", ai = ai, bi = bi,
    #          #               ci = ci, di = di,
    #          #               data = data)
    #          
    #          RE_res <- rma(ai, bi, ci, di, 
    #                        method = "REML", measure = "OR", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        else if (input$effect_type ==  "Relative risk") {
    #          
    #          #data <- escalc("RR", ai = ai, bi = bi,
    #          #               ci = ci, di = di,
    #          #               data = data)
    #          
    #          RE_res <- rma(ai = ai, n1i = N1i, 
    #                        ci = ci, n2i = N2i, 
    #                        method = "REML", measure = "OR", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        else if (input$effect_type ==  "Hedge's g") {
    #          
    #          #data <- escalc("SMD", m1i = m1i, sd1i = sd1i, n1i = N1i,
    #          #                      m2i = m2i, sd2i = sd2i, n2i = N2i,
    #          #               data = data)
    #          
    #          RE_res <- rma(m1i = m1i, sd1i = sd1i, n1i = N1i,
    #                        m2i = m2i, sd2i = sd2i, n2i = N2i,
    #                        method = "REML", measure = "SMD", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        else if (input$effect_type ==  "Pearson r") {
    #          
    #          #data <- escalc("ZCOR", ri, ni
    #          #               data = data)
    #          
    #          RE_res <- rma(ri = ri, ni = ni,
    #                        method = "REML", measure = "ZCOR", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        print(RE_res)
    #        
    #      }
    #      
    #      ##### Equal-effects
    #      if (input$ma_model == "Equal-effects") {
    #        
    #        if (input$effect_type == "Odds ratio") {
    #          
    #          #data <- escalc("OR", ai = ai, bi = bi,
    #          #               ci = ci, di = di,
    #          #               data = data)
    #          
    #          EE_res <- rma(ai, bi, ci, di, 
    #                        method = "EE", measure = "OR", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        else if (input$effect_type ==  "Relative risk") {
    #          
    #          #data <- escalc("RR", ai = ai, bi = bi,
    #          #               ci = ci, di = di,
    #          #               data = data)
    #          
    #          EE_res <- rma(ai = ai, n1i = N1i, 
    #                        ci = ci, n2i = N2i, 
    #                        method = "EE", measure = "OR", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        else if (input$effect_type ==  "Hedge's g") {
    #          
    #          #data <- escalc("SMD", m1i = m1i, sd1i = sd1i, n1i = N1i,
    #          #                      m2i = m2i, sd2i = sd2i, n2i = N2i,
    #          #               data = data)
    #          
    #          EE_res <- rma(m1i = m1i, sd1i = sd1i, n1i = N1i,
    #                        m2i = m2i, sd2i = sd2i, n2i = N2i,
    #                        method = "EE", measure = "SMD", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        else if (input$effect_type ==  "Pearson r") {
    #          
    #          #data <- escalc("ZCOR", ri, ni
    #          #               data = data)
    #          
    #          EE_res <- rma(ri = ri, ni = ni,
    #                        method = "EE", measure = "ZCOR", 
    #                        data = effectsinput(), slab = short_cite)
    #        }
    #        
    #        print(EE_res)
    #        
    #      }
    
  })
  
  
}  










################## run the app ####################
shinyApp(ui, server)


