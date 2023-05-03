#libraries
library(shiny)

# user interface
ui <- fluidPage(
  titlePanel("Dataset Validation"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload your data file (CSV)"),
      helpText("Dataset must have following columns (they can be empty): ai, bi, ci, country, d, d_var, 
               di, duration_week, effect_size_number, experiment_number, f_stat, 
               frequency_n, gender_1, gender_2, grade, implementation, intensity_n, 
               long_cite, m1i, m2i, mean_age, n1i, n2i, ni, participant_design, 
               peer_reviewed, ri, rob, robins sample_size, sd1i, sd2i, short_cite, 
               study_design, study_id, t, unique_row.", "outcome", "outcome_full")
    ),
    
    mainPanel(
      h3("Validation Results:"),
      verbatimTextOutput("validation_result"),
      h4("Template for your dataset:"),
      HTML(paste0("Find the Google Sheets template here: <a href=", "https://docs.google.com/spreadsheets/d/1DlOEy653YUrsjfoB6dsyCYZ-0kewtQxG7xQLzUcA-ts/edit?usp=sharing",">Template</a>"))
    )
  )
)

# server
server <- function(input, output) {
  # Read data file
  data <- reactive({
    req(input$datafile)
    inFile <- input$datafile
    read.csv(inFile$datapath, stringsAsFactors = FALSE)
  })
  
  # Validate the data
  validate_data_types <- function(data) {
    errors <- c()
    
    char_vars <- c("country", "short_cite", "long_cite", "rob", "robins", "study_design", "study_id", "peer_reviewed", "participant_design", "implementation", "outcome", "outcome_full")
    int_vars <- c("duration_week", "frequency_n", "intensity_n", "gender_1", "gender_2", "unique_row", "sample_size", "experiment_number", "grade")
    float_vars <- c("ai", "bi", "ci", "d", "d_var", "di", "effect_size_number", "f_stat", "m1i", "m2i", "mean_age", "n1i", "n2i", "sd1i", "sd2i", "t")
    
    for (var in char_vars) {
      if (!is.character(data()[[var]])) {
        errors <- append(errors, paste(var, "should be a character string."))
      }
    }
    
    for (var in int_vars) {
      if (!is.integer(data()[[var]])) {
        errors <- append(errors, paste(var, "should be an integer."))
      }
    }
    
    for (var in float_vars) {
      if (!is.numeric(data()[[var]])) {
        errors <- append(errors, paste(var, "should be a float."))
      }
    }
    
    return(errors)
  }
  
  output$validation_result <- renderText({
    validation_errors <- validate_data_types(data)
    if (length(validation_errors) > 0) {
      return(paste(validation_errors, collapse = "\n"))
    } else {
      return("Data types are valid.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
