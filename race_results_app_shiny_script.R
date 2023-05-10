# Install and load the required libraries

# Initial Shiny script by Jehangeer Aswani, May 2023. 

# load packages
library(tidyverse)
library(shiny)
library(readxl)
library(DT)
library(janitor)
library(hms)
library(writexl)


# Define the UI
ui <- fluidPage(# Define the title of the app
  titlePanel("Race Results Sorter App"),
  # Define the sidebar
  sidebarLayout(
    sidebarPanel(
      # Define the download button for the completed example excel template
      downloadButton("downloadExampleTemplate", "Download Completed Example Template"),
      # Define the download button for the excel template
      downloadButton("downloadTemplate", "Download Blank Excel Template"),
      # Define the file input for uploading the filled-out Excel template
      fileInput("uploadFile", "Upload Filled-Out Excel Template"),
      # Define select input for race category
      selectInput(
        "category",
        "Race Category",
        choices = "All Categories",selected = "All Categories"),
      # Define a Submit button
      actionButton("submit_button", "Submit"),
      # Define a Download button
      downloadButton("download_btn", "Download Sorted Results")
    ),
    # Define the main panel
    mainPanel(
      h3("What is the Race Results Sorter App For?"),
      p("When the race is over, it can be a real chore for race organizers to manually sort out results and rankings in a spreadsheet! Especially when your race has multiple categories (e.g. Men's 5k, Women's 10k, etc.) There must be a better way!"),
      p("The Race Results Sorter App does the post-race data management work automatically and without expensive race timing equipment. All you need are names, bib numbers, start times, and finish times."),
      p(paste("This app is currently in experimental mode as of", Sys.Date(),". To share your feedback please contact "),
        a("Benjamin Meyer.", 
          href = "http://www.benjamin-meyer.net")),
      h3("Instructions"),
      p("Use this app to sort and rank race results by category. Download and fill out the Excel Template at the upper left. Follow instructions in the 'ReadMe' tab for more details. See the completed example template for how your race results data should look."),
      br(),
      h3("Results"),
      h5(em("Upload Your Filled-out Template and Hit the 'Submit' Button to Show Sorted Results by Category")),
      # Define the datatable output
      DTOutput("datatable"))
  ))

# Define the server
server <- function(input, output, session) {
  
  # Define the function for downloading the Excel template
  output$downloadTemplate <- downloadHandler(
    filename <- function() {
      "race_entrant_data_blank_template.xlsx"
    },
    content <- function(file) {
      file.copy("data/race_entrant_data_blank_template.xlsx", file)
    })
  
  # Define the function for downloading the Excel template
  output$downloadExampleTemplate <- downloadHandler(
    filename <- function() {
      "race_entrant_data_example.xlsx"
    },
    content <- function(file) {
      file.copy("data/race_entrant_data_example.xlsx", file)
    })
  
  
  # Define the reactive function for uploading the filled-out Excel template
  filled_out_template <- reactive({
    # Check if a file has been uploaded
    if (is.null(input$uploadFile)) {
      return(NULL)
    }
    # Read the uploaded Excel file using the readxl package
    race_start_data <- readxl::read_xlsx(input$uploadFile$datapath,
                                         sheet = "race_start_data") |>
      clean_names()
    
    race_end_data <- readxl::read_xlsx(input$uploadFile$datapath,
                                       sheet = "race_end_data") |>
      clean_names()
    
    list(race_start_data = race_start_data, 
         race_end_data = race_end_data)
  })
  
  # Define the reactive function for displaying the results as a datatable
  race_results <- reactive({
    # Check if a file has been uploaded
    if (is.null(filled_out_template())) {
      return(NULL)
    }
    
    # Join the start and end race data and compute duration
    results <- left_join(filled_out_template()$race_start_data,
                         filled_out_template()$race_end_data,
                         by = c("bib_number", "start_date")) |> 
      transform(start_time = as_hms(start_time),
                finish_time = as_hms(finish_time)) |> 
      mutate(time_duration = as_hms(finish_time - start_time))
    
    # Group the data by category and compute rankings
    if (input$category == "All Categories") {
      results <- results %>%
        arrange(race_category, time_duration) |> 
        mutate(category_rank = row_number())
    } else {
      results <- results %>%
        filter(race_category == input$category) |> 
        arrange(time_duration) |> 
        mutate(category_rank = row_number())
    }
    
    return(results)
  })
  
  # Define the function for displaying the datatable when user click on submit
  observeEvent(input$submit_button, {
    output$datatable <- renderDT({
      race_results()
    })
  })
  
  # Define the unique race categories for the select input
  observe({
    if (!is.null(filled_out_template())) {
      updateSelectInput(session, 
                        "category", 
                        choices = c(
                          #"All Categories", 
                          unique(filled_out_template()$race_start_data$race_category)))
    }
  })
  # Define the function for downloading the datatable
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("Race Results-", Sys.Date(), ".xlsx")
    },
    
    content =  function(file) {
      write_xlsx(race_results(), file)
    }
  )
}

# Run the app
shinyApp(ui, server)