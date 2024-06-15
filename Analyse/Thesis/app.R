library(shiny)
library(tidymodels)
library(dplyr)
library(readr)
library(kknn)
library(glmnet)
library(xgboost)

final_knn_fit_pay_or_okay <- readRDS("final_knn_fit.rds")
final_lasso_fit_pass <- readRDS("final_lasso_fit_pass.rds")
final_boost_fit_price <- readRDS("final_boost_fit_price.rds")

# Load the test data
# test_data <- read_csv("test_q3.csv")
# test_data$pay_or_okay <- as.factor(test_data$pay_or_okay)

websites_data <- read.csv("websites_data.csv") %>% arrange(desc(popularity)) %>% distinct(site, .keep_all = TRUE)


# UI changes
ui <- fluidPage(
  titlePanel("Pay-or-Okay Wall Prediction"),
  sidebarLayout(
    sidebarPanel(
      textInput("website", "Enter Website Details", value = ""),
      sliderInput("threshold",
                  "Select Probability Threshold:",
                  min = 0.1,
                  max = 1,
                  value = 0.5,
                  step = 0.1),
      textInput("email", "Enter Your Email (Optional)"),
      actionButton("predict", "Predict"),
      tags$a(href = "https://github.com/RogierFrans/Scriptie", "More information on the GitHub Repository", target = "_blank")
    ),
    mainPanel(
      uiOutput("results_ui"),
      uiOutput("prediction_light"),
      textOutput("result_explanation"),
      textOutput("price_information")  # Added this line for price information
    )
  )
)

# Server changes
server <- function(input, output, session) {
  # Load the saved models
  #setwd("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Analyse/Thesis")
  # Create reactive values to store predictions
  predictions <- reactiveVal(list(pay_or_okay = NULL, pass = NULL, price = NULL, service = NULL))
  
  extract_base_domain <- function(site) {
    parts <- strsplit(site, '\\.')[[1]]
    if (length(parts) > 2) {
      return(paste(parts[length(parts)-1], parts[length(parts)], sep = '.'))
    }
    return(site)
  }
  
  observeEvent(input$predict, {
    req(input$website)
    
    base_domain <- extract_base_domain(input$website)
    
    # Filter the website data to find the matching base domain
    matching_data <- websites_data %>%
      filter(extract_base_domain(site) == base_domain)
    
    if (nrow(matching_data) == 0) {
      showNotification("No matching website found in the dataset. Maybe in the future it will be added.", type = "error")
      predictions(list(pay_or_okay = NULL, pass = NULL, price = NULL, service = NULL))
      return(NULL)
    }
    
    # Predict with Pay-or-Okay model
    pay_or_okay_prediction <- predict(final_knn_fit_pay_or_okay, matching_data, type = "prob")
    pay_or_okay_class <- ifelse(pay_or_okay_prediction$.pred_1 >= input$threshold, 1, 0)
    
    if (pay_or_okay_class == 1) {
      # Predict with Pass model if Pay-or-Okay is green
      pass_prediction <- predict(final_lasso_fit_pass, matching_data, type = "prob")
      pass_class <- ifelse(pass_prediction$.pred_1 >= input$threshold, 1, 0)
      
      if (pass_class == 0) {
        # Predict with Price model if Pass is red
        price_prediction <- predict(final_boost_fit_price, matching_data)
        normal_price <- exp(price_prediction)  # Assuming log transformation, reverse it
        predictions(list(pay_or_okay = pay_or_okay_class, pass = pass_class, price = normal_price, service = NULL))
      } else {
        # If Pass is green, set the price based on the subscription service
        price <- 2.99
        predictions(list(pay_or_okay = pay_or_okay_class, pass = pass_class, price = price, service = "Freechoice"))
      }
    } else {
      predictions(list(pay_or_okay = pay_or_okay_class, pass = NULL, price = NULL, service = NULL))
    }
  })
  
  output$results_ui <- renderUI({
    preds <- predictions()
    
    if (!is.null(preds$pay_or_okay)) {
      tagList(
        div(style = "margin-top: 20px;",
            div(style = if (preds$pay_or_okay == 1) "background-color: #d4edda; color: #155724; padding: 10px; border-radius: 5px; margin-bottom: 10px;"
                else "background-color: #f8d7da; color: #721c24; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                strong("Pay-or-Okay Result: "), 
                if (preds$pay_or_okay == 1) "Green Light (Recommended)" else "Red Light (Not Recommended)")
        ),
        if (!is.null(preds$pass)) {
          div(style = if (preds$pass == 1) "background-color: #d4edda; color: #155724; padding: 10px; border-radius: 5px; margin-bottom: 10px;"
              else "background-color: #f8d7da; color: #721c24; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
              strong("Pass Result: "), 
              if (preds$pass == 1) "Green Light (Recommended)" else "Red Light (Not Recommended)")
        },
        if (!is.null(preds$price)) {
          div(style = "background-color: #cce5ff; color: #004085; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
              strong("Suggested Price: "), preds$price)
        }
      )
    }
  })
  
  output$price_information <- renderText({
    preds <- predictions()
    if (!is.null(preds$service)) {
      if (preds$service == "Contentpass") {
        "You are recommended for Contentpass at €3.99 per month. This service eliminates banner ads, video ads, and personalized tracking, covering around 450 websites."
      } else if (preds$service == "Freechoice") {
        "You are recommended for Freechoice at €2.99 per month. This service allows you to avoid tracking while still being exposed to ads, covering around 234 websites."
      }
    }
  })
  
  output$result_explanation <- renderText({
    preds <- predictions()
    if (!is.null(preds$pay_or_okay)) {
      "The result is based on the characteristics of your website compared to other websites in our dataset."
    }
  })
  
  output$prediction_light <- renderUI({
    preds <- predictions()
    if (!is.null(preds$pay_or_okay)) {
      if (preds$pay_or_okay == 1) {
        tags$div(style = "color: green; font-size: 36px; font-weight: bold;", "🟢 Pay-or-Okay: Green Light")
      } else {
        tags$div(style = "color: red; font-size: 36px; font-weight: bold;", "🔴 Pay-or-Okay: Red Light")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

        