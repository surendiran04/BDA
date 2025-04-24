library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(shinydashboard)
library(proxy)
library(reshape2)
library(mongolite)

source("hybrid.R") #Contains logic for hybrid recommendation filtering

mongo_url <- "mongodb://localhost:27017"
# Load data
users_db <- mongo(collection = "users_data", db = "ecommerce", url = mongo_url)
interactions_db <- mongo(collection = "ratings", db = "ecommerce", url = mongo_url)
products_db <- mongo(collection = "items_data", db = "ecommerce", url = mongo_url)

products <- products_db$find()
users <- users_db$find()
interactions <- interactions_db$find()
#products <- read.csv("../data/walmart_dataset_clean.csv")

num_reco_val <- reactiveVal(5)  # Default value

# Clean and validate image URLs
products_clean <- products %>%
  select(Product_Id = `Uniq_Id`, Product_Name, Product_Brand, Product_Price, 
         Product_Rating, Product_Reviews_Count, Product_Image_Url, Product_Category, 
         Product_Tags, Product_Description) %>%
  mutate(
    Product_Name = tolower(Product_Name),
    Product_Tags = tolower(Product_Tags),
    Product_Brand = tolower(Product_Brand),
    Product_Category = tolower(Product_Category),
    Product_Description = tolower(Product_Description),
    Product_Rating = as.numeric(Product_Rating),
    review_count = as.numeric(Product_Reviews_Count),
    Product_Image_Url = str_split(Product_Image_Url, "\\|") %>% sapply(`[`, 1),
    Product_Image_Url = ifelse(str_detect(Product_Image_Url, "^http"), Product_Image_Url, NA)
  ) %>%
  filter(!is.na(Product_Image_Url))

# Trending logic
# Normalization function
normalize <- function(x) {
  if (all(is.na(x)) || max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))
  }
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Make trending_products a reactive function
trending_products <- reactive({
    # Clean and group interaction data
    interactions_clean <- interactions%>%
      mutate(product_id = trimws(as.character(product_id))) %>%
      group_by(product_id) %>%
      tally(name = "interaction_count")
  
    # Join, Normalize, Score, Select Top 10
    trending_result <- products_clean %>%
      inner_join(interactions_clean, by = c("Product_Id" = "product_id")) %>%
      mutate(
        norm_interaction = normalize(interaction_count),
        norm_rating = normalize(Product_Rating),
        norm_reviews = normalize(review_count),
        norm_rating = ifelse(is.na(norm_rating), 0, norm_rating),
        norm_reviews = ifelse(is.na(norm_reviews), 0, norm_reviews),
        trending_score = (norm_interaction * 0.6) + (norm_rating * 0.3) + (norm_reviews * 0.1)
      ) %>%
      arrange(desc(trending_score)) %>%
      slice_head(n = 10)
  
    # For debugging - log to console instead of printing directly
    cat("\n--- Trending Products ---\n")
    print(trending_result %>% select(Product_Name, trending_score))
  
    # Return the trending products
    trending_result
  })

# Collaborative Filtering Function
get_personalized_recommendations <- function(user_id, n = 5) {
  if (!(user_id %in% interactions$user_id)) return(NULL)
  
  interaction_matrix <- dcast(interactions, user_id ~ product_id, value.var = "rating", fun.aggregate = sum, fill = 0)
  #UTILITY MATRIX
  cat("\n--- Interaction Matrix (First 5 Rows) ---\n")
  print(head(interaction_matrix,5))

  rownames(interaction_matrix) <- interaction_matrix$user_id
  interaction_matrix <- interaction_matrix[, -1]
  
  user_sim <- as.matrix(proxy::simil(as.matrix(interaction_matrix), method = "cosine"))

  #COMPUTED SIMILARITIES
  cat("\n--- User Similarity Matrix (First 5 Rows) ---\n")
  print(head(user_sim,5))
  
  sim_users <- sort(user_sim[as.character(user_id), ], decreasing = TRUE)
  sim_users <- sim_users[names(sim_users) != as.character(user_id)]
  
  if (length(sim_users) == 0) return(NULL)
  
  weighted_scores <- colSums(interaction_matrix[names(sim_users), , drop = FALSE] * sim_users)
  user_items <- interaction_matrix[as.character(user_id), ]
  weighted_scores[names(user_items[user_items > 0])] <- 0  # Making it 0 so the same product wont be recommended
  
  # Print the top N weighted scores
  cat("\n--- Top Weighted Scores ---\n")
  print(head(sort(weighted_scores, decreasing = TRUE), n))  

  #TOP N RECOMMENDATIONS
  top_items <- sort(weighted_scores, decreasing = TRUE)
  if (length(top_items) == 0) return(NULL)
  top_ids <- names(head(top_items, n))

  
  products_clean %>% filter(Product_Id %in% top_ids)
}

current_product <- NULL

server <- function(input, output, session) {

  sink("output.txt", append = TRUE)
  filters <- reactiveValues(price = NULL, rating = NULL)
  
  show_product_modal <- function(prod) {
     current_product <<- prod 
    showModal(modalDialog(
      title = toupper(prod$Product_Name),
      tags$div(style = "text-align: center;",
               tags$img(src = prod$Product_Image_Url, style = "max-width: 100%; max-height: 300px;")
      ),
      tags$div(style = "margin-top: 15px;",
               tags$p(strong("Brand: "), prod$Product_Brand),
               tags$p(strong("Price: $"), prod$Product_Price),
               tags$p(strong("Rating: "), paste0("⭐ ", prod$Product_Rating, " (", prod$Product_Reviews_Count, " reviews)")),
               if(!is.null(prod$Product_Description) && !is.na(prod$Product_Description) && prod$Product_Description != "") {
                 tags$div(
                   tags$p(strong("Description:")),
                   tags$p(prod$Product_Description, style = "text-align: justify; font-size: 14px;")
                 )
               }
      ),
      size = "m",
      easyClose = TRUE,
      footer = tags$div(
      style = "display: flex; justify-content: space-between; width: 100%;",
      actionButton("buy_button", "🛒 Buy", class = "btn-success"),
      modalButton("Close")
    )
    ))
  }

  observeEvent(input$buy_button, {
    # Ensure the modal is showing a product
    if (!is.null(input$user_id) && input$user_id != "" && !is.null(current_product)) {
      # Show a modal to collect the user's rating
      showModal(modalDialog(
        title = "Rate the Product",
        sliderInput("user_rating", "Your Rating (1 to 5):", min = 1, max = 5, value = 3, step = 0.5),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_rating", "Submit")
        )
      ))
    } else {
      showNotification("Error: Unable to record purchase. Please try again.", type = "error")
    }
  })

  observeEvent(input$submit_rating, {
    # Ensure the user has provided a valid rating
    if (!is.null(input$user_rating) && !is.null(current_product)) {
      # Insert the interaction into the database
      new_interaction <- data.frame(
        user_id = as.numeric(input$user_id),
        product_id = current_product$Product_Id,  # Product ID from the modal
        rating = as.numeric(input$user_rating),  # User-provided rating
        timestamp = as.character(Sys.time())  # Current timestamp
      )
      interactions_db$insert(new_interaction)  # Insert into MongoDB
      interactions <<- interactions_db$find()  # Refresh the local interactions data

    # Notify the user
      showNotification("Purchased successfully!", type = "message")
      removeModal()  # Close the modal dialog
    } else {
      showNotification("Error: Unable to record rating. Please try again.", type = "error")
    }
  })
  
  observeEvent(input$filter_btn, {
    showModal(modalDialog(
      title = "Apply Filters",
      selectInput("filter_type", "Select Filter:", choices = c("None", "Price", "Rating")),
      conditionalPanel(
        condition = "input.filter_type == 'Price'",
        sliderInput("price_range", "Select Price Range ($):", min = 1, max = 1000, value = c(1, 500), step = 10)
      ),
      conditionalPanel(
        condition = "input.filter_type == 'Rating'",
        radioButtons("rating_filter", "Rating:", choices = c("Above 3 stars", "Above 4 stars"))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_filters", "Apply")
      )
    ))
  })
  
  observeEvent(input$apply_filters, {
    if (input$filter_type == "Price") {
      filters$price <- input$price_range
      filters$rating <- NULL
    } else if (input$filter_type == "Rating") {
      filters$rating <- input$rating_filter
      filters$price <- NULL
    } else {
      filters$price <- NULL
      filters$rating <- NULL
    }
    removeModal()
  })
  
  observeEvent(input$user_id, {
    if (input$user_id != "") {
      user <- users %>% filter(user_id == as.numeric(input$user_id))
      if (nrow(user) == 0) {
        # Show modal to collect additional user details
        showModal(modalDialog(
          title = "New User Registration",
          textInput("new_user_name", "Name:", value = paste("User", input$user_id)),
          selectInput("new_user_gender", "Gender:", choices = c("M", "F", "Other")),
          numericInput("new_user_age", "Age:", value = NA, min = 1, max = 120),
          textInput("new_user_location", "Location:", value = "Unknown"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_new_user", "Save")
          )
        ))
      }
    }
  })

  observeEvent(input$save_new_user, {
    if (input$user_id != "" && input$new_user_name != "") {
      # Insert new user into the database
      new_user <- data.frame(
        user_id = as.numeric(input$user_id),
        name = input$new_user_name,
        gender = input$new_user_gender,
        age = input$new_user_age,
        location = input$new_user_location
      )
      users_db$insert(new_user)  # Insert into MongoDB
      users <<- users_db$find()  # Refresh the local `users` data

      removeModal()  # Close the modal dialog
    }
  })

  #Filtering after constraint based
  filtered_products <- reactive({
    results <- products_clean
    if (input$search != "" && input$user_id != "") {
      # CONTENT BASED FILTERING + ITEM BASED (HYBRID APPROACH) - hybrid.R
      hybrid_result <- hybrid(interactions, products_clean,input$user_id, input$search, num_reco_val())
      results <- results %>%
        filter(Product_Name %in% hybrid_result$Product_Name)

       # Print a small subset of the hybrid results
      cat("\n--- Filtered Products After Hybrid Recommendation ---\n")
      print(head(results, 5))

    }
    if (input$search != "" && input$user_id == "") {
      search_term <- tolower(input$search)
      results <- results %>%
        filter(str_detect(Product_Name, fixed(search_term)) |
                str_detect(Product_Tags, fixed(search_term)) |
                str_detect(Product_Description, fixed(search_term)) |
                str_detect(Product_Brand, fixed(search_term)) |
                str_detect(Product_Category, fixed(search_term)) )
    }
    if (!is.null(filters$price)) {
      results <- results %>%
        filter(Product_Price >= filters$price[1], Product_Price <= filters$price[2]) %>%
          arrange(Product_Price) #SORT BY PRICE
      filter_message <- paste0("🔍 Showing results filtered by Price: $", filters$price[1], " - $", filters$price[2])
    }
    if (!is.null(filters$rating)) {
      rating_threshold <- ifelse(filters$rating == "Above 3 stars", 3, 4)
      results <- results %>% filter(Product_Rating >= rating_threshold) %>%
        arrange(desc(Product_Rating))  #SORT BY RATING
      filter_message <- paste0("🔍 Showing results with ", filters$rating)
    }

    # Print a small subset of the final filtered results
    cat("\n--- Final Filtered Products ---\n")
    print(head(results, 5))

    head(results, num_reco_val())
  })

  user_name <- reactive({
  if (input$user_id != "") {
    user <- users %>% filter(user_id == as.numeric(input$user_id))
    if (nrow(user) > 0) {
      return(user$name)  # Assuming the column for user names is 'name'
    }
  }
  return(NULL)
  })

  output$user_name_ui <- renderUI({
    if (!is.null(user_name())) {
      tags$p(style = "font-weight: bold; color: #007bff; margin-bottom: 15px; font-size: 18px;",
           paste("👤 Wecome Back! ", user_name()))
    }
    else if(is.null(user_name()) && input$user_id != "") {
      tags$p(style = "font-weight: bold; color: #007bff; margin-bottom: 15px; font-size: 18px;",
           paste("👤 Wecome New User"))
    }
  })

  output$recommendation_ui <- renderUI({
    display <- NULL
    filter_message <- NULL
    if (input$user_id != "" && input$search == "" && is.null(filters$price) && is.null(filters$rating)) {
      #num_reco <- ifelse(is.null(input$num_reco) || input$num_reco == "", num_reco, input$num_reco)
      #print(num_reco)
      display <- get_personalized_recommendations(input$user_id, num_reco_val())
    } else if (input$search == "" && is.null(filters$price) && is.null(filters$rating)) {
      display <- products_clean %>%
        group_by(Product_Category) %>%
        slice_sample(n = 1) %>%
        ungroup() %>%
        head(num_reco_val())
    } else {
      display <- filtered_products()
    }
    
    if (is.null(display) || nrow(display) == 0) {
      return(HTML("<i>No matching or personalized products found.</i>"))
    }

    
    
    fluidRow(
      lapply(1:nrow(display), function(i) {
        prod <- display[i, ]
        column(4,
               actionButton(
                 inputId = paste0("product_", i),
                 label = HTML(paste0(
                   "<img src='", prod$Product_Image_Url, "' style='height:150px; width:auto; display:block; margin:auto;'><br>",
                   "<b>", str_trunc(prod$Product_Name, 40), "</b><br>",
                   prod$Product_Brand, "<br>",
                   "💲", prod$Product_Price, "<br>",
                   "⭐ ", prod$Product_Rating, " (", prod$Product_Reviews_Count, " reviews)"
                 )),
                 style = "width:100%; text-align:left; white-space: normal;"
               )
        )
      })
    )
  })

  output$filter_msg <- renderUI({
    if (!is.null(filters$price)) {
      tags$p(style = "color: #007bff; font-style: italic; margin-top: 5px;",
            paste0("🔍 Price: $", filters$price[1], " - $", filters$price[2]))
    } else if (!is.null(filters$rating)) {
      tags$p(style = "color: #007bff; font-style: italic; margin-top: 5px;",
            paste0("🔍 Rating: ", filters$rating))
    }
  })

  output$trending_ui <- renderUI({
    items <- trending_products()
    if (nrow(items) == 0) return(HTML("<i>No trending products found.</i>"))
    
    tagList(
      tags$div(style = "display: flex; overflow-x: auto; gap: 10px;",
               lapply(1:nrow(items), function(i) {
                 prod <- items[i, ]
                 tags$div(style = "min-width: 220px; flex: 0 0 auto;",
                          box(
                            actionButton(
                              inputId = paste0("trending_", i),
                              label = HTML(paste0(
                                "<img src='", prod$Product_Image_Url, "' style='height:150px; width:auto; display:block; margin:auto;'><br>",
                                "<b>", str_trunc(prod$Product_Name, 40), "</b><br>",
                                prod$Product_Brand, "<br>",
                                "💲", prod$Product_Price, "<br>",
                                "⭐ ", prod$Product_Rating
                              )),
                              style = "text-align: left; width: 100%; white-space: normal;"
                            ),
                            width = 12
                          )
                 )
               })
      )
    )
  })
  
  
  current_reco_products <- reactiveVal(NULL)
  current_trending_products <- reactiveVal(NULL)
  
  observe({
    display <- NULL
    if (input$user_id != "" && input$search == "" && is.null(filters$price) && is.null(filters$rating)) {
      display <- get_personalized_recommendations(input$user_id, num_reco_val())
    } else if (input$search == "" && is.null(filters$price) && is.null(filters$rating)) {
      display <- products_clean %>%
        group_by(Product_Category) %>%
        slice_sample(n = 1) %>%
        ungroup() %>%
        head(num_reco_val())
    } else {
      display <- filtered_products()
    }
    current_reco_products(display)
  })
  
  observe({
    current_trending_products(trending_products())
  })

  observe({
  if (!is.null(input$num_reco) && input$num_reco != "" && !is.na(as.numeric(input$num_reco))) {
    num_reco_val(as.numeric(input$num_reco))
  }
})


  
  observe({
    reco_products <- current_reco_products()
    if (!is.null(reco_products)) {
      for (i in 1:nrow(reco_products)) {
        local({
          local_i <- i
          observeEvent(input[[paste0("product_", local_i)]], {
            show_product_modal(reco_products[local_i, ])
          })
        })
      }
    }
  })
  
  observe({
    trend_products <- current_trending_products()
    if (!is.null(trend_products)) {
      for (i in 1:nrow(trend_products)) {
        local({
          local_i <- i
          observeEvent(input[[paste0("trending_", local_i)]], {
            show_product_modal(trend_products[local_i, ])
          })
        })
      }
    }
  })
  sink()
}
