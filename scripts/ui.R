library(shiny)
library(shinydashboard)

shinyUI(
  navbarPage("🛒 E-Commerce Recommendation System",
             id = "main_navbar",
             header = tags$head(
               tags$style(HTML("
                 .navbar {
                   background-color: #2c3e50;
                   border: none;
                   border-radius: 0;
                 }
                 .navbar-default .navbar-brand {
                   color: #ecf0f1;
                   font-weight: bold;
                   font-size: 20px;
                 }
                 .navbar-default .navbar-nav > li > a {
                   color: #ecf0f1;
                 }

                 /* Hover effect */
                 .navbar-default .navbar-nav > li > a:hover {
                   background-color: #34495e;
                   color: #ffffff;
                 }

                 /* Active tab highlight */
                 .navbar-default .navbar-nav > .active > a {
                   background-color: #1abc9c !important;
                   color: white !important;
                 }

                 /* Page body padding */
                 .container-fluid {
                   padding-top: 20px;
                 }
                 
               "))
             ),
             tabPanel("Home",
                      fluidPage(
                        # Centered Search and First Row with User ID on left
                        fluidRow(
                          column(3),
                          column(6,
                                 div(style = "text-align: center;",
                                     textInput("search", "🔍 Search Products...", "")
                                 )
                          ),
                          column(3, div(style = "text-align: right;",
                                        actionButton("filter_btn", "⚙️ Filter", class = "btn-primary", style = "margin-top: 25px;"),
                                        br(),
                                        uiOutput("filter_msg")
                          ))
                        ),
                        
                        # User ID moved to top left
                        fluidRow(
                          column(4,
                              uiOutput("user_name_ui"),  # Placeholder for user name
                              textInput("user_id", "👤 Enter User ID", "")
                              ),
                          column(4, numericInput("num_reco", "No. of Recommendations", value = 5, min = 1, max = 20)),
                          column(4)
                          ),
                        
                        tags$hr(),
                        h4("🎯 Recommended Products"),
                        uiOutput("recommendation_ui"),
                        
                        tags$hr(),
                        h4("🔥 Trending Products"),
                        uiOutput("trending_ui")
                      )
             )
  )
)