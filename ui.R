## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)
source('functions/helpers.R')

shinyUI(
  
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Recommender by Rating", tabName = "rating"),
        menuItem("Recommender by Genre", tabName = "genre")
      )
    ),
    
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "rating",
                includeCSS("css/movies.css"), 
                fluidRow(
                  box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings2')
                      )
                  )
                ),
                fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results2")
                  )
                )
        ),
        
        
        # Second tab content
        tabItem(tabName = "genre",
                includeCSS("css/movies.css"), 
                fluidRow(
                  box(width = 12, title = "Step 1: Select as many movies as possible (this will also determine your favoriate genre)", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings1')
                      )
                  )
                ),
                fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies of your favoriate genre based on your selection",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results1")
                  )
                )
        )
        
        

      )
    )
  ) 
)