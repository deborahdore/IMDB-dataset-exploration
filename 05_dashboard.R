
library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)
library(magrittr)
library(ggpubr)
library(viridis)

# source("05_dashboard_prepare.R")
load("05_dashboard_workspace.rda")



##### USER INTERFACE ##########################################################

ui <- fluidPage(
  
  dashboardPage(
    
    dashboardHeader(title = "IMDB Analysis"),
    
    dashboardSidebar(sidebarMenu(id = "menu", sidebarMenuOutput("menu"))),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "success",
                fluidRow(
                  box(width = 12, solidHeader = TRUE, status = "primary",
                      title = "Correlations",
                      "correlation plots")
                ),
                fluidRow(
                  box(width = 12, solidHeader = TRUE, status = "primary",
                      title = textOutput("topDensity_text"),
                      column(width = 3,
                             sliderInput(inputId = "topDensity",
                                         label = "How many of the best in ratings, votes and translations?",
                                         value = 800, min = 300, max = 1000, step = 100)
                      ),
                      column(width = 9,  plotOutput("topDensity"))
                  )
                )
        ),
        tabItem(tabName = "seasons",
                fluidRow(
                  box(width = 12, solidHeader = TRUE, status = "primary",
                      title = "Total number of Seasons",
                      column(width = 1,
                             checkboxInput(inputId = "seasonsViolin",
                                           label = "distinction by success", value = FALSE)
                      ),
                      column(width = 11, plotOutput("seasonsViolin"))
                  )
                )
        ),
        tabItem(tabName = "genres",
                fluidRow(
                  box(width = 12, solidHeader = TRUE, status = "primary",
                      column(width = 1,
                             checkboxGroupInput(inputId = "genresBar", label = "Genres",
                                                choices = all_genres %>% sort(),
                                                selected = c("Action", "Adventure", "Crime", "Mystery", "Drama", "Animation", "Sci-Fi", "Thriller", "Fantasy", "Comedy", "Romance"))
                      ),
                      column(width = 11, plotOutput("genresBar"))
                  )
                ),
                fluidRow(
                  box(width = 12, solidHeader = TRUE, status = "primary",
                      column(width = 1,
                             checkboxGroupInput(inputId = "genresViolin", label = "Genres",
                                                choices = all_genres %>% sort(),
                                                selected = c("Action", "Crime", "Mystery", "Drama", "Sci-Fi", "Comedy"))
                      ),
                      column(width = 11, plotOutput("genresViolin"))
                      )
                )
        ),
        tabItem(tabName = "genresSeasons",
                fluidRow(
                  box(width = 12, solidHeader = TRUE, status = "primary",
                      column(width = 2,
                             sliderInput(inputId = "genresLine",
                                         label = "Until which season?",
                                         value = 8, min = 2, max = 15, step = 1)
                      ),
                      column(width = 10, plotOutput("genresLine", height = "600px"))
                  )
                )
        )
      )
    )
  )
)
    
    

##### SERVER ##################################################################

server <- function(input, output) {
  
  output$menu = renderMenu({
    sidebarMenu(
      menuItem("What is success?", tabName = "success"),
      menuItem("Seasons", tabName = "seasons"),
      menuItem("Genres", tabName = "genres"),
      menuItem("Genres and Seasons", tabName = "genresSeasons")
    )
  })
  
  output$topDensity_text = renderText({
    paste0(value_list[[paste0("topDensity_", input$topDensity)]],
                " successful series")
    })
  
  output$topDensity = renderPlot(plot_list[[paste0("topDensity_", input$topDensity)]])
 
  output$seasonsViolin = renderPlot(plot_list[[paste0("seasonsViolin_", input$seasonsViolin)]])
  
  output$genresBar = renderPlot(fun_genresBar(all_genres = input$genresBar, tb))
  
  output$genresViolin = renderPlot(fun_genresViolin(all_genres = input$genresViolin, tb2))
  
  output$genresLine = renderPlot(plot_list[[paste0("genresLine_", input$genresLine)]])
}


##### SHINY APP ###############################################################

shinyApp(ui = ui, server = server)

