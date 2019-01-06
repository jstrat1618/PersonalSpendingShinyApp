library(plotly)
library(shiny)

shinyUI(fluidPage(

  headerPanel("Welcome!!!"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("omitCurrentMonth",
                   label = "Omit This Month: ",
                  c("Yes"=TRUE, "No" = FALSE),
                  selected = "Yes"),
      
      selectInput("omitSavings",
                  label = "Omit Savings: ",
                  c("Yes"=TRUE, "No" = FALSE),
                  selected = "Yes"),
      
      selectInput("omitOutliers",
                  label = "Omit Outliers: ",
                  c("Yes"=TRUE, "No" = FALSE),
                  selected = "Yes"),
      
      sliderInput("yoy_change", min = -25, max = 25, value = 0, 
                  label = "YOY Change", post = " %"),
      
      headerPanel(""),
      headerPanel("Next Month's Spending"),
      textOutput("yoy_proj")
      
      
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("mnth_plt"),
      textOutput("last_month_spending"),
      tableOutput("yoy_tbl")
    )
  )
))
