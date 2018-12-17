library(shiny)

shinyUI(fluidPage(

  headerPanel("Welcome!!!"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("omit",
                   label = "Omit This Month: ",
                  c("Yes"=TRUE, "No" = FALSE),
                  selected = "Yes")
      
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mnth_plt"),
      textOutput("last_month_spending")
    )
  )
))
