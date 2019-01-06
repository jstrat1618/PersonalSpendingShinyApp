library(RSQLite)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(ggplot2)
library(ggthemes)
library(plotly)
library(shiny)

conn <- dbConnect(drv = SQLite(), "FakeDatabase.db")

tmp_dat <- dbGetQuery(conn, "SELECT * FROM Expenses")

savings <- dbGetQuery(conn, "SELECT * FROM SavingsTransfers")

outs <- dbGetQuery(conn, "SELECT * FROM OutlyingExpenses")


dat <-
  tmp_dat %>%
  mutate(Date = as.Date(Date, origin = '1970-01-01')) %>%
  as_tibble()



toMonthly <- function(df){
  df %>%
    mutate(Month = month(Date, label=TRUE),
           Year = year(Date),
           month_begins = floor_date(Date, unit="month")) %>%
    group_by(Month, Year) %>%
    summarise(
      month_begins = month_begins[1],
      spent = sum(spent)) %>%
    ungroup() %>%
    arrange(month_begins) %>%
    mutate(last_yr_spent = lag(spent, 12),
           last_month_spent = lag(spent, 1),
           month_year = paste(Month, substr(as.character(Year), 3,4), sep=""),
           month_year = factor(month_year)) %>%
    select(month_year, everything(.))
  
}

dbDisconnect(conn)

todays_date <- Sys.Date()
todays_month <- month(todays_date, label = TRUE)
todays_year <- year(todays_date)



pct_change <- function(ynew, yold) (ynew-yold)/yold

shinyServer(function(input, output) {
  
  output$mnth_plt <- renderPlotly({
    
    if(input$omitSavings)dat <- filter(dat, !(expense_id %in% savings$expense_id))
    
    if(input$omitOutliers)dat <- filter(dat, !(expense_id %in% outs$expense_id))
    
    
    mdat <- toMonthly(dat)
    
    if(input$omitCurrentMonth){
      mdat <- 
        mdat %>%
        filter(Month != todays_month | Year != todays_year )
    }

    mnth_plt <-
      mdat %>%
      ggplot(aes(reorder(month_year, month_begins), spent,
                 fill = as.character(Month),
                 text = paste('Timeframe: ', reorder(month_year, month_begins),
                              'Spent:', dollar(spent))))+
        geom_bar(stat = "identity")+
        scale_y_continuous(labels = scales::dollar)+
        theme_bw()+
        theme(axis.text.x  = element_text(angle = 90))+
        labs(x="", y="Spent", fill="Month")
    
    plt <-ggplotly(mnth_plt, tooltip = "text")
    
    print(plt)  

  })
  
  output$last_month_spending <- renderText({
    
    if(input$omitSavings)dat <- filter(dat, !(expense_id %in% savings$expense_id))
    
    if(input$omitOutliers)dat <- filter(dat, !(expense_id %in% outs$expense_id))
    
    
    mdat <- toMonthly(dat)
    
    last_month_dat <- mdat[(nrow(mdat) - 1),]
    last_month_spnt <- dollar_format()(last_month_dat$spent)
    yoy_change <- pct_change(last_month_dat$spent,last_month_dat$last_yr_spent)
    yoy_up_down <- ifelse(yoy_change >0, "increase", "decrease")
    
    mom_change <- pct_change(last_month_dat$spent, last_month_dat$last_month_spent)
    mom_up_down <- ifelse(mom_change >0, "increase", "decrease")
    
    paste("Last month we spent ", last_month_spnt, ", a ", percent(yoy_change),
          yoy_up_down, " YOY, and ", percent(mom_change), " ", mom_up_down, 
          " from last month.",
          sep="")
    
    
  })
  
  output$yoy_tbl <- renderTable({
    
    if(input$omitSavings)dat <- filter(dat, !(expense_id %in% savings$expense_id))
    
    if(input$omitOutliers)dat <- filter(dat, !(expense_id %in% outs$expense_id))
    
    
    mdat <- toMonthly(dat)
    
    out_df <- 
      mdat %>%
      mutate(yoy_roc = percent(pct_change(spent, last_yr_spent)),
             mom_roc = percent(pct_change(spent,  lag(spent, 1)))) %>%
      drop_na() %>%
      mutate_at(vars(spent, last_yr_spent), function(x)dollar(x)) %>%
      select(Month, Year, spent, last_yr_spent, yoy_roc, mom_roc) %>%
      rename(Spent = spent, 
             `Last Year` = last_yr_spent,
             `YOY ROC` = yoy_roc,
             `MOM ROC` = mom_roc)
    
    if(input$omitCurrentMonth){
      out_df %>%
        filter(Month != todays_month | Year != todays_year )
    } else out_df
    
    
  })
  
  output$yoy_proj <- renderText({
    if(input$omitSavings)dat <- filter(dat, !(expense_id %in% savings$expense_id))
    
    if(input$omitOutliers)dat <- filter(dat, !(expense_id %in% outs$expense_id))
    
    mdat <- toMonthly(dat)
    
    last_year_spent <-
      mdat %>%
      filter(Month == todays_month & Year == (todays_year - 1)) %>%
      select(spent) %>%
      as.numeric()
    
    yoy_change <- input$yoy_change/100
    spending_proj <- dollar(last_year_spent*(1+yoy_change))
  
    paste("Based on YOY spending, you will spend ",  as.character(spending_proj),
          ".",  sep='')
    
  })
  

  

})
