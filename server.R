library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(shiny)

dat <- read_csv("../../Misc/PersonalFinances/CleanedFinancialData/MasterSpending.csv")

mdat <- 
  dat %>%
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
         month_year = paste(Month, substr(as.character(Year), 3,4), sep=""),
         month_year = factor(month_year)) %>%
  select(month_year, everything(.))

todays_date <- Sys.Date()
todays_month <- month(todays_date, label = TRUE)
todays_year <- year(todays_date)

pct_change <- function(ynew, yold) (ynew-yold)/yold

shinyServer(function(input, output) {

  output$mnth_plt <- renderPlot({
    
    if(input$omit){
      mdat <- 
        mdat %>%
        filter(Month != todays_month | Year != todays_year )
    }

    mnth_plt <-
      mdat %>%
      ggplot(aes(reorder(month_year, month_begins), spent))+
        geom_bar(stat = "identity")+
        theme(axis.text.x  = element_text(angle = 90))+
        labs(x="", y="Spent")
    
    print(mnth_plt)
      

  })
  
  output$last_month_spending <- renderText({
    
    last_month_dat <- mdat[(nrow(mdat) - 1),]
    last_month_spnt <- dollar_format()(last_month_dat$spent)
    yoy_change <- pct_change(last_month_dat$spent,last_month_dat$last_yr_spent)
    change <- ifelse(yoy_change >0, "increase", "decrease")
    
    paste("Last month we spent ", last_month_spnt, ", a ", percent(yoy_change),
          change, ".", sep="")
    
    
  })
  
  output$yoy_tbl <- renderTable({
    out_df <- 
      mdat %>%
      filter(!is.na(last_yr_spent)) %>%
      mutate(yoy_roc = percent(pct_change(spent, last_yr_spent))) %>%
      mutate_at(vars(spent, last_yr_spent), function(x)dollar(x)) %>%
      select(Month, Year, spent, last_yr_spent, yoy_roc) %>%
      rename(Spent = spent, 
             `Last Yea` = last_yr_spent,
             `YOY Change` = yoy_roc)
    
    if(input$omit){
      out_df %>%
        filter(Month != todays_month | Year != todays_year )
    } else out_df
    
    
  })

})