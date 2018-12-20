library(dplyr)
library(RSQLite)

conn <- dbConnect(SQLite(), "FakeDatabase.db")

origin_date <- as.Date("2017-06-01")
end_date <- as.Date("2018-12-31")

date_seq <- seq(origin_date, end_date, by = 1)
items <- c(rep(letters, 2), "transfer")
alpha <- 6
beta <- 5

num_transactions <- 3500 #num
set.seed(2018)
dates <- sample(date_seq, size = num_transactions, replace = TRUE)

set.seed(2018)
purchases <- sample(items, size = num_transactions, replace = TRUE)
set.seed(2018)
amts <- round(rgamma(num_transactions, shape = alpha, scale = beta),2)


dat <-tibble(expense_id = 1:num_transactions, Date = dates, 
             purchase = purchases, spent = amt)

dbWriteTable(conn, name = "Expenses", value = dat, append=TRUE)

out_df <-
  dat %>%
  arrange(desc(spent)) %>%
  head()

dbWriteTable(conn, name = "OutlyingExpenses", value = out_df, append=TRUE)

tdf <- filter(dat, purchase == "transfer")

dbWriteTable(conn, name = "SavingsTransfers", value = tdf, append=TRUE)


dbDisconnect(conn)

