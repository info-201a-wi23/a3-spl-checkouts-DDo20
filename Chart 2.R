library("ggplot2")
library("dplyr")
library("stringr")
library(plotly)

book_10_df <- read.csv("~/info201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

book_10_df <- book_10_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
book_10_df$date <- as.Date(book_10_df$date, format = "%Y-%m-%d")

asoiaf_df <- book_10_df %>% 
  filter(str_detect(Creator, "George R. R. Martin"))

asoiaf_df$Title <- tolower(asoiaf_df$Title)

asoiaf_df$Title[str_detect(asoiaf_df$Title, "a game of thrones")] <- "A Game of Thrones"
asoiaf_df$Title[str_detect(asoiaf_df$Title, "a clash of kings")] <- "A Clash of Kings"
asoiaf_df$Title[str_detect(asoiaf_df$Title, "a storm of swords")] <- "A Storm of Swords"
asoiaf_df$Title[str_detect(asoiaf_df$Title, "a feast for crows")] <- "A Feast for Crows"
asoiaf_df$Title[str_detect(asoiaf_df$Title, "a dance with dragons")] <- "A Dance with Dragons"

asoiaf_title <- asoiaf_df %>% 
  filter(Title %in% 
           c("A Game of Thrones", "A Clash of Kings", "A Storm of Swords", "A Feast for Crows", "A Dance with Dragons"))

asoiaf_sum_check_df <- asoiaf_title %>% 
  group_by(date, Title) %>%
  summarize(Total_Checkout = sum(Checkouts))

asoiaf_plot <- ggplot(data = asoiaf_sum_check_df) +
  geom_line(aes(x = date,
                y = Total_Checkout,
                color = Title)) +
  scale_y_continuous(breaks = seq(50, 300, 50))+
  labs(title = "Comparing George R.R. Martin Books (all formats)",
       subtitle = "From 2017 to 2023",
       x = "Year",
       y = "Total Checkouts",
       fill = "Title") 

ggplotly(asoiaf_plot)
