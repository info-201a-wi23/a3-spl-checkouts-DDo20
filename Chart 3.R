library("ggplot2")
library("dplyr")

book_10_df <- read.csv("~/info201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

pop_fan <- book_10_df %>%
  group_by(Title, CheckoutYear) %>%
  filter(str_detect(MaterialType, "BOOK")) %>%
  filter(str_detect(Subjects, "Fantasy")) %>%
  summarize(Total = sum(Checkouts))
pop_fan <- pop_fan %>%
  group_by(CheckoutYear) %>%
  filter(Total == max(Total))

ggplot(pop_fan) + geom_col(mapping = aes(
  x = CheckoutYear,
  y = Total,
  fill = Title
)) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  labs(
    title = "Most Checkouts for Fantasy genre",
    subtitle = "From 2017 to 2023",
    x = "Year",
    y = "Number Checkouts",
    fill = "Book Title"
  )+ 
  coord_flip()