
```{r}

library(tidyr)
library(dplyr)
library(tibble)

```


# GETTING THE DATA

```{r}

airbnb_tokyo <- read.csv("listings_tokyo.csv") # data per Sept 22th
View(airbnb_tokyo)

```

```{r}

airbnb_tokyo <- as_tibble(airbnb_tokyo)
airbnb_tokyo

dim(airbnb_tokyo)
str(airbnb_tokyo)
head(airbnb_tokyo, 5)

```


```{r}

airbnb_tokyo %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

```


# DATA CLEANING 

# replace NA with 0 and drop unnecessary column


```{r}

airbnb_tokyo <- airbnb_tokyo %>%
  select(-neighbourhood_group) %>%
  replace(is.na(.), 0)

```

```{r}

airbnb_tokyo %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
dim(airbnb_tokyo)

```



# EXPLORATORY DATA ANALYSIS

```{r}

desc_tokyo <- airbnb_tokyo %>%
  select(where(is.numeric))  %>%
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(Mean = mean(value), Max = max(value), Min = min(value), S.Dev = sd(value), Sum = sum(value)) %>% 
  column_to_rownames('name') %>%
  t # supaya lebih mudah diliatnya

```

```{r}

desc_tokyo <- data.frame(desc_tokyo)
desc_tokyo

```


# DATA ANALYSIS

# How many accomodations are listed in Airbnb Tokyo?

```{r}

accomodations <- airbnb_tokyo %>%
  distinct(id)
         
View(accomodations)
dim(accomodations)


```


  
# Which Tokyo neighbourhood have the most accomodations?

```{r}

most_listings <- airbnb_tokyo %>%
  group_by(neighbourhood) %>%
  summarise(listings_per_neighbourhood = n()) %>%
  arrange(desc(listings_per_neighbourhood))

View(most_listings)
dim(most_listings)
head(most_listings, 10)

```



# Which Tokyo neighbourhoods most possible to earn the highest revenue?

```{r}

library(scales)

est_revenue <- airbnb_tokyo %>%
  mutate(est_revenue = price * minimum_nights) %>%
  group_by(neighbourhood) %>%
  summarise(across(c(est_revenue), sum)) %>%
  arrange(desc(est_revenue)) %>%
  mutate(est_revenue = comma(est_revenue))

View(est_revenue)
dim(est_revenue)
head(est_revenue, 10)

```

  

# Which accommodation type is the most common in Tokyo?

```{r}

common_type <- airbnb_tokyo %>%
  group_by(room_type) %>%
  summarise(sum_of_room_type = n()) %>%
  arrange(desc(sum_of_room_type))
  
common_type

```



# room type distribution within Tokyo based on City

```{r}

room_type_per_neighbourhood <- airbnb_tokyo %>%
  group_by(neighbourhood, room_type) %>%
  summarise(sum_of_room = n()) %>%
  spread(room_type, sum_of_room) %>%
  replace(is.na(.), 0) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(Total))

room_type_per_neighbourhood

```

                                

# what is the most booked room type based on number of reviews received? # orang paling suka room type yang seperti apa

```{r}

common_type_booked <- airbnb_tokyo %>%
  filter(number_of_reviews > 1) %>%
  group_by(room_type) %>%
  summarise(sum_of_room_type = n()) %>%
  arrange(desc(sum_of_room_type))

common_type_booked

```



# Is there a host who have more than one listings? Who is the top 20?

```{r}

top_hosts <- airbnb_tokyo %>%
  group_by(host_id, host_name) %>%
  summarise(listings_per_host = n()) %>%
  filter(listings_per_host > 1) %>%
  arrange(desc(listings_per_host))

top_hosts
View(top_hosts)

top20_hosts <- head(top_hosts, 20)
top20_hosts

```


```{r}

top_reviewed <- airbnb_tokyo %>%
  group_by(host_id, host_name) %>%
  summarise(number_of_reviews = sum(number_of_reviews), listings_per_host = n() ) %>%
  arrange(desc(number_of_reviews))

top_reviewed
View(top_reviewed)


```


```{r}

top_host_reviewed_1y <- airbnb_tokyo %>%
  filter(last_review >= as.Date ("2021-09-22")) %>%
  group_by(host_id, host_name) %>%
  summarise(across(c(number_of_reviews), sum)) %>%
  arrange(desc(number_of_reviews))

top_host_reviewed_1y
View(top_host_reviewed_1y)

```



# How is the minimum nights available? What is the most common?

```{r}

nights <- airbnb_tokyo %>%
  count(minimum_nights) %>%
  arrange(desc(n))
nights

head(nights, 10)

```



# What is the average nights booked in each accomodation in last 1 year? based on minimum nights available

```{r}

avg_night <- airbnb_tokyo %>%
  filter(last_review >= as.Date("2021-09-22"), number_of_reviews > 0) %>%
  group_by(id, name) %>%
  summarise(mean_nights = mean(minimum_nights))

View(avg_night)
avg_night
```



## how is the correlation between minimum nights and booked accomodation? if there is, how many nights is the most interesting?
# (minimum_nights and review, orang paling suka brp malem

```{r}

popular_neighbourhood <- airbnb_tokyo %>%
  filter(last_review >= as.Date("2021-09-22"), number_of_reviews > 0) %>%
  select(c(neighbourhood, number_of_reviews)) %>%
  group_by(neighbourhood) %>%
  summarise(across(c(number_of_reviews), sum)) %>%
  arrange(desc(number_of_reviews))

View(popular_neighbourhood)    # popular neighbourhood BOOKED, not only has many listings
popular_neighbourhood

```


```{r}

popular_night <- airbnb_tokyo %>%
  filter(last_review >= as.Date("2021-09-22"), number_of_reviews > 0) %>%
  group_by(neighbourhood, minimum_nights) %>%
  summarise(across(c(number_of_reviews), sum)) %>%
  arrange(desc(number_of_reviews))

View(popular_night)
popular_night

most_popular_night <- popular_night%>%
  group_by(neighbourhood) %>%
  slice(1:1) %>%
  arrange(desc(number_of_reviews))
   

View(most_popular_night)  # yang bener, orang paling banyak pesen berapa malem
most_popular_night
```



## REVIEWS


```{r}

# importing the dataset

reviews_detailed <- read.csv("reviews.csv")
as_tibble(reviews_detailed)

# inspecting NA

reviews_detailed %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))




```

```{r}

# to find out the latest reviews

reviews_sorted <- reviews_detailed %>%
  arrange(desc(date))

reviews_sorted

```


```{r}

# To find out when listings receive the most reviews

review_dates <- reviews_detailed %>%
  group_by(date) %>%
  count(date) %>%
  arrange(desc(n))

review_dates
View(review_dates)

```


# finding out amount of reviews received in 2022

```{r}

review2022 <- reviews_detailed %>%
  filter(grepl("2022", date)) %>%
  group_by(date) %>%
  count(date) %>%
  arrange(desc(n))
View(review2022)

review2022

```


```{r}

review2022_latest <- reviews_detailed %>%
  filter(date >= as.Date("2022-01-01")) %>%
  group_by(date) %>%
  count(date) %>%
  arrange(desc(date))
View(review2022_latest) 

review2022_latest

```

```{r}
library(dplyr)
new <- left_join(airbnb_tokyo, reviews_detailed, by = c("id" = "listing_id"))
head(new)

#menyimpan summary ke excel

install.packages("data.table")
library(data.table)

fwrite(new, "C:\\Users\\ASUS\\Desktop\\Data Analyst Portofolio\\Airbnb Tokyo\\listings_reviews.csv")
```




# DATA VISUALIZATION

#pandemic correlation vizualisation (2020,2021, 2022)

