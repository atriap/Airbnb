# Understanding Analytics : Airbnb Tokyo

This is a self-made project with Airbnb Tokyo as the subject of my analysis, simply because i am curious about its analytics and Airbnb has the data opensource in  [Inside Airbnb](http://insideairbnb.com/).

## Overview

![Dashboard 2](https://user-images.githubusercontent.com/104981673/204445849-1c37bc47-0d32-4483-b024-5e3a8f514994.png)





This visualization was made with Tableau, check the interactive dashboard [here](https://public.tableau.com/app/profile/atrria/viz/AirbnbTokyo_16696444219970/Dashboard2). However, for further exploration, I used R programming language with Rstudio to analyze the data i obtained.
To answer my curiosity, I made several questions, as follows:

1. How many accomodations are in Tokyo?
2. Which Tokyo neighbourhood have the most accomodations?
3. Which Tokyo neighbourhood most possible to earn the highest revenue?
4. Which accommodation type is the most common in Tokyo?
5. What is the most favourite room type?
6. Is there a host who have more than one listings?
7. What is the most common minimum nights required by hosts?




## Getting The Data

```r
library(tidyr)
library(dplyr)
library(tibble)
```

```r
airbnb_tokyo <- read.csv("listings_tokyo.csv") # data per Sept 22th
View(airbnb_tokyo)
```

```r
airbnb_tokyo <- as_tibble(airbnb_tokyo)
airbnb_tokyo
```

```r
dim(airbnb_tokyo)
str(airbnb_tokyo)
head(airbnb_tokyo, 5)
```

```r
airbnb_tokyo %>%
select(everything()) %>%
summarise_all(funs(sum([is.na](http://is.na/)(.))))
```

## Data Cleaning

For better, easier, and effective analysis, data cleaning was conducted by several steps:

```r
# Replace NA with 0 and drop unnecessary column

airbnb_tokyo <- airbnb_tokyo %>%
select(-neighbourhood_group) %>%
replace([is.na](http://is.na/)(.), 0)
```

```r
airbnb_tokyo %>%
select(everything()) %>%
summarise_all(funs(sum([is.na](http://is.na/)(.))))

dim(airbnb_tokyo)
```

## Understanding the Data

### Data Description

```r
desc_tokyo <- airbnb_tokyo %>%
select(where(is.numeric))  %>%
pivot_longer(cols = everything()) %>%
group_by(name) %>%
summarise(Mean = mean(value), Max = max(value), Min = min(value), S.Dev = sd(value), Sum = sum(value)) %>%
column_to_rownames('name') %>%
t # supaya lebih mudah diliatnya
```

```r
desc_tokyo <- data.frame(desc_tokyo)
desc_tokyo
```

## Answering the Questions

### How many accomodations are listed in Airbnb Tokyo?

```r
accomodations <- airbnb_tokyo %>%
distinct(id)
```

```r
View(accomodations)
dim(accomodations)
```

### Which Tokyo neighbourhood have the most accomodations?

```r
most_listings <- airbnb_tokyo %>%
group_by(neighbourhood) %>%
summarise(listings_per_neighbourhood = n()) %>%
arrange(desc(listings_per_neighbourhood))
```

```r
View(most_listings)
dim(most_listings)
head(most_listings, 10)
```

### Which Tokyo neighbourhood most possible to earn the highest revenue?

```r
est_revenue <- airbnb_tokyo %>%
mutate(est_revenue = price * minimum_nights) %>%
group_by(neighbourhood) %>%
summarise(across(c(est_revenue), sum)) %>%
arrange(desc(est_revenue)) %>%
mutate(est_revenue = comma(est_revenue))
```

```r
View(est_revenue)
dim(est_revenue)
head(est_revenue, 10)
```

### Which accommodation type is the most common in Tokyo?

```r
common_type <- airbnb_tokyo %>%
group_by(room_type) %>%
summarise(sum_of_room_type = n()) %>%
arrange(desc(sum_of_room_type))

common_type
```

## Room type distribution within Tokyo based on Neighbourhood

```r
room_type_per_neighbourhood <- airbnb_tokyo %>%
pivot_longer(cols = everything()) %>%
group_by(neighbourhood) %>%
summarise(Sum = sum(value)) %>%
column_to_rownames('name') %>%
t                                           
```

## What is the most favourite room type?

To find which type is the most booked room type based on number of reviews received

```r
common_type_booked <- airbnb_tokyo %>%
  filter(number_of_reviews > 1) %>%
  group_by(room_type) %>%
  summarise(sum_of_room_type = n()) %>%
  arrange(desc(sum_of_room_type))

common_type_booked
```

## Is there a host who have more than one listings?

Who is the top 20?

```r
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

Most hosts have more than one listings, but how many were booked?

To estimate the number of bookings, i sorted it out based on number of reviews they had received.

```r
top_reviewed <- airbnb_tokyo %>%
group_by(host_id, host_name) %>%
summarise(across(c(number_of_reviews), sum)) %>%
arrange(desc(number_of_reviews))

top_reviewed
View(top_reviewed)
```

```r

# To find out the most reviewed in 1 last year

top_reviewed_1y <- airbnb_tokyo %>%
filter(last_review >= as.Date ("2021-09-22")) %>%
group_by(host_id, host_name) %>%
summarise(across(c(number_of_reviews), sum)) %>%
arrange(desc(number_of_reviews))

top_reviewed_1y
View(top_reviewed_1y)
```

## What is the most common minimum nights required by hosts?

```r
nights <- airbnb_tokyo %>%
count(minimum_nights) %>%
arrange(desc(n))
nights

head(nights,10)
```

## What is the average nights booked in each accomodation in last one year? based on minimum nights available

```r
avg_night <- airbnb_tokyo %>%
filter(last_review >= as.Date("2021-09-22"), number_of_reviews > 0) %>%
group_by(id, name) %>%
summarise(mean_nights = mean(minimum_nights))

View(avg_night)
```

## how is the correlation between minimum nights and booked accomodation? if there is, how many nights is the most interesting?

based on minimum_nights and review, how many nights are the most booked
```r
popular_neighbourhood <- airbnb_tokyo %>%
filter(last_review >= as.Date("2021-09-22"), number_of_reviews > 0) %>%
select(c(neighbourhood, minimum_nights, number_of_reviews)) %>%
group_by(neighbourhood) %>%
summarise(across(c(number_of_reviews), sum)) %>%
arrange(desc(number_of_reviews))

View(popular_neighbourhood)    # popular neighbourhood BOOKED, not only has many listings
```

```r
popular_night <- airbnb_tokyo %>%
  filter(last_review >= as.Date("2021-09-22"), number_of_reviews > 0) %>%
  group_by(neighbourhood, minimum_nights) %>%
  summarise(across(c(number_of_reviews), sum)) %>%
  arrange(desc(number_of_reviews))

View(popular_night)
popular_night

most_popular_night <- popular_night%>%
  group_by(neighbourhood) %>%
  slice(1:1) %>%  # slice is used to find out the most popular night in each neighbourhood
  arrange(desc(number_of_reviews))
   

View(most_popular_night)
most_popular_night
```

## How about the reviews?

```r
# importing the dataset

reviews_detailed <- read.csv("reviews.csv")
as_tibble(reviews_detailed)
```

```r
# inspecting NA

reviews_detailed %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
```

```r
# to sort into the latest reviews

reviews_sorted <- reviews_detailed %>%
arrange(desc(date))

View(reviews_sorted)
```

```r
# To find out when listings receive the most reviews

review_dates <- reviews_detailed %>%
group_by(date) %>%
count(date) %>%
arrange(desc(n))

View(review_dates)
```

```r
# finding out amount of reviews received in 2022

review2022 <- reviews_detailed %>%
filter(grepl("2022", date)) %>%
group_by(date) %>%
count(date) %>%
arrange(desc(n))

View(review2022)
```

# see the latest review received

```r
review2022_latest <- reviews_detailed %>%
filter(date >= as.Date("2022-01-01")) %>%
group_by(date) %>%
count(date) %>%
arrange(desc(date))

View(review2022_latest)
```
