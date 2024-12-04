library("tidyverse")
library(readr)
library("Metrics")
library(ie2misc)
library("tibble")

# Clear working memory
rm(list=ls())


# Set the working directory.
setwd("/Users/madeleineroberts/rWorkingDirectory")

# Import data sets
historical_senate_polls <- read_csv("senate_polls_historical2.csv")

# Clean data sets for merging
historical_senate_polls$candidate_name <- toupper(historical_senate_polls$candidate_name)
historical_senate_polls$state <- toupper(historical_senate_polls$state)

colnames(historical_senate_polls)[colnames(historical_senate_polls) == "candidate_name"] ="candidate"
colnames(historical_senate_polls)[colnames(historical_senate_polls) == "cycle"] ="year"
colnames(historical_senate_polls)[colnames(historical_senate_polls) == "pct"] ="poll_pct"


# Reorder columns
historical_senate_polls <- historical_senate_polls %>% relocate(candidate, .after = pollster)
historical_senate_polls <- historical_senate_polls %>% relocate(state, .after = candidate)
historical_senate_polls <- historical_senate_polls %>% relocate(poll_pct, .after = state)
historical_senate_polls <- historical_senate_polls %>% relocate(sample_size, .after = poll_pct)
historical_senate_polls <- historical_senate_polls %>% relocate(fte_grade, .after = sample_size)
historical_senate_polls <- historical_senate_polls %>% relocate(end_date, .after = fte_grade)
historical_senate_polls <- historical_senate_polls %>% relocate(election_date, .after = end_date)

# Assign poll weights for pollster rating
# Would rather use a switch statement but I am being lazy
historical_senate_polls$pollster_weights <- ifelse(historical_senate_polls$fte_grade == "A+", 13/91, 
                                                   ifelse(historical_senate_polls$fte_grade == "A", 12/91,
                                                   ifelse(historical_senate_polls$fte_grade == "A-", 11/91,
                                                   ifelse(historical_senate_polls$fte_grade == "A/B", 10/91,
                                                   ifelse(historical_senate_polls$fte_grade == "B+", 9/91,
                                                   ifelse(historical_senate_polls$fte_grade == "B", 8/91,
                                                   ifelse(historical_senate_polls$fte_grade == "B-", 7/91,
                                                   ifelse(historical_senate_polls$fte_grade == "B", 6/91,
                                                   ifelse(historical_senate_polls$fte_grade == "B/C", 5/91,
                                                   ifelse(historical_senate_polls$fte_grade == "C+", 4/91,
                                                   ifelse(historical_senate_polls$fte_grade == "C", 3/91,
                                                   ifelse(historical_senate_polls$fte_grade == "C-", 2/91,
                                                   ifelse(historical_senate_polls$fte_grade == "C/D", 1/91,
                                                   ifelse(is.na(historical_senate_polls$fte_grade), 1/91,
                                                          1/91))))))))))))))

historical_senate_polls <- historical_senate_polls %>% relocate(pollster_weights, .after = fte_grade)



# Calculate the days between poll and election day
historical_senate_polls$days_to_election <- as.Date(as.character(historical_senate_polls$election_date), format="%m/%d/%Y")-
  as.Date(as.character(historical_senate_polls$end_date), format="%m/%d/%Y")

historical_senate_polls <- historical_senate_polls %>% relocate(days_to_election, .after = pollster_weights)

# Use exponential decay to weight days until election, this will allow for the polling closest to election day to have increased influence
# https://stats.stackexchange.com/questions/454415/how-to-account-for-the-recency-of-the-observations-in-a-regression-problem
# could also use this instead:
# https://search.r-project.org/CRAN/refmans/nzelect/html/weight_polls.html
gamma = 1/100
historical_senate_polls$recency_weights <- exp(-1*gamma*as.numeric(historical_senate_polls$days_to_election))
historical_senate_polls <- historical_senate_polls %>% relocate(recency_weights, .after = days_to_election)


# weight sample size
# https://fivethirtyeight.com/features/polls-now-weighted-by-sample-size/
historical_senate_polls$sample_size_weights <- (historical_senate_polls$sample_size/600) ^ 0.5
historical_senate_polls <- historical_senate_polls %>% relocate(sample_size_weights, .after = sample_size)


# Total weights per poll
historical_senate_polls$total_poll_weights <- historical_senate_polls$sample_size_weights*historical_senate_polls$pollster_weights*historical_senate_polls$recency_weights
historical_senate_polls <- historical_senate_polls %>% relocate(total_poll_weights, .after = poll_pct)

historical_senate_polls$weighted_poll <- historical_senate_polls$poll_pct*historical_senate_polls$total_poll_weights
historical_senate_polls <- historical_senate_polls %>% relocate(weighted_poll, .after = poll_pct)


# Calculate the weighted poll average per candidate

weighted_polls <-aggregate(historical_senate_polls$weighted_poll, list(historical_senate_polls$candidate, historical_senate_polls$party, historical_senate_polls$year, historical_senate_polls$state, historical_senate_polls$election_date), FUN=sum,  na.rm = TRUE) 
weight_totals <-aggregate(historical_senate_polls$total_poll_weights, list(historical_senate_polls$candidate, historical_senate_polls$party, historical_senate_polls$year, historical_senate_polls$state, historical_senate_polls$election_date), FUN=sum,  na.rm = TRUE) 
colnames(weighted_polls) <- c('candidate', 'party', 'year', 'state', 'election_date', 'weighted_poll')
colnames(weight_totals) <- c('candidate', 'party', 'year', 'state', 'election_date', 'weight_totals')

weighted_poll_av <- merge(weighted_polls, weight_totals, by=c('candidate', 'party', 'year', 'state', 'election_date'))

weighted_poll_av$weighted_average <- weighted_poll_av$weighted_poll / weighted_poll_av$weight_totals

dems_weighted_poll_av <- subset(weighted_poll_av, party == "DEM")

write.csv(weighted_poll_av,"/Users/madeleineroberts/rWorkingDirectory/weighted_poll_av.csv", row.names = FALSE)
write.csv(dems_weighted_poll_av,"/Users/madeleineroberts/rWorkingDirectory/dem_weighted_poll_av.csv", row.names = FALSE)



