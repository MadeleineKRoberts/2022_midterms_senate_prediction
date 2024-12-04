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
senate_results <- read_csv("1976-2020-senate.csv")
training_data <- read_csv("training_data_final.csv")
# house_senate_past <- read_csv("gov_sen_house_totals.csv")

# Rearrange columns 
senate_results <- senate_results %>% relocate(candidate, .after = state_po)
senate_results <- senate_results %>% relocate(party_simplified, .after = candidate)
senate_results <- senate_results %>% relocate(candidatevotes, .after = party_simplified)
senate_results <- senate_results %>% relocate(totalvotes, .after = candidatevotes)

# Remove non-Democratic and non-Republican candidated from data set
senate_two_party_results <- subset(senate_results, party_simplified != "OTHER" & party_simplified !="LIBERTARIAN")

# Calculate two-party total vote share
total_dr_votes <- senate_two_party_results %>%
  group_by(year, state, special) %>%
  summarise_at(vars(candidatevotes), list(two_party_vote_total = sum))

senate_two_party_results<- merge(senate_two_party_results, total_dr_votes, by = c("year", "state"))
senate_two_party_results <- senate_two_party_results %>% relocate(two_party_vote_total, .after = totalvotes)

# Calculate two party vote percentage
senate_two_party_results$two_party_vote_percentage <- (senate_two_party_results$candidatevotes / senate_two_party_results$two_party_vote_total)*100
senate_two_party_results <- senate_two_party_results %>% relocate(two_party_vote_percentage, .after = two_party_vote_total)


# POLITICAL FUNDEMENTALS
# Add dummy variable for if the president is a Democrat (1) or Republican (0)
pres_party <- read_csv("pres_party.csv")
pres_party$incmbnt_pres_party <- ifelse(pres_party$party == "D", 1 , ifelse(pres_party$party == "R", 0, -1 ))
colnames(pres_party)[colnames(pres_party) == "years"] ="year"
pres_party <- pres_party[c(1,4)]

senate_two_party_results<- merge(senate_two_party_results, pres_party, by = "year")
senate_two_party_results <- senate_two_party_results %>% relocate(incmbnt_pres_party, .after = two_party_vote_percentage)


# Presidential approval rating
# Late October president approval ratings compiled from
# https://news.gallup.com/interactives/185273/presidential-job-approval-center.aspx
pres_approval <- read_csv("pres_approval.csv")
pres_approval <- pres_approval[c(1,3)]

senate_two_party_results<- merge(senate_two_party_results, pres_approval, by = "year")
senate_two_party_results <- senate_two_party_results %>% relocate(pres_approval_late_oct, .after = incmbnt_pres_party)


# MERGE POLLS AND FUNDEMENTALS
agg = aggregate(senate_two_party_results,
                by = list(senate_two_party_results$year),
                FUN = mean)

agg = subset(agg, select = c("year","incmbnt_pres_party","pres_approval_late_oct"))

training_data <- merge(training_data, agg, by="year")

# Model 
trained_regression <- lm(formula = obs_vote_pct ~ weighted_poll_average + past_sen_dem_vote_share + past_sen_dem_vote_share_x2, data = training_data)

# Predict 2022 senate 
senate_2022 <- read_csv("2022_SENATE_final.csv")
colnames(senate_2022)[colnames(senate_2022) == "inflation_sept"] ="inflation"
colnames(senate_2022)[colnames(senate_2022) == "inc_pres_party"] ="incmbnt_pres_party"
colnames(senate_2022)[colnames(senate_2022) == "weighted_poll_av"] ="weighted_poll_average"
senate_2022$predicted_dem_vote_share = predict(trained_regression, senate_2022)

summary(trained_regression)



