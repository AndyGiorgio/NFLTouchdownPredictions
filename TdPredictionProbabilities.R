# Andy Giorgio NFL Project
# Data Preprocessing, Model Building, Prediction, and Betting Strategy

# Import Libraries
library(rvest)
library(dplyr)
library(MASS)

# Function to load and clean receiving data
load_receiving_data <- function(url) {
    webpage <- read_html(url)
    rec_table <- webpage %>%
        html_table(fill = TRUE) %>%
        .[[1]]
    
    colnames(rec_table) <- rec_table[1,]
    rec_table <- rec_table[-1,]
    
    rec_table <- rec_table %>%
        rename(recTD = TD,
               recYds = Yds,
               recYG = 'Y/G') %>%
        dplyr::select(Player, G, recYds, recTD) %>%
        mutate(G = as.numeric(G),
               recYds = as.numeric(recYds),
               recTD = as.numeric(recTD)) %>%
        filter(G > 7)
    
    return(rec_table)
}

# Function to load and clean redzone receiving data
load_redzone_receiving_data <- function(url) {
    webpage <- read_html(url)
    redzone_table <- webpage %>%
        html_table(fill = TRUE) %>%
        .[[1]]
    
    colnames(redzone_table) <- redzone_table[1,]
    redzone_table <- redzone_table[-1,]
    
    # Renaming and converting columns
    colnames(redzone_table)[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)] <- c("Tgt20", "Rec20", "CtchP20", "Yds20", "Td20", 
                                                                             "TgtP20", "Tgt10", "Rec10", "CtchP10", "Yds10", 
                                                                             "Td10", "TgtP10")
    
    return(redzone_table)
}

# Function to combine receiving data
combine_receiving_data <- function(rec_table, redzone_table) {
    combined_data <- inner_join(redzone_table, rec_table, by = "Player") %>%
        mutate_all(~ gsub(",", "", .)) %>% 
        mutate_all(~ gsub("%", "", .)) %>%
        mutate(Tgt20 = as.numeric(Tgt20),
               TgtP20 = as.numeric(TgtP20),
               Tgt10 = as.numeric(Tgt10),
               TgtP10 = as.numeric(TgtP10),
               recYds = as.numeric(recYds),
               G = as.numeric(G),
               recTD = as.numeric(recTD),
               Rec10 = as.numeric(Rec10),
               Tgt20G = Tgt20 / G,
               Rec10G = Rec10 / G,
               recTDG = recTD / G) %>%
        mutate(recYG = recYds / G,
               recTDG = ifelse(recTDG > 1, 1, recTDG))
    
    return(combined_data)
}

# Function to load and clean rushing data
load_rushing_data <- function(url) {
    webpage <- read_html(url)
    rush_table <- webpage %>%
        html_table(fill = TRUE) %>%
        .[[1]]
    
    colnames(rush_table) <- rush_table[1,]
    rush_table <- rush_table[-1,]
    
    rush_table <- rush_table %>%
        rename(rushTD = TD,
               rushYds = Yds,
               rushYG = 'Y/G') %>%
        dplyr::select(Player, G, rushYds, rushTD, `Y/A`) %>%
        mutate(G = as.numeric(G),
               rushYds = as.numeric(rushYds),
               rushTD = as.numeric(rushTD),
               `Y/A` = as.numeric(`Y/A`)) %>%
        filter(G > 7)
    
    return(rush_table)
}

# Function to load and clean redzone rushing data
load_redzone_rushing_data <- function(url) {
    webpage <- read_html(url)
    redzone_rush_table <- webpage %>%
        html_table(fill = TRUE) %>%
        .[[1]]
    
    colnames(redzone_rush_table) <- redzone_rush_table[1,]
    redzone_rush_table <- redzone_rush_table[-1,]
    
    # Renaming and converting columns
    colnames(redzone_rush_table)[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)] <- c("Att20", "Yds20", "TD20", "RushP20", 
                                                                                  "Att10", "Yds10", "TD10", "RushP10", "Att5", 
                                                                                  "Yds5", "TD5", "RushP5")
    
    return(redzone_rush_table)
}

# Combine rushing data
combine_rushing_data <- function(rush_table, redzone_rush_table) {
    combined_rush_data <- inner_join(redzone_rush_table, rush_table, by = "Player") %>%
        mutate_all(~ gsub(",", "", .)) %>%
        mutate_all(~ gsub("%", "", .)) %>%
        mutate(Att20 = as.numeric(Att20),
               Yds20 = as.numeric(Yds20),
               RushP20 = as.numeric(RushP20),
               Att10 = as.numeric(Att10),
               Yds10 = as.numeric(Yds10),
               RushP10 = as.numeric(RushP10),
               Att5 = as.numeric(Att5),
               Yds5 = as.numeric(Yds5),
               RushP5 = as.numeric(RushP5),
               G = as.numeric(G),
               rushTD = as.numeric(rushTD),
               `Y/A` = as.numeric(`Y/A`),
               Yds20G = Yds20 / G,
               Yds10G = Yds10 / G,
               Yds5G = Yds5 / G,
               Att5G = Att5 / G,
               rushTDG = rushTD / G) %>%
        mutate(rushTDG = ifelse(rushTDG > 1, 1, rushTDG))
    
    return(combined_rush_data)
}

# Fit logistic regression models for touchdown predictions
fit_model <- function(data, formula) {
    model <- glm(formula, data = data, family = binomial)
    return(model)
}

# Make predictions based on model
make_predictions <- function(model, data) {
    data$predicted_prob <- predict(model, newdata = data, type = "response")
    return(data)
}

# Calculate odds based on predicted probabilities
calculate_odds <- function(data) {
    data$Odds <- if_else(data$predicted_prob < 0.5,
                         (100 / data$predicted_prob) - 100,
                         100 * (-(data$predicted_prob) / (1 - data$predicted_prob)))
    return(data)
}

# Combine rushing and receiving data, and filter for betting decisions
combine_and_filter_for_betting <- function(rec_pred, rush_pred) {
    filtered_rec_pred <- rec_pred %>%
        filter(Player %in% rush_pred$Player) %>%
        dplyr::select(Player, Tm, predicted_prob)
    
    rush_pred %>%
        dplyr::select(Player, Tm, predicted_prob)
    
    combined_pred <- bind_rows(filtered_rec_pred, rush_pred) %>%
        group_by(Player, Tm) %>%
        summarise(predicted_prob = sum(predicted_prob) - prod(predicted_prob), .groups = "drop") %>%
        mutate(Odds = if_else(predicted_prob < 0.5,
                              (100 / predicted_prob) - 100,
                              100 * (-(predicted_prob) / (1 - predicted_prob))))
    
    return(combined_pred)
}

# Main Execution
urlRec <- "https://www.pro-football-reference.com/years/2024/receiving.htm"
urlRush <- "https://www.pro-football-reference.com/years/2024/rushing.htm"
urlRedRec <- "https://www.pro-football-reference.com/years/2024/redzone-receiving.htm"
urlRedRush <- "https://www.pro-football-reference.com/years/2024/redzone-rushing.htm"

# Load data
rec_table <- load_receiving_data(urlRec)
redzone_rec_table <- load_redzone_receiving_data(urlRedRec)
rush_table <- load_rushing_data(urlRush)
redzone_rush_table <- load_redzone_rushing_data(urlRedRush)

# Combine data
rec_combined <- combine_receiving_data(rec_table, redzone_rec_table)
rush_combined <- combine_rushing_data(rush_table, redzone_rush_table)

# Fit models
rec_model <- fit_model(rec_combined, recTDG ~ Tgt20G + recYG + Rec10G)
rush_model <- fit_model(rush_combined, rushTDG ~ Yds20G + Yds10G + Att5G)

# Make predictions
rec_pred <- make_predictions(rec_model, rec_combined)
rush_pred <- make_predictions(rush_model, rush_combined)

# Calculate betting odds
rec_pred_odds <- calculate_odds(rec_pred)
rush_pred_odds <- calculate_odds(rush_pred)

# Combine data and filter for betting decisions
final_betting_predictions <- combine_and_filter_for_betting(rec_pred_odds, rush_pred_odds)

# View final output
final_betting_predictions
