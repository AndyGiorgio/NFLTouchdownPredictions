############################
# Andy Giorgio NFL Project #
# Determining Variables    #
############################

# Install required packages (you may already have these installed)
install.packages("rvest")
install.packages("dplyr")
library(rvest)
library(dplyr)

# Set the URL for FantasyPros snap count analysis (snap counts for 2023)
url <- "https://www.fantasypros.com/nfl/reports/snap-count-analysis/?year=2023"
webpage <- read_html(url)
snap_count_table <- webpage %>%
    html_table(fill = TRUE) %>%
    .[[1]]

# Preview the snap count data
head(snap_count_table)

# Standardize column names
names(snap_count_table) <- make.names(names(snap_count_table))

# Set the URL for passing stats from Pro Football Reference (2023)
urlPass <- "https://www.pro-football-reference.com/years/2023/passing.htm"
webpage1 <- read_html(urlPass)
pass_table <- webpage1 %>%
    html_table(fill = TRUE) %>%
    .[[1]]

# Preview passing stats data
head(pass_table)

# Set the URL for rushing stats from Pro Football Reference (2023)
urlRush <- "https://www.pro-football-reference.com/years/2023/rushing.htm"
webpage2 <- read_html(urlRush)
rush_table <- webpage2 %>%
    html_table(fill = TRUE) %>%
    .[[1]]

# Clean up rushing stats table
colnames(rush_table) <- rush_table[1,]
rush_table <- rush_table[-1,]
rush_table <- rush_table %>%
    rename(rushTD = TD,
           rushYds = Yds,
           rushYG = 'Y/G')

# Filter rushing stats based on attempts (more than 10)
rush_td <- rush_table %>%
    dplyr::select(Player, rushTD, `Y/A`, rushYG, Att) %>%
    filter(Att > 10)

# Set the URL for receiving stats from Pro Football Reference (2023)
urlRec <- "https://www.pro-football-reference.com/years/2023/receiving.htm"
webpage3 <- read_html(urlRec)
rec_table1 <- webpage3 %>%
    html_table(fill = TRUE) %>%
    .[[1]]

# Clean up receiving stats table and filter for targets greater than 5
rec_table1 <- rec_table1 %>%
    rename(recTD = TD,
           recYds = Yds,
           recYG = 'Y/G') %>%
    filter(Tgt > 5)

colnames(rec_table1) <- rec_table1[1,]
rec_table1 <- rec_table1[-1,]

# Preview receiving stats table
head(rec_table1)

# Select relevant columns for receiving touchdowns
rec_td <- rec_table1 %>%
    dplyr::select(Player, recTD, recYG)

# Load necessary libraries for statistical analysis
library(MASS)

# Set the URL for wide receiver stats from FantasyPros (2023)
wr_url <- "https://www.fantasypros.com/nfl/advanced-stats-wr.php?year=2023"
recweb2 <- read_html(wr_url)
rec_table <- recweb2 %>%
    html_table(fill = TRUE) %>%
    .[[1]]

# Preview wide receiver stats data
head(rec_table)

# Clean and standardize wide receiver stats table
colnames(rec_table) <- rec_table[1,]
rec_table <- rec_table[-1,]
rec_table <- rec_table %>%
    mutate_all(~ gsub(",", "", .)) %>%
    mutate_all(~ gsub("%", "", .)) %>%
    mutate(Player = gsub("\\s*\\([^\\)]+\\)", "", Player))

# Convert columns to numeric and clean up data
rec_table <- rec_table %>%
    mutate(rec = as.numeric(REC),
           yds = as.numeric(YDS),
           `Y/R` = as.numeric(`Y/R`),
           tgt = as.numeric(TGT),
           tgtShare = as.numeric(`% TM`),
           tgtRzn = as.numeric(`RZ TGT`),
           g = as.numeric(G),
           `Y/G` = yds / g)

# Clean and filter player names for rec_td
rec_td <- rec_td %>%
    mutate(Player = gsub("\\*|\\+|\\s*\\([^\\)]+\\)", "", Player),
           recTD = as.numeric(recTD),
           recYG = as.numeric(recYG)) %>%
    filter(Player != "Player")

# Clean and filter player names for rush_td
rush_td <- rush_td %>%
    mutate(Player = gsub("\\*|\\+|\\s*\\([^\\)]+\\)", "", Player),
           rushTD = as.numeric(rushTD),
           rushYG = as.numeric(rushYG),
           Att = as.numeric(Att)) %>%
    filter(Player != "Player")

# Preview cleaned data
head(rec_td)

# Merge receiving stats with touchdown data
rec_table <- inner_join(rec_table, rec_td, by = "Player")

# Visualize the relationships between receiving variables
pairs(test_rec, main = "Scatterplot Matrix of Receiving Data")

# Create test dataset for receiving touchdown modeling
test_rec <- rec_table %>%
    dplyr::select(recTD, tgt, tgtShare, tgtRzn, recYG)

# Fit a null model and a full model to the data
empty_model1 <- lm(recTD ~ 1, data = test_rec)  # Null model with no predictors
full_model1 <- lm(recTD ~ ., data = test_rec)    # Full model with all predictors

# Perform stepwise model selection
selected_model <- stepAIC(empty_model1, scope = list(lower = empty_model1, upper = full_model1), direction = "forward")

# Show model summary and VIF for multicollinearity
summary(selected_model)
vif(selected_model)

# Clean and standardize rushing stats table
rush_table <- rush_table %>%
    mutate_all(~ gsub(",", "", .)) %>%
    mutate_all(~ gsub("%", "", .)) %>%
    mutate(Player = gsub("\\s*\\([^\\)]+\\)", "", Player))

rush_table <- rush_table %>%
    mutate(Att = as.numeric(Att),
           rushYds = as.numeric(rushYds),
           `Y/A` = as.numeric(`Y/A`),
           rushYG = as.numeric(rushYG),
           rushTD = as.numeric(rushTD),
           g = as.numeric(G))

# Visualize the relationships between rushing variables
pairs(test_rush, main = "Scatterplot Matrix of Rushing Data")

# Create test dataset for rushing touchdown modeling
test_rush <- rush_table %>%
    dplyr::select(rushTD, `Y/A`, rushYG, Att) %>%
    filter(`Y/A` > 0,
           Att > 10)

# Fit a null model and a full model to the data
empty_model1 <- lm(rushTD ~ 1, data = test_rush)  # Null model with no predictors
full_model1 <- lm(rushTD ~ ., data = test_rush)    # Full model with all predictors

# Perform stepwise model selection
selected_model <- stepAIC(empty_model1, scope = list(lower = empty_model1, upper = full_model1), direction = "both")

# Show model summary and VIF
summary(selected_model)
vif(selected_model)

# Fit a Poisson model for count data
empty_model_poisson <- glm(rushTD ~ 1, data = test_rush, family = "poisson")  # Null model
full_model_poisson <- glm(rushTD ~ ., data = test_rush, family = "poisson")  # Full model

# Perform stepwise selection using Poisson model
selected_model_poisson <- stepAIC(empty_model_poisson, 
                                  scope = list(lower = empty_model_poisson, 
                                               upper = full_model_poisson), 
                                  direction = "both")

# View the summary of the final selected Poisson model
summary(selected_model_poisson)

# Set URL for redzone rushing stats and clean the data
urlRush <- "https://www.pro-football-reference.com/years/2023/redzone-rushing.htm"
pageRush <- read_html(urlRush)
tablesRush <- pageRush %>% html_table()
combined_tableRush <- bind_rows(tablesRush)

# Clean and rename redzone rushing data
colnames(combined_tableRush) <- combined_tableRush[1,]
combined_tableRush <- combined_tableRush[-1,]
names(combined_tableRush)[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)] <- c("Att20", "Yds20", "TD20", "RushP20", "Att10", "Yds10", "TD10", "RushP10", "Att5", "Yds5", "TD5", "RushP5")

# Further cleaning for redzone rushing data
combined_tableRush <- combined_tableRush %>%
    mutate_all(~ gsub(",", "", .)) %>% 
    mutate_all(~ gsub("%", "", .)) %>%
    mutate(Player = gsub("\\s*\\([^\\)]+\\)", "", Player))

# Filter and join with rush_td
combined_tableRush <- combined_tableRush %>%
    filter(Tm != "2TM") %>%
    inner_join(rush_td, by = "Player")

# Prepare test data for redzone rushing analysis
test_combined <- combined_tableRush %>%
    dplyr::select(16,4,11,12,18) %>%
    mutate_all(as.numeric)

# Visualize rushing touchdowns distribution
ggplot(test_combined, aes(x = rushTD)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Distribution of Touchdowns", x = "TD", y = "Count") +
    theme_minimal()

# Fit models to the redzone rushing data
empty_model <- lm(rushTD ~ 1, data = test_combined)  # Null model with no predictors
full_model <- lm(rushTD ~ ., data = test_combined)    # Full model with all predictors
summary(full_model)
