# NFLTouchdownPredictions
Predicting NFL touchdown scorers using logistic regression and player data from the season

## Project Overview

This project predicts NFL touchdown scorers using logistic regression. It combines seasonal and red zone stats for receiving and rushing data to estimate scoring probabilities. It then compares implied odds of the model with Vegas odds and bets on players with value. Weeks 13-18 of the 2024 NFL season are tracked in spreadsheets.

## Files in this Repository

- `VariableSelection.R`: Code for selecting which variables are most significant for predicting touchdowns in the NFL.
- `TdPredictionProbabilities.R`: Code for predicting player's scoring probability based on stats and converting into expected betting odds.
- `NFL Touchdown Odds-RecTracker.csv`: Google Sheet export of tracked bets on running backs the model found value in.
- `NFL Touchdown Odds-RushTracker.csv`: Google Sheet export of tracked bets on wide receivers the model found value in.
- `NFLTouchdownPredictionsReport.pdf` : Written report documenting the entire project and methodology as well as results and takeaways.

## Data Sources

- [Pro Football Reference](https://www.pro-football-reference.com/): Used for player stats and red zone data.

## How It Works

1. Scrapes and cleans receiving and rushing stats.
2. Builds logistic regression models for each position.
3. Calculates TD probabilities and implied betting odds.
4. Combines predictions for dual-threat players.
5. Players with expected odds less than Vegas odds are bet on and tracked.

## Future Improvements

- Incorporate team defense vs. position.
- Add week-by-week data for better feature engineering.

## Author

**Andy Giorgio**  
Villanova University  
Statistics & Economics | Minor in CS  
