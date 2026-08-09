Football Ranking Shiny App

Files:
- app.R          Main Shiny application
- players.csv    Current baseline player data
- matches.csv    Future match history (starts empty)

Required R packages:
install.packages(c("shiny","DT","ggplot2","dplyr","readr","scales"))

Run:
1. Put all three files in the same folder.
2. Open app.R in RStudio.
3. Click "Run App".

Features:
- Live ranking
- +11 participation bonus once per player per Cup ID
- Stage-based win/loss rating values
- Opponent-strength adjustment
- Automatic ranking updates
- Click a ranking row to open Player Profile
- Player rating-history line chart
- Win/loss/performance chart
- Full player match history
- CSV persistence
