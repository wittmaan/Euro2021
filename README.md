## UEFA Euro 2021 Prediction

According to this [approach](https://econpapers.repec.org/paper/innwpaper/2014-17.htm)
here is my attempt to do some predictions for the UEFA Euro 2021:

1. Fetch quoted odds from https://www.oddschecker.com/football/world-cup/winner using fetchData.R
2. Transform them to odds and calculate aggregated probabilities using calcProbabilities.R
3. Run the simulations via the given java program using probabilites2021xxxx.csv as input.
4. Copy all csv-files to the app folder and view the results in the shiny app.

This app is also running on shinyapps.io: https://wittmaan.shinyapps.io/FIFA_WORLD_CUP_2018_Prediction/
