# FootballAnalysis

Based on the prior win loss data for 20 teams over last 8 seasons we try to estimate the most efficient model,

Features inclueded are
1. Half time score
2. Number of Yellow Cards
3. Home team
4. Away team
5. match refree
6. shots on target
7. home/away game for liverpool
8. total shots
9. number of fouls
10. number of challenges

for each team the features are present in the data. Total 22 features.

The classification tries to predict the categorical labels 1- Win, 2-Draw, 3-Loss

The preprocessing and the EDA data is done in non parametric as well as parametric assumptions

1. Linear Regression to understand dependencies
2. Random forest model for classification with grid search parameter tuning
3. Multiclass SVM with scaled features
4. XGboost with hyperparameter tuned and Cross validation implementation 
5. KNN based classification
