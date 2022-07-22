# Natality insights in the middle of pandemic

Using a database with more than 80,000 tuples, we generate a systematic sample to work on with a size of 10% of the total population. Was analyzed with R software but can be perfectly analyzed with Python, you can find the analysis with it [here](https://gitlab.com/StanDoge_/natality-insights-2020)

## Data dict 📃
Here are eight categorical variables, seven numerical variables where three of them aren't significant for analysis, only works as sample id or expansion factor. 
| id | muestra | fex | local_parto | mes_nac | dep_nac | sex_nac | peso_nac | talla_nac | clase_parto | madre_edad | semana_gestacion | madre_dept | madre_area
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Identifier - integer | Systematic sample - integer | Expansion factor - integer | Place of birth - hospital | Birth month - month | Birth department - Salvador's department | Born's sex - sex | Born's weight - pounds | Born's size - centimeters | Birth kind  | Mother's age - years | Gestation week - weeks | Mother's department - Salvador's department | Mother's residence - Rural/Urban |
| Integer | Integer | Integer | Hospital's name | Month's name | Salvador's department | Sex | # | Cm | Normal/Attended | Years | Weeks | Salvador's department | Rural/Urban |

## About the repo ⚡
This was a college team homework for Statistic & Probability signature. Incluides from descriptive statistic, probability distribution to inference statistic.
Enjoy ~ 🎍
