# neighbourhood_info

Taking data from PHE fingertips local health to combine into Bolton integrated care neighbourhood level info
https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133184/pat/402/par/E08000001/ati/3/are/E02000984/cid/4/tbm/1

app is here: 
https://shanwilkinson2.shinyapps.io/bolton_neighbourhoods/

# to fix
* Turton - some indicators not displaying properly on chart
* Life expectancy not displaying properly on chart as m/f - if not persons, add in gender into the indicator name
* England - some indicators have no max/ min value - check if need to add na.rm = TRUE when calculating
