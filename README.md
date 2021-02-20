# nyc-covid-ridership-decrease

nyc-covid-ridership-decrease.R produces two outputs:
1) ts_combi_sf.Rda: turnstile entries and exits per station per month; comparison between 2019 and 2020.
2) ny_demographics.Rda: Demographic information per census tract including: income, race, primary mode of transportation to work 

app.R takes those two outputs, and knits them together into an interactive shiny app which features a time slider. Additionally, it shows the covid case rate per day in NYC.

netAnalysis.R conducts analysis on the NYC subway network and produces various centrality measures, and plots them geographically. The conversion of the gtfs data to igraph format was done using the gtfs_to_igraph.R method from https://github.com/rafapereirabr/gtfs_to_igraph
