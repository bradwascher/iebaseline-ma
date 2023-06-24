# Calculating Statewide Election Results by Congressional District 

### About
This script calculates by congressional district the results of every federal and statewide election in Oregon between 2016 and 2022.

It achieves this by merging each cycle's precinct-level election results with two assignment matrices: One list pairs census blocks into congressional districts, while the other list pairs census blocks into voting precincts. When the lists are combined with the 2016-2022 results, previous elections' precincts can quickly be assigned into new districts.

These calculations were used in an [article](https://www.insideelections.com/news/article/oregon-redistricting-mostly-good-news-for-democrats) analyzing the electoral competitiveness of Oregon's congressional map.

### Data Sources
[U.S. Census Bureau](https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=Blocks+%282020%29)

[Oregon Secretary of State](https://sos.oregon.gov/elections/Pages/electionhistory-stats.aspx)

[Voting and Election Science Team](https://dataverse.harvard.edu/dataverse/electionscience)

[OpenElections](https://github.com/openelections/openelections-data-or)
