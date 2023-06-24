#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# Setup and startup---------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

### About ###
# This script calculates by congressional district the results of every federal and statewide election in Oregon between 2016 and 2022.
# It achieves this by merging each cycle's precinct-level election results with two assignment matrices:
# One list pairs census blocks into congressional districts, while the other list pairs census blocks into voting precincts.
# When the lists are combined with the 2016-2022 results, previous elections' precincts can quickly be assigned into new districts.
# The 2016-2020 calculations were used in an article analyzing the electoral competitiveness of the map:
# https://www.insideelections.com/news/article/oregon-redistricting-mostly-good-news-for-democrats


### Data Sources ###
# U.S. Census Bureau: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=Blocks+%282020%29
# Oregon Secretary of State: https://sos.oregon.gov/elections/Pages/electionhistory-stats.aspx
# Voting and Election Science Team: https://dataverse.harvard.edu/dataverse/electionscience
# OpenElections: https://github.com/openelections/openelections-data-or


{ 
  library(tidyverse) # install.packages("tidyverse")
  library(dataverse) # install.packages("dataverse")
  library(readxl)    # install.packages("readxl")
  library(sf)        # install.packages("sf")
  library(modeest)   # install.packages("modeest")
}

options(scipen = 999)
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
pick_state = "OR"




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# create matrices that assign census blocks into precincts into districts --------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# read in assignment matrix matching census blocks into congressional districts 
key_cd_pct <- read.csv("block_assignment_files/match_block_district_all.csv", header=TRUE)
names(key_cd_pct) <- c("censusblock","cd")


# create function to clean assignment matrix matching precincts into congressional districts
getkeys <- function(x_dfresults) {
  
  key <- x_dfresults %>%
    mutate(countyprecinct = as.factor((paste(str_remove(as.character(COUNTY), "^0+"), str_remove(as.character((PRECINCT)), "^0+"))))) %>%
    select(censusblock = GEOID20, countyprecinct) %>%
    distinct %>%
    merge(key_cd_pct, by = "censusblock") %>%
    distinct(countyprecinct, cd)
  key %>%
    merge(as.data.frame(table(key$countyprecinct)) %>% rename(countyprecinct = Var1) %>% arrange(countyprecinct), by = "countyprecinct") %>%
    mutate(countyprecinct = as.factor(countyprecinct)) %>%
    select(countyprecinct, Freq, cd) %>%
    arrange(countyprecinct, Freq, cd)
  
}

# apply function to get precinct-cd assignment matrices for cycles of interest
key_2016 <- getkeys(read.csv("block_assignment_files/match_block_precinct_2016.csv", header=TRUE))

key_2018 <- getkeys(read.csv("block_assignment_files/match_block_precinct_2018.csv", header=TRUE))

key_2020 <- getkeys(read.csv("block_assignment_files/match_block_precinct_2020.csv", header=TRUE))


# get precinct-cd assignment matrix for 2022
key_2022 <- read.csv("https://raw.githubusercontent.com/openelections/openelections-data-or/master/2022/20221108__or__general__precinct.csv") %>%
  mutate(countyprecinct = paste(county, precinct),
         office = str_replace(office, "U.S.House", "U.S. House"),
         office = str_replace(office, "U.S. House District 2", "U.S. House")) %>%
  filter(office == "U.S. House") %>%
  select(countyprecinct, cd = district) %>% distinct %>%
  filter(cd != 0)




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# read in election results by year------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# 2016 - from VEST
# 2016: Read in precinct-level results from Dataverse and wrangle; Filter for each race
get_file("or_2016.zip", "doi:10.7910/DVN/NH5S2I") %>% writeBin("block_assignment_files/or_2016.zip")
unzip("block_assignment_files/or_2016.zip", exdir = "block_assignment_files/shapefile_2016")
unlink("block_assignment_files/or_2016.zip")

shapefile_2016 <- rgdal::readOGR(dsn = "block_assignment_files/shapefile_2016",
                                 layer = paste0(tolower(pick_state),"_2016"))
shapefile_2016_df <- shapefile_2016@data %>% mutate(countyprecinct = as.factor((paste(str_remove(as.character(COUNTY), "^0+"), str_remove(as.character((PRECINCT)), "^0+"),str_remove(as.character(NAME), "^0+")))))


# 2018 - from VEST
get_file("or_2018.zip", "doi:10.7910/DVN/UBKYRU") %>% writeBin("block_assignment_files/or_2018.zip")
unzip("block_assignment_files/or_2018.zip", exdir = "block_assignment_files/shapefile_2018")
unlink("block_assignment_files/or_2018.zip")

shapefile_2018 <- rgdal::readOGR(dsn = "block_assignment_files/shapefile_2018",
                                 layer = paste0(tolower(pick_state),"_2018"))
shapefile_2018_df <- shapefile_2018@data %>% mutate(countyprecinct = as.factor((paste(str_remove(as.character(COUNTY), "^0+"), str_remove(as.character((PRECINCT)), "^0+"),str_remove(as.character(NAME), "^0+")))))


# 2020 - from VEST
get_file("or_2020.zip", "doi:10.7910/DVN/K7760H") %>% writeBin("block_assignment_files/or_2020.zip")
unzip("block_assignment_files/or_2020.zip", exdir = "block_assignment_files/shapefile_2020")
unlink("block_assignment_files/or_2020.zip")

shapefile_2020 <- rgdal::readOGR(dsn = "block_assignment_files/shapefile_2020",
                                 layer = paste0(tolower(pick_state),"_2020"))
shapefile_2020_df <- shapefile_2020@data %>% mutate(countyprecinct = as.factor((paste(str_remove(as.character(COUNTY), "^0+"), str_remove(as.character((PRECINCT)), "^0+"))))) 


# 2022 - from OpenElections
results_2022 <- read.csv("https://raw.githubusercontent.com/openelections/openelections-data-or/master/2022/20221108__or__general__precinct.csv")




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# create functions to clean precinct data-----------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# function to clean results from VEST
cleanresults_vest <- function(x_dfresults, x_match, pick_gyyoff, pick_yyyy, pick_office){
  
  onlyoffice <- x_dfresults[ , grepl( pick_gyyoff , names(x_dfresults)) ] %>%
    mutate_if(is.character,as.numeric)
  
  merge(x_match,
        data.frame("countyprecinct" = x_dfresults %>% select(countyprecinct),
                   "votes_dem" = onlyoffice[ , grepl(paste0(pick_gyyoff,"D"), names(onlyoffice)) ],
                   "votes_rep" = onlyoffice[ , grepl(paste0(pick_gyyoff,"R"), names(onlyoffice)) ],
                   "votes_total" = rowSums(onlyoffice))) %>%
    rename(Precinct = countyprecinct) %>%
    mutate(year = pick_yyyy, office = pick_office)
  
}


# function to clean results from OpenElections
cleanresults_openelections <- function(x_dfresults, pick_yyyy, pick_office, pick_candidates){
  
  x_dfresults %>%
    filter(office ==  pick_office) %>%
    mutate(countyprecinct = paste(county, precinct),
           votes = as.numeric(votes)) %>%
    merge(key_2022, by = "countyprecinct") %>%
    select(countyprecinct, cd, office, candidate, votes) %>%
    replace(is.na(.), 0) %>% distinct %>%
    filter(candidate %in% pick_candidates) %>%
    mutate(recodevote = ifelse(candidate == pick_candidates[1], "votes_dem",
                               ifelse(candidate == pick_candidates[2], "votes_rep", "votes_other"))) %>%
    select(countyprecinct, cd, office, recodevote,votes) %>%
    group_by(countyprecinct, cd, office, recodevote) %>%
    summarize(votes = sum(votes)) %>%
    pivot_wider(names_from = "recodevote", values_from = "votes") %>%
    replace(is.na(.), 0) %>%
    mutate(Freq = 1,
           year = pick_yyyy,
           votes_total = votes_dem + votes_rep + votes_other) %>%
    select(Precinct = countyprecinct, Freq, cd, votes_dem, votes_rep, votes_total, year, office)
  
}




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# apply functions to clean precinct data for all years and offices----------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

by_precinct_all <- rbind(cleanresults_vest(shapefile_2016_df, key_2016, "G16PRE", 2016, "President"),
                         cleanresults_vest(shapefile_2016_df, key_2016, "G16USS", 2016, "U.S. Senate"),
                         cleanresults_vest(shapefile_2016_df, key_2016, "G16GOV", 2016, "Governor (Special)"),
                         cleanresults_vest(shapefile_2016_df, key_2016, "G16ATG", 2016, "Attorney General"),
                         cleanresults_vest(shapefile_2016_df, key_2016, "G16SOS", 2016, "Secretary of State"),
                         cleanresults_vest(shapefile_2016_df, key_2016, "G16TRE", 2016, "Treasurer"),
                         cleanresults_vest(shapefile_2018_df, key_2018, "G18GOV", 2018, "Governor"),
                         cleanresults_vest(shapefile_2020_df, key_2020, "G20PRE", 2020, "President"),
                         cleanresults_vest(shapefile_2020_df, key_2020, "G20USS", 2020, "U.S. Senate"),
                         cleanresults_vest(shapefile_2020_df, key_2020, "G20ATG", 2020, "Attorney General"),
                         cleanresults_vest(shapefile_2020_df, key_2020, "G20SOS", 2020, "Secretary of State"),
                         cleanresults_vest(shapefile_2020_df, key_2020, "G20TRE", 2020, "Treasurer"),
                         cleanresults_openelections(results_2022, 2022, "U.S. Senate", c("Ron Wyden", "Jo Rae Perkins", "Chris Henry", "Dan Pulju")),
                         cleanresults_openelections(results_2022, 2022, "Governor", c("Tina Kotek", "Christine Drazan", "Betsy Johnson", "Donice Noelle Smith", "R Leon Nobel"))
)





#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# create function that sums precinct data to get results by congressional district------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

cleanresults_bycd <- function (pick_cycle, pick_office) {
  by_precinct_all %>%
    filter(year == pick_cycle, office == pick_office) %>%
    mutate(votes_dem_adj = votes_dem / Freq,
           votes_rep_adj = votes_rep / Freq,
           votes_total_adj = votes_total / Freq) %>%
    group_by(cd) %>%
    summarize(cycle = pick_cycle,
              office = pick_office,
              votes_dem = sum(votes_dem_adj),
              votes_rep = sum(votes_rep_adj),
              votes_total = sum(votes_total_adj),
              percent_dem = votes_dem / votes_total,
              percent_rep = votes_rep / votes_total,
              dem_margin = percent_dem - percent_rep) 
  
}





#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# apply function to get results for all races by congressional district-----------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

by_cd_all <- rbind(cleanresults_bycd(2016, "President"),
                   cleanresults_bycd(2016, "U.S. Senate"),
                   cleanresults_bycd(2016, "Governor (Special)"),
                   cleanresults_bycd(2016, "Attorney General"),
                   cleanresults_bycd(2016, "Secretary of State"),
                   cleanresults_bycd(2016, "Treasurer"),
                   cleanresults_bycd(2018, "Governor"),
                   cleanresults_bycd(2020, "President"),
                   cleanresults_bycd(2020, "U.S. Senate"),
                   cleanresults_bycd(2020, "Attorney General"),
                   cleanresults_bycd(2020, "Secretary of State"),
                   cleanresults_bycd(2020, "Treasurer"),
                   cleanresults_bycd(2022, "U.S. Senate"),
                   cleanresults_bycd(2022, "Governor")
                   
)


#write.csv(by_cd_all, paste0(pick_state,"_by_cd_all.csv"))




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# viewing output------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# average of all results
by_cd_all %>%
  group_by(cd) %>%
  summarize(mean_dem_percent = mean(percent_dem),
            mean_rep_percent = mean(percent_rep),
            dem_margin = mean_dem_percent- mean_rep_percent) %>%
  print(n = Inf)


# just 2020 election results 
bidentrump2020 <- by_cd_all %>%
  filter(cycle == 2020,
         office == "President") %>% 
  group_by(cd) %>%
  summarize(mean_dem_percent = mean(percent_dem),
            mean_rep_percent = mean(percent_rep),
            dem_margin = mean_dem_percent- mean_rep_percent)
bidentrump2020

# number of Biden districts
bidentrump2020[bidentrump2020$dem_margin > 0, ] %>% nrow()

# number of Trump districts
bidentrump2020[bidentrump2020$dem_margin < 0, ] %>% nrow()

# number of districts within 10 points
bidentrump2020[bidentrump2020$dem_margin > -0.1 & bidentrump2020$dem_margin < 0.1, ] %>% nrow()