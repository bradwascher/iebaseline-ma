#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# | setup and startup-------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

#### #### #### #### #### #### #### ###
# — about-----------------------------
#### #### #### #### #### #### #### ###

# This script calculates by congressional district the results of every federal and statewide election in Massachusetts between 2016 and 2022.
# The 2016-2020 calculations were used in an article analyzing the electoral competitiveness of the map:
# insideelections.com/news/article/massachusetts-redistricting-a-common-story-in-the-commonwealth


# data and sources:
# U.S. Census Bureau: census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=Blocks+%282020%29
# Massachusetts Secretary of the Commonwealth: electionstats.state.ma.us
# Voting and Election Science Team: dataverse.harvard.edu/dataverse/electionscience
# MIT Election Lab: github.com/MEDSL/2022-elections-official/blob/main/individual_states/2022-ma-local-precinct-general.zip



# libraries 
{ 
  library(tidyverse) # install.packages("tidyverse")
  library(dataverse) # install.packages("dataverse")
  library(sf)        # install.packages("sf")
  library(modeest)   # install.packages("modeest")
}


# useful settings
pick_state = "MA"

options(scipen = 999) # ensures precinct names don't get converted to scientific notation 
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu") # VEST files are hosted on the Dataverse




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# | create functions--------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

#### #### #### #### #### #### #### ###
# — functions to download results-----
#### #### #### #### #### #### #### ###

# function to download results from VEST via Dataverse (2016, 2018, 2020)
getresults_VEST <- function(cycle, doi, varname_county, varname_precinct) {
  get_file(paste0(tolower(pick_state),"_",cycle,".zip"), paste0("doi:",doi)) %>% writeBin(paste0(tolower(pick_state),"_",cycle,".zip"))
  unzip(paste0(tolower(pick_state),"_",cycle,".zip"), exdir = paste0("shapefiles/shapefile_",cycle))
  unlink(paste0(tolower(pick_state),"_",cycle,".zip"))
  
  shapefile <- rgdal::readOGR(dsn = paste0("shapefiles/shapefile_",cycle),
                              layer = paste0(tolower(pick_state),"_",cycle))
  results <- shapefile@data %>% mutate(county = str_remove({{varname_county}}, "^0+"),
                                       precinct = str_remove({{varname_precinct}}, "^0+"),
                                       countyprecinct = paste(county, precinct))
}


#### #### #### #### #### #### #### ###
#— functions to create match keys-----
#### #### #### #### #### #### #### ###

# function to create block/precincts keys from VEST (2016, 2018, 2020)
getkey_VEST <- function(x_blockassignments) {
  key <- x_blockassignments %>%
    mutate(countyprecinct = as.factor((paste(str_remove(as.character(COUNTY), "^0+"), str_remove(as.character((PRECINCT)), "^0+"))))) %>%
    select(censusblock = GEOID20, countyprecinct) %>%
    distinct %>%
    merge(read.csv("block_assignment_files/match_block_district_all.csv", header=TRUE) %>% rename(censusblock = 1, cd = 2), by = "censusblock") %>%
    distinct(countyprecinct, cd)
  key %>%
    merge(as.data.frame(table(key$countyprecinct)) %>% rename(countyprecinct = Var1) %>% arrange(countyprecinct), by = "countyprecinct") %>%
    mutate(countyprecinct = as.factor(countyprecinct)) %>%
    select(countyprecinct, Freq, cd) %>%
    arrange(countyprecinct, Freq, cd)
}

# function to create precinct/cd keys from MIT (2022)
getkey_MIT2022 <- function(x_results) {
  # clean results by house
  results_22house <- x_results %>%
    filter(office ==  "US HOUSE" | office == "REPRESENTATIVE IN CONGRESS",
           precinct != "COUNTY TOTAL") %>%
    mutate(countyprecinct = paste(county_name, precinct),
           votes = str_replace(votes, "\\*", "0"),
           votes = as.numeric(votes),
           party_simplified = ifelse(party_detailed == "DEMOCRATIC-FARMER-LABOR" & party_simplified == "OTHER", "DEMOCRAT", party_simplified),
           party_detailed = str_replace(party_detailed, "DEMOCRATIC-FARMER-LABOR", "DEMOCRAT")) %>% 
    select(countyprecinct, cd = district, office, party_simplified, candidate, votes) %>%
    mutate(party_recode = ifelse(party_simplified %in% "", candidate, party_simplified),
           party = ifelse(party_recode == "DEMOCRAT", "votes_dem",
                          ifelse(party_recode == "REPUBLICAN", "votes_rep", "votes_other"))) %>%
    replace(is.na(.), "votes_other") %>%
    select(countyprecinct, cd, office, party, votes) %>%
    group_by(countyprecinct, cd, office, party) %>%
    summarize(votes = sum(votes)) %>%
    pivot_wider(names_from = "party", values_from = "votes") %>%
    replace(is.na(.), 0) %>%
    drop_na(cd) %>%
    mutate(cd = as.numeric(cd),
           cycle = 2022,
           office = "US HOUSE",
           votes_total =  across(starts_with("votes_")) %>% rowSums) %>%
    select(countyprecinct, cd, votes_dem, votes_rep, votes_total, cycle, office)
  # see which precincts were split between districts (and in which proportions they were split)
  precincts_split <- results_22house %>%
    merge(as.data.frame(table(results_22house$countyprecinct)) %>% rename(countyprecinct = Var1) %>% arrange(countyprecinct), by = "countyprecinct") %>%
    filter(Freq > 1) %>% select(-Freq)
  precincts_split <- precincts_split %>%
    merge(precincts_split %>%
            group_by(countyprecinct) %>%
            summarize(sum_dem = sum(votes_dem),
                      sum_rep = sum(votes_rep),
                      sum_total = sum(votes_total))) %>%
    mutate(cd = as.numeric(cd),
           split = 1,
           precinctshare_dem = votes_dem / sum_dem,
           precinctshare_rep = votes_rep / sum_rep,
           precinctshare_total = votes_total / sum_total) %>%
    replace(is.na(.), 0) %>%
    select(countyprecinct, cd, split, precinctshare_dem, precinctshare_rep, precinctshare_total)
  # identify precincts that were split with errors
  precincts_problematic <- precincts_split %>%
    group_by(countyprecinct) %>%
    summarize(total_dem = sum(precinctshare_dem),
              total_rep = sum(precinctshare_rep),
              total_total = sum(precinctshare_total)) %>%
    filter(total_dem < 1 | total_rep < 1 | total_total < 1) %>%
    mutate(problematic = ifelse(total_total < 1, "ISSUE TOTAL",
                         ifelse(total_dem < 1 & total_total < 1, "ISSUE TOTAL",       
                         ifelse(total_rep < 1 & total_total < 1, "ISSUE TOTAL",       
                         ifelse(total_rep < 1 & total_rep < 1, "ISSUE TOTAL",
                         ifelse(total_dem < 1, "ISSUE D",
                         ifelse(total_rep < 1, "ISSUE R", NA))))))) %>%
    select(countyprecinct, problematic) %>%
    merge(precincts_split) %>%
    mutate(precinctshare_dem = ifelse(problematic == "ISSUE D", precinctshare_rep, ifelse(problematic == "ISSUE TOTAL", 0.5, precinctshare_dem)),
           precinctshare_rep = ifelse(problematic == "ISSUE R", precinctshare_dem, ifelse(problematic == "ISSUE TOTAL", 0.5, precinctshare_rep)),
           precinctshare_total = ifelse(problematic == "ISSUE TOTAL", 0.5, precinctshare_total)) %>%
    select(-problematic)
  # add together to make a key
  rbind(
    x_results %>%
      mutate(countyprecinct = paste(county_name, precinct)) %>%
      filter(office == "US HOUSE" | office == "REPRESENTATIVE IN CONGRESS", precinct != "COUNTY TOTAL") %>%
      select(countyprecinct, cd = district) %>% distinct %>%
      filter(!(countyprecinct %in% precincts_split$countyprecinct)) %>%
      mutate(cd = as.numeric(cd),
             split = 0,
             precinctshare_dem = 1, 
             precinctshare_rep = 1,
             precinctshare_total = 1),
    precincts_split %>% filter(!(countyprecinct %in% precincts_problematic$countyprecinct)),
    precincts_problematic) %>%
    arrange(countyprecinct)
}


#### #### #### #### #### #### #### ###
#— functions to tidy precinct data----
#### #### #### #### #### #### #### ###

# function to clean precinct data from VEST (2016, 2018, 2020)
cleanresults_VEST <- function(x_results, x_key, varname_gyyoff, varname_county, varname_precinct, pick_yyyy, pick_office){
  # filter results for contest of interest
  onlyoffice <- x_results[ , grepl(varname_gyyoff, names(x_results)) ] %>%
    mutate_if(is.character,as.numeric)
  # reshape results and merge with key
  data.frame("county" = x_results %>% select(county = {{varname_county}}) %>% mutate(county = str_remove(county, "^0+")),
             "precinct" = x_results %>% select(precinct = {{varname_precinct}}) %>% mutate(precinct = str_remove(precinct, "^0+")),
             "votes_dem" = onlyoffice[ , grepl(paste0(varname_gyyoff,"D"), names(onlyoffice)) ],
             "votes_rep" = onlyoffice[ , grepl(paste0(varname_gyyoff,"R"), names(onlyoffice)) ],
             "votes_total" = rowSums(onlyoffice)) %>%
    mutate("countyprecinct" = paste(county, precinct),
           cycle = pick_yyyy,
           office = pick_office) %>%
    merge(x_key) %>%
    mutate(cd = paste0(pick_state,"-", str_pad(cd, 2, pad = "0")),
           precinctshare_dem = 1 / Freq,
           precinctshare_rep = 1 / Freq,
           precinctshare_total = 1/ Freq) %>%
    select(countyprecinct, county, precinct, cd, cycle, office, votes_dem, votes_rep, votes_total, precinctshare_dem, precinctshare_rep, precinctshare_total)
}

# function to clean precinct data from MIT (2022)
cleanresults_MIT <- function(x_results, x_key, pick_yyyy, pick_office, pick_candidates){
  # reshape raw results
  precincts_all <- x_results %>%
    mutate(office = str_replace(office, "US SENATE", "U.S. SENATE"), # rename office titles for consistency
           office = ifelse(office == "U.S. SENATE" & special == TRUE, "U.S. SENATE (SPECIAL)", office),
           office = str_replace(office, "SECRETARY OF THE STATE", "SECRETARY OF STATE"),
           office = str_replace(office, "AUDITOR OF STATE", "AUDITOR"),
           office = str_replace(office, "STATE AUDITOR", "AUDITOR"),
           office = str_replace(office, "STATE TREASURER", "TREASURER"),
           office = str_replace(office, "GENERAL TREASURER", "TREASURER"),
           office = str_replace(office, "TREASURER OF STATE", "TREASURER"),
           office = str_replace(office, "STATE ATTORNEY GENERAL", "ATTORNEY GENERAL"),
           office = str_replace(office, "STATE CONTROLLER", "CONTROLLER"),
           office = str_replace(office, "RAILROAD COMMISSIONER 1", "RAILROAD COMMISSIONER"),
           office = str_replace(office, "JUSTICE OF THE SUPREME COURT PLACE 2", "SUPREME COURT JUSTICE, PLACE 2"),
           office = str_replace(office, "JUSTICE OF THE SUPREME COURT PLACE 3", "SUPREME COURT JUSTICE, PLACE 3"),
           office = str_replace(office, "JUSTICE OF THE SUPREME COURT PLACE 5", "SUPREME COURT JUSTICE, PLACE 5"),
           office = str_replace(office, "JUSTICE OF THE SUPREME COURT PLACE 9", "SUPREME COURT JUSTICE, PLACE 9"),
           office = str_replace(office, "ASSOCIATE JUSTICE OF THE SUPREME COURT, PLACE 5", "SUPREME COURT JUSTICE, PLACE 5"),
           office = str_replace(office, "JUSTICE OF THE SUPREME COURT POSITION 2", "SUPREME COURT JUSTICE, PLACE 2"),
           office = str_replace(office, "JUSTICE OF THE SUPREME COURT", "SUPREME COURT JUSTICE, PLACE 1"),
           office = str_replace(office, "JUDGE OF THE COURT OF APPEALS PLACE 1", "COURT OF APPEALS JUDGE, PLACE 1"),
           office = str_replace(office, "JUDGE OF THE COURT OF APPEALS PLACE 2", "COURT OF APPEALS JUDGE, PLACE 2"),
           office = str_replace(office, "JUDGE OF THE COURT OF APPEALS POSITION 1", "COURT OF APPEALS JUDGE, PLACE 1"),
           office = str_replace(office, "JUDGE OF THE COURT OF APPEALS POSITION 2", "COURT OF APPEALS JUDGE, PLACE 2"),
           office = str_replace(office, "COURT OF CRIMINAL APPEALS PLACE 5", "COURT OF CRIMINAL APPEALS JUDGE, PLACE 5"),
           office = str_replace(office, "COURT OF CRIMINAL APPEALS PLACE 6", "COURT OF CRIMINAL APPEALS JUDGE, PLACE 6"),
           party_detailed = str_replace(party_detailed, "DEMOCRATIC-FARMER-LABOR", "DEMOCRAT")) %>%
    filter(office == toupper(pick_office), # filter to the specified election
           candidate %in% pick_candidates) %>%
    mutate(countyprecinct = paste(county_name, precinct), # countyprecinct is a more reliable unique identifier for each precinct
           votes = str_replace(votes, "\\*", "0"),
           votes = as.numeric(votes),
           party = ifelse(candidate == pick_candidates[1], "votes_dem",
                   ifelse(candidate == pick_candidates[2], "votes_rep", "votes_other"))) %>%
    select(countyprecinct, county_name, precinct, party, votes) %>%
    group_by(countyprecinct, county_name, precinct, party) %>% # pivot results to long tidy format 
    summarize(votes = sum(votes)) %>%
    pivot_wider(names_from = "party", values_from = "votes") %>%
    replace(is.na(.), 0) %>%
    mutate(cycle = pick_yyyy, # add other variables of interest
           office = pick_office,
           votes_total = across(starts_with("votes_")) %>% rowSums) %>%
    select(countyprecinct, county = county_name, precinct, votes_dem, votes_rep, votes_total, cycle, office)
  # merge with key
  precincts_merged <- merge(precincts_all, x_key, by = "countyprecinct") %>% 
    drop_na(cd) %>%
    select(countyprecinct, county, precinct, cd, cycle, office, votes_dem, votes_rep, votes_total, precinctshare_dem, precinctshare_rep, precinctshare_total)
  # calculate vote totals for split precincts
  precincts_split <- precincts_merged %>% 
    filter(precinct %in% x_key[x_key$split == 1, ]) %>%
    mutate(votes_dem_adj = votes_dem * precinctshare_dem,
           votes_rep_adj = votes_rep * precinctshare_rep,
           votes_total_adj = votes_total * precinctshare_total) %>%
    mutate(problematic = ifelse(votes_total_adj >= votes_dem_adj + votes_rep_adj, FALSE,
                         ifelse(votes_total_adj < votes_dem_adj + votes_rep_adj, TRUE, NA))) %>%
    mutate(votes_total_scaled = ifelse(votes_total_adj >= votes_dem_adj + votes_rep_adj, votes_total_adj,
                                ifelse(votes_total_adj < votes_dem_adj + votes_rep_adj, votes_dem_adj + votes_rep_adj, NA)))
  precincts_split <- merge(precincts_split,
                           precincts_split %>%
                             select(countyprecinct, county, precinct, votes_total_scaled) %>%
                             group_by(countyprecinct, county, precinct) %>%
                             summarize(votes_total_sumscale = sum(votes_total_scaled))) %>%
    mutate(diff = votes_total - votes_total_sumscale) %>%
    mutate(votes_total_adjusted = ifelse(problematic == TRUE, votes_total_scaled, votes_total_scaled + diff),
           precinctshare_totalADJUST = votes_total_adjusted/votes_total) %>%
    replace(is.na(.), 0) %>%
    select(-precinctshare_total) %>% 
    select(countyprecinct, county, precinct, cd, cycle, office, votes_dem, votes_rep, votes_total, precinctshare_dem, precinctshare_rep, precinctshare_total = precinctshare_totalADJUST)
  # identify precincts that cast votes in the statewide election but not the House race -- these were dropped in the merge
  precincts_dropped <- precincts_all %>% filter(countyprecinct %in% setdiff(precincts_all$countyprecinct, x_key$countyprecinct)) %>%
    left_join(precincts_merged %>% distinct(county, cd) %>% group_by(county) %>% summarize(cd = mfv(cd))) %>%
    mutate(precinctshare_dem = 1, precinctshare_rep = 1, precinctshare_total = 1) %>%
    select(countyprecinct, county, precinct, cd, cycle, office, votes_dem, votes_rep, votes_total, precinctshare_dem, precinctshare_rep, precinctshare_total)
  # put it all together
  rbind(precincts_merged %>% filter(!(countyprecinct %in% x_key[x_key$split == 1, ])),
        precincts_split,
        precincts_dropped) %>%
    mutate(cd = paste0(pick_state,"-", str_pad(cd, 2, pad = "0"))) %>%
    arrange(countyprecinct)
}


#### #### #### #### #### #### #### ###
#— functions to aggregate data--------
#### #### #### #### #### #### #### ###

# function to sum precinct data up to the congressional district level
cleanresults_bycd <- function (x_results, pick_yyyy, pick_office) {
  x_results %>%
    mutate(state = pick_state,
           votes_dem_adj = votes_dem * precinctshare_dem,
           votes_rep_adj = votes_rep * precinctshare_rep,
           votes_total_adj = votes_total * precinctshare_total) %>%
    group_by(state, cd, cycle, office) %>%
    summarize(votes_dem = sum(votes_dem_adj),
              votes_rep = sum(votes_rep_adj),
              votes_total = sum(votes_total_adj)) %>%
    mutate(percent_dem = votes_dem / votes_total,
           percent_rep = votes_rep / votes_total,
           dem_margin = percent_dem - percent_rep)
}

# function to calculate Baseline scores by cd
baseline_cd <- function (x, year_start, year_end, trim) {
  x %>%
    filter(cycle >= year_start & cycle <= year_end) %>%
    group_by(cd) %>%
    dplyr::summarize(nraces = n(),
                     baseline_dem = mean(percent_dem, trim = trim/nraces),
                     baseline_rep = mean(percent_rep, trim = trim/nraces),
                     baseline_margin = baseline_dem - baseline_rep) %>%
    select(-nraces)
}




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
# | collect, clean, and aggregate precinct results--------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###

# read in election results: VEST (2016, 2018, 2020); MIT (2022)
results_2016 <- getresults_VEST(cycle = 2016, doi = "10.7910/DVN/NH5S2I", TOWN, WP_NAME)

results_2018 <- getresults_VEST(cycle = 2018, doi = "10.7910/DVN/UBKYRU", TOWN, WP_NAME)

results_2020 <- getresults_VEST(cycle = 2020, doi = "10.7910/DVN/K7760H", TOWN, WP_NAME)

temp <- tempfile()
download.file("https://github.com/MEDSL/2022-elections-official/raw/main/individual_states/2022-ma-local-precinct-general.zip", temp)
results_2022 <- read.csv(unz(temp, "ma22_cleaned.csv"))
unlink(temp)


# create keys that match census blocks into precincts
key_2016 <- getkey_VEST(read.csv("block_assignment_files/match_block_precinct_2016.csv", header=TRUE))

key_2018 <- getkey_VEST(read.csv("block_assignment_files/match_block_precinct_2018.csv", header=TRUE))

key_2020 <- getkey_VEST(read.csv("block_assignment_files/match_block_precinct_2020.csv", header=TRUE))

key_2022 <- getkey_MIT2022(results_2022)


# clean precinct data for all years and offices
by_precinct <- rbind(cleanresults_VEST(results_2016, key_2016, "G16PRE", TOWN, WP_NAME, 2016, "President"),
                     cleanresults_VEST(results_2018, key_2018, "G18USS", TOWN, WP_NAME, 2018, "U.S. Senate"),
                     cleanresults_VEST(results_2018, key_2018, "G18GOV", TOWN, WP_NAME, 2018, "Governor"),
                     cleanresults_VEST(results_2018, key_2018, "G18ATG", TOWN, WP_NAME, 2018, "Attorney General"),
                     cleanresults_VEST(results_2018, key_2018, "G18SOC", TOWN, WP_NAME, 2018, "Secretary of the Commonwealth"),
                     cleanresults_VEST(results_2018, key_2018, "G18TRE", TOWN, WP_NAME, 2018, "Treasurer"),
                     cleanresults_VEST(results_2018, key_2018, "G18AUD", TOWN, WP_NAME, 2018, "Auditor"),
                     cleanresults_VEST(results_2020, key_2020, "G20PRE", TOWN, WP_NAME, 2020, "President"),
                     cleanresults_VEST(results_2020, key_2020, "G20USS", TOWN, WP_NAME, 2020, "U.S. Senate"),
                     cleanresults_MIT(results_2022, key_2022, 2022, "Governor", c("HEALEY", "DIEHL", "REED")),
                     cleanresults_MIT(results_2022, key_2022, 2022, "Attorney General", c("ANDREA JOY CAMPBELL", "JAMES R MCMAHON III")),
                     cleanresults_MIT(results_2022, key_2022, 2022, "Secretary of State", c("WILLIAM FRANCIS GALVIN", "RAYLA CAMPBELL", "JUAN SANCHEZ")) %>% mutate(office = str_replace(office, "State", "the Commonwealth")),
                     cleanresults_MIT(results_2022, key_2022, 2022, "Auditor", c("DIANA DIZOGLIO", "ANTHONY AMORE", "GLORIA A CABALLERO-ROCA", "DANIEL WERNER RIEK", "DOMINIC GIANNONE III")))


# aggregate results by congressional district for all races
by_cd <- cleanresults_bycd(by_precinct)

# compute baseline (absent from here, the official calculation also includes 2022 House results)
baseline22_cd <- baseline_cd(by_cd, 2016, 2022, 1)
