# This code analyzes U.S. House elections
# Currently for 2016 only, but could be expanded to earlier years.
# Data from U.S. Federal Elections Commission -- https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/#election-results

install.packages("tidyverse")
library(tidyverse)
library(readxl)

house2016raw <- read_excel("federalelections2016.xlsx", sheet = "2016 US House Results by State")

# get rid of blank spaces, #, %, (, ) in column names
names(house2016raw) <- gsub(" ","_",names(house2016raw))
names(house2016raw) <- gsub("#", "",names(house2016raw)) 
names(house2016raw) <- gsub(",", "",names(house2016raw)) 
names(house2016raw) <- gsub("%", "pct",names(house2016raw))
names(house2016raw) <- gsub("[()]", "",names(house2016raw))

# this method not used in favor of following solution -- save total District Votes in separate DF.
# recode n/a FECcode for Scattered candidates to preserve vote totals once we eliminate records without FECcode
# house2016 <- house2016raw %>%
#  mutate(FEC_ID = ifelse(CANDIDATE_NAME=="Scattered","Scattered",FEC_ID))

# find vote totals in general elections - not used in final analysis 
district_total <- house2016raw %>%
  filter(TOTAL_VOTES=="District Votes:") %>%
  group_by(STATE,D) %>%
  mutate(general_total=as.integer(GENERAL_VOTES))  

# remove all records not relating to a candidate
house2016 <- house2016raw %>%
  filter(FEC_ID != 'n/a' & FEC_ID !="FULL TERM:")

# HI, KY, PA held special elections, 1 district per state, for unexpired terms -- remove these
house2016 <- house2016 %>%
  filter(!D %in% c('1 - UNEXPIRED TERM','01 - UNEXPIRED TERM','02 - UNEXPIRED TERM'))

# create flag for non-voting members from U.S. territories and D.C. and for Top Two and Runoff states
# North Carolina ended primary run-offs in 2014
# Georgia is the only state that requires a general election runoff if needed, but none were needed for House districts in 2016
house2016 <- house2016 %>%
  mutate(nonvoting = ifelse(STATE %in% c('American Samoa','District of Columbia','Guam','Northern Mariana Islands','Puerto Rico','Virgin Islands'),1,0),
         toptwo = ifelse(STATE %in% c('California','Washington','Louisiana'),1,0),
         p_runoff = ifelse(STATE %in% c('Alabama','Arkansas','Georgia','Mississippi','Oklahoma', 'South Carolina','South Dakota','Texas'),1,0))

# replace NA with 0 for the vote totals and shares
house2016 <- house2016 %>%
mutate(GENERAL_VOTES=ifelse(is.na(GENERAL_VOTES),0,GENERAL_VOTES),
       PRIMARY_VOTES=ifelse(is.na(PRIMARY_VOTES),0,PRIMARY_VOTES),
       RUNOFF_VOTES=ifelse(is.na(RUNOFF_VOTES),0,RUNOFF_VOTES), 
       GENERAL_pct = ifelse(is.na(GENERAL_pct), 0, GENERAL_pct),
       RUNOFF_pct = ifelse(is.na(RUNOFF_pct), 0, RUNOFF_pct),
       GE_RUNOFF_ELECTION_VOTES_LA=ifelse(is.na(GE_RUNOFF_ELECTION_VOTES_LA),0,GE_RUNOFF_ELECTION_VOTES_LA) )

# create primary and general vote totals as integers
# Unopposed in primary, plus 2 cases where cand was unopposed in general (states did not report vote totals): assign vote share as 100%
house2016 <- house2016 %>%
  mutate(general = ifelse(GENERAL_VOTES == 'Unopposed', 0, as.integer(GENERAL_VOTES)),
         primary = ifelse(PRIMARY_VOTES %in% c('Unopposed','*','#'), 0, as.integer(PRIMARY_VOTES)),
         GENERAL_pct = ifelse(GENERAL_VOTES == 'Unopposed', 1, GENERAL_pct),
         PRIMARY_pct = ifelse(PRIMARY_VOTES == 'Unopposed', 1, PRIMARY_pct),
         GE_RUNOFF_ELECTION_pct_LA = ifelse(GE_RUNOFF_ELECTION_VOTES_LA >0, as.numeric(GE_RUNOFF_ELECTION_pct_LA), NA),
         winner = ifelse(is.na(GE_WINNER_INDICATOR),0,1))

# LA: put general info into primary, and runoff info into general - not implemented
#house2016 <-  house2016 %>%
#  mutate(primary = ifelse(STATE_ABBREVIATION == 'LA', general, primary),
#         general = ifelse(STATE_ABBREVIATION == 'LA', GE_RUNOFF_ELECTION_VOTES_LA, general),
#         PRIMARY_pct = ifelse(STATE_ABBREVIATION == 'LA', GENERAL_pct, PRIMARY_pct),
#         GENERAL_pct = ifelse(STATE_ABBREVIATION == 'LA', GE_RUNOFF_ELECTION_pct_LA, GENERAL_pct))
        
# remove territories and DC
house2016 <- house2016 %>%         
   filter(nonvoting == 0)

# 3 states allow multiple nominations, and theses generate dupliate entries with the same FEC_ID. Filter to keep the highest GENERAL_VOTES
# eliminate duplicate candidate names and use totals for multiple nominations in CT, NY, and SC
# the PARTY will be the one on whose line the candidate won the most votes (typically Dem or Rep)
house2016 <- house2016 %>%         
  group_by(FEC_ID) %>%
  filter(general == max(general))

# Replace general election votes and vote share with COMBINED_GE_PARTY_TOTALS_CT_NY_SC and COMBINED_pct_CT_NY_SC if needed
house2016 <- house2016 %>%
   mutate(general = ifelse(is.na(COMBINED_GE_PARTY_TOTALS_CT_NY_SC),general,COMBINED_GE_PARTY_TOTALS_CT_NY_SC),
          GENERAL_pct = ifelse(is.na(COMBINED_GE_PARTY_TOTALS_CT_NY_SC),GENERAL_pct,COMBINED_pct_CT_NY_SC))

# error in the original data for Illinois District 2 Republican primary - 277,303 is typo for 27,303
# correct info http://www.elections.il.gov/ElectionResults.aspx?ID=SKR13%2f24Geo%3d
# Chaffetz, Jason and Teng, Chia-Chi were incorrectly listed as Utah-2, should be Utah-3 per
# https://ballotpedia.org/Utah%27s_3rd_Congressional_District_election,_2016
house2016 <- house2016 %>%  
  ungroup() %>%
  mutate(primary = ifelse(FEC_ID=="H6IL02199",27303,primary),
                  D = ifelse(FEC_ID %in% c("H8UT03089","H6UT03166"),"03",D))

# find races with incumbents
house2016 <-  house2016 %>%
  ungroup() %>%
  mutate(incumbent = ifelse(is.na(I), 0, 1))
house2016 <-  house2016 %>%
  group_by(STATE,D) %>%
  mutate(incumbent_in_race = ifelse(any(incumbent==1), 1, 0))

# using max() just because we need to make sure to select only 1 from each district
house2016 <-  house2016 %>%
  group_by(STATE,D) %>%
  mutate(winner_id = max(FEC_ID[winner == 1]), incumbent_id = max(FEC_ID[incumbent == 1])) %>%
  mutate(incumbent_won = ifelse(winner_id == incumbent_id, 1,0)) %>%  
  mutate(incumbent_won = ifelse(is.na(incumbent_won),0,incumbent_won)) %>%
  mutate(winner_party = substr(PARTY[winner == 1],1,1),
         incumbent_party = ifelse(incumbent_in_race == 1,substr(PARTY[incumbent ==1],1,1),winner_party),
         incumbent_party = ifelse((STATE=="Florida" & D=="10") | (STATE=="Virginia"&D=="04")| (STATE=="Nevada"&D=="03"),"R",incumbent_party), #recode R open seats that switched to D: FL-10, VA-4, NV-3
         incumbent_party = ifelse(STATE=="Florida" & D %in% c("02","18"),"D",incumbent_party), #recode D open seats that switched to R: FL-2, FL-18
                  changed_party = ifelse(winner_party != incumbent_party,1,0))

# find summary totals by state & district including vote totals and number of candidates in general and in primaries 
# ignore write-ins based on PARTY=W or PARTY starts with W/
#        except PARTY = W(D)/D, W(R)/R, or W(GRE)/GRE include because this means write in for primary but on ballot for General.
#        there were only 4 Dems, 2 Rep and 2 Green who got on the ballot this way in 2016
# the Dem and Rep party primary vote totals exclude write in votes (except for the 4 Dems and 2 Reps above)
# most Democrats have PARTY=D, but some are multiply nominated, and also there is DFL and DNL - all start with 'D'
# Peter Welch of VT is listed as "D/R" -because he won the Rep. nomination as a write-in, but is D
# primary total votes by party are not meaningful for CA, WA, LA, use total_primary_count instead
housecontests <- house2016 %>%
  group_by(STATE,D) %>%
  mutate(PARTY = ifelse(PARTY=='W(D)/D','D',
                        ifelse(PARTY=='W(R)/R','R',
                               ifelse(PARTY=='W(GRE)/GRE)','GRE',PARTY)))) %>%
  filter(!grepl("W\\(",PARTY) & !PARTY=="W") %>%
    summarize(general_candidate_count = sum(general>0), 
              total_primary_count=sum(primary>0), 
              dem_general_count=sum((GENERAL_VOTES> 0 & substr(PARTY,1,1)=="D")),
              rep_general_count=sum((GENERAL_VOTES> 0 & substr(PARTY,1,2) %in% c("R", "R ", "R/"))),
              dem_primary_votes=sum(primary[substr(PARTY,1,1)=="D"]),dem_candidates=sum(substr(PARTY,1,1)=="D"),
              rep_primary_votes=sum(primary[substr(PARTY,1,2) %in% c("R", "R ", "R/")]),
              primary=sum(primary),
              rep_candidates=sum(substr(PARTY,1,2) %in% c("R", "R ", "R/")),
              incumbent_in_race=max(incumbent_in_race),
              incumbent_won = max(incumbent_won),
              rep_primary_win_pct=max(PRIMARY_pct[substr(PARTY,1,2) %in% c("R", "R ", "R/")], na.rm=TRUE),
              dem_primary_win_pct=max(PRIMARY_pct[substr(PARTY,1,1)=="D"],na.rm=TRUE),
              toptwo=max(toptwo),
              p_runoff=max(p_runoff),
              general=sum(general),
              p_runoff_votes = sum(RUNOFF_VOTES),
              p_runoff_win_pct = max(RUNOFF_pct),
              g_runoff =sum(GE_RUNOFF_ELECTION_VOTES_LA),
              g_runoff_win_pct = max(GE_RUNOFF_ELECTION_pct_LA,na.rm=TRUE),
              ST = max(STATE_ABBREVIATION),
              winner_party=max(winner_party),
              incumbent_party=max(incumbent_party),
              changed_party=max(changed_party),
              win_pct=max(GENERAL_pct))

housecontests <- housecontests %>%
  mutate(general_plurality=ifelse(win_pct < .5,1,0),
         dem_primary_win_pct= ifelse(dem_primary_win_pct=="-Inf",NA,dem_primary_win_pct),
         rep_primary_win_pct= ifelse(rep_primary_win_pct=="-Inf",NA,rep_primary_win_pct),
         p_runoff_win_pct= ifelse(p_runoff_win_pct=="-Inf",NA,p_runoff_win_pct),
         g_runoff_win_pct= ifelse(g_runoff_win_pct=="-Inf",NA,g_runoff_win_pct),
         key_primary_pct=ifelse(winner_party =="D",dem_primary_win_pct,rep_primary_win_pct),
         key_primary_votes=ifelse(winner_party =="D",dem_primary_votes,rep_primary_votes),
         non_major_count=general_candidate_count-dem_general_count-rep_general_count,
         unopposed_general=ifelse(general_candidate_count==1,1,0),
         no_major_opponent=ifelse(dem_general_count+rep_general_count<2,1,0), # in CA & WA, means same party opponent
         unopposed_primary=ifelse(key_primary_pct==1,1,0),
         g_non_comp=ifelse(win_pct>0.6,1,0),
         p_non_comp=ifelse(key_primary_pct>0.6,1,0),
         primary_plurality = ifelse(key_primary_pct<0.5,1,0),
         closed_primary= ifelse(ST %in% c('CT','DE','FL','KY','ME','MD','NV','NM','NY','PA','OR'),1,0),
         semi_closed_primary= ifelse(ST %in% c('AZ','CO','ID','KS','MA','NE','NJ','NH','NC','RI','UT','WV'),1,0))

write.csv(housecontests,"housecontests.csv")
write.csv(house2016,"house2016.csv")


# only a handful of runoffs -- can look at individually. Only TX-15 had runoffs in both D & R primaries. In general, need to 
# separate primary runoff totals and pcts by party, same as for primary results.

# to-do items:
# GENERAL_pct - should be 1 for Unopposed, but showing up as 0.(only 2 of these)
# GE_RUNOFF_ELECTION_pct_LA decimal values not imported, defaults to Boolean

