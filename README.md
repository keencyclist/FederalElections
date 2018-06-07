# FederalElections
Plurality winners in open-seat primary elections to the U.S. House of Representatives 2016
This analysis uses data from the U.S. Federal Elections Commission on Elections Returns
https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/#election-results
So far only for U.S. House for 2016, but could be easily expanded to the Senate and earlier years.
The code takes the FEC's spreadsheet and converts it into a tidy format, and then produces relevant summary statistics per contest, including:
* number of candidates (excluding-write ins) in primaries and generals
* winners share of the vote in primaries and generals
* number of voters in primaries and generals
* flags for open seat race, plurality winner, incumbent defeated, party changed

There is an accounting for states that have different systems, such as those that use Top Two and those that have runoff elections for primaries where no candidate received a majority.
