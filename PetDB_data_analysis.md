<!-- for readable local version use 
output: html_document
 -->

## Geographic analysis of PetDB records

The following is a working document written in R markdown. The objective
in the code outlined by this document is to first determine the country
of affiliation of published works recorded in the PetDB database of
geochemical data.

The dois of PetDB records were retrieved by data mining and are stored
in the file “specimen_locations_new.csv”.

These dois are then used (in Sections 1 and 2) to manually search the
Web of Science database to retrievev author address data for \~1500
publications.

The bulk of the code which follows is concerned with automatically
detecting countries in those addresses and cleaning up various errors
and difficulties with the data.

Finally, the datasets are combined to give a record of the country from
which the samples, authors and 1st author derive for \~1500 geochemical
publications.

In the final sections some preliminary data analysis is made looking at
the patterns of: which countries tend to conduct research domestically
and abroad; which are most and least subject to other countries’
research; and which are most and least likely to include local
researchers as co-authors when researching samples from abroad.

## 1 Make list of DOIs to search Web of Science

``` r
# read in scraped PetDB data
# scaping done using Jupiter notebook 
# https://github.com/GeoCoLab/notebooks/tree/main/specimen-locations 
refs<-read.csv("./data/PetDB_specimen_locations.csv", encoding = "UTF-8")

# filter out records with no doi or no specimen location (mostly deep ocean samples)
dois<-refs%>%
  filter(specimen_locations!="", doi!="")%>%
  pull(doi)

# collapse vector of dois into single string separated by " OR "
string<-paste(dois, collapse = " OR ")

# output search string to text file for easier copy/pasting
# if it doesn't already exist (because slow!)
if(!file.exists("./data/search_string.txt")){
  write.table(string, "./data/search_string.txt", row.names = F)
}
```

## 2. Manually search Web of Science by DOI

Search done at
<https://www.webofscience.com/wos/woscc/summary/1bd1f26a-e825-446a-ba7f-4cc2e8126141-0bad9599/relevance/1>

From 2030 DOIs found 1493 records

Results in “WoS_records.csv”

## 3. Get list of countries

List prepared from list of all world countries and territories with an
ISO code at XXX

Various simplifications and synomyms have been added to give a unified
preferred name to all affiliation and sample locations

This allows us to match these later in the analysis

``` r
# get list of countries
countries<-read.csv("./data/countries.csv")
head(countries)%>%as_tibble()
```

    ## # A tibble: 6 x 7
    ##   Name           Code  Synonym Semi.autonomous.~ Authority Preferred.Name Region
    ##   <chr>          <chr> <chr>   <chr>             <chr>     <chr>          <chr> 
    ## 1 Afghanistan    AF    ""      ""                ""        Afghanistan    Asia  
    ## 2 Åland Islands  AX    ""      "Y"               "Finland" Åland Islands  Europe
    ## 3 Albania        AL    ""      ""                ""        Albania        Europe
    ## 4 Algeria        DZ    ""      ""                ""        Algeria        Africa
    ## 5 American Samoa AS    ""      "Y"               "United ~ American Samoa Ocean~
    ## 6 Andorra        AD    ""      ""                ""        Andorra        Europe

``` r
# make capitalised vector of countries
country<-toupper(countries$Name)
```

## 4. Load Web of Science data and tidy up

All records returned by manual doi search at Web of Science loaded in a
dataframe.

Each step is repeated for all authors (the Addresses field =\>\> add)
and the corresponding author (the Reprint.Addresses field =\>\>
corradd). The latter is taken to be the first author hereafter.

``` r
# Read in Web of Science data
wos<-read.csv("./data/WoS_records.csv")
# str(wos) # check structure if necessary
```

Key fields from Web of Science (WoS) data

    ## # A tibble: 6 x 5
    ##   Addresses      Reprint.Addresses     DOI     Publication.Year Funding.Orgs    
    ##   <chr>          <chr>                 <chr>              <int> <chr>           
    ## 1 [Gaschnig, Ri~ Gaschnig, RM (corres~ 10.101~             2021 National Scienc~
    ## 2 [Bezard, Rach~ Bezard, R (correspon~ 10.101~             2021 BMBF (German Mi~
    ## 3 [Stephenson, ~ Stephenson, SN (corr~ 10.102~             2021 University of C~
    ## 4 [Agustin-Flor~ Ferres, D (correspon~ 10.101~             2021 Comision Federa~
    ## 5 [Ashwal, Lewi~ Ashwal, LD (correspo~ 10.102~             2021 National Resear~
    ## 6 [Salazar-Muno~ Murcia, H (correspon~ 10.101~             2021 Vicerrectoria d~

Cleaning up WoS data

1.  filter out missing addresses
2.  capitalise everything
3.  add spaces after every comma (to avoid words running together once
    punctuation removed)
4.  remove all punctuation
5.  split each address into a vector of single words

``` r
# filter out records with no address
# all author addresses
add<-filter(wos, Addresses!="")%>% # capitalise
  pull(Addresses)%>%
  toupper()

# corresponding author address
corradd<-filter(wos, Reprint.Addresses!="")%>% # capitalise
  pull(Reprint.Addresses)%>%
  toupper()

# indexes (for later)
numbers<-1:length(add)
numbers2<-1:length(corradd)

#length(add)
# add space after commas to avoid concatenated words after removing punctuation e.g. KeilGermany 
add2<-lapply(add, function(x) gsub(",",", ",x))
corradd2<-lapply(corradd, function(x) gsub(",",", ",x))

# remove all punctuation
gadd<-lapply(add2, function(x) gsub("[[:punct:]\n]","",x))
gcorradd<-lapply(corradd2, function(x) gsub("[[:punct:]\n]","",x))

# split each address into single words whereever there's a space
# causes issues with two word countries e.g. South Africa which have to be dealt with
sadd<-lapply(gadd, function(x) strsplit(x, " ")%>%unlist())
scorradd<-lapply(gcorradd, function(x) strsplit(x, " ")%>%unlist())

# make all words upper case to avoid matching issues between capitalised and proper case spellings
cadd<-lapply(sadd, toupper)
ccorradd<-lapply(scorradd, toupper)
```

Addresses field is now a list with each list element containing a vector
of every part of the address

    ## [1] "[GASCHNIG, RICHARD M.; RADER, SHELBY T.] UNIV MASSACHUSETTS LOWELL, DEPT ENVIRONM EARTH & ATMOSPHER SCI, LOWELL, MA 01854 USA; [RADER, SHELBY T.] INDIANA UNIV, DEPT EARTH & ATMOSPHER SCI, BLOOMINGTON, IN USA; [REINHARD, CHRISTOPHER T.] GEORGIA INST TECHNOL, SCH EARTH & ATMOSPHER SCI, ATLANTA, GA 30332 USA; [REINHARD, CHRISTOPHER T.; PLANAVSKY, NOAH] NASA, ASTROBIOL INST, ALTERNAT EARTHS TEAM, WASHINGTON, DC 20546 USA; [OWENS, JEREMY D.] FLORIDA STATE UNIV, DEPT EARTH OCEAN & ATMOSPHER SCI, TALLAHASSEE, FL 32306 USA; [PLANAVSKY, NOAH; WANG, XIANGLI; ASAEL, DAN] YALE UNIV, DEPT GEOL & GEOPHYS, NEW HAVEN, CT USA; [WANG, XIANGLI] UNIV S ALABAMA, DEPT MARINE SCI, MOBILE, AL USA; [WANG, XIANGLI] DAUPHIN ISL SEA LAB, DAUPHIN ISL, AL USA; [GREANEY, ALLISON] UNIV CALIF SANTA BARBARA, DEPT EARTH SCI, SANTA BARBARA, CA 93106 USA; [GREANEY, ALLISON] OAK RIDGE NATL LAB, OAK RIDGE, TN USA; [HELZ, ROSALIND] US GEOL SURVEY, 959 NATL CTR, RESTON, VA 22092 USA"

becomes

    ## [[1]]
    ##   [1] "GASCHNIG"      ""              "RICHARD"       "M"            
    ##   [5] "RADER"         ""              "SHELBY"        "T"            
    ##   [9] "UNIV"          "MASSACHUSETTS" "LOWELL"        ""             
    ##  [13] "DEPT"          "ENVIRONM"      "EARTH"         ""             
    ##  [17] "ATMOSPHER"     "SCI"           ""              "LOWELL"       
    ##  [21] ""              "MA"            "01854"         "USA"          
    ##  [25] "RADER"         ""              "SHELBY"        "T"            
    ##  [29] "INDIANA"       "UNIV"          ""              "DEPT"         
    ##  [33] "EARTH"         ""              "ATMOSPHER"     "SCI"          
    ##  [37] ""              "BLOOMINGTON"   ""              "IN"           
    ##  [41] "USA"           "REINHARD"      ""              "CHRISTOPHER"  
    ##  [45] "T"             "GEORGIA"       "INST"          "TECHNOL"      
    ##  [49] ""              "SCH"           "EARTH"         ""             
    ##  [53] "ATMOSPHER"     "SCI"           ""              "ATLANTA"      
    ##  [57] ""              "GA"            "30332"         "USA"          
    ##  [61] "REINHARD"      ""              "CHRISTOPHER"   "T"            
    ##  [65] "PLANAVSKY"     ""              "NOAH"          "NASA"         
    ##  [69] ""              "ASTROBIOL"     "INST"          ""             
    ##  [73] "ALTERNAT"      "EARTHS"        "TEAM"          ""             
    ##  [77] "WASHINGTON"    ""              "DC"            "20546"        
    ##  [81] "USA"           "OWENS"         ""              "JEREMY"       
    ##  [85] "D"             "FLORIDA"       "STATE"         "UNIV"         
    ##  [89] ""              "DEPT"          "EARTH"         "OCEAN"        
    ##  [93] ""              "ATMOSPHER"     "SCI"           ""             
    ##  [97] "TALLAHASSEE"   ""              "FL"            "32306"        
    ## [101] "USA"           "PLANAVSKY"     ""              "NOAH"         
    ## [105] "WANG"          ""              "XIANGLI"       "ASAEL"        
    ## [109] ""              "DAN"           "YALE"          "UNIV"         
    ## [113] ""              "DEPT"          "GEOL"          ""             
    ## [117] "GEOPHYS"       ""              "NEW"           "HAVEN"        
    ## [121] ""              "CT"            "USA"           "WANG"         
    ## [125] ""              "XIANGLI"       "UNIV"          "S"            
    ## [129] "ALABAMA"       ""              "DEPT"          "MARINE"       
    ## [133] "SCI"           ""              "MOBILE"        ""             
    ## [137] "AL"            "USA"           "WANG"          ""             
    ## [141] "XIANGLI"       "DAUPHIN"       "ISL"           "SEA"          
    ## [145] "LAB"           ""              "DAUPHIN"       "ISL"          
    ## [149] ""              "AL"            "USA"           "GREANEY"      
    ## [153] ""              "ALLISON"       "UNIV"          "CALIF"        
    ## [157] "SANTA"         "BARBARA"       ""              "DEPT"         
    ## [161] "EARTH"         "SCI"           ""              "SANTA"        
    ## [165] "BARBARA"       ""              "CA"            "93106"        
    ## [169] "USA"           "GREANEY"       ""              "ALLISON"      
    ## [173] "OAK"           "RIDGE"         "NATL"          "LAB"          
    ## [177] ""              "OAK"           "RIDGE"         ""             
    ## [181] "TN"            "USA"           "HELZ"          ""             
    ## [185] "ROSALIND"      "US"            "GEOL"          "SURVEY"       
    ## [189] ""              "959"           "NATL"          "CTR"          
    ## [193] ""              "RESTON"        ""              "VA"           
    ## [197] "22092"         "USA"

## 5. Match to countries

Now we can match words in each vector to our list of countries

``` r
# string match each element of address list to list of countries
ladd<-lapply(cadd, function(x) x[which(x %in% country)] )
lcorradd<-lapply(ccorradd, function(x) x[which(x %in% country)] )
```

and check the ouput

``` r
# check output
ladd[1:5]
```

    ## [[1]]
    ##  [1] "USA"     "USA"     "GEORGIA" "USA"     "USA"     "USA"     "USA"    
    ##  [8] "USA"     "USA"     "USA"     "USA"     "USA"    
    ## 
    ## [[2]]
    ## [1] "GERMANY"   "AUSTRALIA" "GERMANY"   "GERMANY"   "GERMANY"   "USA"      
    ## [7] "RUSSIA"    "USA"      
    ## 
    ## [[3]]
    ## [1] "ENGLAND"   "ENGLAND"   "ENGLAND"   "ENGLAND"   "AUSTRALIA"
    ## 
    ## [[4]]
    ## [1] "MEXICO" "MEXICO" "MEXICO" "MEXICO" "MEXICO" "MEXICO" "MEXICO" "MEXICO"
    ## [9] "MEXICO"
    ## 
    ## [[5]]
    ## [1] "GERMANY"

Because we split into single words this won’t have worked for multi-word
country names (e.g. South Africa)

We need to deal with these separately

``` r
# subset of countries with spaces in name
longcountry<-countries$Name[str_detect(countries$Name, " ")]

# for each country find addresses which match
findlong<-lapply(longcountry, function(x) str_detect(toupper(add), pattern = toupper(x)))
findlong2<-lapply(longcountry, function(x) str_detect(toupper(corradd), pattern = toupper(x)))

# give each list element of findlong the name of the corresponding country
names(findlong)<-longcountry
names(findlong2)<-longcountry

# sum no. of matches for each country
sums<-lapply(findlong, sum)
sums2<-lapply(findlong2, sum)

# subset of "long countries" which occur in addresses list
longcountry[sums>0]
```

    ##  [1] "Brunei Darussalam" "Costa Rica"        "Czech Republic"   
    ##  [4] "Fed Rep Ger"       "Hong Kong"         "South Korea"      
    ##  [7] "New Caledonia"     "New Zealand"       "Papua N Guinea"   
    ## [10] "Saudi Arabia"      "Solomon Islands"   "South Africa"     
    ## [13] "Trinidad Tobago"   "Viet Nam"

``` r
longcountry[sums2>0]
```

    ## [1] "Czech Republic"  "Hong Kong"       "South Korea"     "New Caledonia"  
    ## [5] "New Zealand"     "Saudi Arabia"    "South Africa"    "Trinidad Tobago"

We make a helper function which acts on an index variable x
(i.e. 1:length of list) using lapply()

Takes a country name, the list of addresses (add or corradd) and a
parsed list (e.g. nadd) as inputs

If it detects the country name in element x of add then that country is
appended to the to corresponding element of the parsed list

Otherwise the parsed list is left unchanged

``` r
# make helper function to add a country to a list element
appendCountry <- function(x, country_name, parsed_list, addresses_list=add) {
  name<-toupper(country_name)
  address<-toupper(addresses_list[[x]])
  if(str_detect(address, pattern=name)){
    c(parsed_list[[x]], name)
    } else {parsed_list[[x]]}
}
```

We then loop through the list of “long countries” (ones with 2+ words)
and apply our helper function to the entire address list, adding the
country in question to the parsed address list (nadd) for any records
which match the country in question

``` r
# index for lapply within loop (on j)
i<-1:length(ladd)
# copy ladd to new list to which matching long countries will be added
nadd<-ladd 
# pull out subset of long countries which occur in dataset
longcountry_use<-longcountry[sums>0]

for(j in 1:length(longcountry_use)){
  print(paste0(longcountry_use[j], " (+", sums[longcountry_use[j]], ")"))
  nadd<-lapply(i, function(x) appendCountry(x, longcountry_use[j], nadd))
}
```

    ## [1] "Brunei Darussalam (+1)"
    ## [1] "Costa Rica (+4)"
    ## [1] "Czech Republic (+11)"
    ## [1] "Fed Rep Ger (+3)"
    ## [1] "Hong Kong (+12)"
    ## [1] "South Korea (+28)"
    ## [1] "New Caledonia (+4)"
    ## [1] "New Zealand (+19)"
    ## [1] "Papua N Guinea (+2)"
    ## [1] "Saudi Arabia (+12)"
    ## [1] "Solomon Islands (+1)"
    ## [1] "South Africa (+54)"
    ## [1] "Trinidad Tobago (+1)"
    ## [1] "Viet Nam (+1)"

We then repeat this exercise for the corresponding author addresses

``` r
# index
i2<-1:length(lcorradd)
# copy parsed list
ncorradd<-lcorradd
# subset countries which appear
longcountry_use2<-longcountry[sums2>0]

for(j in 1:length(longcountry_use2)){
  print(paste0(longcountry_use2[j], " (+", sums2[longcountry_use2[j]], ")"))
  ncorradd<-lapply(i2, function(x) appendCountry(x, longcountry_use2[j], ncorradd, addresses_list = corradd))
}
```

    ## [1] "Czech Republic (+5)"
    ## [1] "Hong Kong (+2)"
    ## [1] "South Korea (+15)"
    ## [1] "New Caledonia (+1)"
    ## [1] "New Zealand (+10)"
    ## [1] "Saudi Arabia (+5)"
    ## [1] "South Africa (+23)"
    ## [1] "Trinidad Tobago (+1)"

NB: this approach doesn’t work well for single word countries due to too
many partial matches (e.g. US on authors names) and getting a
universally applicable regex query is hard!

## 6. Multiple corresponding authors

A few records have multiple addresses listed as corresponding authors.

Making the assumption that the first listed is the true corresponding
author we need to find this to give a single first author affiliation.

``` r
# Determine length of vector of countries for each element of parsed corr. author list
corrlength<-lapply(ncorradd, function (x) x %>% unique() %>% length())%>%unlist()
# any with >1 have a problem with multiple
multiples<-i2[corrlength>1]
```

For example:

``` r
ncorradd[[multiples[3]]]
```

    ## [1] "CHINA"     "AUSTRALIA" "AUSTRALIA" "SPAIN"

We define a helper function to find the first mentioned country in the
address list

Acts on an index variable i (i.e 1:length of address list) to find the
start (and end) position in the address list (corradd) of each of the
countries listed in the parsed address list (ncorradd - e.g. “CHINA”,
“AUSTRALIA”, “SPAIN”) and combines in a data frame

By filtering for the smallest starting position then pulling the country
field the first occuring country is returns

``` r
findFirst<-function(i){
  data.frame(str_locate(toupper(corradd[i]), ncorradd[[i]]))%>%
    mutate(country = ncorradd[[i]])%>%
    filter(start==min(start))%>%
    unique()%>%
    pull(country)
}
```

e.g. for the example above

``` r
findFirst(multiples[3])
```

    ## [1] "CHINA"

if we look at he original address this is indeed the first entry

``` r
corradd[[multiples[3]]]
```

    ## [1] "LU, JG (CORRESPONDING AUTHOR), MINIST NAT RESOURCES, KEY LAB SUBMARINE GEOSCI, INST OCEANOG 2, HANGZHOU 310012, PEOPLES R CHINA.; TILHAC, R (CORRESPONDING AUTHOR), MACQUARIE UNIV, AUSTRALIAN RES COUNCIL, CTR EXCELLENCE CORE CRUST FLUID SYST CCFS, SYDNEY, NSW 2109, AUSTRALIA.; LU, JG; TILHAC, R (CORRESPONDING AUTHOR), MACQUARIE UNIV, GEMOC, DEPT EARTH & ENVIRONM SCI, SYDNEY, NSW 2109, AUSTRALIA.; TILHAC, R (CORRESPONDING AUTHOR), UNIV GRANADA, INST ANDALUZ CIENCIAS TIERRA IACT, CSIC, GRANADA 18100, SPAIN."

We then loop over the length of the parsed address list (ncorradd)
applying the findFirst function wherever i occurs in multiples (our
index of records with a multiple corresponding author issue)

``` r
for(j in 1:length(ncorradd)){
  if(j %in% multiples) {
    #print(j)
    ncorradd[[j]]<-findFirst(j)
    #print(ncorradd[[j]])
    }
}
```

## 8 USA addresses giving only state codes

Another issue in the data, mostly in older records, is United States
addresses that only give a state two letter code (e.g. Woods Hole, MA)

No easy way to automatically pattern match these. Matching on the two
letter codes causing far more issues than it solves as it frequently
picks up authors’ initials.

As it is only a small subset of samples the easiest is to manually trawl
for the corresponding index numbers

``` r
# find USA addresses that only give a state code (AARRRRGH)

# IDs found manually by trawling through 
USA_problems<-c(1312, 1319, 1326, 1331, 1334, 1336, 
                1340, 1342, 1349, 1351, 1355, 1356, 
                1357, 1365, 1366, 1370, 1376, 1378, 
                1379, 1381, 1384, 1386, 1388, 1390, 
                1392, 1393, 1394, 1396, 1397, 1398, 
                1399, 1401, 1402, 1403, 1405, 1406, 
                1408, 1409, 1417, 1418, 1421, 1422, 
                1423, 1424, 1427, 1428, 1429, 1430, 
                1432, 1435, 1437, 1438, 1440, 1443)

# apply loop to append "USA" to parsed list whenever i is in USA_problems
for(i in 1:length(nadd)){
  if(i %in% USA_problems){
    nadd[[i]]<-c(nadd[[i]], "USA")
  }
}
```

Finally we check if there are any record which have no matching author
country

``` r
## find any records with no matches
NAs<-lapply(nadd, length)==0
# index of unmatched records if needed for troubleshooting
unmatched<-numbers[NAs]
# check
if(sum(NAs)==0) print("All good!")
```

    ## [1] "All good!"

## 9 Solving incorrect matches

Finally there are a few issues with incorrect matching which need to be
manually solved

First Mexico matching New Mexico

``` r
### Fix "New Mexico" matches "Mexico"
mex<-numbers[str_detect(add, pattern="Mexico|MEXICO")]
nmex<-numbers[str_detect(add, pattern="New Mexico|NEW MEXICO")]

# by inspection mexico and new mexico never appear together 
# so matches for both are not really mexico
mex_false<-mex[mex %in% nmex]

# loop over nadd changing "MEXICO" to "USA" in all the mex_false records
for(i in 1:length(nadd)){
  if(i %in% mex_false){
    nadd[[i]]<-replace(nadd[[i]], nadd[[i]]=="MEXICO", "USA")
  }
}

# do the same corrections for corresponding author
mex2<-numbers2[str_detect(corradd, pattern="Mexico|MEXICO")]
nmex2<-numbers2[str_detect(corradd, pattern="New Mexico|NEW MEXICO")]
mex_false2<-mex2[mex2 %in% nmex2]

for(i in 1:length(ncorradd)){
  if(i %in% mex_false2){
    ncorradd[[i]]<-replace(ncorradd[[i]], ncorradd[[i]]=="MEXICO", "USA")
  }
}
```

Georgia matches the USA state

Congo matches Democratic Republic of the Congo

## 10. Final bits of tidying up

The final steps to get the data in a usable and graphable format are to

1.  Keep only non-duplicate countries - for the time being we’re not
    worried about the number of authors from a given country

2.  Link each parsed list element to it’s position in the original
    address list. By making an ID variable we can then join the final
    datatable of author affiliations back to the original WoS data
    including dois etc.

3.  Make the list into a dataframe

``` r
## final bits
uadd<-lapply(nadd, unique)
ucorradd<-lapply(ncorradd, unique)

index<-1:length(uadd)
index2<-1:length(ucorradd)

dadd<-lapply(index, function(x) data.frame(name=uadd[[x]], ID=x))
dcorradd<-lapply(index2, function(x) data.frame(name=ucorradd[[x]], ID=x))

dfadd<-do.call(rbind.data.frame, dadd)
dfcorradd<-do.call(rbind.data.frame, dcorradd)
```

The final output looks like this:

    ##        name ID
    ## 1       USA  1
    ## 2   GERMANY  2
    ## 3 AUSTRALIA  2
    ## 4       USA  2
    ## 5    RUSSIA  2
    ## 6   ENGLAND  3

We also want to replace non-unique country synonyms (e.g. Scotland,
England, United Kingdom, UK) with a single preferred name from our table
of countries

To do this we join the dataframes (dfadd and dfcorradd) to the countries
table (by name) and select the Preferred.Name field

``` r
## change countries to preferred names
# e.g. England -> United Kingdom 

pcadd<-inner_join(dfadd, mutate(countries, name=toupper(Name)), by="name")%>%
  select(ID, Affiliation=Preferred.Name)%>%
  unique()
head(pcadd)
```

    ##   ID    Affiliation
    ## 1  1  United States
    ## 2  2        Germany
    ## 3  2      Australia
    ## 4  2  United States
    ## 5  2         Russia
    ## 6  3 United Kingdom

``` r
pccorradd<-inner_join(dfcorradd, mutate(countries, name=toupper(Name)), by="name")%>%
  select(ID, Affiliation_1st=Preferred.Name)%>%
  unique()
head(pccorradd)
```

    ##   ID Affiliation_1st
    ## 1  1   United States
    ## 2  2         Germany
    ## 3  3  United Kingdom
    ## 4  4          Mexico
    ## 5  5    South Africa
    ## 6  6        Colombia

In the tables we’ve made each reference/article has multiple lines (if
there are multiple author affiliations)

We can make a short version collapsing the affiliations into a comma
separated list which is useful for some thing (e.g. validating the data)

``` r
pcadd_short<-pcadd%>%
  group_by(ID)%>%
  summarise(Affiliation=paste(Affiliation, collapse = ", "))
head(pcadd_short)
```

    ## # A tibble: 6 x 2
    ##      ID Affiliation                              
    ##   <int> <chr>                                    
    ## 1     1 United States                            
    ## 2     2 Germany, Australia, United States, Russia
    ## 3     3 United Kingdom, Australia, New Zealand   
    ## 4     4 Mexico                                   
    ## 5     5 Germany, South Africa                    
    ## 6     6 Colombia, Singapore, Brazil

## Join all the data together

Now we have a nice neat list of country affiliations for each reference
we can join these data back to the original WoS data table

Using the doi field in the WoS data we can join to the specimen data
mined from PetDB which includes the processed/geocoded sample locations
(see external Python script)

First we prepare the data. It’s very important to filter out the missing
addresses from WoS data like we did at the start otherwise the rows
won’t line up correcly

Capitalising all the dois is likewise important and there are
inconsistencies in upper and lower case between WoS and PetDB data which
will prevent them joining correctly

``` r
wos2<-wos%>%
  # filter out missing addresses
  filter(Addresses!="")%>% #
  # add ID field and capitalise (and rename) DOI
  mutate(ID=1:nrow(.), doi=toupper(DOI))
```

We also capitalise doi in our specimen data table (refs) and join it to
our countries table to substitute in the preferred names so that
countries are consistently named in both affiliation and
specimen_country fields of the final data table

``` r
refs2<-refs%>%
  # capitalise doi
  mutate(doi=toupper(doi), 
         # remove all punctuation from specimen_locations
         Name=gsub("[[:punct:]\n]","", specimen_locations))%>%
  # join to countries table
  full_join(countries, by="Name")%>%
  # use preferred name and rename specimen_country
  rename(specimen_country=Preferred.Name)
```

To be able to analyse the region from which articles or specimens derive
we also prepare tables from the countries table, renaming fields to make
things easier later

``` r
# regions table to join
regions1<-countries%>%
  select(specimen_country=Preferred.Name, specimen_region=Region)%>%
  unique()

regions2<-countries%>%
  select(Affiliation_1st=Preferred.Name, Region_1st=Region)%>%
  unique()
```

Finally we can join everything together to have a single master table
(and corresponding short version) combining the author affiliations, 1st
author affiliations, specimen location and regions in one (big) table

``` r
## join
master<-inner_join(pcadd, wos2, by="ID")%>%
  inner_join(refs2, by="doi")%>%
  inner_join(pccorradd, by="ID")%>%
  left_join(regions1, by="specimen_country")%>%
  left_join(regions2, by="Affiliation_1st")

master_short<-inner_join(pcadd_short, wos2, by="ID")%>%
  inner_join(refs2, by="doi")%>%
  inner_join(pccorradd, by="ID")%>%
  left_join(regions1, by="specimen_country")%>%
  left_join(regions2, by="Affiliation_1st")
```

The table has too many fields to easily view but we can look at a few
key ones

``` r
master_short%>%
  select(ID, doi, specimen_country, specimen_region, Affiliation, Affiliation_1st, Region_1st)%>%
  #as_tibble()%>%
  head()
```

    ## # A tibble: 6 x 7
    ##      ID doi      specimen_country specimen_region  Affiliation   Affiliation_1st
    ##   <int> <chr>    <chr>            <chr>            <chr>         <chr>          
    ## 1     1 10.1016~ United States    North America    United States United States  
    ## 2     2 10.1016~ United States    North America    Germany, Aus~ Germany        
    ## 3     3 10.1029~ Madagascar       Africa           United Kingd~ United Kingdom 
    ## 4     4 10.1016~ Mexico           Central and Sou~ Mexico        Mexico         
    ## 5     5 10.1029~ South Africa     Africa           Germany, Sou~ South Africa   
    ## 6     6 10.1016~ Colombia         Central and Sou~ Colombia, Si~ Colombia       
    ## # ... with 1 more variable: Region_1st <chr>

Now we can do some stats and graphs!

``` r
# add TRUE/FALSE column to master
## does affiliation of author match specimen? (1 line per affiliation)
master$national <- master$specimen_country == master$Affiliation
## does affiliation of 1st author match specimen country?
master$national_1st <- master$specimen_country == master$Affiliation_1st

# summarise affiliation data
master_forstats <- master %>%
  # group by ID so end up with 1 row per record
  group_by(ID) %>%
  # use sum to detect if 1st author is local and if any author is local
  # sum counts TRUE as 1 and FALSE as 0
  summarise(Local_1st=sum(national_1st)/n(), Local_involved=sum(national)) %>%
  # join to master_short to get access to other fields (lost during summary)
  inner_join(master_short, by="ID") %>%
  # categorise research as Domestic, foreign with local(s) involved, or solely foreign
  mutate(category=case_when(Local_1st==1 ~ "Domestic",
                            Local_1st==0 & Local_involved == 1 ~ "Foreign_involved",
                            Local_1st==0 & Local_involved == 0 ~ "Foreign",
                            TRUE ~ "Other"))%>%
  select(ID, Affiliation_1st, Affiliation, specimen_country, category, Region_1st, specimen_region)
```

Stats by specimen location

``` r
# stats by specimen location
stats_specimen_long <- master_forstats%>%
  # need to make grouping variables factors or .drop option in group_by doesn't work
  mutate(specimen_country=factor(specimen_country, levels=unique(countries$Preferred.Name)),
         category=as.factor(category))%>%
  group_by(specimen_country, category, .drop=FALSE)%>% # .drop = F is important to preserve cases where N=0
  summarise(N=n())%>%
  ungroup()
```

    ## `summarise()` has grouped output by 'specimen_country'. You can override using the `.groups` argument.

``` r
stats_specimen <- stats_specimen_long %>%
  pivot_wider(names_from = category, values_from = N)%>%
  mutate(Total=Domestic+Foreign+Foreign_involved,
         Parachute=log(ifelse(Total==0,NA,(Domestic+Foreign_involved)/Foreign)))
```

Stats by specimen region

    ## `summarise()` has grouped output by 'specimen_region'. You can override using the `.groups` argument.

Stats by first author

``` r
# stats by 1st author affiliation
stats_affiliation_long <-master_forstats %>%
  mutate(Affiliation_1st=factor(Affiliation_1st, levels=unique(countries$Preferred.Name)),
         category=as.factor(category))%>%
  group_by(Affiliation_1st, category, .drop=FALSE)%>%
  summarise(N=n())%>%
  ungroup()
```

    ## `summarise()` has grouped output by 'Affiliation_1st'. You can override using the `.groups` argument.

``` r
stats_affiliation <- stats_affiliation_long %>%
  pivot_wider(names_from = category, values_from = N)%>%
  mutate(Total=Domestic+Foreign+Foreign_involved,
         Prop_foreign=ifelse(Total==0,NA,(Foreign+Foreign_involved)/Total),
         Prop_incl_local=ifelse(Total==0,NA,Foreign_involved/(Foreign+Foreign_involved)))
```

# Blog post

The following data analysis is also published as a blog with neater
graphs here: <https://geocolab.github.io/blog/blog2/>

## Exploring parachute science

An important step for us in looking at how to address inequalities in
access was to demonstrate quantitatively the existence and effects of
such inequalities in analytical geoscience. In particular we have
focused on exploring the extent of parachute science in analytical
geoscience. Parachute science refers to science conducted by researchers
from another country from that in which they are working, without local
involvement. It’s negative effects include: concentration of expertise
(or perceived expertise) away from the areas or communities which are
subject to that expertise; ignorance of or exploitation of important
local knowledge; lack of credit to local collaborators who may be
instrumental to successful field trips among other things. We focused on
parachute science as it is relatively easy to explore through
quantitative data from published and freely available sources and
therefore could be rigorously studied in the timeframe of the hackathon.

In order to do this, we turned to the PetDB database. This is an online
open access database of igneous geochemistry data. The big advantage of
using this as the starting point for our analysis was that each sample
recorded in the database has an associated grid reference. This greatly
simplified the job of determining which country an article was about.
Determining this information directly from the title or tags of an
article would be extremely hard to automate reliably and very slow to do
manually.

## Data extraction

The data for all records in the PetDB database were scraped using a
Python script and the country of the samples determined from the grid
reference (for the purposes of our analysis we included any seafloor
samples collected within the exclusive economic zone of a country as
well as onshore samples). The DOIs were used to manually search the Web
of Science database, returning 1493 records with valid addresses. The
affiliation of first author and of the full author list was determined
automatically using a script in the R programming language to string
match the addresses to a list of all world countries. This was followed
by several stages of semi-automated clean up to deal with mistakes and
inconsistencies in the data. At each stage during development of the
code, 50 records were randomly chosen and manually checked to assess the
accuracy of the matching and only when no problems were found was it
judged to be complete. The country for samples, 1st author and all
authors were finally standardised to a list of preferred country names
(e.g. England, UK, Scotland all became United Kingdom) to allow reliable
matching of names.

## Analysing the data

The final dataset consists of 1426 references for which the affiliation
of all authors and country could be determined (70 records returned by
Web of Science included no address data). We categorised each as either
Domestic, where the first author affiliation and country of sample
origin matched; Foreign with local involvement, where the affiliation of
someone other than the first author matched the country the samples were
from; or Foreign where none of the authors’ affiliations matched the
sample country. To understand if the extent of parachuate science varied
by country, we summarised the data by country of sample origin and
calculated for each country the parachute index, defined as the log of
the ratio of publications (concerning a given country) which include
local researchers to those which include no local researchers (Raja et
al., 2021, <https://doi.org/10.31223/X5802N>). We also summarise the
data by the country of the first author’s affiliation and calculated for
each country the proportion of foreign research and the proportion of
that foreign research which included local researchers.

## Where are the samples from?

![](PetDB_data_analysis_files/figure-markdown_github/sample%20total-1.png)

The largest number of publications concern samples from the United
States and China (N\>100) with Russia, South Africa and Canada (N\>50)
and Japan, Italy, Ethiopia, Australia, Ecuador, Portugal and Oman all
well represented (N\>25).

Just from this graph, Ethiopia, Oman, Mexico, and Kenya jump out as
being highly represented in the form of samples but (as we will see)
little represented in the form of authorship and particularly first
authorship. This might tend to suggest issues of “parachute science”.

This will explored in more detail below.

## Parachute index

Following the approach of Raja et al. (2021 preprint, available here:
<https://doi.org/10.31223/X5802N>). We have calculated for each country
**sampled** the “parachute index”. This is defined as the log of the
ratio of publications including local researchers to those not including
researchers.

Negative numbers tend to imply a greater degree of parachute science
while positive numbers imply a higher degree of domestic research
(i.e. research on samples from a given country by researchers in that
country)

Plotting the parachute index by country of sample origin

    ## Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please
    ## use `guide = "none"` instead.

![](PetDB_data_analysis_files/figure-markdown_github/parachute%20index-1.png)

There are two main things clear from this plot. Firstly, notwithstanding
a few countries with high positive indices but few total publications at
the far left of the plot, the countries with positive indices (showing
high rates of local involvement) are dominated by those from Europe,
North America and Asia (pale blues and purple) as well as New Zealand
and Australia. At the other end of the spectrum on the right hand side
of the graph, countries with negative parachute indices (showing high
levels of parachute science with no local involvement) tend to be in
Africa, Central and South America, the Caribbean and Oceania.

It is particlarly notable just how many countries do not have a single
publication with a local co-author. In several cases, including
Botswana, Tonga and the N Marianas Islands this is despite 10 or more
references in the database concering those countries.

## Stats by region

Since there are quite a few countries with a small overall number of
publications, these tendencies are perhaps more clearly shown by
summarising the parachute index data by regions and plotting the
results.

![](PetDB_data_analysis_files/figure-markdown_github/parachute%20index%20regional-1.png)

## Which countries do the most research in other countries?

It’s also useful to look at the country of the first author. Where are
the places doing the most parachute science? (rather than subject to it
as the above graphs illustrate)

First we recalculate similar summary statistics to those above for
samples’ country and region of origin. This time we use the affiliation
of the 1st author of each publication as the variable to summarise on.

Instead of parachute index we calculate simply the proportion of foreign
research conducted (relative to the total number of publications from
that country) and the proportion of that foreign research in which there
was at least 1 local collaborator included as an author.

First we plot a bar chart showing the total publications from each
country categorised as domestic, foreign and foreign with local
involvement:

![](PetDB_data_analysis_files/figure-markdown_github/plot%20research%20categories-1.png)

Is is clear from this chart that foreign research makes up a significant
proportion of the publications for most of the countries with the
largest numbers of publications (\>50 publications). In particular, we
can see that domestic research makes up a tiny proportion of the total
output from the UK, France and Germany but that these countries have a
large number of 1st authored papers, second only to the USA and China.

The USA and China have a somewhat larger proportion of domestic research
and are the best represented in the dataset in terms of nubmer of first
authored publications.

To look in more detail at the breakdown of research conducted abroad
versus domestically we can plot the proportion of foreign research
calculated above by country

![](PetDB_data_analysis_files/figure-markdown_github/plot%20prop.%20foreign-1.png)

This plot reiterates several of the same features as the previous one. A
large proportion of the published outputs from many European countries -
including the UK, France and Germany - conduct a large proportion (\>75%
in most cases) of research abroad.

At the other end of the scale, on the left of the graph, countries with
a large proportion of research conducted domestically are more typically
in Africa, Asia or South America though Greece and Portugal also feature
at this end of the graph.

A number of countries at the right of the graph show 100% of foreign
research. Many of these however are represented by only a handful of
publications (purple colours) and may not be particularly
representative.

Lastly we can look at the proportion of foreign research which includes
local collaborators:

![](PetDB_data_analysis_files/figure-markdown_github/plot%20prop%20incl.%20local-1.png)

Of countries with a large number of publications based on foreign
research (green and yellow colours), the USA, Australia, Germany, United
Kingdom show the lowest apparent engagement with local researchers with
\<30% of publications including a local collaborator. Japan, China,
Italy, France, South Africa and Canada are only slightly better with
30-50% of publications featuring a local co-author.

A number of countries with 100% local collaboration have low number of
total foreign research publication (purple colours) as in the previous
plot and may not be representative.

On the limited data available, Mexico, Brazil, Czech Republic and Eqypt
appear to show a high rate of engagement with local collaborators during
foreign research. However, more data would be needed to establish this
was representative.
