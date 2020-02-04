library(data.table)
library(dplyr)

varnames = fread("variable_name_lookup.csv", na.strings = c(""))

census2018 = fread("/home/aaron/Documents/census/Data/2018/online survey/csv/Clean2018CensusFulltabJan2019.tsv", 
                   sep = "\t", na.strings = c(""))
census2017 = fread("/home/aaron/Documents/census/Data/2017/online survey/csv/Clean2017CensusFulltabMar2018.tsv",
                   sep = "\t", na.strings = c(""))
census2016 = fread("/home/aaron/Documents/census/Data/2016/online survey/csv/Census2016Fulltab.tsv",
                   sep = "\t", na.strings = c(""))
# census2015 = fread("C:\\Users\\Aaron\\Documents\\census\\data2\\2015\\Online survey\\csv\\Clean2015CensusFulltabJul2018.tsv",
#                    sep = "\t")
# census2014 = fread("C:\\Users\\Aaron\\Documents\\census\\data2\\2014\\Online survey\\csv\\Census2014Fulltab.tsv",
#                    sep = "\t")
# census2013 = fread("C:\\Users\\Aaron\\Documents\\census\\data2\\2013\\Online survey\\csv\\Census2013Fulltab.tsv",
#                    sep = "\t")

namesWant = intersect(
  intersect(
    intersect(varnames$varnames[!is.na(varnames$label)],
              names(census2018)),
    names(census2017)),
  names(census2016))

census2018.s = filter(census2018, !is.na(weightbfpublic)) %>%
  select(which(names(census2018) %in% namesWant))
census2018.s$weights = census2018$weightbfpublic[
  !is.na(census2018$weightbfpublic)]
census2018.s$normWeights = census2018.s$weights / sum(census2018.s$weights)

census2017.s = filter(census2017, !is.na(weightbfarrival)) %>%
  select(which(names(census2017) %in% namesWant))
census2017.s$weights = census2017$weightbfarrival[
  !is.na(census2017$weightbfarrival)]
census2017.s$normWeights = census2017.s$weights / sum(census2017.s$weights)

census2016.s = filter(census2016, !is.na(weightbmorg1)) %>%
  select(which(names(census2016) %in% namesWant))
census2016.s$weights = census2016$weightbmorg1[
  !is.na(census2016$weightbmorg1)]
census2016.s$normWeights = census2016.s$weights / sum(census2016.s$weights)

censusResults = list("2018" = census2018.s,
                 "2017" = census2017.s,
                 "2016" = census2017.s)



save(censusResults, file = "census_3yr_dashboard_v2.RData")
# 
# fwrite(census2018.s, file="census_2018_dashboard.csv", sep = ",")
# fwrite(census2017.s, file="census_2017_dashboard.csv", sep = ",")
# fwrite(census2016.s, file="census_2016_dashboard.csv", sep = ",")
# 
# 
# 
# fwrite(censusResults, file="census_3yr_dashboard.csv", sep = ",")
# save(censusResults, file = "census_3yr_dashboard.RData")
# varnames2 = varnames[varnames %in% namesWant]
# fwrite(variable_name_lookup_v2.csv)

