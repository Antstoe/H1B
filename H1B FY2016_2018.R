library(dplyr)
library(ggplot2)
library(readxl)
library(hashmap)
library(ggmap)
#Empty data frame
h1b_df = data.frame()

for(year in seq(2018,2016)) {
  
  print(paste0("Reading ", year, " records .."))
  raw_data_path = file.path("C:", "Users", "Antstoe", "Desktop", "School", "Bootcamp", "SpringBoard", "CSV - testing", "Excel",year,"_raw_data.xlsx")
  new_df = read_excel(raw_data_path)
  
  print(paste0("Raw data size: ", as.character(dim(new_df))))  

  
  # Adding Year column to dataframe
  print("Mutating year ..")
  new_df = new_df %>% 
    mutate(YEAR = as.character(year))
  
  print(paste0("Mutated data size: ", as.character(dim(new_df))))  
  
  # Selecting only the relevant columns
  new_df = new_df %>% 
    select(CASE_NUMBER, 
           CASE_STATUS,
           EMPLOYER_NAME,
           SOC_NAME,
           SOC_CODE,
           JOB_TITLE,
           FULL_TIME_POSITION,
           PREVAILING_WAGE,
           PW_UNIT_OF_PAY,
           WORKSITE_CITY,
           WORKSITE_STATE,
           YEAR)
  
  # Merging data with already transformed data
  print("Merging data ..")
  h1b_df = rbind(h1b_df, new_df)
  
  print(paste0("Merged data size: ",as.character(dim(h1b_df))))
}

# Saving read data frame 
saveRDS(h1b_df,"h1b_df_no_transform.rds")

# h1b_df_tx will undergo all transformations
h1b_df_tx <- h1b_df
colnames(h1b_df_tx)

h1b_df_tx %>%
  group_by(PW_UNIT_OF_PAY) %>%
  summarise(count = n(), percentage = 100*count/(dim(h1b_df_tx)[1]))

pw_unit_to_yearly <- function(prevailing_wage, pw_unit_of_pay) {
  return(ifelse(pw_unit_of_pay == "Year", 
                prevailing_wage, 
                ifelse(pw_unit_of_pay == "Hour", 
                       2080*prevailing_wage, 
                       ifelse(pw_unit_of_pay== "Week", 
                              52*prevailing_wage, 
                              ifelse(pw_unit_of_pay == "Month", 
                                     12*prevailing_wage, 
                                     26*prevailing_wage)))))
}


h1b_df_tx %>%
  filter(!is.na(PW_UNIT_OF_PAY)) %>%
  mutate(PREVAILING_WAGE = as.numeric(PREVAILING_WAGE)) %>%
  mutate(PREVAILING_WAGE =  pw_unit_to_yearly(PREVAILING_WAGE, PW_UNIT_OF_PAY)) %>%
  select(- PW_UNIT_OF_PAY) -> h1b_df_tx

h1b_df_tx %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise(count = n(),percentage = 100*count/(dim(h1b_df_tx)[1]))

# Generic ggplot graphics configuration I will be using for all my plots
get_theme <- function() {
  return(theme(axis.title = element_text(size = rel(1.5)),
               legend.position = "bottom",
               legend.text = element_text(size = rel(1.5)),
               legend.title = element_text(size=rel(1.5)),
               axis.text = element_text(size=rel(1.5)))) 
}

# Avoid scientific notation in plot
options(scipen = 999)

g <- ggplot(data = h1b_df_tx, aes(x=YEAR, y = PREVAILING_WAGE))
g <- g + geom_boxplot(aes(fill=FULL_TIME_POSITION)) + coord_cartesian(ylim=c(0,125000))
g <- g + xlab("YEAR") + ylab("WAGE (USD)") + get_theme()

g 

h1b_df_tx %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise('75%' = quantile(PREVAILING_WAGE,probs = 0.75,na.rm=TRUE))

h1b_df_tx %>% 
  mutate(FULL_TIME_POSITION = ifelse(is.na(FULL_TIME_POSITION), 
                                     ifelse(PREVAILING_WAGE > 70000,'Y','N'), 
                                     FULL_TIME_POSITION)) -> h1b_df_tx

# naribole Worksite Mutation
split_first <- function(word, split = " ") {
  return(strsplit(word,split= split)[[1]][1])
}

h1b_df_tx$WORKSITE_CITY <- sapply(h1b_df_tx$WORKSITE_CITY,split_first, split=",")

#read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
state_abbs = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
               "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
               "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
               "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
               "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

state_full = c("alaska","alabama","arkansas","arizona","california","colorado",
               "connecticut","district of columbia","delaware","florida","georgia",
               "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
               "louisiana","massachusetts","maryland","maine","michigan","minnesota",
               "missouri","mississippi","montana","north carolina","north dakota",
               "nebraska","new hampshire","new jersey","new mexico","nevada",
               "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
               "rhode island","south carolina","south dakota","tennessee","texas",
               "utah","virginia","vermont","washington","wisconsin",
               "west virginia","wyoming")

state_hash = hashmap(state_abbs,state_full)
h1b_df_tx$WORKSITE_STATE_FULL = sapply(h1b_df_tx$WORKSITE_STATE, function(x,y) {return(toupper(y[[x]]))}, y = state_hash)

site_merge <- function(x,y) {
  return(paste0(x,", ",y))
}

h1b_df_tx %>%
  rename(WORKSITE_STATE_ABB = WORKSITE_STATE) -> h1b_df_tx

h1b_df_tx$WORKSITE = mapply(site_merge,h1b_df_tx$WORKSITE_CITY,h1b_df_tx$WORKSITE_STATE_FULL)

#Worksite Spell Checker
wrong_names = c("NEW YROK, NEW YORK", "SUUNYVALE, CALIFORNIA", "SAN FRANSISCO, CALIFORNIA")

h1b_df_tx %>% 
  filter(WORKSITE %in% wrong_names) %>%
  group_by(WORKSITE) %>%
  summarise(count = n())

h1b_df_tx %>% 
  group_by(WORKSITE) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) -> sites_count

site_hash = hashmap(sites_count$WORKSITE, sites_count$count)

get_inserts <- function(split_left,split_right, i, letters) {
  # Generate insertions of a single letter
  return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = split_right[i])))
}

get_deletes <- function(split_left,split_right, i) {
  # Generate deletion of one letter from word
  return(paste0(split_left[i], substr(split_right[i],2,nchar(split_right[i]))))
}

get_replaces <- function(split_left,split_right, i,letters) {
  # Generate replacement of a letter by a-z or space
  if(!is.null(split_right[i]) &  nchar(split_right[i]) > 0) {
    return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = substr(split_right[i],2,nchar(split_right[i])))))
  }
  return(NULL)
}

get_transposes <- function(split_left, split_right,i) {
  # Generate interchanging of the positions of adjacent letters
  if(!is.null(split_right[i]) & nchar(split_right[i]) > 1) {
    return(paste0(split_left[i],substr(split_right[i],2,2),substr(split_right[i],1,1),substr(split_right[i],3,nchar(split_right[i]))))
  }
  return(NULL)
}

edits1site <- function(site) {
  # All edits that are one edit away from site
  letters = toupper(strsplit("abcdefghijklmnopqrstuvwxyz ",split='')[[1]])
  site_len <- nchar(site)
  #print(site_len)
  if(site_len < 4) {
    return(site)
  }
  split_left <- sapply(seq(0,site_len), substr,x = site,start = 1)
  split_right <- sapply(seq(1,site_len+1), substr,x = site,stop = site_len) 
  deletes <- sapply(seq(1,site_len+1),get_deletes, split_left = split_left, split_right = split_right)
  transposes <- unlist(sapply(seq(1,site_len+1),get_transposes, split_left = split_left, split_right = split_right))
  replaces <- unlist(sapply(seq(1,site_len+1),get_replaces, split_left = split_left, split_right = split_right, letters=letters))
  inserts <- unlist(sapply(seq(1,site_len+1),get_inserts, split_left = split_left, split_right = split_right,letters = letters))
  
  return(unique(c(deletes,transposes,replaces,inserts)))
}

edits2site <- function(site) { 
  # All edits that are two edits away from `word`
  edits1_sites = edits1site(site)
  return (unlist(sapply(edits1_sites, edits1site)))
}

get_prob <- function(site, site_hash) {
  # probability of site in our dataset
  return(site_hash[[site]])
}

known <- function(sites,site_hash = site_hash) {
  # The subset of candidate sites that appear in the dictionary of sites
  return(sites[site_hash$has_keys(sites)])
}

find_candidates <- function(site,...) {
  # Generate possible spelling corrections for word
  return(c(known(site,...), known(edits1site(site),...), c(site)))
}

site_spell_correcter <- function(site,...) {
  # best possible correction to the site
  candidates = find_candidates(site,...)
  best_candi = candidates[which.max(sapply(candidates,get_prob, ...))]
  
  #if(get_prob(best_candi,...) > get_prob(site,...) ) {
  #  return(best_candi)
  #}
  return(best_candi)
}

site_count <- function(site, site_hash) {
  
  if(site_hash$has_key(site)) {
    return(site_hash[[site]])
  }
  return(site)
}

sites <- sites_count$WORKSITE
sites_before <- c()
sites_after <- c()
count <- 0

for(site in sites) {
  # Count of current Worksite
  curr_count <- site_count(site,site_hash)
  #print(paste0(site, ", ",curr_count))
  
  if(curr_count < 100) { # Threshold
    #print(paste0(site, ", ",curr_count))
    corrected <- site_spell_correcter(site,site_hash)
    
    if(corrected != site) { # Correction occurred
      count <- count + 1
      sites_before[count] <- site
      sites_after[count] <- corrected
      corrected_count <- site_count(corrected,site_hash)
      #print(paste0(site, " : ", curr_count,", ",corrected, " : ", corrected_count))
    }
  }  
}

sites_corrected_hash <- hashmap(sites_before,sites_after)

print(paste0("Number of worksite spelling corrections: ", length(sites_after)))

#Merging corrected versions of worksite values
worksite_correct <- function(x, hash) {
  if(hash$has_key(x)) {
    return(hash[[x]])
  }
  return(x)
}

h1b_df_tx$WORKSITE_CORRECTED <- sapply(h1b_df_tx$WORKSITE,worksite_correct,hash=sites_corrected_hash)

h1b_df_tx %>%
  select(-WORKSITE) %>%
  rename(WORKSITE = WORKSITE_CORRECTED) -> h1b_df_tx

#Geocoding find longitude and latitude
library(ggmap)

top_sites <- (sites_count$WORKSITE)[1:2500]

site_geocodes <- cbind(geocode(top_sites),top_sites)

site_geocodes %>%
  rename(WORKSITE = top_sites) -> site_geocodes

saveRDS(site_geocodes,"geocodes.RDS")

N_sites_geocoded <- dim(site_geocodes)[1]

share <- 100*sum((sites_count$count)[1:N_sites_geocoded])/(dim(h1b_df_tx)[1])

print(paste0("Records captured by geocoded sites: ", share))

h1b_df_tx <- full_join(h1b_df_tx,site_geocodes,by="WORKSITE")

head(h1b_df_tx)

#Cost of Living Index
coli_data = read.csv("data/coli_data.csv", stringsAsFactors = FALSE)

coli_data <- coli_data %>% 
  select(WORKSITE_CITY = city, COLI = coli, WORKSITE_STATE_ABB = state)

coli_data$WORKSITE_STATE_FULL = sapply(coli_data$WORKSITE_STATE_ABB, function(x,y) {return(toupper(y[[x]]))}, y = state_hash)

coli_data$WORKSITE = mapply(site_merge,toupper(coli_data$WORKSITE_CITY),coli_data$WORKSITE_STATE_FULL)

coli_data %>%
  select(WORKSITE, COLI) -> coli_data

head(coli_data)

#Merge coli dataframe to main dataframe
h1b_df_tx <- left_join(h1b_df_tx,coli_data, by = "WORKSITE")

saveRDS(h1b_df_tx,"h1b_transformed.rds")

#h1b_df_tx %>%
#  select(-PW_UNIT_OF_PAY, -SOC_NAME,-CASE_NUMBER,-WORKSITE_CITY) -> h1b_compact

#saveRDS(h1b_compact, "h1b_compact.rds")

# Saving my data frame as CSV
write.csv(h1b_df, '/Users/Antstoe/Desktop/School/Bootcamp/SpringBoard/CSV - testing/Excel/FinalH1BDF2.csv', row.names=T)
