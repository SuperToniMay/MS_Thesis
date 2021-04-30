# for each animal-observation ----
# 1) extract all grps observed on that day
# 2) build "metagrp" of unobserved -- observed/not needs to be a predictor
# 3) build grp-specific, individual-specific, observation-specific predictors


# target predictors ----
# -- is your [mom/grandma/daughter(s)] in the group?
# -- What proportion of your cohort is here?
# -- What proportion of the grp matches your current reproductive status?
# -- How big is the group
# -- (distance??)
# -- how many rams
# -- how many sibs/half sibs (and/or some other measure of "relatedness" to the group)
# -- PLUS OTHERS IN RUT

# individual locations are in census. 
census <- read.csv("census_withLatLong_20200511.csv", header = T)

# cut to just 2009 to dry-run
y09 <- subset(census, year == 2009)

# extract the first individual
y09[1, ]

# get all observations on that date.
day_obs <- y09[which(y09$date == y09$date[1]), ] # which pulls all observations from that day; y09 maps them to rows in y09
day_obs$groupid <- factor(day_obs$groupid)

# extract all groups on that day
day_grps <- levels(factor(day_obs$groupid)) 

# add a "group" for unobserved animals
# what I really need is a day-specific list of who's alive
# FOR NOW, just use everyone in y09
y09_animals <- levels(factor(y09$fulltag_clean))
unobs <- y09_animals[!(y09_animals %in% day_obs$fulltag_clean)]
# we should maybe revisit putting all those animals into one grp vs. many grps...
# but, leaving it for now. 

# add unobs grp onto day_grps
day_grps <- c(day_grps, "unobs")

# make a row for each group for the focal individual
pec_day1_grps <- expand.grid(y09$fulltag_clean[1], day_grps) # -- might need to be changed with multiple groups
names(pec_day1_grps) <- c("fulltag_clean", "grp")

# add a column indicating which group Pec is in:
pec_day1_grps$present <- ifelse(as.character(pec_day1_grps$grp) == as.character(y09$groupid[1]), 1, 0)
pec_day1_grps$grpsize <- c(table(factor(day_obs$groupid)), length(unobs))

# bring in BRPOP
brpop <- read.csv("BRPOP_v02.csv", header = T)
k <- subset(brpop, fulltag == "PECCARY")
pop09 <- subset(brpop, (yearb <= 2009 & yeard >= 2009) | (yearb <= 2009 & is.na(yeard) == T) | (is.na(yearb) == T & yeard >= 2009))
pec_data <- pop09[which(as.character(pop09$fulltag) == as.character(y09$fulltag_clean[1])), ]
# is Pec's mom in pop09?
pecsmom <- pec_data$mother_idno
pecs_mom_09 <- subset(pop09, as.character(idno) == as.character(pecsmom))$name
pec_sibs <- pop09[which(as.character(pop09$mother_idno) == as.character(pec_data$mother_idno)), ]

# check on presence of maternal grandmother
pec_grandma_idno <- brpop[which(as.character(brpop$idno) == as.character(pec_data$mother_idno)), ]$mother_idno
brpop_pec_grandma <- brpop[which(as.character(brpop$idno) == as.character(pec_grandma_idno)), ]
grandma_in_pop09 <- pop09[which(as.character(pop09$idno) == as.character(pec_grandma_idno))]

# get pec offspring
pec_offspring <- pop09[which(as.character(pop09$mother_idno) == as.character(pec_data$idno)), ]

# build covariates
# mom indicator
mom_grp <- subset(day_obs, as.character(fulltag_clean) == as.character(pecs_mom_09))$groupid
table(mom_grp)
pec_day1_grps$mom_in_grp <- rep(0, dim(pec_day1_grps)[1])
pec_day1_grps$mom_in_grp[ifelse(length(mom_grp) == 0, dim(pec_day1_grps)[1], mom_grp)] <- 1

# number of female sibs in grp
fem_sibs_day <- subset(day_obs, as.character(fulltag_clean) %in% as.character(pec_sibs$fulltag))
fem_sibs_tab <- table(fem_sibs_day$groupid)
pec_day1_grps$fem_sibs_in_grp <- c(fem_sibs_tab, dim(pec_sibs)[1] - sum(fem_sibs_tab))

# extract # individuals in your cohort that are in your grp (and in other groups)
# get focal animal's birth year
pec_data$yearb
# need birth year for everybody in each group (or rather, to know who in each grp was born in pec_data$yearb)
pec_grp_by <- pop09$yearb[which(as.character(pop09$fulltag) %in% as.character(day_obs$fulltag_clean))]
pec_cohort_in_pec_grp <- length(which(pec_grp_by == pec_data$yearb))
other_grps_by <- pop09$yearb[(which(as.character(pop09$fulltag) %in% as.character(day_obs$fulltag_clean) == FALSE))]
pec_cohort_not_in_pec_grp <- length(which(other_grps_by == pec_data$yearb))
pec_day1_grps$cohort <- c(pec_cohort_in_pec_grp, pec_cohort_not_in_pec_grp)

# loop over all rows in census in 2009. build a dataframe
# just like pec_day1_grp for each obs
# rbind together all those dataframes
indobs_grp_data <- vector("list", dim(y09)[1])

brpop <- read.csv("BRPOP_v02.csv", header = T)
pop09 <- subset(brpop, (yearb <= 2009 & yeard >= 2009) | (yearb <= 2009 & is.na(yeard) == T) | (is.na(yearb) == T & yeard >= 2009))
pop09_reduced <- subset(pop09, fulltag != "")

# y09   = census data for 2009
# pop09 = pop data for 2009

# add birth year for each animal-day onto the census (y09)
y09$yearb <- y09$demog_class <- rep(NA, dim(y09)[1])
for(i in 1:dim(y09)[1]){
  y09$yearb[i] <- pop09$yearb[which(as.character(pop09$fulltag) == as.character(y09$fulltag_clean)[i])]
  # y09$ad_sex[i] <- as.character(pop09$sex[which(as.character(pop09$fulltag) == 
  #                                                 as.character(y09$fulltag_clean)[i])])
  y09$demog_class[i] <- as.character(pop09$sex[which(as.character(pop09$fulltag) == 
                                                       as.character(y09$fulltag_clean)[i])])
  y09$demog_class[i] <- ifelse(y09$demog_class[i] == "" & as.character(y09$fulltag)[i] == "LAMB", "LAMB", as.character(y09$demog_class[i]))
  
  #  y09$lambs[i] <- as.character(pop09$sex[which(as.character(pop09$fulltag) == as.character(y09$fulltag_clean)[i])])
}

indobs_grp_data <- vector("list", dim(y09)[1])

year_in <- 2009

# for(i in 1:dim(y09)[1]){
#   # get all observations on that date.
#   day_obs <- y09[which(y09$date == y09$date[i]), ] # which pulls all observations from that day; y09 maps them to rows in y09
#   day_obs$groupid <- factor(day_obs$groupid)
# 
#   # extract all groups on that day
#   day_grps <- levels(factor(day_obs$groupid))
# 
#   # add a "group" for unobserved animals
#   # what I really need is a day-specific list of who's alive
#   # FOR NOW, just use everyone in y09
#   y09_animals <- levels(factor(y09$fulltag_clean))
#   unobs <- y09_animals[!(y09_animals %in% day_obs$fulltag_clean)]
#   # we should maybe revisit putting all those animals into one grp vs. many grps...
#   # but, leaving it for now.
# 
#   # add unobs grp onto day_grps
#   day_grps <- c(day_grps, "unobs")
# 
#   # bring in BRPOP
#   if(as.character(y09$fulltag_clean[i]) %in% as.character(pop09_reduced$fulltag)){
# 
#     # make a row for each group for the focal individual
#     indobs_grp_data[[i]] <- expand.grid(y09$fulltag_clean[i], day_grps) # -- might need to be changed with multiple groups
#     names(indobs_grp_data[[i]]) <- c("fulltag_clean", "grp")
# 
#     # add a column indicating which group Pec is in:
#     indobs_grp_data[[i]]$present <- ifelse(as.character(indobs_grp_data[[i]]$grp) == as.character(y09$groupid[i]), 1, 0)
#     indobs_grp_data[[i]]$grpsize <- c(table(factor(day_obs$groupid)), length(unobs))
# 
#     k <- subset(brpop, as.character(fulltag) == as.character(y09$fulltag_clean[i]))
#     individ_data <- pop09[which(as.character(pop09$fulltag) == as.character(y09$fulltag_clean[i])), ]
#     # is Pec's mom in pop09?
#     individ_mom <- individ_data$mother_idno
#     individ_mom_09 <- subset(pop09, as.character(idno) == as.character(individ_mom))$name
#     individ_sibs <- pop09[which(as.character(pop09$mother_idno) == as.character(individ_data$mother_idno)), ]
# 
#     # check on presence of maternal grandmother
#     individ_grandma_idno <- brpop[which(as.character(brpop$idno) == as.character(individ_data$mother_idno)), ]$mother_idno
#     brpop_individ_grandma <- brpop[which(as.character(brpop$idno) == as.character(individ_grandma_idno)), ]
#     grandma_in_pop09 <- pop09[which(as.character(pop09$idno) == as.character(individ_grandma_idno))]
# 
#     # get pec offspring
#     individ_offspring <- pop09[which(as.character(pop09$mother_idno) == as.character(individ_data$idno)), ]
# 
#     # build covariates
#     # mom indicator
#     mom_grp <- subset(day_obs, as.character(fulltag_clean) == as.character(individ_mom_09))$groupid
#     table(mom_grp)
#     indobs_grp_data[[i]]$mom_in_grp <- rep(0, dim(indobs_grp_data[[i]])[1])
#     indobs_grp_data[[i]]$mom_in_grp[ifelse(length(mom_grp) == 0, dim(indobs_grp_data[[i]])[1], mom_grp)] <- 1
# 
#     # number of female sibs in grp
#     fem_sibs_day <- subset(day_obs, as.character(fulltag_clean) %in% as.character(individ_sibs$fulltag))
#     fem_sibs_tab <- table(fem_sibs_day$groupid)
#     indobs_grp_data[[i]]$fem_sibs_in_grp <- c(fem_sibs_tab, dim(individ_sibs)[1] - sum(fem_sibs_tab))
# 
#     # count of animals in focal animal cohort (NEW 2020-11-06) ----
#     focal_animal_cohort <- k$yearb
#     # make an indicator for all obs in day_obs of whether they're from focal animal cohort
#     grp_cohort_tab <- table(day_obs$groupid, factor(day_obs$yearb == focal_animal_cohort,
#                                                     levels = c("FALSE", "TRUE")))[, 2]
#     # get unobserved in cohort
#     unobserved <- pop09[(which(as.character(pop09$fulltag) %in% as.character(day_obs$fulltag_clean) == FALSE)), ]
#     indobs_grp_data[[i]]$same_cohort_count <- c(grp_cohort_tab,
#                                                 table(factor(unobserved$yearb == focal_animal_cohort,
#                                                              levels = c("FALSE", "TRUE")))["TRUE"]) # this is the number of unobserved animals from cohort
# 
#     # get sex/lamb structure of each group
#     grp_demog_tab <- table(day_obs$groupid, factor(day_obs$demog_class, levels = c("F", "M", "LAMB", "")))
#     unobs_demog_tab <- table(factor(unobserved$sex, levels = c("F", "M", "")))
#     indobs_grp_data[[i]]$no_ewes <- c(grp_demog_tab[, 1], unobs_demog_tab[1])
#     indobs_grp_data[[i]]$no_rams <- c(grp_demog_tab[, 2], unobs_demog_tab[2])
#     indobs_grp_data[[i]]$no_lambs <- c(grp_demog_tab[, 3], 0)
# 
#     ## individual-level traits that are constant for all possible group selections within an individual
#     # individual's age
#     indobs_grp_data[[i]]$focal_animal_age <- rep(year_in - focal_animal_cohort, dim(indobs_grp_data[[i]])[1])
# 
#     # does a given ewe have a lamb?
# 
# 
#   }
#   print(i)
# }
# 
# discrete_choice_data_09 <- do.call("rbind", indobs_grp_data)
# dim(discrete_choice_data_09)
# table(discrete_choice_data_09$present)
# 
# head(discrete_choice_data_09)


# KRM revisions to get in lamb status 2020-11-20 ----
# pec_lamb_boundary_dates <- subset(y09, fulltag_clean == "PECCARY" & lamb_present == "TRUE")
# first_date <- pec_lamb_boundary_dates$date[1]
# last_date <- pec_lamb_boundary_dates$date[dim(pec_lamb_boundary_dates)[1]]
# posix_first <- as.POSIXlt(as.character(first_date), format = "%m/%d/%Y")
# posix_last <- as.POSIXlt(as.character(last_date), format = "%m/%d/%Y")
# 
# ifelse(as.POSIXlt(as.character(y09$date[400]), format = "%m/%d/%Y") > posix_first & 
#               as.POSIXlt(as.character(y09$date[400]), format = "%m/%d/%Y") < posix_last, 1, 0)

for(i in 1:dim(y09)[1]){
  # get all observations on that date.
  day_obs <- y09[which(y09$date == y09$date[i]), ] # which pulls all observations from that day; y09 maps them to rows in y09
  day_obs$groupid <- factor(day_obs$groupid)
  
  # extract all groups on that day
  day_grps <- levels(factor(day_obs$groupid))
  
  # add a "group" for unobserved animals
  # what I really need is a day-specific list of who's alive
  # FOR NOW, just use everyone in y09
  y09_animals <- levels(factor(y09$fulltag_clean))
  unobs <- y09_animals[!(y09_animals %in% day_obs$fulltag_clean)]
  # we should maybe revisit putting all those animals into one grp vs. many grps...
  # but, leaving it for now.
  
  # add unobs grp onto day_grps
  day_grps <- c(day_grps, "unobs")
  
  # bring in BRPOP
  if(as.character(y09$fulltag_clean[i]) %in% as.character(pop09_reduced$fulltag)){
    
    # make a row for each group for the focal individual
    indobs_grp_data[[i]] <- expand.grid(y09$fulltag_clean[i], day_grps) # -- might need to be changed with multiple groups
    names(indobs_grp_data[[i]]) <- c("fulltag_clean", "grp")
    
    # extract observation's Julian date
    indobs_grp_data[[i]]$julian <- (as.POSIXlt(as.character(y09$date[i]), format = "%m/%d/%Y"))$yday
    
    # add a column indicating which group Pec is in:
    indobs_grp_data[[i]]$present <- ifelse(as.character(indobs_grp_data[[i]]$grp) == as.character(y09$groupid[i]), 1, 0)
    indobs_grp_data[[i]]$grpsize <- c(table(factor(day_obs$groupid)), length(unobs))
    
    k <- subset(brpop, as.character(fulltag) == as.character(y09$fulltag_clean[i]))
    individ_data <- pop09[which(as.character(pop09$fulltag) == as.character(y09$fulltag_clean[i])), ]
    # is Pec's mom in pop09?
    individ_mom <- individ_data$mother_idno
    individ_mom_09 <- subset(pop09, as.character(idno) == as.character(individ_mom))$name
    individ_sibs <- pop09[which(as.character(pop09$mother_idno) == as.character(individ_data$mother_idno)), ]
    
    # check on presence of maternal grandmother
    individ_grandma_idno <- brpop[which(as.character(brpop$idno) == as.character(individ_data$mother_idno)), ]$mother_idno
    brpop_individ_grandma <- brpop[which(as.character(brpop$idno) == as.character(individ_grandma_idno)), ]
    grandma_in_pop09 <- pop09[which(as.character(pop09$idno) == as.character(individ_grandma_idno))]
    
    # get pec offspring
    individ_offspring <- pop09[which(as.character(pop09$mother_idno) == as.character(individ_data$idno)), ]
    
    # build covariates
    # mom indicator
    mom_grp <- subset(day_obs, as.character(fulltag_clean) == as.character(individ_mom_09))$groupid
    table(mom_grp)
    indobs_grp_data[[i]]$mom_in_grp <- rep(0, dim(indobs_grp_data[[i]])[1])
    indobs_grp_data[[i]]$mom_in_grp[ifelse(length(mom_grp) == 0, dim(indobs_grp_data[[i]])[1], mom_grp)] <- 1
    
    # number of female sibs in grp
    fem_sibs_day <- subset(day_obs, as.character(fulltag_clean) %in% as.character(individ_sibs$fulltag))
    fem_sibs_tab <- table(fem_sibs_day$groupid)
    indobs_grp_data[[i]]$fem_sibs_in_grp <- c(fem_sibs_tab, dim(individ_sibs)[1] - sum(fem_sibs_tab))
    
    # count of animals in focal animal cohort (NEW 2020-11-06) ----
    focal_animal_cohort <- k$yearb
    # make an indicator for all obs in day_obs of whether they're from focal animal cohort
    grp_cohort_tab <- table(day_obs$groupid, factor(day_obs$yearb == focal_animal_cohort,
                                                    levels = c("FALSE", "TRUE")))[, 2]
    # get unobserved in cohort
    unobserved <- pop09[(which(as.character(pop09$fulltag) %in% as.character(day_obs$fulltag_clean) == FALSE)), ]
    indobs_grp_data[[i]]$same_cohort_count <- c(grp_cohort_tab,
                                                table(factor(unobserved$yearb == focal_animal_cohort,
                                                             levels = c("FALSE", "TRUE")))["TRUE"]) # this is the number of unobserved animals from cohort
    
    # get sex/lamb structure of each group
    grp_demog_tab <- table(day_obs$groupid, factor(day_obs$demog_class, levels = c("F", "M", "LAMB", "")))
    unobs_demog_tab <- table(factor(unobserved$sex, levels = c("F", "M", "")))
    indobs_grp_data[[i]]$no_ewes <- c(grp_demog_tab[, 1], unobs_demog_tab[1])
    indobs_grp_data[[i]]$no_rams <- c(grp_demog_tab[, 2], unobs_demog_tab[2])
    indobs_grp_data[[i]]$no_lambs <- c(grp_demog_tab[, 3], 0)
    
    ## individual-level traits that are constant for all possible group selections within an individual
    # individual's age
    indobs_grp_data[[i]]$focal_animal_age <- rep(year_in - focal_animal_cohort, dim(indobs_grp_data[[i]])[1])
    
    ## does a given animal have a lamb on this day?
    # initialize focal_lamb_status variable
    indobs_grp_data[[i]]$focal_lamb_status <- NA
    # extract dates on which this animal had a lamb using the lamb_present field in the census data
    ewe_lamb_boundary_dates <- subset(y09, fulltag_clean == as.character(y09$fulltag_clean[i]) & lamb_present == "TRUE")
    # assess whether any lamb dates are present and proceed
    if(dim(ewe_lamb_boundary_dates)[1] > 0){
      # extract the first date the ewe had a lamb
      first_date <- ewe_lamb_boundary_dates$date[1]
      # extract the last date the ewe had a lamb
      last_date <- ewe_lamb_boundary_dates$date[dim(ewe_lamb_boundary_dates)[1]]
      # convert first date to POSIX
      posix_first <- as.POSIXlt(as.character(first_date), format = "%m/%d/%Y")
      # convert last date to POSIX
      posix_last <- as.POSIXlt(as.character(last_date), format = "%m/%d/%Y")
      # test whether current date is between first and last date.  If yes, assign 1, otherwise 0
      indobs_grp_data[[i]]$focal_lamb_status <- ifelse(as.POSIXlt(as.character(y09$date[i]), format = "%m/%d/%Y") > posix_first &
                                                         as.POSIXlt(as.character(y09$date[i]), format = "%m/%d/%Y") < posix_last, 1, 0)
    }
    # Add value for choice set
    indobs_grp_data[[i]]$choice_set <- rep(i, dim(indobs_grp_data[[i]])[1])
    
    
  } # \if
  
  
  print(i)
}

discrete_choice_data_09 <- do.call("rbind", indobs_grp_data)
dim(discrete_choice_data_09)
table(discrete_choice_data_09$present)
discrete_choice_data_09$animal_day <- paste(discrete_choice_data_09$fulltag_clean, "_",
                                            discrete_choice_data_09$julian, sep = "")
discrete_choice_data_09$grp_day <- paste(discrete_choice_data_09$grp, "_",
                                         discrete_choice_data_09$julian, sep = "")

head(discrete_choice_data_09)

# modeling ----

require(mclogit)
response_mat <- subset(discrete_choice_data_09, select = c("present", "choice_set"))
response_mat <- discrete_choice_data_09[ ,c(4, 14)]

mc_logit_test <- mclogit(cbind(discrete_choice_data_09$present,
                               discrete_choice_data_09$choice_set) ~
                           discrete_choice_data_09$mom_in_grp + 
                           discrete_choice_data_09$focal_lamb_status)
#cbind req 2 : 1 ind 4 grp your in = present; choice set = set of grps ind could have chosen
#modeled as fxn of pred. put in pred. with + b/w 

summary(mc_logit_test)

table(mc_logit_test)

head(pop09)

