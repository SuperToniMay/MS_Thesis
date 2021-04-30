#--------------------------------------------------#
#-- Animal movement discrete choice model set-up --#
#-- K. Manlove and T. Proescholdt, Nov 30, 2020 ---#
#--------------------------------------------------#

#-- Script overview
# for each animal-observation ----
# 1) extract all grps observed on that day
# 2) build "metagrp" of unobserved -- observed/not needs to be a predictor
# 3) build grp-specific, individual-specific, observation-specific predictors
# 4) integrate predictors into dataframe
# 5) fit discrete-choice models

# target predictors ----
# -- is your [mom/grandma/daughter(s)] in the group?
# -- What proportion of your cohort is here?
# -- What proportion of the grp matches your current reproductive status?
# -- How big is the group
# -- (distance??)
# -- how many rams
# -- how many sibs/half sibs (and/or some other measure of "relatedness" to the group)
# -- PLUS OTHERS IN RUT

# set focal year ----
year_in <- 2000

# Read in and subset required datasets ----
# census (individual-day location_ records)
census <- read.csv("census_withLatLong_20200511.csv", header = T)
# subset census to just focal year
census_year <- subset(census, year == year_in)
# BRPOP (static data for individuals throughout their lifetimes)
brpop <- read.csv("BRPOP_v02.csv", header = T)
# subset down to only animals who are (or could be) alive in year_in
brpop_year <- subset(brpop, (yearb <= year_in & yeard >= year_in) |
                       # individuals who were born before and died after focal year
                       (yearb <= year_in & is.na(yeard) == T) | 
                       # individuals who were born before focal year and have no record of death
                       (is.na(yearb) == T & yeard >= year_in))
                       # individuals missing a birth year, but with a death year after focal year
# store a second vsn of year-specific brpop that includes only animals without missing fulltags.
brpop_year_reduced <- subset(brpop_year, fulltag != "")

# add birth year for each animal-day onto census_year
census_year$yearb <- census_year$demog_class <- rep(NA, dim(census_year)[1])
for(i in 1:dim(census_year)[1]){
  #"replacement has length zero" error, here and line 54 (means no brpop row)
  # fill in yearb in census_year
  census_year$yearb[i] <- brpop_year$yearb[which(as.character(brpop_year$fulltag) == 
                                                   as.character(census_year$fulltag_clean)[i])]
  # fill in demog class in census_year
  census_year$demog_class[i] <- as.character(brpop_year$sex[which(as.character(brpop_year$fulltag) == 
                                                       as.character(census_year$fulltag_clean)[i])])
  # update demog_class to include lamb information in addition to sex
  census_year$demog_class[i] <- ifelse(census_year$demog_class[i] == "" & 
                                         as.character(census_year$fulltag)[i] == "LAMB", 
                                       "LAMB", as.character(census_year$demog_class[i]))
}

# build a storage list of length individual-observation to record attributes of individual
# and choice sets on each day to be modeled (each item in the list contains one individual's
# choice set on one observation event)
indobs_grp_data <- vector("list", dim(census_year)[1])

# loop over every observation in census_year
for(i in 1:dim(census_year)[1]){
  # get all census_year observations occurring on the focal date.
  day_obs <- census_year[which(census_year$date == census_year$date[i]), ] 
  # ('which' pulls all observations from that day; census_year maps them to rows in census_year)

  # make groupid a factor to list its levels easily  
  day_obs$groupid <- factor(day_obs$groupid)
  # extract all groups on that day
  day_grps <- levels(factor(day_obs$groupid)) 
  
  # add a "group" for unobserved animals
  # what I really need is a day-specific list of who's alive
  # FOR NOW, just use everyone in census_year
  census_year_animals <- levels(factor(census_year$fulltag_clean))
  # make an "unobs" group of animals that were not seen on a given day.
  unobs <- census_year_animals[!(census_year_animals %in% day_obs$fulltag_clean)]
  # (we should maybe revisit putting all those animals into one grp vs. many grps...)

  # add unobs grp row onto day_grps
  day_grps <- c(day_grps, "unobs")
  
  # bring in BRPOP
  if(as.character(census_year$fulltag_clean[i]) %in% as.character(brpop_year_reduced$fulltag)){
    
    # make a row for each group for the focal individual
    indobs_grp_data[[i]] <- expand.grid(census_year$fulltag_clean[i], day_grps) # -- might need to be changed with multiple groups
    names(indobs_grp_data[[i]]) <- c("fulltag_clean", "grp")
    
    # extract observation's Julian date
    indobs_grp_data[[i]]$julian <- (as.POSIXlt(as.character(census_year$date[i]), format = "%m/%d/%Y"))$yday
    
    # 'present' is an indicator variable taking on value 1 when focal animal is present in the group
    indobs_grp_data[[i]]$present <- ifelse(as.character(indobs_grp_data[[i]]$grp) == 
                                             as.character(census_year$groupid[i]), 1, 0)
    # 'grpsize' is a quantitative variable of number of ID'd animals present in each group
    indobs_grp_data[[i]]$grpsize <- c(table(factor(day_obs$groupid)), length(unobs))
    
    # extract brpop_year record for our focal animal (match brpop_year$fulltag to census_year$ fulltag_clean)
    individ_data <- brpop_year[which(as.character(brpop_year$fulltag) == as.character(census_year$fulltag_clean[i])), ]
    # extract focal individual's mom's ID number
    individ_mom <- individ_data$mother_idno
    # subset brpop_year down to just focal animal's mom. dim(individ_mom_year)[1] == 0 if mom has died.
    individ_mom_year <- subset(brpop_year, as.character(idno) == as.character(individ_mom))$name
    # subset brpop_year down to any other animals with the same mother_idno (same mom)
    individ_sibs <- brpop_year[which(as.character(brpop_year$mother_idno) == as.character(individ_data$mother_idno)), ]
    
    # extract focal individual's maternal grandmother's idno
    individ_grandma_idno <- brpop[which(as.character(brpop$idno) == 
                                          as.character(individ_data$mother_idno)), ]$mother_idno
    # subset brpop_year down to just focal animal's maternal grandmother. 
    # dim(brpop_individ_grandma)[1] == 0 if grandmother has died.
    brpop_individ_grandma <- brpop[which(as.character(brpop$idno) == as.character(individ_grandma_idno)), ]
    grandma_in_brpop_year <- brpop_year[which(as.character(brpop_year$idno) == as.character(individ_grandma_idno)), ]
    
    # identify any other offspring from the focal animal that are currently alive
    individ_offspring <- brpop_year[which(as.character(brpop_year$mother_idno) == as.character(individ_data$idno)), ]
    
    # build covariates
    # 'mom_in_grp': indicator variable taking on value 1 if mom is in group. 
    # 1) extract mom's group ID
    mom_grp <- subset(day_obs, as.character(fulltag_clean) == as.character(individ_mom_year))$groupid
    # 2) build vector of 0's for all groups
    indobs_grp_data[[i]]$mom_in_grp <- rep(0, dim(indobs_grp_data[[i]])[1])
    # 3) overwrite 0 with 1 if groupid = mom_grp
    indobs_grp_data[[i]]$mom_in_grp[ifelse(length(mom_grp) == 0, dim(indobs_grp_data[[i]])[1], mom_grp)] <- 1
    
    # 'fem_sibs_in_grp': number of female sibs in grp
    # 1) extract set of female sibs observed on each day
    fem_sibs_day <- subset(day_obs, as.character(fulltag_clean) %in% as.character(individ_sibs$fulltag))
    # 2) store a table of groupids for those female sibs
    fem_sibs_tab <- table(fem_sibs_day$groupid)
    # bind fems_sibs_tab (and any remaining sibs alive but not accounted for that day) onto indobs_grp_data
    indobs_grp_data[[i]]$fem_sibs_in_grp <- c(fem_sibs_tab, dim(individ_sibs)[1] - sum(fem_sibs_tab))
    
    # 'same_cohort_count': count number of animals from focal animal's cohort present in each group
    # 1) identify focal animal's birth cohort and store in focal_animal_cohort
    focal_animal_cohort <- individ_data$yearb
    # 2) make an indicator for all obs in day_obs of whether they're from focal animal cohort
    grp_cohort_tab <- table(day_obs$groupid, factor(day_obs$yearb == focal_animal_cohort,
                                                    levels = c("FALSE", "TRUE")))[, 2]
    # 3) add in animals from focal cohort that were unobserved
    unobserved <- brpop_year[(which(as.character(brpop_year$fulltag) %in% 
                                      as.character(day_obs$fulltag_clean) == FALSE)), ]
    # 4) bind grp_cohort_tab and unobserved together into a vector and append to individ_grp_data
    indobs_grp_data[[i]]$same_cohort_count <- c(grp_cohort_tab, 
                                                table(factor(unobserved$yearb == focal_animal_cohort, 
                                                             levels = c("FALSE", "TRUE")))["TRUE"]) 
    # (this is the number of unobserved animals from cohort)
    
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
    ewe_lamb_boundary_dates <- subset(census_year, fulltag_clean == as.character(census_year$fulltag_clean[i]) & lamb_present == "TRUE")
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
      indobs_grp_data[[i]]$focal_lamb_status <- ifelse(as.POSIXlt(as.character(census_year$date[i]), format = "%m/%d/%Y") > posix_first & 
                                                         as.POSIXlt(as.character(census_year$date[i]), format = "%m/%d/%Y") < posix_last, 1, 0)
    }
    # Add counter to uniquely identify each choice set
    indobs_grp_data[[i]]$choice_set <- rep(i, dim(indobs_grp_data[[i]])[1])
    # Add in year number
    indobs_grp_data[[i]]$year_in <- rep(year_in, dim(indobs_grp_data[[i]])[1])
    
  } # \if
  
  print(i)
}

# bind all individual-day choice-set dataframes into a single big dataframe
discrete_choice_data_year <- do.call("rbind", indobs_grp_data)
# check its size
dim(discrete_choice_data_year)
# check to be sure `present' got recorded
table(discrete_choice_data_year$present)
# `animal_day': variable of fulltag_clean pasted to julian.
discrete_choice_data_year$animal_day <- paste(discrete_choice_data_year$fulltag_clean, "_", 
                                            discrete_choice_data_year$julian, sep = "")
# `grp_dat': grp pasted to julian
discrete_choice_data_year$grp_day <- paste(discrete_choice_data_year$grp, "_", 
                                         discrete_choice_data_year$julian, sep = "")
# display top of dataframe for visual inspection
head(discrete_choice_data_year)
# write out discrete choice data for this year
# (you'll need to make a "DiscreteChoiceData" subdirectory in  your working directory to write these into)
write.csv(discrete_choice_data_year, paste("MS_Thesis/Discrete_Choice/Processed_Data/DiscreteChoiceData/discrete_choice_data_", year_in, ".csv", sep = ""))


# Analysis ----
require(mclogit)
response_mat <- subset(discrete_choice_data_year, select = c("present", "choice_set"))
response_mat <- discrete_choice_data_year[ , c(4, 14)]

data <- discrete_choice_data_year
mc_logit_test <- mclogit(cbind(data$present, 
                               data$choice_set) ~ 
                           data$mom_in_grp * data$focal_animal_age +
                           data$grpsize + I(data$grpsize^2) + 
                           data$same_cohort_count) 
                          # data$fem_sibs_in_grp + 
                          # data$focal_animal_age)
summary(mc_logit_test)
plot(mc_logit_test)
confint_choice <- confint(mc_logit_test)

coefficient_names <- c("Mom in Group", "Group Size", "Group Size^2", "Cohort Mates\nPresent", "Mom by Age")

par(mar = c(4, 6, 2, 2))
plot(x = 0, y = 0, ylim = c(0, 6), xlim = c(-1, 1), cex = 0,
     xlab = "Effect on Utility", ylab = "" , yaxt = "n")
require(graphics)
for (i in c(1:5)) {
  segments(y0 = i, y1 = i, x0 = confint_choice[i, 1], x1 = confint_choice[i, 2], lwd = 3)
  
}
segments(y0 = 0, y1 = 6, x0 = 0, x1 = 0, lty = 2, col = "grey70")
axis(side = 2, at = c(1:5), labels = coefficient_names, las = 1, cex.axis = .8)

head(data)



barplot(prop.table(table(data$mom_in_grp, 
                         data$present), 1),
        legend = T)


mc_logit_test <- mclogit(cbind(data$present, 
                               data$choice_set) ~ 
                           data$mom_in_grp + 
                           data$mom_in_grp:data$focal_animal_age +
                           data$grpsize + I(data$grpsize^2) + 
                           data$same_cohort_count)

# list out all year files
year_dat <- list.files("MS_Thesis/Discrete_Choice/Processed_Data/DiscreteChoiceData/")
year_dat <- year_dat[-1]

require(mclogit)
require(graphics)

coefficient_names <- c("Mom in Group", "Group Size", "Group Size^2",
                       "Cohort Mates\nPresent", "Mom by Age")



#All years in one plot ----
# loop over year files
discrete_choice_data_year <- response_mat <- mc_logit_test <- confint_choice <- dat_out <- vector("list", length(year_dat))

for(y in 1:length(year_dat)){
  discrete_choice_data_year[[y]] <- read.csv(paste("MS_Thesis/Discrete_Choice/Processed_Data/DiscreteChoiceData/",
                                                   year_dat[y], sep = ""), header = T)
  
  
  # Analysis ----
  response_mat[[y]] <- subset(discrete_choice_data_year[[y]], select = c("present", "choice_set"))
  
  data <- discrete_choice_data_year[[y]]
  mc_logit_test[[y]] <- mclogit(cbind(data$present,
                                      data$choice_set) ~
                                  data$mom_in_grp +
                                  data$mom_in_grp:data$focal_animal_age +
                                  data$grpsize + I(data$grpsize^2) +
                                  data$same_cohort_count)
  summary(mc_logit_test[[y]])
  # plot(mc_logit_test)
  confint_choice[[y]] <- confint(mc_logit_test[[y]])
  dat_out[[y]] <- cbind(confint_choice[[y]],
                        rep(discrete_choice_data_year[[y]]$year_in[1], dim(confint_choice[[y]])[1]),
                        coefficient_names)
  print(y)
}

confint_full <- as.data.frame(do.call("rbind", dat_out))
str(confint_full)
names(confint_full) <- c("lower_bound", "upper_bound", "year", "coefficient_name")
confint_full$lower_bound <- as.numeric(as.character(confint_full$lower_bound))
confint_full$upper_bound <- as.numeric(as.character(confint_full$upper_bound))
confint_full$year <- as.numeric(as.character(confint_full$year))

confint_reordered <- confint_full[order(confint_full$coefficient_name), ]
head(confint_reordered)

par(mar = c(4, 6, 2, 2))
plot(x = 0, y = 0, ylim = c(0, dim(confint_reordered)[1]), xlim = c(1/5, 5), cex = 0,
     xlab = "Multiplicative change in Utility", ylab = "" , yaxt = "n", log = "x")
for (i in c(1:dim(confint_reordered)[1])) {
  segments(y0 = i, y1 = i, x0 = exp(confint_reordered$lower_bound[i]),
           x1 = exp(confint_reordered$upper_bound[i]), lwd = 1.5)
}
for(i in 1:length(coefficient_names)){
  abline(h = i * length(year_dat) + 0.5, lty = 3, lwd = .75, col = "grey80")
}



segments(y0 = 0, y1 = dim(confint_reordered)[1], x0 = 1, x1 = 1, lty = 2, col = "grey70")
axis(side = 2, at = (c(5:1) * length(year_dat)) - 5.5, labels = rev(coefficient_names), las = 1, cex.axis = .8)



