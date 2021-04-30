# Read in and subset required datasets ----
# census (individual-day location_ records)
census <- read.csv("MS_Thesis/Raw_Data/census_withLatLong_20200511.csv", header = T)
brpop <- read.csv("MS_Thesis/Raw_Data/BRPOP_v02.csv", header = T)

# goal: build a figure where lines correspond to each individual
# and x-axis covers years
# color-code lines by sex. 
require(graphics)
brpop$lifetime <- brpop$yeard - brpop$yearb

recr <- subset(brpop, fulltag != "")
recr$yearb2 <- ifelse(is.na(recr$yearb) == T, 1979,
                      recr$yearb)
par(mfrow = c(1, 3))
plot(x = 0, y = 0, 
     xlim = c(1979, 2017),
     ylim = c(0, 127), las = 1)
for(i in 1:126){
  segments(x0 = recr$yearb2[i], x1 = recr$yeard[i],
           y0 = i, y1 = i)
}

plot(x = 0, y = 0, 
     xlim = c(1979, 2017),
     ylim = c(0, 127), las = 1)
for(i in 1:126){
  segments(x0 = recr$yearb2[i+126], x1 = recr$yeard[i+126],
           y0 = i, y1 = i)
}

plot(x = 0, y = 0, 
     xlim = c(1979, 2017),
     ylim = c(0, 127), las = 1)
for(i in 1:126){
  segments(x0 = recr$yearb2[i+(2*126)], x1 = recr$yeard[i+(2*126)],
           y0 = i, y1 = i)
}

# who really lives a long time?
par(mfrow = c(1, 1))
hist(recr$lifetime)
median(na.omit(recr$lifetime))

long_lived <- subset(recr, lifetime >= 13)
dim(long_lived)
table(long_lived$sex)
long_lived$fulltag
long_lived_sm <- subset(long_lived, select = c("fulltag", "yearb"))

# females only
ewes <- subset(recr, sex == "F")
par(mfrow = c(1, 2))
plot(x = 0, y = 0, 
     xlim = c(1979, 2017),
     ylim = c(0, 100), las = 1)
for(i in 1:100){
  segments(x0 = ewes$yearb2[i], x1 = ewes$yeard[i],
           y0 = i, y1 = i)
}

plot(x = 0, y = 0, 
     xlim = c(1979, 2017),
     ylim = c(0, 100), las = 1)
for(i in 1:100){
  segments(x0 = ewes$yearb2[i + 100], x1 = ewes$yeard[i + 100],
           y0 = i, y1 = i)
}

# for each ewe in each year, add up total # of offspring & 
# total # female offspring alive

ewe_year_list <- vector("list", dim(ewes)[1])

for(i in 1:dim(ewes)[1]){
  if(is.na(ewes$yearb[i]) == F & is.na(ewes$yeard[i]) == F){
  focal_years <- c(ewes$yearb[i]:ewes$yeard[i])
  ewe_name <- rep(ewes$fulltag[i], length(focal_years))
  ewe_idno <- rep(ewes$idno[i], length(focal_years))
  ewe_index <- rep(i, length(focal_years))
  total_offspring <- female_offspring <- rep(NA, length(focal_years))
  ewe_year_list[[i]] <- data.frame(focal_years = focal_years,
                                   total_offspring = total_offspring,
                                   female_offspring = female_offspring,
                                   ewe_name = ewe_name,
                                   ewe_idno = ewe_idno,
                                   ewe_index = ewe_index)
  for(j in 1:length(focal_years)){
    her_offspring_in_focal_year <- subset(recr, mother_idno == ewes$idno[i] & 
                                            yearb <= focal_years[j] & 
                                            yeard >= focal_years[j])
    ewe_year_list[[i]]$total_offspring[j] <- dim(her_offspring_in_focal_year)
    her_female_offspring_in_focal_year <- subset(recr, mother_idno == ewes$idno[i] & 
                                                   yearb <= focal_years[j] & 
                                                   yeard >= focal_years[j] & sex == "F")
    ewe_year_list[[i]]$female_offspring[j] <- dim(her_female_offspring_in_focal_year)
  }
  }
  print(i)
}

ewe_year_list[[150]]
ewe_year_offspring <- do.call("rbind", ewe_year_list)
dim(ewe_year_offspring)
head(ewe_year_offspring)

# females only
ewes <- subset(recr, sex == "F")
par(mfrow = c(1, 2))
plot(x = 0, y = 0, 
     xlim = c(1979, 2017),
     ylim = c(0, 100), las = 1)
points(ewe_year_offspring$ewe_index ~ ewe_year_offspring$focal_years,
       cex = ewe_year_offspring$female_offspring/3)
for(i in 1:100){
  segments(x0 = ewes$yearb2[i], x1 = ewes$yeard[i],
           y0 = i, y1 = i)
}

plot(x = 0, y = 0, 
     xlim = c(1979, 2017),
     ylim = c(0, 100), las = 1)
points((ewe_year_offspring$ewe_index - 100) ~ ewe_year_offspring$focal_years,
       cex = ewe_year_offspring$female_offspring/3)

for(i in 1:100){
  segments(x0 = ewes$yearb2[i + 100], x1 = ewes$yeard[i + 100],
           y0 = i, y1 = i)
}


