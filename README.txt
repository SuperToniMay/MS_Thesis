My project is for my M.S. thesis and the analysis I will be doing of archival data that are already collected.
So far, I plan to have two major parts to this analysis: 1. A Discrete Choice Model & 2. and Association Matrix.
These data span over 40 years and include information on each individual bighorn sheep in the population.
We know each individual, each individual's sex, mother and often father, and birthdate. Therefore, we also know each individual's siblings and fellow members of their cohort.
The data are collected during two main field seasons: Spring/Summer & Fall.
These data inclube observations of group locations and composition. We know who was observed with who where and when.
I am mainly interested in the ewes and what we can learn about their choices in group (Discrete Choice) and who to spend the most time with (Association Matrix).

In my .gitignore file, I have included the following:
.Rproj.user/ because this file is a part of my user profile in R, not my project.
.Rhistory for a similar reason. This is specific to R.
.RData & .Ruserdata for the same reasons.
Raw_Data/ because the two current datasets I have inherited are massive, and I won't be editing them. I will be pulling from them to create smaller, more usable dataset specific to my project.
*.png and *.jpg for any results/figures I put in my directory, because I can't track images, and I can regenerate them from the code anyway.
/Figures/ because I plan for those folders to only include images.
/Results/ because those will be generated out of the code, and I'm not sure what type of files I will be putting in there.
