#Use package arules
library(arules)

### Import dataset
### a)
movie_df <- read.csv("movies_binary.csv")
movie_matrix <- as.matrix(movie_df)
?as
movie_transactions <- as(movie_matrix, Class = "transactions")
inspect(head(movie_transactions))
### b) Total number and names of unique movies
# 32 unique movies
itemInfo(movie_transactions)
### c)
itemFrequencyPlot(movie_transactions, ylim = c(0, 1), topN = 10, 
                  main = "Movies by support %", col = "#053259" )
# Top2 movies 
# No1. StarWars.EpisodeIV.ANewHope.1977.:  0.8998999
# No2. StarWars.EpisodeV.TheEmpireStrikesBack.1980. : 0.8588589 
# Within all the people in this dataset, 89.9% of the people have watched StarWars.EpisodeIV
# and 85.8% of the people have watched StarWars.EpisodeV.
# This shows how popular these two movies are. 

### d) Transactions that contain both up.2009 and lion king
up2009_and_lionking <- subset(movie_transactions, 
                              items %ain% c("Up.2009.", "LionKing.The.1994."))
summary(up2009_and_lionking)
# Answer: 776 transactions

### E) 
?apriori
movie_rules <- apriori(movie_transactions, list(supp = 0.7, conf = 0.7, minlen = 2, maxlen = 2))
inspect(head(movie_rules))
summary(movie_rules)
# X -> Y
# mean value of support: 0.7485 -> 74.8% of the people in the dataset 
# watch these two movies combination of X and Y. High support value, 
# which shows that these movies are popular
# mean value of confidence: 0.8945 -> When people watch Movie X, 89.45% 
# of the time they watch Movie Y 
# mean value of lift: 1.06 -> Low lift value. People who watch movies X, 
# watch movie Y only 1.06 times more than all the other people. 

### f)
inspect(movie_rules)
# 22 rules found
# Obvious rule: X -> Y and Y -> X. 
# We could see that from first 2 rules. HarryPotter.2002 -> HarryPotter.2001 
# and HarryPotter.2001 -> HarryPotter2002
# Less obvious rule: People who watch X.men watch Lion King. 
# It is less obvious because X men and lion king are in two different genre 
# (Action and Musical)

### g) Order the rules in increasing order by confidence
inspect(sort(movie_rules, by = "confidence", decreasing = FALSE ))
# Highest Confidence
# HarryPotter.2002 -> HarryPotter,,2001 and Up.2009 -> Lionking.1994 
# and Starwars.1980 -> StarWars1977. These three all have a confidence of 1.0 
# which means, if people watch Harry Potter.2002 100% of the time, 
# they watch HarryPotter.2001. This applies to all three rules we found
# Lowest Confidence 
# StarWars.1977 -> Up.2009
# Confidence of 80%. When people watch StarWars.1977, 80% of the time 
# they watch Up.2009

### h) Order rules in increasing order by lift
inspect(sort(movie_rules, by = "lift", decreasing = FALSE))
# Highest lift
# Up.2009 -> LionKing.1994 and LionKing.1994 and Up.2009
# Lift value of 1.24 shows that people who watch up.2009 are 1.24 times 
# more likely to watch Lionking and vice versa
# Lowest lift
# X.Men -> StarWars.1977 and StarWars.1977 -> X.men
# Lift value of 0.98 shows that people who watch X.men are less likely 
# to watch StarWars and vice versa

### i) 
# 2 rules I trust 
# 1st: HarryPotter.2002 -> HarryPotter.2001
# 2nd: StarWars.1980 -> StarWars1977]
# These two rules have Confidence of 1 and lift of over 1 which means 
# when people watch the left hand side of the movie, 100% of the time 
# they watch the right hand side too. And they are more likely to watch 
# them than all the other people. Also, these rules make sense because 
# when people watch a new episode of a series they will also watch 
# the previous one too. Otherwise, they will get confused. 
# 2 rules I don't trust
# 1st: StarWars.1980 -> X.men.2000
# 2nd: StarWars.1977 -> X.men.2000
# They have Confidence of about 0.847 and 0.833 which means when people 
# watch StarWars movies, they only watch X.men about 83 - 85 % of the time. 
# Also, they have Lift value of less than one, which means people 
# who watch Star.Wars are less likely to watch X.men than others. 

### j)
# Recommendations to the company: 
# I would suggest the streaming company to give recommendation to customers based 
# on the rules we found. We found out that people tend to watch movies that are 
# in the same genre or series. That is supported by high confidence and lift values. 
# For instance {Up.2009 -> LionKing.1994} have a confidence of 1 and lift of 
# 1.23. This makes sense because they belong in the same genre. This applies to 
# harry potter and StarWars too. 
# Therefore,  if a customer watches an action movie then the company should recommend
# action movies to a customer. However, we cannot just recommend any action movie 
# because those two movies might have low confidence or lift value. If two movies
# have low confidence or lift value then that means they are generally not watched 
# together. Thus, I would  suggest the company to recommend movies that have higher 
# confidence and lift value. 


### k)
bad_rules <- apriori(movie_transactions, list(supp = 0.5, conf = 0.6, minlen = 2))
inspect(head(bad_rules))
summary(bad_rules)
# Answer: 620 rules found

### L)
library(arulesViz)
plot(bad_rules, measure = c("support", "lift"), shading = "lift")

### m)
# First of all rules that have lift of <=1.0 should be excluded. 
# Lift value of less than or equal to one means, they either do not have 
# any association or they negatively affect each other.
badrules_badlift <- subset(bad_rules, subset = lift <= 1)
inspect(sort(badrules_badlift, by = "confidence"))
# badrules_badlift is a subset of badrules where lift <= 1, and 
# I sorted the rules by Confidence in descending order. 
# By looking at the subset we could clearly see that these rules contain
# movies that are not in the same genre. 
# For instance the first rule is PulpFiction.1994, LionKing -> StarWars.1977 
# and it has confidence of 0.889 and lift of 0.988. 
# It means when people watch PulpFiction and LionKing, 88.9 % of the time 
# they watch StarWars. And people who watch PulpFiction and Lionking are 
# less likely to watch StarWars. 
# This makes sense because they don't belong in the same genre. 
# This applies to most of the rules in this subset. 
# Most of the movies in the left hand side does not belong to 
# the movie genre on the right hand side. 


### n) 
# Music streaming services. We could apply association rules to music streaming
# service because people often have similarity in their music choices. They could 
# mine association between genre, artists, and songs. If they see a lot of 
# association between some artists, they could use this association as a 
# recommendation for customers. The data we need would be a list of artists, songs
# and its genre for each of the customers. For instance they might be able to 
# find association of {Drake -> Travis Scott}, by mining which artists
# people listen to. Then, if this association have a high confidence and a lift
# value they could recommend Travis Scott's song to people who listen to Drake




