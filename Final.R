#Title: Math 23c Final Project
#Written by: Ben Dreier and Michael Cheng
#Date: May 14, 2019
#Sources: Scripts from Paul Bamberg, data from Cori Tucker-Price
#PLEASE DO NOT DISTRIBUTE THIS DATASET—MY PROCTOR PLANS TO PUBLISH THIS IN A BOOK AND DOES NOT WANT THIS DATA RELEASED PUBLICLY
#Have a great summer! =)

#Master outline/grading checklist

#DOES THE DATASET MEET THE REQUIRED DATASET STANDARDS??
#1- dataframe: yes, we have a .csv file with columns and rows
#2- yes, name, street name, city, former church, and hometown are all categorical columns, and several logical columns are made throughout this data analysis
#3- yes, we used a python script to add two numerical columns: 1st, distance of hometown from LA; 2nd, population of home state in 1940
#4-yes, there are over 3000 rows representing over 3000 church members

#DOES THE DATASET MEET THE REQUIRED GRAPHICAL DISPLAY STANDARDS?
#1- yes, see section 5 for bar plot
#2- yes, see section 2 for histograms (also in other places)
#3- 
#4- yes, see contingency table in section 1

#DOES THE DATASET MEET THE REQUIRED ANALYSIS STANDARDS?
#1- yes, see permutation test in sections 2, 3
#2- yes, p-values are used throughout this project, including in sections 2 and 3
#3- yes, see contingency table in section 1
#4- yes, see section 2 for a comparison

#WHAT BONUS POINTS HAVE WE ACHIEVED?
#2- Yes, our dataset has over 3000 individuals, a very large dataset. If you look in Section 2, we actually take samples from the population there.
#3- See one-page document on ethical issues related to collection of data in attached files.
#8- See Section 1 for convincing demonstration of a relationship that might not have been statistically significant but turns out to be so, also in Section 2
#9- See Section 4 or 5; state population may have been statistically significant but in fact does not seem to correlate with anything
#12- See Section 2 for a permutation test that works better than classical methods
#14- See Section 4 for a use of linear regression
#15- See Section 4 for calculation and display of a logistic regression curve
#16- See Section 4 for an appropriate use of correlation
#19- See Section 1 for pie charts
#20- See Section 2 for calculation of a confidence interval
#22- Ben and Michael are two people; the team has exactly two members! =)

#Note that this is organized by section. 
#Also, section 5 has some of the most interesting analysis because it compares statistics by church denomination. highly suggest you go there! :)

#SECTION 0: LOADING THE DATA FILE AND BASIC ANALYSIS
#install.packages("ggplot2") Make sure to install ggplot2 once
library("ggplot2")

#First, let's load the data file. Let M be the data file.
M <- read.csv("MembershipEdited.csv"); head(M) #notice that the original file was edited to add two numeric columns using a Python script since the original file did not have any numeric columns and was simply a list of members
#BONUS POINT 2: notice that our dataset has over 3,000 individuals. This is definitely a data set so large it can be used as a population from which samples are taken, see section 2 for actual samples from the population
#Ben wrote a python script, distancegetter.py, that gets the distance of each hometown from LA (aka how far an individual moved to LA) and their home state's population

#Note: since certain individuals' Hometowns did not have data associated with them, those individuals have been deleted for the sake of this analysis. 
#Less than 0.3% of individuals were deleted; I deleted Ritta Penden, Susan Fleming, John Hunt, George H. Jones, Wilfred Wein, Coxdelia Marshall, and Melvin Johnson since their hometowns either did not exist or had no data according to the API
#Since these individuals represent less than 0.3% of the total number of individuals, which is over 3000, this should not meaningfully impact our data

#SECTION 1: Hometown analysis WITH A CONTINGENCY TABLE
#MEETS REQUIRED ANALYSIS 3 AND BONUS POINTS 8, 19.
#Is having a hometown that isn't Los Angeles correlated with being a Convert from another/no religion?
#My hypothesis: In theory, it could make sense because people who moved from further away towns might have joined the church in search of community, then converted religions
#While people originally from LA already had a community from growing up there; or another factor could dominate

#First, let's find out the number of people from LA and the number of converts
HomeLA <- which(M$Hometown == "Los Angeles, CA"); HomeLA #People with hometowns LA
length(HomeLA) #Los Angeles, CA has 1,232
HomeNotLA <- which(M$Hometown != "Los Angeles, CA"); HomeNotLA #All people who were not from LA 
length(HomeNotLA) #Not LA has 1,814
ConvertsA <-which(M$Former.Church == "Convert"); ConvertsA #All the converts
length(ConvertsA) #427 converts
NotConverts <-which(M$Former.Church != "Convert"); NotConverts #All the non-converts
length(NotConverts) #2,619 non-converts

#Second, let's make some logical columns
LA <- M$Hometown == "Los Angeles, CA"
Converts <- M$Former.Church == "Convert"
HomeR <- data.frame(LA, Converts) #make a dataframe with just the logical columns

#Now let's build a contingency table #NOTICE THATTHIS MEETS REQUIRED GRAPHICAL DISPLAY CRITERIA 4
tbl <- table(HomeR$LA,HomeR$Converts); tbl
#Looking at the table, there are 1811 who are not originally from LA and aren't converts; 3 who are not originally from LA and are converts; 808 originally from LA who are not converts, and 424 originally from LA who are converts

#Now let's compare this with what the table would look like if Hometown and Convert status were independent
tbl #our actual table
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected #evidently, the tables are pretty different!!
#In particular, the value for not originally from LA and is converts is much lower in the actual table compared to the expected table

#These tables look quite different. Is the difference significant? Let's use the chi-squared test and see.
chisq.test(HomeR$LA,HomeR$Converts)
#The p-value is incredibly tiny, at below 2.2 * 10^-16, and the odds this arose by chance is less than 1 in a quadrillion; since the p-value is far less than 0.05 we reject the null hypothesis
#Therefore, having a Hometown of LA is very correlated with being a Convert to Christianity, which completely goes against the theory I laid out earlier
#It appears that having a hometown that isn't LA is very correlated against being a Convert to Christianity
#In other words, almost everyone who joined the People's Independent Church of Christ who was not originally from LA was already a Christian; but according to the table roughly 1/3 of people with hometown LA were Converts to Christianity—a very, very interesting finding!! 
#Not quite sure why this is, but perhaps the Church was very appealing to people who were already Christians moving in from out of state
#It makes sense that most migrants traveling west would be churched folk because that kind of migration was based on social networks and their faith sustained them during the journey. that's one hypothesis
#Quote from Proctor on significance of this: It tells me that the church was able to pull in people from LA with no religious affiliation at a time (WWII) when people were looking for resources and help.  The church was taken over by the second pastor (Clayton Russell) at this point.  He was the first black preaching radio broadcaster on the West Coast and lived a celebrity lifestyle.  So I know that the church was a big draw for migrants and local Angelenos.   But the fact that non churched people in LA were joining at such a high rate proves larger claims about the church’s significance.

#Earning Bonus Point 8
#This relationship between having a hometown of Los Angeles and being a Convert to Christianity was not apparent at all—my hypothesis at the beginning was totally wrong. But evidently, this relationship is very strong considering 424 of the 427 converts were from LA despite the fact that 1,814 of the 3,046 church members had a non-LA hometown.
#Therefore, this is a convincing relationship that might not have looked statistically significant but turns out to be so.

#BONUS POINT 19: Pie charts were not made in the class scripts, these charts are different  
#Let's build pie charts to illustrate our data!

# Pie Chart for Converts and Non-Converts
slices <- c(427, 2626) #notice that the numbers are slightly different since I added back the residents who were excluded earlier
lbls <- c("Converts to Christianity", "Non-Converts")
pct <-  round(slices/sum(slices)*100, 2) 
lbls <- paste(lbls, slices, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Converts and Non-Converts at the Church")

# Pie Chart for Converts and Non-Converts, separated by From/Not From LA
slices <- c(424, 3, 808, 1818) #notice that the numbers are slightly different since I added back the residents who were excluded earlier
lbls <- c("Convert Locals", "Convert Migrants", "Non-Convert Locals", "Non-Convert Migrants")
pct <-  round(slices/sum(slices)*100, 2) 
lbls <- paste(lbls, slices, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Converts, Non-Converts, Migrants, and Locals at the Church")

# Pie Chart for Locals and Migrants
slices <- c(1232, 1821) #notice that the numbers are slightly different since I added back the residents who were excluded earlier 
lbls <- c("Locals", "Migrants")
pct <-  round(slices/sum(slices)*100, 2) 
lbls <- paste(lbls, slices, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Locals and Migrants at the Church")


#END REQUIRED ANALYSIS 3 AND BONUS POINTS 8,11
#END SECTION 1

#SECTION 2: Comparing distances for converts and non-converts with CLT, permutation tests
#BONUS POINTS 2, 12, 20, 8 (done again)
#REQUIRED ANALYSIS 1,3
#After conducting a contingency table analysis of the differences between converts and non-converts, let's figure out whether this difference is reality using standard deviations and confidence intervals
#Let's look at the mean distance migrated for the total population and compare the mean distance migrated for the Converts, and see if this is a statistically significant difference
km <- M$Distance/1000; km #convert our distances from meters to km

mu <- mean(km); mu #mean distance migrated from hometown for full population is 1580.57 km. Remember that someone whose hometown was LA who still lives in LA counts as zero!
sigma <- sd(km); sigma #standard deviation 1513.49 km
migD <- sum(km * (M$Former.Church == "Convert"))/sum((M$Former.Church == "Convert")); migD #mean distance migrated for Converts is 7.26 km
#How likely is it that the discrepancy between mean distance migrated for Converts and the full population arose by chance?

#Approach #1 
#The CLT says that if we draw a sample of size n from the total population,
#the mean will be 1580.57 km, the SD will be sigma/sqrt(n), the distribution normal
n = 25 #draw samples of size 25
curve(dnorm(x, mu, sigma/sqrt(n)), from = 0, to = 2500)
abline(v = migD, col = "red")     #our mean distance for converts looks good, it's on the far left tail
pnorm(migD, mu, sigma/sqrt(n), lower.tail = FALSE) #and notice that an average distance greater than ours should arise roughly 100% of the time
#BONUS POINT 2: notice how we drew samples from the total population here! This was possible because of how large our dataset is, with over 3000 entries

#Here is a way to shade some of the area
xpoints <- c(migD,seq(migD,2500,1),2500) #define polygon to shade
ypoints <- c(0,dnorm(seq(migD,2500,1), mu, sigma/sqrt(n)),0) 
curve(dnorm(x, mu, sigma/sqrt(n)), from = 0, to = 2500)
abline(v = migD, col = "red")
polygon(xpoints,ypoints,col="skyblue") #notice that virtually the entire graph is sky blue, this visually demonstrates how virtually every other outcome would be more likely than the one we got if the distance traveled for Converts was random

#Approach 2
#Equivalent approach - create a statistic whose distribution is N(0,1)
#In the days of printed statistical tables, this was a good idea.
Z = (migD-mu)/(sigma/sqrt(n)); Z 

#Calculate the probability of a sample mean as extreme as what we observed
PValue <- pnorm(Z, lower.tail = FALSE); PValue   #same result, the probability of a sample mean greater than our sample mean is 1

#Alternatively, we can assume that we do not know the LA-side wigma
#If we use our sample standard deviation S, we create a t statistic.
#Studentize the data, using S instead of the national sigma.
S <- sd(km); S
t = (migD-mu)/(S/sqrt(n)); t 
PValue <- pt(t, df = n-1, lower.tail = FALSE); PValue
#the p-value is pretty much the same at p = 1; it's virtually certain that if you generated random sample means you'd get a higher mean than our actual mean for Converts
curve(dnorm(x, mu, S/sqrt(n)), from = 0, to = 2500)
abline(v = migD, col = "red")     #our mean score looks really good

#For n this large, the t distribution is essentially standard normal
t = (migD-mu)/(sigma/sqrt(n)); t 
PValue <- pt(t, df = n-1, lower.tail = FALSE); PValue #about the same P-value as earlier result

#BONUS POINT 20: I calculated a confidence interval here!

#notice that the lower end of the confidence interval is 955.83
L <- mean(km) + qt(0.025, n-1) * sd(km)/sqrt(n); L
#and the higher end of the confidence interval is 2205.31
H <- mean(km) - qt(0.025, n-1) * sd(km)/sqrt(n); H

#So if the distance migrated from one's hometown followed a normal distribution, then the 95% confidence interval for the distance migrated would be [955.83,2205.31]
#But because actual distance migrated from one's hometown for Converts to Christianity is just 7.26, FAR outside the actual confidence interval, we know that this probably did not arise by chance
#In other words, I have shown more quantiatively than Section 1 that Converts are likely to have migrated a much lower distance, and that this difference is statistically significant.

#BONUS POINT 8: Again, this shows the same relationship as in Section 1 and how it did not arise randomly. The statistical difference the mean distance traveled for Converts and the mean distance traveled for all people at this Church is clear

#finally, let's try a permutation test
#Now let's do a permutation test
isConvert <- M$Former.Church == "Convert"; isConvert
N <- 10000
diffs <- numeric(N)
for(i in 1:N){
  Samp <- sample(isConvert); Samp #permuted isBaptist column
  disConverts <- sum((km) * (Samp == TRUE))/sum(Samp == TRUE); disConverts
  dOthers <- sum((km) * (Samp == FALSE))/sum(Samp == FALSE); dOthers
  diffs[i] = disConverts - dOthers #as likely to be negative or positive
}
mean(diffs) #should be close to zero, this is indeed near zero
hist(diffs, breaks = "FD", xlim = c(-2000,500), xlab = "Observed Differences", main = "Histogram of Observed Differences") #now display the observed difference on the histogram

other <- sum(km * (M$Former.Church != "Convert"))/sum((M$Former.Church != "Convert")); other
observed <- migD - other ; observed #observed difference between mean for Converts and non-Convert; this is -1829 km
abline(v = observed, col = "red") #notice that the observed difference is very far off from the random simulations
#what is the probability that a difference this large could have arisen with a random subset?
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue #notice that the p-value is about 1, so the probability of randomly exceeding the actual value is 100%; so it is extremely unlikely that this difference arose by chance
#evidently, the difference between Converts and non-Converts probably did not arise randomly, as there is a 100% chance that a random simulation would have a difference of lesser magnitude than the actual difference seen
#compared to the confidence interval analysis above, we actually have a concrete probability that this happened by chance and know more than just the fact that the actual result is outside the confidence interval 

#BONUS POINT 12: This is an example of a permutation test working much better than classical methods. Using this permutation test, we have shown that the difference probably did not arise out of random chance and is statistically significant. But with classical methods like CLT/standard deviation, it is harder to know whether the difference happened randomly/not randomly. Therefore, this permutation test works better than classical methods in demonstrating the statistical significance of Baptists and distance traveled.
#REQUIRED ANALYSIS 3: This is a clear comparison of a CLT analysis with a simulation, permutation test
#END SECTION 2

#SECTION 3: Permutation test with Baptists
#Looking at the dataset, we have a lot of Baptists! Now I want to figure out whether being a Baptist is correlated at all with Distance Traveled using a Permutation test

#the following code creates a column that is True if an individual was Baptist before joining the People's Independent Church of Christ, false otherwise (defined as their church having the word Baptist in it)
Churches <- (M$Former.Church); Churches #column of all individuals churches
Denoms <- c("Baptist", "Methodist", "AME", "Episcopal", "CME", "Presbyterian", "Catholic") # Enumerate some words pointing to church denomination
# Function to get which churches contain which denomination strings
whichContain <- function(ch, dn){
  denoms <- rep("", length(ch))
  i <- 1
  for(c in ch){
    for(d in dn){
      #Check if the church has the denomination
      if( is.na(grepl(d, c)) ){break}
      if( grepl(d, c) ){
        denoms[i] = paste(denoms[i], d);
      }
    }
    i <- i + 1
  }
  return(denoms)
}
WhichDenoms <- whichContain(Churches, Denoms)
isBaptist <- WhichDenoms == " Baptist"; isBaptist #true if an individual is Baptist

sum(isBaptist) #so we have 1,185 baptists; out of 3,046 individuals this is a lot

km <- M$Distance / 1000 #divide distance by 1000 to get distance in km
mean(km) #mean distance for all Individuals is 1580.57 km
median(km) #median distance for all Individuals is 2137 km

#Calculate the observed Distance Traveled for Baptists and non-Baptists
dBaptists <- sum((km) * (isBaptist == TRUE))/sum(isBaptist == TRUE); dBaptists #average distance for Baptists is 2028.802 km
dOthers <- sum((km) * (isBaptist == FALSE))/sum(isBaptist == FALSE); dOthers #average distance for non-Baptists is 1295.159 km
observed <- dBaptists - dOthers; observed #on average, Baptists traveled 733.64 km farther than non-Baptists
 
#Now let's do a permutation test
N <- 10000
diffs <- numeric(N)
for(i in 1:N){
  Samp <- sample(isBaptist); Samp #permuted isBaptist column
  dBaptists <- sum((km) * (Samp == TRUE))/sum(Samp == TRUE); dBaptists
  dOthers <- sum((km) * (Samp == FALSE))/sum(Samp == FALSE)
  diffs[i] = dBaptists - dOthers #as likely to be negative or positive
}
mean(diffs) #should be close to zero, this is indeed near zero
hist(diffs, breaks = "FD", xlim = c(-500,900), main = "Histogram of Observed Differences", xlab = "Observed Differences") #now display the observed difference on the histogram
abline(v = observed, col = "red") #notice that the observed difference is very far off from the random simulations
#what is the probability that a difference this large could have arisen with a random subset?
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue #notice that the p-value is about 0.0001, which is far less than 0.05 (our typical threshold for signifiance). Therefore, the difference between Distance traveled for Baptists and non-Baptists is statistically significant. It is incredibly unlikely that it arose by chance.
#for whatever reason, Baptists at the People's Independent Church migrated from further distances than non-Baptists, on aveage 733.64 km further. This difference did not occur by random chance.

#END SECTION 3

#SECTION 4: Is there a relationship between distance traveled and state population?
#BONUS POINTS: 9, 14, 15, 16

#Let's try to analyze our only two numeric columns, distance traveled and state population
#Is there some kind of relationship between traveling from a farther away state and coming from a larger or smaller state?
km <- M$Distance/1000; km #convert our distances from meters to km
plot(km ~ M$State.Population, col = "blue", xlab = "State population in 1940", ylab = "Distance traveled") #here is a scatter plot comparing distance migrated with state population
mod <- lm(km ~ M$State.Population); mod #we found the regression line; apparently Distance Traveled to LA = -1.407*10^4 * (State population) + 2.404*10^3

#BONUS POINT 14! used linear regression
abline(mod, col = "green") #now let's add the regression line 
#according to this regression line, the larger a migrant's state population, the less distance they traveled to LA
#So migrants from smaller states were far more likely to travel longer distances to LA; perhaps reflecting lower populations in the Northeast/Midwest in 1940?
#However, this relationship might not be statistically significant.
summary(mod) #Multiple R squared is 0.05, adjusted R squared is 0.05
#Evidently, our R squared value is about 0.05, indicating a very inaccurate regression line. Our regression line only explains about 5% of the variability of the response data around its mean, indicating a very weak correlation. distance traveled and state population in 1940 don't seem to be correlated well with each other, but let's use correlation to verify!

#BONUS POINT 16! used correlation here.
#Now let's look at the correlation
res <- cor(km, M$State.Population); res
round(res,2) #our correlation is -0.22. Since our correlation is negative, an increase in state population predicts a decrease the distance traveled. However, again this is a very weak correlation. There may not be a real relationship between the variables 

#BONUS POINT 15! logistic regression
#let's try to use a logistic regression to model Distance Traveled as a function of State Population. Note that we need to normalize these variables between 0 and 1, so I divided each variable by its maximimum value in the dataset
pop <- M$State.Population/max(M$State.Population)
k <- km/max(km)
plot(pop, k, xlab = "State population in 1940", ylab = "Distance Traveled") #here's another plot 

#Start with minus the log of the likelihood function from Paul's code
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*pop)/(1+exp(alpha+beta*pop)) )*k
        + log(1/(1+exp(alpha+beta*pop)))*(1-k) )
}

#R has a function that will maximize this function of alpha and beta
#install.packages("stats4")   #needs to be run at most once
library(stats4)
results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
results@coef #alpha = -0.158, beta = -1.725 are the parameters for our logistic regression curve
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE) #the blue line is the logistic regression curve
#The logistic regression curve does not look terrible, but considering how low our correlation was earlier it is unlikely that there is a substantial correlation between distance traveled and state population in 1940

#BONUS POINT 9! Evidently, there could have been a relationship between state population and distance traveled, but our analysis indicates that there is likely no significant relationship after all. Therefore, this relationship turns out to be statistically insignificant.

#END SECTION 4

#SECTION 5: Distances traveled and population of state by church denomination
#another example of bonus point 9
#Continuing from Section 3, let's analyze the average distances traveled for all major church denominations

#the following code creates a column that is True if an individual was a certain denomination before joining the People's Independent Church of Christ, false otherwise (defined as their church having a certain word in it)
Churches <- (M$Former.Church); Churches #column of all individuals churches
Denoms <- c("Baptist", "Methodist", "AME", "Episcopal", "CME", "Presbyterian", "Catholic") # Enumerate some words pointing to church denomination
# Function to get which churches contain which denomination strings
whichContain <- function(ch, dn){
  denoms <- rep("", length(ch))
  i <- 1
  for(c in ch){
    for(d in dn){
      #Check if the church has the denomination
      if( is.na(grepl(d, c)) ){break}
      if( grepl(d, c) ){
        denoms[i] = paste(denoms[i], d);
      }
    }
    i <- i + 1
  }
  return(denoms)
}
WhichDenoms <- whichContain(Churches, Denoms)
isBaptist <- WhichDenoms == " Baptist"; isBaptist #true if an individual is Baptist
isMethodist <- WhichDenoms == " Methodist"; isMethodist
isAME <- WhichDenoms == " AME"; isAME
isEpiscopal <- WhichDenoms == " Episcopal"; isEpiscopal
isCME <- WhichDenoms == " CME"; isCME
isPresbyterian <- WhichDenoms == " Presbyterian"; isPresbyterian
isCatholic <- WhichDenoms == " Catholic"; isCatholic

#Notice that I added the individuals who were deleted at the very beginning (since their hometowns couldn't be traced) back in for the purposes of counting as we have their former churches
numBap <- sum(isBaptist) + 1; numBap #so we have 1,186 baptists; out of 3,046 individuals this is a lot
numMeth <- sum(isMethodist); numMeth #123 Methodists
numAME <- sum(isAME)+2; numAME #324 AMEs/ African Methodists
numEp <- sum(isEpiscopal)+2; numEp #43 Episcopals
numCME <- sum(isCME); numCME #70 CMEs
numPres <-sum(isPresbyterian); numPres #25 Presbyterians
numCat <- sum(isCatholic); numCat #62 Catholics
numC <- sum(M$Former.Church == "Convert"); numC #427 Converts (same method as Section 1)
#So our algorithm accounts for 2,255 of the 3,046 individuals in our dataset. This is pretty good for just a word search!
other <- 3046 - sum(numBap, numMeth, numAME, numEp, numCME, numPres, numCat, numC) +2; other #793 other denominations

#Let's make a vector of all the individuals' denominations and graph them
des <- c(rep("Baptist", numBap), rep("Methodist", numMeth), rep("AME", numAME), rep("Episcopal", numEp), rep("CME", numCME), rep("Presbyterian", numPres), rep("Catholic", numCat), rep("Convert", numC), rep("Other", other)); des
table(des) #look at our cute little table!
barplot(table(des), col = "pink", xlab = "Denomination", ylab = "Number of Individuals", main = "Denominations of Church Membership") #note: you may need to resize your window to get all of the labels to show
#THIS BARPLOT IS REQUIRED GRAPHICAL DISPLAY 1

#(BONUS POINT 19 AGAIN): let's make a pie chart of the denominations! :)
# Pie Chart for Denominations
slices <- c(numBap, numMeth, numAME, numEp, numCME, numPres, numCat, numC, other) 
lbls <- c("Baptist", "Methodist", "AME", "Episcopal", "CME", "Presbyterian", "Catholic", "Convert to Christianity", "Other")
pct <-  round(slices/sum(slices)*100, 2) 
lbls <- paste(lbls, slices, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Denominations at the Church")

Converts <- M$Former.Church == "Convert"

#Now let's look at average distance traveled by church denomination, remembering that 0 km = from LA
disB <- sum(isBaptist * km)/sum(isBaptist); disB #2028.80 km for Baptists
disM <- sum(isMethodist * km)/sum(isMethodist); disM #2581.766 km for Methodists
disA <- sum(isAME * km)/sum(isAME); disA #2087.33 km for AME
disE <- sum(isEpiscopal * km)/sum(isEpiscopal); disE #2234.66 km for Episcopal
disCM <- sum(isCME * km)/sum(isCME); disCM #1894.95 km for CME
disP <- sum(isPresbyterian * km)/sum(isPresbyterian); disP #2041.20 km for Presbyterians
disCA <- sum(isCatholic * km)/sum(isCatholic); disCA #802.37 km for Catholics
disC <- sum(Converts * km)/sum(Converts); disC #7.26 km for Converts
disO <- sum((!isBaptist & !isMethodist & !isAME & !isEpiscopal & !isCME & !isPresbyterian & !isCatholic & !Converts) * km)/ sum((!isBaptist & !isMethodist & !isAME & !isEpiscopal & !isCME & !isPresbyterian & !isCatholic & !Converts)); disO #1381.13 km for others

#Let's make a graph of the distances. For the purposes of making a barplot, I'm rounding the distances to the nearest whole number
dis <- c(rep("Baptist", disB), rep("Methodist", disM), rep("AME", disA), rep("Episcopal", disE), rep("CME", disCM), rep("Presbyterian", disP), rep("Catholic", disCA), rep("Convert", disC), rep("Other", disO)); dis
table(dis) #look at our cute little table!
barplot(table(dis), col = "orange", xlab = "Denomination", ylab = "Average Distance Traveled to LA (km)", main = "Average Distance Traveled to LA (km) by Denomination") #note: you may need to resize your window to get all of the labels to show
#We knew about Converts from Section 1, but notice how Catholics did not migrate very far either compared to the other denominations

#Finally, let's look at average home state population by denomination
pB <- sum(isBaptist * M$State.Population)/sum(isBaptist); pB #5476799 for Baptists
pM <- sum(isMethodist * M$State.Population)/sum(isMethodist); pM #5072736 for Methodists
pA <- sum(isAME * M$State.Population)/sum(isAME); pA #5060682 for AME
pE <- sum(isEpiscopal * M$State.Population)/sum(isEpiscopal); pE #6310050 for Episcopal
pCM <- sum(isCME * M$State.Population)/sum(isCME); pCM #5022214 for CME
pP <- sum(isPresbyterian * M$State.Population)/sum(isPresbyterian); pP #4982980 for Presbyterians
pCA <- sum(isCatholic * M$State.Population)/sum(isCatholic); pCA #68705221 for Catholics
pC <- sum(Converts * M$State.Population)/sum(Converts); pC #6906233 for Converts
pO <- sum((!isBaptist & !isMethodist & !isAME & !isEpiscopal & !isCME & !isPresbyterian & !isCatholic & !Converts) * M$State.Population)/ sum((!isBaptist & !isMethodist & !isAME & !isEpiscopal & !isCME & !isPresbyterian & !isCatholic & !Converts)); pO #6270838 for others

#Let's make a graph of the average home state populations
p <- c(rep("Baptist", pB), rep("Methodist", pM), rep("AME", pA), rep("Episcopal", pE), rep("CME", pCM), rep("Presbyterian", pP), rep("Catholic", pCA), rep("Convert", pC), rep("Other", pO)); p
table(p) #look at our cute little table!
barplot(table(p), col = "green", xlab = "Denomination", ylab = "Average Home State Population", main = "Average Home State Population by Denomination") #note: you may need to resize your window to get all of the labels to show
#It doesn't look like there are many clear differences by denomination
#BONUS POINT 9: here's another relationship that might have been statistically significant, but are not so

#END SECTION 5


# Let's try to map
# First, go through and extract 

# Only install these once
install.packages('ggmap')
install.packages("leaflet")
install.packages("geojsonio")


library(ggmap)
library(leaflet)
library(geojson)
citation("ggmap")

# Key removed for security; if you need to run this part of the code yourself, I can provide a key
register_google(key="_KEY_", write=TRUE)

# Iterate through hometowns and get their latitude and longitude
# This takes a while and requires a key, try to only run it once if at all
# You can also just use the backup file I created below

##Skip This >##
locs <- {}
for(i in seq(1, length(M$Hometown))){
  town <- toString(M$Hometown[i])
  loc <- geocode(town)
  locs <- rbind(locs, loc)
}

write.csv(locs, "locations2.csv")
##Skip This <##

#Instead of running this again, just load from a file backup I made
locs_file <- read.csv("locations2.csv")


# Before mapping, Count occurances of each state

counts = {}
for(i in 1:length(states$name)){
  curr_state <- toString(states$name[i])
  state_count <- length(M$State[M$State == curr_state])
  counts = c(counts, state_count)
}

# Put these counts in the states object used for visualization
states$count <- counts

# Now we can decide how to break up the bins
barplot((sort(log10(counts+1), decreasing=TRUE)))
# The log of counts has a nice linear shape, so we'll base bins off of that

bins <- c(0, 10^seq(0, 3.5, 0.5))
pal <- colorBin("YlOrRd", domain = states$count, bins = bins)

# Now we can map the counts
# Get state outlines
states <- geojsonio::geojson_read("us-states.json", what="sp")

m <- leaflet(states)
m <- addTiles(m)
m <- addPolygons(m, fillColor = ~pal(counts), weight=1, color="white", fillOpacity = 0.7)
m

# If we want to plot every individual location:
addMarkers(m, lng=locs_file$lon, lat=locs_file$lat, label=M$Former.Church)
