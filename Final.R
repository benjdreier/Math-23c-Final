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
#1- 
#2- 
#3- 
#4- yes, see contingency table in section 1

#DOES THE DATASET MEET THE REQUIRED ANALYSIS STANDARDS?
#1- 
#2- 
#3- yes, see contingency table in section 1
#4- 

#WHAT BONUS POINTS HAVE WE ACHIEVED?
#3- See one-page document on ethical issues related to collection of data in attached files.
#8- See Section 1 for convincing demonstration of a relationship that might not have been statistically significant but turns out to be so, also in Section 2
#20- See Section 2 for calculation of a confidence interval
#22- Ben and Michael are two people; the team has exactly two members! =)

#Note that this is organized by section. 

#SECTION 0: LOADING THE DATA FILE AND BASIC ANALYSIS
#install.packages("ggplot2") Make sure to install ggplot2 once
library("ggplot2")

#First, let's load the data file. Let M be the data file.
M <- read.csv("MembershipEdited.csv"); head(M) #notice that the original file was edited to add two numeric columns using a Python script since the original file did not have any numeric columns and was simply a list of members
#Ben wrote a python script, distancegetter.py, that gets the distance of each hometown from LA (aka how far an individual moved to LA) and their home state's population

#Note: since certain individuals' Hometowns did not have data associated with them, those individuals have been deleted for the sake of this analysis. 
#Less than 0.3% of individuals were deleted; I deleted Ritta Penden, Susan Fleming, John Hunt, George H. Jones, Wilfred Wein, Coxdelia Marshall, and Melvin Johnson since their hometowns either did not exist or had no data according to the API
#Since these individuals represent less than 0.3% of the total number of individuals, which is over 3000, this should not meaningfully impact our data
#Now, let's figure out which people currently lived in Los Angelos and place them in the dataframe livesLA.

livesLA <- which(M$City == "Los Angeles"); livesLA
livesBH <- which(M$City == "Beverly Hills"); livesBH #Beverly Hills
livesHW <- which(M$City == "Hollywood"); livesHW #Hollywood
livesPD <- which(M$City == "Pasadena"); livesPD #Pasadena

#Now let's categorize people's hometowns
Q <- unique(M$Hometown); Q #this is the list of all unique hometowns, of which there are 394

N <- length(Q) #394
Home <- numeric(N); Home

for(i in length(Q)){
  Home[i] <- which(M$Hometown == Q[i])
}

#SECTION 1: Hometown analysis WITH A CONTINGENCY TABLE
#MEETS REQUIRED ANALYSIS 3 AND BONUS POINT 8.
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

#END REQUIRED ANALYSIS 3 AND BONUS POINT 8
#END SECTION 1


#SECTION 2: Comparing distances for converts and non-converts
#BONUS POINT 20, 8 (done again)
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

#END SECTION 2








# Ben's work

# Let's talk churches. It looks like there are 759 unique churches listed here:
Churches <- unique(M$Former.Church); Churches

# Enumerate some words pointing to church denomination
Denoms <- c("Baptist", "Methodist", "AME", "Episcopal", "CME", "Presbyterian", "Catholic")

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

grepl(Denoms[3], Churches[4])

WhichDenoms <- whichContain(Churches, Denoms)

Churches[which(WhichDenoms == "")]

length(Churches)

denoms[3]
