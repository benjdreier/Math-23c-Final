#Title: Math 23c Final Project
#Written by: Ben Dreier and Michael Cheng
#Date: May 1, 2019
#Sources: Scripts from Paul Bamberg, data from Cori Tucker-Price

#First, let's load the data file. Let M be the data file.
M <- read.csv("MembershipRolls.csv"); head(M)

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

HomeH <- which(M$Hometown == "Houston, TX "); HomeH #Houston, TX has 123
HomeLA <- which(M$Hometown == "Los Angeles, CA"); HomeLA #Los Angeles, CA has 1,232

#REQUIRED ANALYSIS 3. Hometown analysis WITH A CONTINGENCY TABLE
#Is having a hometown that isn't Los Angeles correlated with being a Convert from another/no religion?
#In theory, it could make sense because people who moved from further away towns might have joined the church in search of community, then converted religions
#While people originally from LA already had a community from growing up there; or another factor could dominate

#First, let's find out the number of people from LA and the number of converts
HomeLA <- which(M$Hometown == "Los Angeles, CA"); HomeLA #People with hometowns LA
length(HomeLA) #Los Angeles, CA has 1,232
HomeNotLA <- which(M$Hometown != "Los Angeles, CA"); HomeNotLA #All people who were not from LA 
length(HomeNotLA) #Not LA has 1,821
ConvertsA <-which(M$Former.Church == "Convert"); ConvertsA #All the converts
length(Converts) #427 converts
NotConverts <-which(M$Former.Church != "Convert"); NotConverts #All the non-converts
length(NotConverts) #2,626 non-converts

#Second, let's make some logical columns
LA <- M$Hometown == "Los Angeles, CA"
Converts <- M$Former.Church == "Convert"
HomeR <- data.frame(LA, Converts) #make a dataframe with just the logical columns

#Now let's build a contingency table
tbl <- table(HomeR$LA,HomeR$Converts); tbl
#Looking at the table, there are 1818 who are not originally from LA and aren't converts; 3 who are not originally from LA and are converts; 808 originally from LA who are not converts, and 424 originally from LA who are converts

#Now let's compare this with what the table would look like if Hometown and Convert status were independent
tbl #our actual table
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected #evidently, the tables are pretty different!!

#These tables look quite different. Is the difference significant? Let's use the chi-squared test and see.
chisq.test(HomeR$LA,HomeR$Converts)
#The p-value is incredibly tiny and the odds this arose by chance is less than 1 in a quadrillion; since the p-value is far less than 0.05 we reject the null hypothesis
#Therefore, having a Hometown of LA is very correlated with being a Convert to Christianity
#It appears that having a hometown that isn't LA is very correlated against being a Convert to Christianity
#In other words, almost everyone who joined the People's Independent Church of Christ who was not originally from LA was already a Christian; but according to the table roughly 1/3 of people with hometown LA were Converts to Christianity—a very, very interesting finding!! 

#END REQUIRED ANALYSIS 3


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
