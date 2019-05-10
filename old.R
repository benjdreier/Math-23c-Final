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

# Ben's work

# Let's talk churches. It looks like there are 759 unique churches listed here:
Churches <- unique(M$Former.Church); Churches

# Enumerate some words pointing to church denomination
Denoms <- c("Baptist", "Methodist", "AME", "ME", "Episcopal", "CME", "Presbyterian", "Catholic")

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
  
Churches[which(WhichDenoms == "")]

length(Churches)

denoms[3]

