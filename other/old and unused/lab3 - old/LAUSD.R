
#=====> Extract Teacher URLs From Search Results <=====#
extractNames <- function(SearchResults, teacher="/value-added/teacher/"){
  d <- read.delim(SearchResults, sep='`')
	d <- as.character(d[,1])
	n <- length(d)
	
	#=====> Find Teacher Lines <=====#
	redD <- c(lapply(strsplit(d, teacher), length), recursive=TRUE)
	redD <- redD > 1
	
	#=====> Location Left <=====#
	tNames <- rep(sum(redD))
	k    <- 0
	for(i in which(redD)){
		k       <- k+1
		temp    <- strsplit(d[i], 'href=')[[1]][2]
		tNames[k]   <- strsplit(temp, '>')[[1]][1]
	}
	return(tNames[1:(length(tNames)-3)]) # remove three linked teachers in side-bar
}


#=====> Apply extractNames() To Build URL List <=====#
SearchList <- list("http://projects.latimes.com/value-added/search/teacher/?q=a+a+a", "http://projects.latimes.com/value-added/search/teacher/?q=a+a+e", "http://projects.latimes.com/value-added/search/teacher/?q=a+a+i", "http://projects.latimes.com/value-added/search/teacher/?q=a+a+o", "http://projects.latimes.com/value-added/search/teacher/?q=a+a+u", "http://projects.latimes.com/value-added/search/teacher/?q=a+a+y", "http://projects.latimes.com/value-added/search/teacher/?q=e+a+a", "http://projects.latimes.com/value-added/search/teacher/?q=e+a+e", "http://projects.latimes.com/value-added/search/teacher/?q=e+a+i", "http://projects.latimes.com/value-added/search/teacher/?q=e+a+o", "http://projects.latimes.com/value-added/search/teacher/?q=e+a+u", "http://projects.latimes.com/value-added/search/teacher/?q=e+a+y", "http://projects.latimes.com/value-added/search/teacher/?q=i+a+a", "http://projects.latimes.com/value-added/search/teacher/?q=i+a+e", "http://projects.latimes.com/value-added/search/teacher/?q=i+a+i", "http://projects.latimes.com/value-added/search/teacher/?q=i+a+o", "http://projects.latimes.com/value-added/search/teacher/?q=i+a+u", "http://projects.latimes.com/value-added/search/teacher/?q=i+a+y", "http://projects.latimes.com/value-added/search/teacher/?q=o+a+a", "http://projects.latimes.com/value-added/search/teacher/?q=o+a+e", "http://projects.latimes.com/value-added/search/teacher/?q=o+a+i", "http://projects.latimes.com/value-added/search/teacher/?q=o+a+o", "http://projects.latimes.com/value-added/search/teacher/?q=o+a+u", "http://projects.latimes.com/value-added/search/teacher/?q=o+a+y", "http://projects.latimes.com/value-added/search/teacher/?q=u+a+a", "http://projects.latimes.com/value-added/search/teacher/?q=u+a+e", "http://projects.latimes.com/value-added/search/teacher/?q=u+a+i", "http://projects.latimes.com/value-added/search/teacher/?q=u+a+o", "http://projects.latimes.com/value-added/search/teacher/?q=u+a+u", "http://projects.latimes.com/value-added/search/teacher/?q=u+a+y", "http://projects.latimes.com/value-added/search/teacher/?q=y+a+a", "http://projects.latimes.com/value-added/search/teacher/?q=y+a+e", "http://projects.latimes.com/value-added/search/teacher/?q=y+a+i", "http://projects.latimes.com/value-added/search/teacher/?q=y+a+o", "http://projects.latimes.com/value-added/search/teacher/?q=y+a+u", "http://projects.latimes.com/value-added/search/teacher/?q=y+a+y")


bigNames <- vector("list", 36)
for(i in 1:36){
  bigNames[[i]] <- extractNames(SearchList[[i]])
  Sys.sleep(exp(runif(1,0.5,2)))
}

bigNames <- unlist(bigNames)
bigNames <- unique(bigNames)
addRoot <- function(x){
  paste('http://projects.latimes.com', x, sep="")
}
teacherURLs <- unlist(lapply(bigNames, addRoot))
write.csv(teacherURLs, "teacherURLS.csv", row.names=F)
  
  
  
#=====> Extract 'left' Attribute Prior Diamond <=====#
extractDat <- function(URL, diamond="value_added_v2/graph/red_diamond.png"){
	d <- read.delim(URL, sep='`')
	d <- as.character(d[,1])
	n <- length(d)
	
	#=====> Find Red Diamond Lines <=====#
	redD <- c(lapply(strsplit(d, diamond), length), recursive=TRUE)
	redD <- redD > 1
	
	#=====> Location Left <=====#
	left <- rep(sum(redD))
	k    <- 0
	for(i in which(redD)){
		k       <- k+1
		temp    <- strsplit(d[i-1], ' left: ')[[1]][2]
		temp    <- strsplit(temp, 'px;')[[1]][1]
		left[k] <- as.numeric(temp)
	}
	Mpx <- left[1]
  Epx <- left[2]
 
  #=====> Find Number of Students <=====#
  findn <- c(lapply(strsplit(d, 'strong'), length), recursive=TRUE)
	findn <-  findn > 1
	temp <- d[which(findn)[1]]
	temp    <- strsplit(temp, ' test scores from <strong>')[[1]][2]
	temp    <- strsplit(temp, '</strong> students')[[1]][1]
	numS <- as.numeric(temp)
  
  #=====> Find School Name <=====#
  school <- c(lapply(strsplit(d, 'teacher at'), length), recursive=TRUE)
  school <- school > 1
  temp <- d[which(school)]
  temp    <- strsplit(temp, '/>')[[1]][2]
  sch    <- strsplit(temp, '</a>')[[1]][1]
  
  #=====> Combine Data <=====#
  return(list(Mpx, Epx, numS, sch))
}

 
#=====> Apply extractDat() On Sample Of Teacher URLS <=====#
URLs <- read.csv("teacherURLs.csv")
set.seed(344)
n <- 150
ind <- sample(1:dim(URLs)[1], n)

teacher <- as.character(URLs[ind,])
Mpx <- rep(0,n)
Epx <- rep(0,n)
numS <- rep(0,n)
sch <- rep('a',n)

for(i in 1:n){
  temp <- extractDat(as.character(URLs[ind[i],]))
  Mpx[i] <- temp[[1]]
  Epx[i] <- temp[[2]]
  numS[i] <- temp[[3]]
  sch[i] <- temp[[4]]
  Sys.sleep(exp(runif(1,0.5,2)))
}

LAUSDsamp <- data.frame(teacher, Mpx, Epx, numS, sch)
names(LAUSDsamp) <- c("teacherURL", "math", "eng", "nstudents", "school")
write.csv(LAUSDsamp, "LAUSDsamp.csv", row.names=F)
  
  
  
g <- extractDat(URL) 
URL <- 'http://projects.latimes.com/value-added/teacher/julie-rene-rivas/'