scrapeWiki <- function(URL){
  # TODO: convert HTML numbers (&#160;%)
  
  # URL <- "http://en.wikipedia.org/wiki/Democracy_index"
  d <- read.delim(URL)
  d <- as.character(d[,1])
  
  key1 <- "wikitable"
  key2 <- "/table"
  
  #=====> find the wikitables <=====#
  findKey1 <- unlist(lapply(strsplit(d, key1), length))
	tStart <- which(findKey1 > 1)
  findKey2 <- unlist(lapply(strsplit(d, key2), length))
  key2Ind <- which(findKey2 > 1)
# Edits to tEnd 
  n    <- length(tStart) # moved from lower down
  tEnd <- rep(NA, n)
  for(i in 1:n){
  	tEnd[i] <- key2Ind[(key2Ind - tStart[1]) > 0][i]
  }
  
  #=====> format each table into a dataframe <=====#
  extractCell <- function(x){
  
  # Added {1,100} to expression below
    y <- regexpr(">\\w{1,100}<", x)
    y <- substr(x, y + 1, as.integer(unlist(attributes(y))) + y - 2)
    z <- regexpr(">.+<", y)
    substr(y, z + 1, as.integer(unlist(attributes(z))) + z - 2)
    
    x <- substr(x, 2, nchar(x) - 1)
    y <- unlist(strsplit(x, ">"))
    y <- y[y != "td" | y != "th"]
    z <- unlist(strsplit(y, "<"))
    }
  
  regexpr(">.+", x)
  
  
  dataTabs <- list()
  for(i in 1:n){
    tab <- d[tStart[i]:tEnd[i]]
    findTH <- c(lapply(strsplit(tab, "/th"), length), recursive=TRUE)
    nHeads <- sum(findTH > 1)
    findTR <- c(lapply(strsplit(tab, "/tr"), length), recursive=TRUE)
    nRows <- ifelse(nHeads > 0, sum(findTR > 1) - 1, sum(findTR > 1))
    findTD <- c(lapply(strsplit(tab, "/td"), length), recursive=TRUE)
    nCells <- sum(findTD > 1)
    tabCells <- tab[findTD > 1]
    cellVec <- unlist(lapply(tabCells, extractCell))
    dframe <- data.frame(matrix(cellVec, nrow = nRows, byrow = TRUE))
    if(nHeads > 0){
      tabHead <- tab[findTH > 1]
      names(dframe) <- unlist(lapply(tabHead, extractCell))
    }
    dataTabs <- c(dataTabs, dframe)
  }
  

  
}