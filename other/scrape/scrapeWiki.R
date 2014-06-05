scrapeWiki <- function(URL){
  # TODO: convert HTML numbers (&#160;%)
  #       replace heading spaces with "."
  #       fill in missing cells with NA (URL <- "http://en.wikipedia.org/wiki/International_wheat_production_statistics")
  #       add additional checks for correct wikitable
  #       redo find wikitables section using grepl()
  
  # Tester: URL <- "http://en.wikipedia.org/wiki/Democracy_index"

  d <- read.delim(URL)
  d <- as.character(d[,1])
  
  key1 <- "wikitable"
  key2 <- "/table"
  
  #=====> find the wikitables <=====#
  findKey1 <- unlist(lapply(strsplit(d, key1), length))
	tStart <- which(findKey1 > 1)
  findKey2 <- unlist(lapply(strsplit(d, key2), length))
  key2Ind <- which(findKey2 > 1)
  tEnd <- c(key2Ind[(key2Ind - tStart[1]) > 0][1], key2Ind[(key2Ind - tStart[2]) > 0][1], key2Ind[(key2Ind - tStart[3]) > 0][1])
  
  #=====> format each table into a dataframe <=====#
  removeTags <- function(x){
    tag <- "<.*?>"
    x <- gsub(tag,"",x)
  }
  n <- length(tStart)
  dataTabs <- list(1,2,3)
  
  for(i in 1:n){
    tab <- d[tStart[i]:tEnd[i]]
    findTH <- grepl("</th>",tab)
    nHeads <- sum(findTH)
    findTR <- grepl("</tr>",tab)
    nRows <- ifelse(nHeads > 0, sum(findTR) - 1, sum(findTR ))
    findTD <- grepl("</td>",tab)
    nCells <- sum(findTD)
    tabCells <- tab[findTD]
    cellVec <- unlist(lapply(tabCells, removeTags))
    dframe <- data.frame(matrix(cellVec, nrow = nRows, byrow = TRUE))
    if(nHeads > 0){
      tabHead <- tab[findTH]
      names(dframe) <- unlist(lapply(tabHead, removeTags))
    }
    dataTabs[[i]] <- dframe
  }
  
  return(dataTabs)
}