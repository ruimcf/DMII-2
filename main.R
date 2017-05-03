library(rvest)
library(stringr)

## GET list of --MOVIE ID'S-- from a query string
searchTitle <- function(query, max=200){
  query <- URLencode(query)
  resultPage <- read_html(str_interp("http://www.imdb.com/find?q=${query}&s=tt"))
  filmList <- html_nodes(resultPage, ".result_text")
  listCount <- length(filmList)
  if(!listCount){ 
    return(NULL)
  }
  if(max < listCount){
    listCount = max
  }
  movieList <- c()
  for(i in 1:listCount){
    if(!i%%2){
      movie <- html_nodes(resultPage, str_interp(".even:nth-child(${i}) .result_text"))
    }else{
      movie <- html_nodes(resultPage, str_interp(".odd:nth-child(${i}) .result_text"))
    }
    movie <- str_split(html_children(movie),"/")[[1]][3]
    movieList <- c(movieList, movie)
  }
  return(movieList)
}

movieList <- searchTitle("Kill Bill")