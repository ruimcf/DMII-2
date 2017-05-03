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

getTitle <- function(movieID, page=NULL){
  if(is.null(page)){
    page <- read_html(str_interp("http://www.imdb.com/title/${movieID}"))
  }
  return(html_text(html_nodes(page, "#title-overview-widget h1")))
}

getYear <- function(movieID, page=NULL){
  if(is.null(page)){
    page <- read_html(str_interp("http://www.imdb.com/title/${movieID}"))
  }
  return(page %>% html_nodes("#titleYear a") %>% html_text() %>% as.numeric())
}

getCast <- function(movieID, page=NULL){
  if(is.null(page)){
    page <- read_html(str_interp("http://www.imdb.com/title/${movieID}"))
  }
  return(page %>% html_nodes("#titleCast .itemprop span") %>% html_text())
}


getDirector <- function(movieID, page=NULL){
  if(is.null(page)){
    page <- read_html(str_interp("http://www.imdb.com/title/${movieID}"))
  }
  return(html_nodes(page, ".credit_summary_item .itemprop")[1] %>% html_text())
}

getDetails <- function(movieID){
  link <- str_interp("http://www.imdb.com/title/${movieID}")
  page <- read_html(link)
  description <- c(link=link, title=getTitle(movieID,page), year=getYear(movieID,page),
                   director=getDirector(movieID,page), cast=getCast(movieID,page))
  return(description)
}

movieList <- searchTitle("Kill Bill")
details <- getDetails(movieList[1])
details