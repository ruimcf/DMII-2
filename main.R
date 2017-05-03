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

<<<<<<< HEAD
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
=======
extractScore <- function(html_node) {
  scoreImg <- html_node(html_node, "h2+ img")
  if(!is.na(scoreImg)){
    score <- html_attr(scoreImg, "alt")
  }
  else{
    score <- NA
  }
  return(score)
}

getReviews <- function(movie_id){
  indexReviewsPage <- read_html(str_interp("http://www.imdb.com/title/${movie_id}/reviews-index?"))
  showAllPartialUrl <- html_nodes(indexReviewsPage, "table+ table a+ a") %>% html_attr("href")
  showAllReviewsPartialUrl <-gsub("-index","", showAllPartialUrl)
  showAllReviewsUrl <- str_interp("http://www.imdb.com/title/${movie_id}/${showAllReviewsPartialUrl}")
  
  listReviewsPage <- read_html(showAllReviewsUrl) 
  reviewsNodeList <- html_nodes(listReviewsPage, "#tn15content div+ p , hr+ div")
  reviews <- list()
  for(i in seq(1, length(reviewsNodeList), 2)){
    score <- extractScore(reviewsNodeList[i])          
    text <- html_text(reviewsNodeList[i+1])
    reviews$scores <- c(reviews$scores, score)
    reviews$text <- c(reviews$text, text)
  }
  return(reviews)
}

movieList <- searchTitle("Kill Bill")
movieReviewsList <- getReviews(movieList[1])
print(movieReviewsList$text[1])
print(movieReviewsList$scores[1])
>>>>>>> 5e0c0cb3c3f4b5bc69de4477f44510e67a0b5da0
