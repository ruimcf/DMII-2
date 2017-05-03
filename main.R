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
