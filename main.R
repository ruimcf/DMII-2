library(rvest)
library(stringr)

## GET list of --MOVIE ID'S-- from a query string
searchTitle <- function(query, max=200){
  query <- URLencode(query)
  resultPage <- read_html(str_interp("http://www.imdb.com/find?q=${query}&s=tt"))
  filmList <- html_nodes(resultPage, ".findList")
  filmList <- html_children(filmList)
  listCount <- length(filmList)
  if(!listCount){ 
    return(NULL)
  }
  if(max < listCount){
    listCount = max
  }
  movieList <- c()
  for(i in 1:listCount){
    movieAnchor <- html_children(html_children(filmList[i])[2])[1]
    movie <- str_split(movieAnchor,"/")[[1]][3]
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

extractScore <- function(html_node) {
  scoreImg <- html_node(html_node, "h2+ img")
  if(!is.na(scoreImg)){
    score <- html_attr(scoreImg, "alt")
    score <- as.integer(str_split(score,"/")[[1]][1])
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
    title <- html_text(html_node(reviewsNodeList[i], "h2"))
    text <- html_text(reviewsNodeList[i+1])
    text <- paste(title, text, "\n")
    reviews$scores <- c(reviews$scores, score)
    reviews$text <- c(reviews$text, text)
  }
  return(reviews)
}

removeNaScores <- function(X){
  X$text <- X$text[!is.na(X$scores)]
  X$scores <- X$scores[!is.na(X$scores)]
  return(X)
}

movieList <- searchTitle("Kill Bill")
details <- getDetails(movieList[1])
print(details)
# movieReviewsList <- getReviews(movieList[1])
load("KillBillReviews.Rdata") # loads movieReviewsList
print(movieReviewsList$text[1])
print(movieReviewsList$scores[1])
movieReviewsList <- removeNaScores(movieReviewsList)
library(tm)
library(SnowballC)
library(wordcloud)
reviews <- VCorpus(VectorSource(movieReviewsList$text))
## terms = 34201
## non sparse entries = 232338/56678125

## Stripping  white space
reviews <- tm_map(reviews, stripWhitespace)
## terms = X
## non sparse entries = X

## Converting everything to lowercase
reviews <- tm_map(reviews, content_transformer(tolower))
## terms = 34201
## non sparse entries =X

## Remove ponctuation, symbols, and digits, everything that isn't a word
f <- content_transformer(function(x, pattern, sub) gsub(pattern, sub, x))
reviews <- tm_map(reviews, f, "\\W|\\d", " ")
## Fix double space caused by previous transformation, not sure if needed
reviews <- tm_map(reviews, f, "  ", " ")

## Removing English stopwords
## We are not really sure that all reviews are in english
reviews <- tm_map(reviews, removeWords, stopwords("english"))
## terms =X
## non sparse entries:X

## Stemming the words (keeping only the "root" of each word)
reviews <- tm_map(reviews, stemDocument)
## terms =X
## non sparse entries:X


dtm <- DocumentTermMatrix(reviews)
inspect(dtm)
dtm2 <- weightTfIdf(dtm)


for(i in 1:10){
  wordcloud(reviews[movieReviewsList$scores == i], colors = rainbow(20))
  readline(prompt=str_interp("Score: ${i}\nPress [enter] to continue"))
}