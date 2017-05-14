library(rvest)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(e1071) 
library(performanceEstimation)

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

getReviews <- function(movie_id, count = 0){
  indexReviewsPage <- read_html(str_interp("http://www.imdb.com/title/${movie_id}/reviews-index?"))
  showAllPartialUrl <- html_nodes(indexReviewsPage, "table+ table a+ a") %>% html_attr("href")
  if(count > 0){
    showAllPartialUrl <- gsub("count=(\\d)+", str_interp("count=${count}"), showAllPartialUrl)
  }
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

getTitlesByGenre <- function(genre, count = 50){
  url <- str_interp("http://www.imdb.com/search/title?genres=${genre}&sort=num_votes")
  print(url)
  page <- read_html(url)
  table <- html_nodes(page, ".lister-list")
  filmList <- html_children(table)
  movieList <- c()
  for(i in 1:count){
    m <- regexpr("data-tconst=\"\\w+\"", filmList[i])
    movieId <- str_split(regmatches(filmList[i], m)[1], "\"")[[1]][2]
    movieList <- c(movieList, movieId)
  }
  return(movieList)
}


transformCorpus <- function(reviews){
  ## Stripping  white space
  reviews <- tm_map(reviews, stripWhitespace)
  ## Converting everything to lowercase
  reviews <- tm_map(reviews, content_transformer(tolower))
  ## Remove ponctuation, symbols, and digits, everything that isn't a word
  f <- content_transformer(function(x, pattern, sub) gsub(pattern, sub, x))
  reviews <- tm_map(reviews, f, "\\W|\\d|_", " ")
  ## Fix double space caused by previous transformation, not sure if needed
  reviews <- tm_map(reviews, f, "  ", " ")
  ## Removing English stopwords
  ## We are not really sure that all reviews are in english
  reviews <- tm_map(reviews, removeWords, stopwords("english"))
  ## Stemming the words (keeping only the "root" of each word)
  reviews <- tm_map(reviews, stemDocument)
  return(reviews)
}

mostCommon <- function(reviews, count = 0.8){
  corpus <- transformCorpus(VCorpus(VectorSource(movieReviewsList$text[reviews$scores == 1])))
  dtm <- DocumentTermMatrix(corpus)
  dtm <- weightTfIdf(dtm)
  inspect(dtm)
  common <- findFreqTerms(dtm, count)
  for(i in 2:10){
    corpus <-transformCorpus(VCorpus(VectorSource(movieReviewsList$text[reviews$scores == i])))
    dtm <- DocumentTermMatrix(corpus)
    dtm <- weightTfIdf(dtm)
    mostFrequent <- findFreqTerms(dtm, count)
    toRemove <- c()
    flag <- FALSE
    for(j in 1:length(common)){
      if(!(common[j] %in% mostFrequent)){
        toRemove <- c(toRemove, j)
        flag = TRUE
      }
    }
    if(flag){
      common <- common[-toRemove]
    }
  }
  return(common)
}


movieList <- searchTitle("Kill Bill")
details <- getDetails(movieList[1])
print(details)
# movieReviewsList <- getReviews(movieList[1])
load("KillBillReviews.Rdata") # loads movieReviewsList
print(movieReviewsList$text[1])
print(movieReviewsList$scores[1])
movieReviewsList <- removeNaScores(movieReviewsList)
reviews <- VCorpus(VectorSource(movieReviewsList$text))

reviews <- transformCorpus(reviews)
dtm <- DocumentTermMatrix(reviews)
inspect(dtm)
#dtm <- removeSparseTerms(dtm, 0.95)
dtm2 <- weightTfIdf(dtm)

## REMOVER PALAVRAS COMUNS
common <- mostCommon(movieReviewsList, 0.2)
commonReviews <- tm_map(reviews, removeWords, common)

finalDataSet <- cbind(data.frame(as.matrix(dtm2), Score=movieReviewsList$scores))

estimationRegression <- performanceEstimation(
  PredTask(Score ~ ., finalDataSet),
  c(workflowVariants(learner="svm", learner.pars=list(cost=c(1,3,10,13), kernel=c("linear")))),
  EstimationTask(metrics="mse")
  )

toRank <- function(score) {
  if(score < 5) {
    return(c("low"))
  }
  else if(score > 7) {
    return(c("high"))
  }
  else {
    return(c("medium"))
  }
}

buildTrainingDataSet_Class <- function(weightdtm, scores) {
  classes <- sapply(scores, toRank) 
  return(cbind(data.frame(as.matrix(weightdtm), Score=classes)))
}

finalDataSet_class <- buildTrainingDataSet_Class(dtm2, movieReviewsList$scores)

estimationClassification <- performanceEstimation(
  PredTask(Score ~ ., finalDataSet_class),
  c( Workflow(learner="naiveBayes"),
     workflowVariants(learner="svm", learner.pars=list(kernel=c("linear", "radial")))
       ),
  EstimationTask(metrics="err", method=CV())
  ) 





#randomRows <- sample(1: nrow(finalDataSet), as.integer(0.7*nrow(finalDataSet)))
#train <- finalDataSet[randomRows, ]
#test <- finalDataSet[-randomRows, ]
#m <- svm(Score ~ ., train) 
#preds <- predict(m, test) 

## Best result: cost=10, kernel=linear. MSE = 5.01

for(i in 1:10){
  wordcloud(commonReviews[movieReviewsList$scores == i], colors = rainbow(20))
  readline(prompt=str_interp("Score: ${i}\nPress [enter] to continue"))
}


genreList <- c("")

