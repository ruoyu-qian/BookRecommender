## server.R

# install.packages("devtools")
# devtools::install_github("stefanwilhelm/ShinyRatingInput")
# install.packages("shiny")
# install.packages('Rcpp')

source('functions/system1_functions.R')

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

movies2 = cbind(movies)
movies2$row_names = rownames(movies)
movie_ratings = read.csv("movie_ratings.csv")
rating_count = rep(1,nrow(movie_ratings))
movie_ratings["rating_count"] = rating_count
movie_rating_agg = aggregate(cbind(Rating, rating_count) ~ MovieID + Title + genre1 + 
                               genre2 + genre3 + genre4, data = movie_ratings, FUN = sum, na.rm = TRUE)

# get ratings from database
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

# rank the movies by rating count
tmp = ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID')

ranking = tmp[order(-tmp$ratings_per_movie),]

# get matrix
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

# modeling
rec.UBCF = Recommender(Rmat, "UBCF",
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))
rec.UBCF@model$weighted = FALSE

shinyServer(function(input, output, session) {
  ### System I
  # show the moview to be rated
  output$ratings1 <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 1)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings1 <- get_user_ratings(value_list)
      user_rating_df = as.data.frame(user_ratings1)
      
      genre = get_favorite_genre(user_rating_df,movie_rating_agg)
      
      if (is.null(genre)){
        genre_list = c("Drama","Animation","Comedy","Crime","Adventure","Action")
        genre_index = sample(1:6, 1)
        genre = genre_list[genre_index]
      }
      
      top_rated = top_rate_recommendation(movie_ratings,movie_rating_agg,10,genre)
      
      user_predicted_ids1 = top_rated[1:10,"MovieID"]
      titles = top_rated[1:10,"Title"]
      user_results = top_rated[1:10,"mean_rating_weighted"]
      #      user_predicted_ids = 1:10
      movie_index = c()
      for (i in 1:10)
      {
        mslice = filter(movies2, MovieID == user_predicted_ids1[i])
        movie_index = c(movie_index,mslice$row_names)
      }
      movie_index = strtoi(movie_index, base=0L)
      recom_results1 <- data.table(Rank = 1:10, 
                                  MovieID = movie_index,
                                  #                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  #                                  Title = movies$Title[user_predicted_ids], 
                                  Title = titles,
                                  Predicted_rating =  user_results,
                                  Genre = genre)
      
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results1 <- renderUI({
    
    num_rows <- 2
    num_movies <- 5
    recom_result1 <- df1()
    print(recom_result1)
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result1$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result1$MovieID[(i - 1) * num_movies + j]])
            ),
            div(style="text-align:center; font-size: 120%; color: #f0ad4e;", 
               strong(paste("Genre: ", recom_result1$Genre[(i - 1) * num_movies + j]))
            )
            
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  ### System II
  # show the moview to be rated
  output$ratings2 <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = ranking$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(ranking$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", ranking$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings2 <- get_user_ratings(value_list)

      # construct new Rmat
      movieIDs = colnames(Rmat)
      n.item = ncol(Rmat)
      # length(unique(ratings$MovieID)) # as as n.item
      new.ratings = rep(NA, n.item)
      new.ratings[which(movieIDs == "m1193")] = 5
      
      movieIDs = colnames(Rmat)
      n.item = ncol(Rmat)
      new.ratings = rep(NA, n.item)
      for (m in user_ratings2$MovieID) {
        new.ratings[which(movieIDs == paste0('m', m))] = user_ratings2[user_ratings2$MovieID == m, 'Rating']
      }
      new.user = matrix(new.ratings, 
                        nrow=1, ncol=n.item,
                        dimnames = list(
                          user=paste('feng'),
                          item=movieIDs
                        ))
      new.Rmat = as(new.user, 'realRatingMatrix')
      
      recom = predict(rec.UBCF, new.Rmat, type = 'ratings')
      user_predicted_ids2 = order(as(recom, "matrix"), decreasing = TRUE)[1:10]
      user_predicted_ratings = sort(as(recom, "matrix"), decreasing = TRUE)[1:10]
      MovieID = ratings$MovieID[user_predicted_ids2]
      print(MovieID)
      # recom2 = predict(rec.UBCF, new.Rmat, type = 'topN')
      # recom2@items
      # recom2@ratings
      #movies[match(user_predicted_ids, movies$MovieID), ]
      
      recom_results2 <- data.table(Rank = 1:10, 
                                  MovieID = ratings$MovieID[user_predicted_ids2], 
                                  Title = ranking[match(ratings$MovieID[user_predicted_ids2], ranking$MovieID), 'Title'], 
                                  Predicted_rating =  user_predicted_ratings)
      print(recom_results2)
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results2 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result2 <- df2()
    print(recom_result2)

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = ranking[match(recom_result2$MovieID[(i - 1) * num_movies + j], ranking$MovieID), 'image_url'], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(ranking[match(recom_result2$MovieID[(i - 1) * num_movies + j], ranking$MovieID), 'Title'])
            )
            
        )        
      }))) # columns
    }) # rows

  }) # renderUI function

}) # server function
