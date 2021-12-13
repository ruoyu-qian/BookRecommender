library(dplyr)
library(data.table)

top_rate_recommendation <- function(movie_ratings,movie_rating_agg,top_number,genre){
  total_ratings = nrow(movie_ratings)
  total_users_rated = nrow(unique(movie_ratings[c("UserID")]))
  total_movies_wreviews = nrow(unique(movie_ratings[c("MovieID")]))
  average_ratings_per_movie = total_ratings/total_movies_wreviews
  average_ratings_per_user = total_ratings/total_users_rated
  average_rating = mean(movie_ratings[["Rating"]])
  
  
  movie_rating_agg["rating_weighted"] = movie_rating_agg["Rating"] + average_rating * average_ratings_per_movie
  movie_rating_agg["rating_count_weighted"] = movie_rating_agg["rating_count"] + average_ratings_per_movie
  
  movie_rating_agg["mean_rating"] = movie_rating_agg["Rating"]/movie_rating_agg["rating_count"]
  movie_rating_agg["mean_rating_weighted"] = movie_rating_agg["rating_weighted"]/movie_rating_agg["rating_count_weighted"]
  
  genre_filtered = filter(movie_rating_agg, (genre1 == genre) | (genre2 == genre) | (genre3 == genre) | (genre4 == genre) )
  genre_filtered = genre_filtered[order(genre_filtered$mean_rating_weighted,decreasing = TRUE),]
  rownames(genre_filtered) <- NULL
  return(genre_filtered[1:top_number, ])
}

most_popular_recommendation <- function(movie_ratings,movie_rating_agg,top_number,genre){
  total_ratings = nrow(movie_ratings)
  total_users_rated = nrow(unique(movie_ratings[c("UserID")]))
  total_movies_wreviews = nrow(unique(movie_ratings[c("MovieID")]))
  average_ratings_per_movie = total_ratings/total_movies_wreviews
  average_ratings_per_user = total_ratings/total_users_rated
  average_rating = mean(movie_ratings[["Rating"]])
  
  
  movie_rating_agg["mean_rating"] = movie_rating_agg["Rating"]/movie_rating_agg["rating_count"]
  
  
  genre_filtered = filter(movie_rating_agg, (genre1 == genre) | (genre2 == genre) | (genre3 == genre) | (genre4 == genre) & (mean_rating > average_rating))
  
  genre_filtered = genre_filtered[order(genre_filtered$rating_count,decreasing = TRUE),]
  return(genre_filtered[1:top_number, ])
}

get_favorite_genre <- function(ratings, movie_ratings_agg){

  selected_movies <- subset(movie_ratings_agg, MovieID %in% matrix(ratings[,"MovieID"], dimnames = NULL))
  genres = selected_movies[c("genre1","genre2","genre3","genre4")]
  
  names(genres) <- NULL
  genres <- unlist(genres)
  
  genres <- genres[genres!="unassigned"]
  genres <- sort(genres)
  genres = sort(table(genres),decreasing=TRUE)
  return(names(genres[1]))
}