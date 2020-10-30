
# If the edx and validation datasets aren't already loaded in the R session, load them.

if(!exists("edx")){load("Data/R Data/edx.rda")}; if(!exists("validation")){load("Data/R Data/validation.rda")}

# Create the "Figures" folder if it doesn't already exist in the project's home directory.

if(!dir.exists("Figures")){dir.create("Figures")}

# Variable Importance metrics and figures:

# Define different sets of indices to be used to define different chunks of the edx dataset. Each chunk will correspond to a different worker R session in the parallel processing to follow.

set.seed(2020, sample.kind  = "Rounding")

edx.partition.indices <- createFolds(edx$rating, k = logical_CPUs)

# Define the different chunks of the edx dataset.

chunks <- lapply(edx.partition.indices, function(data, indices){data[indices, ]}, data = edx)

# Define the RunRandomForest function to be applied to each chunk of the edx dataset.

RunRandomForest <- function(tree_num){
  arg <- arg %>% mutate(movieId = as.factor(movieId), userId = as.factor(userId), genres = as.factor(genres))
  ranger(rating ~ movieId + userId + genres + rating_lapse + movie_age + rating_age, data = arg, num.trees = tree_num, importance = "impurity", respect.unordered.factors = TRUE)
}

# Define the cluster object to be referenced to utilize parallel processing when applying the RunRandomForest function.

RunRandomForest.cluster <- makeCluster(logical_CPUs); registerDoParallel(cl = RunRandomForest.cluster)

# Make each of the chunks defined above global in scope across all R sessions. This therefore makes these chunks global in scope within each of the worker R sessions created for parallel processing.

invisible(clusterApply(cl = RunRandomForest.cluster, chunks, make_global))

# Export the "ranger", "%>%" and "mutate" objects to each of the worker R sessions so the RunRandomForest function can be properly called within each worker R session.

clusterExport(cl = RunRandomForest.cluster, c("ranger", "%>%", "mutate"))

# Define a list containing the output of the RunRandomForest function.

set.seed(2020, sample.kind  = "Rounding")

edx.RandomForest <- clusterCall(cl = RunRandomForest.cluster, RunRandomForest, tree_num = 100)

# Close the cluster defined to utilize parallel processing when applying the RunRandomForest function.

stopCluster(RunRandomForest.cluster)

# Define the VariableImpsByChunk matrix containing the variable importances of the different variables for each chunk of the edx dataset.

for(j in 1:20){
  if(j == 1){
    VariableImpsByChunk <-  edx.RandomForest[[j]]$variable.importance
  }else{
    VariableImpsByChunk <- cbind(VariableImpsByChunk, edx.RandomForest[[j]]$variable.importance)
  }
}

# Assign column names to the VariableImpsByChunk matrix.

colnames(VariableImpsByChunk) <- paste("var.imp", seq(1:20), sep = "_")

# Define a sorted vector of average variable importances for the different variables.

VariableImps <- rowMeans(VariableImpsByChunk) %>% sort(decreasing = TRUE)

# Metrics and figures of distribution of ratings:

# Define a dataset consisting of the frequency of each rating.

Ratings.Distribution <- edx %>% group_by(rating) %>% summarize(Count = n(), .groups = "drop")

# Define, show and save a histogram showing the frequency of each rating.

Ratings.Histogram <- Ratings.Distribution %>% mutate(rating = factor(rating)) %>% ggplot(aes(rating, Count)) + geom_col(fill = "cornflowerblue", color = "white") +
  labs(x = "Rating", y = "Count", title = "Distribution of Ratings") + geom_vline(aes(xintercept = 7.02), color = "red") + annotate("text", x = 6.75, y = 2.25*10^6,
  label = round(sum(Ratings.Distribution$rating*Ratings.Distribution$Count)/sum(Ratings.Distribution$Count), 2), color = "red", size = 3) + ggthemes::theme_economist() +
  theme(plot.title = element_text(hjust = 0.5))

Show(Ratings.Histogram)

ggsave(filename = "Ratings_Histogram.png", plot = Ratings.Histogram, path = "Figures")

# Metrics and figures by movie:

# Define a dataset consisting of the frequency, average rating and rating standard deviation for each movie.

Ratings_by_Movie.Distribution <- edx %>% group_by(movieId) %>% summarize(Count = n(), Mu_gvn_Movie = mean(rating), Sd_gvn_Movie = sd(rating), .groups = "drop")

# Define, show and save a histogram showing the distribution of rating counts by movie.

Ratings_by_Movie.Counts.Histogram <- Ratings_by_Movie.Distribution %>% ggplot(aes(Count)) + geom_histogram(fill = "cornflowerblue", color = "white") +
  labs(title = "Distribution of Movie Rating Counts", x = "Movie Rating Count", y = "Movie Count") + geom_vline(aes(xintercept = mean(Count)), color = "red") + annotate("text", x = 1200, y = 550,
  label = round(mean(Ratings_by_Movie.Distribution$Count), 0), color = "red", size = 3) + scale_y_continuous(limits = c(-1, 801)) + scale_x_log10() + ggthemes::theme_economist() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5), legend.position = "none")

Show(Ratings_by_Movie.Counts.Histogram)

ggsave(filename = "Ratings_by_Movie_Counts_Histogram.png", plot = Ratings_by_Movie.Counts.Histogram, path = "Figures")

# Define, show and save a density plot showing the distributions of rating standard deviations by movie, with one density corresponding to those movies with at most the median number of ratings per
# movie, and the other corresponding to those movies with more than the median number of ratings per movie.

Ratings_by_Movie.StDevs.Density <- Ratings_by_Movie.Distribution %>% mutate(Partition = cut(Count, breaks = c(-Inf, median(Count), Inf), labels = c("Rating Count <= 122", "Rating Count > 122"))) %>%
  ggplot(aes(Sd_gvn_Movie, fill = Partition)) + geom_density(alpha = 0.5) + labs(title = "Density of Rating Standard Deviation - By Movie Group", x = "Rating Standard Deviation", y = "Density") +
  ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5))

Show(Ratings_by_Movie.StDevs.Density)

ggsave(filename = "Ratings_by_Movie_StDevs_Density.png", plot = Ratings_by_Movie.StDevs.Density, path = "Figures")

# Define, show and save a scatterplot showing rating counts against rating averages by movie, including a smoothed trend line with a confidence interval.

Ratings_by_Movie.Counts_vs_Avgs.Scatterplot <- Ratings_by_Movie.Distribution %>% ggplot(aes(Count, Mu_gvn_Movie)) + geom_point(color = "cornflowerblue", alpha = 0.3) + geom_smooth() +
  labs(title = "Scatterplot of Movie Rating Count vs Average Rating", x = "Movie Rating Count", y = "Average Rating") + geom_vline(aes(xintercept = mean(Count)), color = "red") +
  annotate("text", x = 2000, y = 5, label = round(mean(Ratings_by_Movie.Distribution$Count), 0), color = "red", size = 3) + ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5))

Show(Ratings_by_Movie.Counts_vs_Avgs.Scatterplot)

ggsave(filename = "Ratings_by_Movie_Counts_vs_Avgs_Scatterplot.png", plot = Ratings_by_Movie.Counts_vs_Avgs.Scatterplot, path = "Figures")

# Metrics and figures by user:

# Define a dataset consisting of the frequency, average rating and rating standard deviation for each user.

Ratings_by_User.Distribution <- edx %>% group_by(userId) %>% summarize(Count = n(), Mu_gvn_User = mean(rating), Sd_gvn_User = sd(rating), .groups = "drop")

# Define, show and save a histogram showing the distribution of rating counts by user.

Ratings_by_User.Counts.Histogram <- Ratings_by_User.Distribution %>% ggplot(aes(Count)) + geom_histogram(fill = "cornflowerblue", color = "white") +
  labs(title = "Distribution of User Rating Counts", x = "User Rating Count", y = "User Count") + geom_vline(aes(xintercept = mean(Count)), color = "red") + annotate("text", x = 175, y = 6000,
  label = round(mean(Ratings_by_User.Distribution$Count), 0), color = "red", size = 3) + scale_y_continuous(limits = c(-1, 8001)) + scale_x_log10() + ggthemes::theme_economist() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5), legend.position = "none")

Show(Ratings_by_User.Counts.Histogram)

ggsave(filename = "Ratings_by_User_Counts_Histogram.png", plot = Ratings_by_User.Counts.Histogram, path = "Figures")

# Define, show and save a density plot showing the distributions of rating standard deviations by user, with one density corresponding to those users with at most the median number of ratings per
# user, and the other corresponding to those users with more than the median number of ratings per user.

Ratings_by_User.StDevs.Density <- Ratings_by_User.Distribution %>% mutate(Partition = cut(Count, breaks = c(-Inf, median(Count), Inf), labels = c("Rating Count <= 62", "Rating Count > 62"))) %>%
  ggplot(aes(Sd_gvn_User, fill = Partition)) + geom_density(alpha = 0.5) + labs(title = "Density of Rating Standard Deviation - By User Group", x = "Rating Standard Deviation", y = "Density") +
  ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5))

Show(Ratings_by_User.StDevs.Density)

ggsave(filename = "Ratings_by_User_StDevs_Density.png", plot = Ratings_by_User.StDevs.Density, path = "Figures")

# Define, show and save a scatterplot showing rating counts against rating averages by user, including a smoothed trend line with a confidence interval.

Ratings_by_User.Counts_vs_Avgs.Scatterplot <- Ratings_by_User.Distribution %>% ggplot(aes(Count, Mu_gvn_User)) + geom_point(color = "cornflowerblue", alpha = 0.3) + geom_smooth() +
  labs(title = "Scatterplot of User Rating Count vs Average Rating", x = "User Rating Count", y = "Average Rating") + geom_vline(aes(xintercept = mean(Count)), color = "red") +
  annotate("text", x = 400, y = 5, label = round(mean(Ratings_by_User.Distribution$Count), 0), color = "red", size = 3) + ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5))

Show(Ratings_by_User.Counts_vs_Avgs.Scatterplot)

ggsave(filename = "Ratings_by_User_Counts_vs_Avgs_Scatterplot.png", plot = Ratings_by_User.Counts_vs_Avgs.Scatterplot, path = "Figures")

# Metrics and figures by genre:

# Define a dataset consisting of the frequency, average rating and rating standard deviation for each unique genre.

Ratings_by_Genre.Distribution <- tibble()

for(j in 1:19){
  temp <- edx %>% group_by_(paste("`", colnames(edx)[j + 8], "`", sep = "")) %>% summarize(Count = n(), Mu_gvn_Genre = mean(rating), Sd_gvn_Genre = sd(rating), .groups = "drop") %>%
  slice_max(.[, 1]) %>% .[, 2:4] %>% mutate(Genre = colnames(edx)[j + 8]) %>% .[, c(4, 1:3)]
  if(j == 1){
    Ratings_by_Genre.Distribution <- as.matrix(temp)
  }else{
    Ratings_by_Genre.Distribution <- rbind(as.matrix(Ratings_by_Genre.Distribution), temp)
  }
}

Ratings_by_Genre.Distribution <- Ratings_by_Genre.Distribution %>% mutate(Count = as.numeric(Count), Mu_gvn_Genre = as.numeric(Mu_gvn_Genre), Sd_gvn_Genre = as.numeric(Sd_gvn_Genre))

# Define, show and save a scatterplot showing average rating up against rating standard deviation for each unique genre.

Ratings_by_Genre.Summary.Scatterplot <- Ratings_by_Genre.Distribution %>% ggplot(aes(x = Mu_gvn_Genre, y = Sd_gvn_Genre, label = Genre)) + geom_point(aes(x = Mu_gvn_Genre, y = Sd_gvn_Genre,
  size = Count), inherit.aes = FALSE) + geom_point(aes(color = Genre, size = Count), show.legend = FALSE) + geom_text_repel() + scale_size_continuous(name = "Count:", breaks = 10^6*c(1, 2, 3),
  labels = c("1*10^6", "2*10^6", "3*10^6")) + labs(title = "Scatterplot of Average Rating vs Rating Standard Deviation - By Genre", x = "Average Rating", y = "Rating Standard Deviation") +
  ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5))

Show(Ratings_by_Genre.Summary.Scatterplot)

ggsave(filename = "Ratings_by_Genre_Summary_Scatterplot.png", plot = Ratings_by_Genre.Summary.Scatterplot, path = "Figures")

# Metrics and figures by rating lapse:

# Define a dataset consisting of the frequency, average rating and rating standard deviation for each rating_lapse value.

Ratings_by_Rating_Lapse.Distribution <- edx %>% group_by(rating_lapse) %>% summarize(Count = n(), Mu_gvn_Rating_Lapse = mean(rating), Sd_gvn_Rating_Lapse = sd(rating), .groups = "drop")

# Define, show and save a histogram showing the frequency of each value of the rating_lapse variable.

Ratings_by_Rating_Lapse.Histogram <- Ratings_by_Rating_Lapse.Distribution %>% ggplot(aes(rating_lapse, Count)) + geom_col(fill = "cornflowerblue", color = "white") +
  labs(x = "Rating Lapse", y = "Rating Count", title = "Distribution of Rating Lapse") + scale_x_continuous(limits = c(-1, 100)) + scale_y_log10() + ggthemes::theme_economist() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5), legend.position = "none")

Show(Ratings_by_Rating_Lapse.Histogram)

ggsave(filename = "Ratings_by_Rating_Lapse_Histogram.png", plot = Ratings_by_Rating_Lapse.Histogram, path = "Figures")

# Define, show and save a scatterplot showing average ratings against number of years lapsed between movie release and rating, including a smoothed trend line with a confidence interval.

Ratings_by_Rating_Lapse.Rating_Lapse_vs_Avgs.Scatterplot <- Ratings_by_Rating_Lapse.Distribution %>% ggplot(aes(rating_lapse, Mu_gvn_Rating_Lapse)) + geom_point(color = "steelblue4", alpha = 0.3) +
  geom_smooth() + labs(title = "Scatterplot of Rating Lapse vs Average Rating", x = "Rating Lapse", y = "Average Rating") + ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5))

Show(Ratings_by_Rating_Lapse.Rating_Lapse_vs_Avgs.Scatterplot)

ggsave(filename = "Ratings_by_Rating_Lapse_Rating_Lapse_vs_Avgs_Scatterplot.png", plot = Ratings_by_Rating_Lapse.Rating_Lapse_vs_Avgs.Scatterplot, path = "Figures")

# Metrics and figures by movie age:

# Define a dataset consisting of the number of movies, number of ratings, average rating and rating standard deviation for each movie_age value.

Ratings_by_Movie_Age.Distribution <- edx %>% group_by(movie_age) %>% summarize(Rating_Count = n(), Movie_Count = n_distinct(movieId), Mu_gvn_Movie_Age = mean(rating),
  Sd_gvn_Movie_Age = sd(rating), .groups = "drop")

# Define, show and save a histogram showing the number of ratings by movie age.

Ratings_by_Movie_Age.Histogram <- Ratings_by_Movie_Age.Distribution %>% ggplot(aes(movie_age, Rating_Count)) + geom_col(fill = "cornflowerblue", color = "white") +
  labs(x = "Movie Age", y = "Rating Count", title = "Distribution of Movie Age") + scale_x_continuous(limits = c(-1, 95)) + scale_y_log10() + ggthemes::theme_economist() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5), legend.position = "none")

Show(Ratings_by_Movie_Age.Histogram)

ggsave(filename = "Ratings_by_Movie_Age_Histogram.png", plot = Ratings_by_Movie_Age.Histogram, path = "Figures")

# Define, show and save a scatterplot showing average ratings against movie age, including a smoothed trend line with a confidence interval.

Ratings_by_Movie_Age.Movie_Age_vs_Avgs.Scatterplot <- Ratings_by_Movie_Age.Distribution %>% ggplot(aes(movie_age, Mu_gvn_Movie_Age)) + geom_point(color = "cornflowerblue", alpha = 0.3) +
  geom_smooth() + labs(title = "Scatterplot of Movie Age vs Average Rating", x = "Movie Age", y = "Average Rating") + ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust= 0.5))

Show(Ratings_by_Movie_Age.Movie_Age_vs_Avgs.Scatterplot)

ggsave(filename = "Ratings_by_Movie_Age_Movie_Age_vs_Avgs_Scatterplot.png", plot = Ratings_by_Movie_Age.Movie_Age_vs_Avgs.Scatterplot, path = "Figures")

# Metrics and figures by rating age:

# Define a dataset consisting of the frequency, average rating and rating standard deviation for each rating_age value.

Ratings_by_Rating_Age.Distribution <- edx %>% group_by(rating_age) %>% summarize(Count = n(), Mu_gvn_Rating_Age = mean(rating), Sd_gvn_Rating_Age = sd(rating), .groups = "drop")

# Define, show and save a bar chart showing the average rating by rating age.

Ratings_by_Rating_Age.Bar_Chart <- Ratings_by_Rating_Age.Distribution %>% ggplot(aes(rating_age, Mu_gvn_Rating_Age)) + geom_col(fill = "cornflowerblue", color = "white") +
  labs(x = "Rating Age", y = "Average Rating", title = "Average Rating by Rating Age") + geom_hline(aes(yintercept = 3.51), color = "red") +
  annotate("text", x = 4, y = 3.7, label = 3.51, color = "red", size = 3) + scale_x_continuous(limits = c(-1, 15)) + ggthemes::theme_economist() + theme(axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5), legend.position = "none")

Show(Ratings_by_Rating_Age.Bar_Chart)

ggsave(filename = "Ratings_by_Rating_Age_Bar_Chart.png", plot = Ratings_by_Rating_Age.Bar_Chart, path = "Figures")

# Define, show and save a histogram showing rating count by rating age.

Ratings_by_Rating_Age.Histogram <- Ratings_by_Rating_Age.Distribution %>% ggplot(aes(rating_age, Count)) + geom_col(fill = "cornflowerblue", color = "white") +
  labs(x = "Rating Age", y = "Rating Count", title = "Distribution of Rating Age") + geom_hline(aes(yintercept = mean(Count)), color = "red") + annotate("text", x = 6.5,
  y = 8*10^5, label = round(mean(Ratings_by_Rating_Age.Distribution$Count), 0), color = "red", size = 3) + scale_x_continuous(limits = c(-1, 15)) + scale_y_log10() + ggthemes::theme_economist() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5), legend.position = "none")

Show(Ratings_by_Rating_Age.Histogram)

ggsave(filename = "Ratings_by_Rating_Age_Histogram.png", plot = Ratings_by_Rating_Age.Histogram, path = "Figures")

rm(logical_CPUs, RunRandomForest.cluster, edx.partition.indices, chunks, j, temp)
