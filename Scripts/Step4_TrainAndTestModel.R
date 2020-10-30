
# If the edx and validation datasets aren't already loaded in the R session, load them.

if(!exists("edx")){load("Data/R Data/edx.rda")}; if(!exists("validation")){load("Data/R Data/validation.rda")}

# Define a function to calculate RMSE.

CalculateRMSE <- function(Actual, Prediction){
  sqrt(mean((Actual - Prediction)^2))
}

# Define a function to calculate regularized genre effects.

DefineGenreEffects <- function(Lambda.p, train.data.p, Mu_Hat.p, Movie.Effects_Hat.p, User.Effects_Hat.p, Rating_Lapse.Effects_Hat.p){
  GenreEffects <- tibble()
  for(j in 1:19){
    temp_data <- train.data.p %>% left_join(Movie.Effects_Hat.p, by = "movieId") %>% left_join(User.Effects_Hat.p, by = "userId") %>% left_join(Rating_Lapse.Effects_Hat.p, by = "rating_lapse") %>%
      group_by_(paste("`", colnames(train.data.p)[j + 8], "`", sep = "")) %>% summarize(Genre.Ct = n(), Genre.Effect_Hat = sum(rating - (Mu_Hat.p + Movie.Effect_Hat + User.Effect_Hat +
      Rating_Lapse.Effect_Hat))/(Genre.Ct + Lambda.p), .groups = "drop") %>% slice_max(.[, 1]) %>% .[, 2:3] %>% mutate(Genre = colnames(train.data.p)[j + 8]) %>% .[, c(3, 1:2)]
    if(j == 1){
      GenreEffects <- as.matrix(temp_data)
    }else{
      GenreEffects <- rbind(as.matrix(GenreEffects), temp_data)
    }
  }
  GenreEffects <- GenreEffects %>% mutate(Genre.Ct = as.numeric(Genre.Ct), Genre.Effect_Hat = as.numeric(Genre.Effect_Hat))
  return(GenreEffects)
}

# Define a function to calculate an average regularized genre effect.

CalculateGenreEffect <- function(G, B){
  ifelse(is.nan(sum(G*B)/sum(G)), 0, sum(G*B)/sum(G))
}

# Define a function to predict ratings for a provided dataset, based on the provided model.

Predict <- function(test.data.p, Mu_Hat.p, Movie.Effects_Hat.p, User.Effects_Hat.p, Rating_Lapse.Effects_Hat.p, Genre.Effects_Hat.p, CalculateGenreEffect){
  Mu_Hat.p + (test.data.p %>% inner_join(Movie.Effects_Hat.p, by = "movieId") %>% .$Movie.Effect_Hat) +
  (test.data.p %>% inner_join(User.Effects_Hat.p, by = "userId") %>% .$User.Effect_Hat) +
  (test.data.p %>% inner_join(Rating_Lapse.Effects_Hat.p, by = "rating_lapse") %>% .$Rating_Lapse.Effect_Hat) +
  (test.data.p[, 9:27] %>% as.matrix() %>% apply(1, function(g){CalculateGenreEffect(g, B = Genre.Effects_Hat.p$Genre.Effect_Hat)}))
}

# Define a function that generates a k-fold cross-validation RMSE matrix, with each element corresponding to a given fold and given value of the Lambda parameter. The contained code first defines
# the training dataset and testing dataset for each fold of the cross-validation, and then, for each value of Lambda, fits a model over that training dataset and evaluates that fitted model over the
# corresponding testing dataset.

GenerateCvRMSEs <- function(K.p, Lambdas.p){
  set.seed(2020, sample.kind = "Rounding")
  cv.test.indices <- createFolds(edx$rating, k = K.p)
  cv.RMSEs <- matrix(nrow = K.p, ncol = length(Lambdas.p))
  for(k in 1:K.p){
    train.data.temp <- edx[-cv.test.indices[[k]]]; test.data.temp <- edx[cv.test.indices[[k]]]

    test.data <- test.data.temp %>% semi_join(train.data.temp, by = "movieId") %>% semi_join(train.data.temp, by = "userId")

    test.data.removed_obs <- test.data.temp %>% anti_join(test.data, by = c("movieId", "userId"))

    train.data <- rbind(train.data.temp, test.data.removed_obs)

    cv.Mu_Hat <- mean(train.data$rating)

    for(l in Lambdas.p){
      cv.Mu_gvn_Movie.Effects_Hat <- train.data %>% group_by(movieId) %>% summarize(Movie.Ct = n(), Movie.Effect_Hat = sum(rating - cv.Mu_Hat)/(Movie.Ct + l), .groups = "drop")

      cv.Mu_gvn_User.Effects_Hat <- train.data %>% left_join(cv.Mu_gvn_Movie.Effects_Hat, by = "movieId") %>% group_by(userId) %>% summarize(User.Ct = n(), User.Effect_Hat =
        sum(rating - (cv.Mu_Hat + Movie.Effect_Hat))/(User.Ct + l), .groups = "drop")

      cv.Mu_gvn_Rating_Lapse.Effects_Hat <- train.data %>% left_join(cv.Mu_gvn_Movie.Effects_Hat, by = "movieId") %>% left_join(cv.Mu_gvn_User.Effects_Hat, by = "userId") %>%
        group_by(rating_lapse) %>% summarize(Rating_Lapse.Ct = n(), Rating_Lapse.Effect_Hat = sum(rating - (cv.Mu_Hat + Movie.Effect_Hat + User.Effect_Hat))/(Rating_Lapse.Ct + l), .groups = "drop")

      cv.Mu_gvn_Genre.Effects_Hat <- DefineGenreEffects(l, train.data, cv.Mu_Hat, cv.Mu_gvn_Movie.Effects_Hat, cv.Mu_gvn_User.Effects_Hat, cv.Mu_gvn_Rating_Lapse.Effects_Hat)

      cv.Y_Hat <- Predict(test.data, cv.Mu_Hat, cv.Mu_gvn_Movie.Effects_Hat, cv.Mu_gvn_User.Effects_Hat, cv.Mu_gvn_Rating_Lapse.Effects_Hat, cv.Mu_gvn_Genre.Effects_Hat, CalculateGenreEffect)

      cv.RMSEs[k, which(Lambdas.p == l)] <- CalculateRMSE(test.data$rating, cv.Y_Hat)

      ProgressBar(round(100*(((k - 1)*length(Lambdas.p) + which(Lambdas.p == l))/(K.p*length(Lambdas.p))), 0))
    }
  }
  return(cv.RMSEs)
}

# Assign the number of folds to use for cross-validation and the values to tune Lambda over.

K <- 5; Lambdas <- seq(4.75, 5.25, 0.05)

# Define the matrix containing the output of the GenerateCvRMSEs function, as well as a vector consisting of the average RMS for each of the values of the Lambda parameter.

CvRMSEs <- GenerateCvRMSEs(K, Lambdas); Mu_CvRMSEs <- colMeans(CvRMSEs)

# Define a plot showing the average RMSEs against the values of Lambda and assign the tuned value of Lambda to Lambda_Hat.

Lambdas_vs_Mu_CvRMSEs.plot <- qplot(Lambdas, Mu_CvRMSEs); Lambda_Hat <- Lambdas[which.min(Mu_CvRMSEs)]

# Define the average rating to be used for our model.

Mu_Hat <- mean(edx$rating)

# Define the movie effects to be used for our model.

Mu_gvn_Movie.Effects_Hat <- edx %>% group_by(movieId) %>% summarize(Movie.Ct = n(), Movie.Effect_Hat = sum(rating - Mu_Hat)/(Movie.Ct + Lambda_Hat), .groups = "drop")

# Define the user effects to be used for our model.

Mu_gvn_User.Effects_Hat <- edx %>% left_join(Mu_gvn_Movie.Effects_Hat, by = "movieId") %>% group_by(userId) %>% summarize(User.Ct = n(),
  User.Effect_Hat = sum(rating - (Mu_Hat + Movie.Effect_Hat))/(User.Ct + Lambda_Hat), .groups = "drop")

# Define the rating_lapse effects to be used for our model.

Mu_gvn_Rating_Lapse.Effects_Hat <- edx %>% left_join(Mu_gvn_Movie.Effects_Hat, by = "movieId") %>% left_join(Mu_gvn_User.Effects_Hat, by = "userId") %>% group_by(rating_lapse) %>%
  summarize(Rating_Lapse.Ct = n(), Rating_Lapse.Effect_Hat = sum(rating - (Mu_Hat + Movie.Effect_Hat + User.Effect_Hat))/(Rating_Lapse.Ct + Lambda_Hat), .groups = "drop")

# Define the genre effects to be used for our model.

Mu_gvn_Genre.Effects_Hat <- DefineGenreEffects(Lambda_Hat, edx, Mu_Hat, Mu_gvn_Movie.Effects_Hat, Mu_gvn_User.Effects_Hat, Mu_gvn_Rating_Lapse.Effects_Hat)

# Define a vector of rating predictions for the validation dataset.

Y_Hat <- Predict(validation, Mu_Hat, Mu_gvn_Movie.Effects_Hat, Mu_gvn_User.Effects_Hat, Mu_gvn_Rating_Lapse.Effects_Hat, Mu_gvn_Genre.Effects_Hat, CalculateGenreEffect)

# Calculate the RMSE of our model with respect to the true ratings in the validation dataset.

Y_Hat.RMSE <- CalculateRMSE(validation$rating, Y_Hat)

save(Y_Hat, file = "Data/R Data/Y_Hat.rda")
