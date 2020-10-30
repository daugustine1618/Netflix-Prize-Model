
# Define the rating_year and release_year variables in both the edx and validation datasets.

edx <- edx %>% mutate(rating_year = year(as_date(as_datetime(timestamp)))) %>% extract(title, c("title", "release_year"), "^(.*)\\s\\((\\d{4})\\)$") %>% dplyr::select(-timestamp)

validation <- validation %>% mutate(rating_year = year(as_date(as_datetime(timestamp)))) %>% extract(title, c("title", "release_year"), "^(.*)\\s\\((\\d{4})\\)$") %>% dplyr::select(-timestamp)

# Define a vector containing the unique genre combination values from the genres field.

edx.UniqueGenreCombos <- unique(edx$genres)

# Define a vector containing the unique individual genres extracted from the various combinations of the genres field. Note that those values of the genres field that are empty strings or "(no genres
# listed)" have been ignored.

edx.UniqueGenres <- str_split(edx.UniqueGenreCombos, "\\|{1}", simplify = TRUE) %>% as.vector() %>% unique() %>% as_tibble() %>% filter(value != "" & value != "(no genres listed)") %>% .$value %>%
  sort()

# Define a function to assign columns of 1s and 0s to each row of its input dataset, with each column corresponding to one of the unique genres listed in edx.UniqueGenres. A 1 in a given column
# indicates that row's value for the genres field includes the unique genre corresponding to that column.

DefineGenreBernoullis <- function(){
  output <- matrix(nrow = length(arg), ncol = length(edx.UniqueGenres))
  r <- 1
  for(x in arg){
    output[r, ] <- ifelse(str_detect(x, edx.UniqueGenres), 1, 0)
    if(r < 0.995*length(arg) + 1){ProgressBar(round(100*(r/length(arg)), 0))}
    r <- r + 1
  }
  return(output)
}

# Define a list with 19 components. Each component will be processed simultaneously and independently by its own logical processor.

edx.UniqueGenres.Chunks <- vector("list", length = 19)

# Assign chunks of rows of the genres field to the components of the above defined list.

for(j in 1:19){
  edx.UniqueGenres.Chunks[[j]] <- edx$genres[((j - 1)*500000 + 1):pmin(j*500000, nrow(edx))]
}

# Define the cluster object to be referenced to utilize parallel processing when applying the DefineGenreBernoullis function.

DefineGenreBernoullis.cluster <- makeCluster(logical_CPUs - 1); registerDoParallel(cl = DefineGenreBernoullis.cluster)

# Make each of the chunks defined above global in scope across all R sessions. This therefore makes these chunks global in scope within each of the worker R sessions created for parallel processing.

invisible(clusterApply(cl = DefineGenreBernoullis.cluster, edx.UniqueGenres.Chunks, make_global))

# Export the "ProgressBar", "str_detect" and "edx.UniqueGenres" objects to each of the worker R sessions so the DefineGenreBernoullis function can be properly called within each worker R session.

clusterExport(cl = DefineGenreBernoullis.cluster, c("ProgressBar", "str_detect", "edx.UniqueGenres"))

# Define a matrix containing the output of the DefineGenreBernoullis function.

edx.GenreBernoullis <- clusterCall(cl = DefineGenreBernoullis.cluster, DefineGenreBernoullis) %>% as.matrix() %>% do.call("rbind", .)

# Close the cluster defined to utilize parallel processing when applying the DefineGenreBernoullis function.

stopCluster(DefineGenreBernoullis.cluster)

# Assign column names to the edx.GenreBernoullis matrix.

colnames(edx.GenreBernoullis) <- paste(edx.UniqueGenres, "Ind", sep = ".")

# Define the movie_age, rating_age and rating_lapse variables in the edx dataset. These variables represent, respectively, the age of each movie as of 2009, the age of each rating as of 2009, and
# the number of years passed between the release year of each movie and the years in which each of its ratings were submitted.

edx <- edx %>% mutate(release_year = as.numeric(release_year), movie_age = pmax(0, 2009 - release_year), rating_age = pmax(0, 2009 - rating_year),
  rating_lapse = pmax(0, rating_year - release_year)) %>% dplyr::select(!c(release_year, rating_year))

# Append the genre Bernoulli variable to the edx dataset and reorder its columns.

edx <- cbind(edx, edx.GenreBernoullis) %>% .[, c(1, 3, 7:8, 2, 4, 6:5, 9:27)]

# The below lines of code repeat the above process of adding and reordering the columns, but now for the validation dataset. Note that because this dataset is smaller, no parallel processing is
# used when applying the DefineGenreBernoullis function.

validation.GenreBernoullis <- matrix(nrow = nrow(validation), ncol = length(edx.UniqueGenres))

arg <- validation$genres; validation.GenreBernoullis <- DefineGenreBernoullis()

colnames(validation.GenreBernoullis) <- paste(edx.UniqueGenres, "Ind", sep = ".")

validation <- validation %>% mutate(release_year = as.numeric(release_year), movie_age = pmax(0, 2009 - release_year), rating_age = pmax(0, 2009 - rating_year),
  rating_lapse = pmax(0, rating_year - release_year)) %>% dplyr::select(!c(release_year, rating_year))

validation <- cbind(validation, validation.GenreBernoullis) %>% .[, c(1, 3, 7:8, 2, 4, 6:5, 9:27)]

# Create the "Data" and "Data/R Data" folders if they don't already exist in the project's home directory.

if(!dir.exists("Data")){dir.create("Data")}

if(!dir.exists("Data/R Data")){dir.create("Data/R Data")}

# Save the edx and validation datasets within the project's home directory.

save(edx, file = "Data/R Data/edx.rda"); save(validation, file = "Data/R Data/validation.rda")

rm(edx.GenreBernoullis, edx.UniqueGenres.Chunks, validation.GenreBernoullis, edx.UniqueGenreCombos, edx.UniqueGenres, arg, DefineGenreBernoullis.cluster, j)
