##Q8#######

#8.1

library(plyr)
read_data <- function(x1, y1){
  df1 <- read.csv(x1, header=TRUE, na.strings=c(""," ","NA"))
  df2 <- read.csv(y1, header=TRUE, na.strings=c(""," ","NA"))
  if(ncol(df1) < ncol(df2)){
    k <- 1
    x <- df1
    y <- df2
  } else if(ncol(df1) > ncol(df2)){
    k <- 1
    x <- df2
    y <- df1
  } else{
    k <- 0
    x <- df1
    y <- df2
  }
  nc <- colnames(x)
  nc1 <- colnames(y)
  nc1 <- nc1[!nc1 %in% nc]
  df_merged <- y

  for(i in 1:nrow(x)){
    row <- x[i,]
    if(row$id %in% y$id){
      r2 <- df_merged[df_merged$id == row$id,]
      df_merged <- df_merged[df_merged$id != row$id,]
      for(j in 1:length(nc)){
        if(is.numeric(row[[nc[j]]])){
          if(row[nc[j]] != r2[nc[j]]){
            r2[nc[j]] <- NA
          }
        }
      }
      if(k == 1){
        for(l in 1:length(nc1)){
          r2[nc1[l]] <- NA
        }
      }
      df_merged <- rbind.fill(df_merged, r2)
    } else{
      df_merged <- rbind.fill(df_merged, row)
    }
  }
  write.csv(df_merged, file = "student_data_merged.csv", row.names = FALSE)
  return(df_merged)
}


#8.2
compute_mean_median <- function(df_merged){
  mean <- c()
  median <- c()
  for(i in 1:nrow(df_merged)){
    row <- df_merged[i,]
    m <- c()
    for(j in 6:ncol(row)){
      m <- rbind(m, row[[j]])
    }
    m <- sort(m)
    mean <- rbind(mean, summary(m)[["Mean"]])
    median <- rbind(median, summary(m)[["Median"]])
  }
  df_merged_new <- cbind(df_merged, Average_grade=mean)
  df_merged_new <- cbind(df_merged_new, Median_grade=median)
  return(df_merged_new)
}


#8.3
compute_mean_gender <- function(df_merged_new){
  male <- c()
  female <- c()
  for(i in 1:nrow(df_merged_new)){
    row <- df_merged_new[i,]
    if(row$gender == 'M'){
      male <- rbind(male, row$Average_grade)
    } else if(row$gender == 'F'){
      female <- rbind(female, row$Average_grade)
    }
  }
  male <- sort(male)
  female <- sort(female)
  print(paste("Male students have a mean grade of",summary(male)[["Mean"]]))
  print(paste("Female students have a mean grade of",summary(female)[["Mean"]]))
}


#8.4 
compute_letter_grades <- function(df_merged_new){
  df_letters <- df_merged_new[,1:16]
  letters <- c()
  for(i in 1:nrow(df_merged_new)){
    num <- df_merged_new[i,]$Average_grade
    if(num >= 90 && num <= 100){
      letters <- rbind(letters, "A")
    } else if(num >= 80 && num <= 89){
      letters <- rbind(letters, "B")
    } else if(num >= 70 && num <= 79){
      letters <- rbind(letters, "C")
    } else if(num < 70){
      letters <- rbind(letters, "F")
    } 
  }
  df_letters <- cbind(df_letters, Letter_grade=letters)
  return(df_letters)
}


#8.5
count_letter_grades <- function(df_letters){
  lev <- levels(df_letters$Letter_grade)
  for(i in 1:length(lev)){
    print(paste("Count of Grade",lev[i], "-", sum(df_letters$Letter_grade == lev[i])))
  }
}


##ex:
df_merged <- read_data("student_data_01.csv", "student_data_02.csv")
df_merged_new <- compute_mean_median(df_merged)
compute_mean_gender(df_merged_new)
df_letters <- compute_letter_grades(df_merged_new)
count_letter_grades(df_letters)

