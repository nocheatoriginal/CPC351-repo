# Task 1

q1_path <- "Data/Q1"
q1_files <- list.files(q1_path, pattern = "^Q1_.*\\.txt$", full.names = TRUE)

course_codes <- sub("^Q1_(.*)\\.txt$", "\\1", basename(q1_files))

student_lists <- lapply(q1_files, function(f) readLines(f, warn = FALSE))
names(student_lists) <- course_codes

enrollment_counts <- sapply(student_lists, length)
max_n <- max(enrollment_counts)
min_n <- min(enrollment_counts)

courses_max <- names(enrollment_counts[enrollment_counts == max_n])
courses_min <- names(enrollment_counts[enrollment_counts == min_n])

cat("[1a] Courses with highest number of students:",
    paste(courses_max, collapse = ", "), "with", max_n, "students\n")
cat("[1a] Course(s) with lowest number of students :",
    paste(courses_min, collapse = ", "), "with", min_n, "students\n")

all_students <- unlist(student_lists)
distinct_students <- unique(all_students)
cat("[1b] Total number of distinct students:", length(distinct_students), "\n")

find_courses_by_student <- function(student_name) {
  courses <- names(student_lists)[sapply(student_lists,
                                         function(x) student_name %in% x)]
  if (length(courses) == 0) {
    cat("[1c] Student", student_name,
        "is not registered for any course.\n")
  } else {
    cat("[1c] Student", student_name,
        "is registered for:", paste(courses, collapse = ", "), "\n")
  }
}

find_courses_by_student("NAME001")

cds512 <- student_lists[["CDS512"]]
cds521 <- student_lists[["CDS521"]]

both_512_521 <- intersect(cds512, cds521)
cat("[1d] Students in CDS512 AND CDS521:", paste(both_512_521, collapse = ", "), "\n")

only_512 <- setdiff(cds512, cds521)
cat("[1e] Students in CDS512 but NOT CDS521:", paste(only_512, collapse = ", "), "\n")

only_521 <- setdiff(cds521, cds512)
cat("[1f] Students in CDS521 but NOT CDS512:", paste(only_521, collapse = ", "), "\n")

in_512_or_521 <- union(cds512, cds521)
not_512_nor_521 <- setdiff(distinct_students, in_512_or_521)
cat("[1g] Students NOT in CDS512 and NOT in CDS521:", paste(not_512_nor_521, collapse = ", "), "\n")

student_course_counts <- table(all_students)
students_3_courses <- names(student_course_counts[student_course_counts == 3])

students_3_courses_results <- if(length(students_3_courses) > 0) students_3_courses else "None"

cat("[1h] Students registered for exactly three courses:", paste(students_3_courses_results, collapse = ", "), "\n")



# Task 2

q2_path <- "Data/Q2"
q2_files <- list.files(q2_path, pattern = "^Q2_Part_.*\\.txt$", full.names = TRUE)

q2_texts <- sapply(q2_files, function(f) {
  paste(readLines(f, warn = FALSE), collapse = " ")
})
full_text <- paste(q2_texts, collapse = " ")

count_word <- function(word, text) {
  pattern <- paste0("\\b", word, "\\b")
  matches <- gregexpr(pattern, tolower(text), ignore.case = TRUE)
  if (matches[[1]][1] == -1) {
    return(0)
  } else {
    return(length(matches[[1]]))
  }
}

words_target <- c("analytics", "insight", "of")
counts <- sapply(words_target, count_word, text = full_text)

cat("[2a] Total occurrences across all 10 files:", paste(names(counts), counts, sep = ": ", collapse = ", "), "\n")


clean_text <- tolower(full_text)
clean_text <- gsub("[^a-z]+", " ", clean_text)
word_vec <- unlist(strsplit(clean_text, "\\s+"))
word_vec <- word_vec[word_vec != ""]

word_freq <- table(word_vec)
word_freq_sorted <- sort(word_freq, decreasing = TRUE)
top_10 <- head(word_freq_sorted, 10)

cat("[2b] Top 10 most frequent words:", paste(names(top_10), top_10, sep = ": ", collapse = ", "), "\n")

task3
## 1. Define 10 points (coordinates)
points <- data.frame(
  id = 1:10,
  x  = c( 60, 180,  80, 140,  20, 100, 200, 140,  40, 100),
  y  = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120)
)

num_points <- nrow(points)

## 2. Euclidean distance matrix
euclid <- function(a, b) sqrt((a$x-b$x)^2 + (a$y-b$y)^2)

base_dist <- matrix(0, nrow=num_points, ncol=num_points)
for (i in 1:num_points) {
  for (j in 1:num_points) {
    if (i != j) base_dist[i,j] <- euclid(points[i,], points[j,])
  }
}

## 3. Random traffic and weather multipliers
set.seed(351)
traffic <- matrix(runif(num_points^2, 0.8, 1.4), nrow=num_points)
traffic <- (traffic + t(traffic)) / 2
diag(traffic) <- 0

weather <- matrix(runif(num_points^2, 0.9, 1.3), nrow=num_points)
weather <- (weather + t(weather)) / 2
diag(weather) <- 0

## 4. Random road closures (ensure graph stays connected)

# Generate random blocked edges
random_blocked_matrix <- function(n) {
  blocked <- matrix(FALSE, n, n)
  
  k <- sample(1:3, 1)   # block 1-3 roads
  chosen <- replicate(k, sample(1:n, 2))
  
  for (m in 1:k) {
    i <- chosen[1,m]; j <- chosen[2,m]
    if (i != j) {
      blocked[i,j] <- TRUE
      blocked[j,i] <- TRUE
    }
  }
  return(blocked)
}

# Keep generating until the graph is still connected
ensure_connected <- function(base_dist) {
  repeat {
    blocked <- random_blocked_matrix(num_points)
    
    tmp <- base_dist
    tmp[blocked] <- Inf
    
    visited <- rep(FALSE, num_points)
    stack <- c(1)
    visited[1] <- TRUE
    
    # DFS to check connectivity
    while(length(stack) > 0) {
      cur <- tail(stack,1)
      stack <- head(stack, -1)
      
      for (nb in which(tmp[cur,] < Inf)) {
        if (!visited[nb]) {
          visited[nb] <- TRUE
          stack <- c(stack, nb)
        }
      }
    }
    
    if (all(visited)) return(blocked)  # connected → OK
  }
}

blocked <- ensure_connected(base_dist)

## 5. Build cost matrices (selection cost / final evaluation cost)
BIG <- 1e9

cost_sel  <- base_dist                 # used for selecting edges
cost_eval <- base_dist * traffic * weather   # used for final cost

# Apply road closures
cost_sel[blocked]  <- BIG
cost_eval[blocked] <- BIG

cat("Random blocked edges (TRUE = closed):\n")
print(blocked)

## 6. Cheapest Link heuristic (Sorted Edges)

# Find which component a node belongs to
find_component <- function(node, comp) {
  for (i in seq_along(comp)) {
    if (node %in% comp[[i]]) return(i)
  }
  return(NULL)
}

cheapest_link <- function(cost_sel, cost_eval) {
  n <- nrow(cost_sel)
  
  # List all available edges
  edges <- data.frame(from=integer(), to=integer(), cost=double())
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (cost_sel[i,j] < BIG) {
        edges <- rbind(edges, data.frame(from=i, to=j, cost=cost_sel[i,j]))
      }
    }
  }
  edges <- edges[order(edges$cost), ]   # sort by smallest cost
  
  # Initialize degree count and components
  degrees <- rep(0, n)
  comps <- as.list(1:n)
  chosen <- data.frame(from=integer(), to=integer(), cost=double())
  
  # Pick n-1 edges while avoiding small cycles
  for (k in 1:nrow(edges)) {
    e <- edges[k,]
    u <- e$from; v <- e$to
    
    if (degrees[u] >= 2 || degrees[v] >= 2) next
    
    cu <- find_component(u, comps)
    cv <- find_component(v, comps)
    
    if (cu != cv) {
      chosen <- rbind(chosen, e)
      degrees[u] <- degrees[u] + 1
      degrees[v] <- degrees[v] + 1
      
      # Merge components
      merged <- c(comps[[cu]], comps[[cv]])
      comps <- comps[-c(cu,cv)]
      comps <- append(comps, list(merged))
    }
    
    if (nrow(chosen) == n-1) break
  }
  
  # Add the final closing edge
  ends <- which(degrees == 1)
  last <- data.frame(from = ends[1], to = ends[2], cost = cost_sel[ends[1], ends[2]])
  chosen <- rbind(chosen, last)
  
  # Rebuild the path in order
  start <- chosen$from[1]
  cur   <- chosen$to[1]
  path  <- c(start, cur)
  rem <- chosen[-1, ]
  
  # Follow edges to reconstruct cycle
  while (length(path) < n) {
    found <- FALSE
    for (i in 1:nrow(rem)) {
      f <- rem$from[i]; t <- rem$to[i]
      if (f == cur && !(t %in% path)) {
        path <- c(path, t); cur <- t; rem <- rem[-i,]; found <- TRUE; break
      } else if (t == cur && !(f %in% path)) {
        path <- c(path, f); cur <- f; rem <- rem[-i,]; found <- TRUE; break
      }
    }
    if (!found) break
  }
  
  path <- c(path, path[1])  # close cycle
  
  # Compute final travel cost (traffic + weather)
  tot <- 0
  for (i in 1:(length(path)-1)) {
    tot <- tot + cost_eval[path[i], path[i+1]]
  }
  
  return(list(path=path, edges=chosen, total_cost=tot))
}

result <- cheapest_link(cost_sel, cost_eval)

cat("\nFinal Hamiltonian Tour:\n")
print(result$path)
cat("\nTotal Cost (with traffic/weather/closures): ", result$total_cost, "\n")

## 7. Plot the tour
tour <- result$path
px <- points$x[tour]
py <- points$y[tour]

plot(points$x, points$y,
     pch = 19, col="blue", cex=1.5,
     xlab="X", ylab="Y",
     main="Hamiltonian Tour (Random Closures + Cheapest Link)")
text(points$x, points$y, labels=points$id, pos=3)

lines(px, py, col="red", lwd=2)
arrows(px[-length(px)], py[-length(py)], px[-1], py[-1], length=0.1, col="red")

points(px[1], py[1], pch=21, bg="yellow", cex=2)
