setwd("~/Desktop/HHN/Sem-6/courses/CPC351/CPC351-repo/GroupProject") # Set working directory!!

#
# Imports:
#
library(readxl)

data_dir <- "Data"

safe_name <- function(x) {
  x <- gsub("\\.[^.]+$", "", basename(x))
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (nchar(x) == 0) x <- "dataset"
  x
}

repository <- list()

csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

for (f in csv_files) {
  nm <- safe_name(f)
  repository[[nm]] <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}

xlsx_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)

for (f in xlsx_files) {
  base_nm <- safe_name(f)
  sheets <- readxl::excel_sheets(f)
  
  if (length(sheets) == 1) {
    repository[[base_nm]] <- readxl::read_excel(f, sheet = sheets[1])
  } else {
    for (sh in sheets) {
      sh_nm <- gsub("[^A-Za-z0-9_]+", "_", sh)
      sh_nm <- gsub("^_+|_+$", "", sh_nm)
      nm <- paste0(base_nm, "__", sh_nm)
      
      repository[[nm]] <- readxl::read_excel(f, sheet = sh)
    }
  }
}

cat("Loaded datasets:\n")
print(names(repository))

inventory <- data.frame(
  name = names(repository),
  nrows = sapply(repository, nrow),
  ncols = sapply(repository, ncol),
  row.names = NULL
)

inventory[order(as.character(inventory[["name"]])), ]
