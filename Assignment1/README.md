# CPC351 - Assignment 1

This repository contains our solutions for Assignment 1 of the CPC351 course. The `Assignment1.R` script performs a series of data analysis tasks based on datasets provided for the assignment.

## Problem Description

The assignment consists of six main tasks that involve data manipulation and analysis using R:

1.  **Student Enrollment Analysis:**
    *   Reads multiple text files, each containing a list of student names for a specific course.
    *   Calculates enrollment statistics, such as the courses with the highest and lowest number of students.
    *   Determines the total number of distinct students across all courses.
    *   Implements a function to find all courses a specific student is registered for.
    *   Performs set operations to identify students enrolled in specific combinations of courses (e.g., in course A AND B, in A but NOT B, etc.).

2.  **Textual Data Analysis:**
    *   Reads and merges content from multiple text files.
    *   Counts the occurrences of specific target words within the combined text.
    *   Cleans the text, tokenizes it into words, and calculates the frequency of each word to identify the top 10 most frequent words.

3.  **Traveling Salesperson Problem (Heuristic Approach):**
    *   Solves the Traveling Salesperson Problem for ten locations on a Cartesian plane.
    *   Uses the Sorted Edges (Cheapest Link) heuristic algorithm to generate a Hamiltonian tour.
    *   The R program will design the round trip, report the total distance, and account for variable travel costs (e.g., traffic, weather).

4.  **Spotify Track Features Analysis:**
    *   Processes the `tracks_features.csv` dataset containing audio features for over 1.2 million songs.
    *   Splits the large CSV file into 40 smaller CSV files based on a specified structure.
    *   Imports the 40 split files back into R and combines them into a single data frame named `complete`.

5.  **Ten Queens Puzzle:**
    *   Solves the Ten Queens Puzzle, which involves placing ten queens on a 10x10 chessboard without any attacking each other.
    *   Represents the board as a 10x10 matrix and takes a vector input representing queen positions.
    *   Determines if a given solution is feasible. If not, it visually highlights and lists all pairs of attacking queens.

6.  **12x12 Sudoku Puzzle Solver:**
    *   Analyzes a 12x12 Sudoku puzzle with numbers 1-9 and letters A-C.
    *   The program reads two input files, maps them to a 12x12 matrix, and validates the puzzle against Sudoku rules (row, column, and 3x4 block uniqueness).
    *   For infeasible puzzles, it must identify and describe all rule violations.

## How to Run the Script

### 1. Data Setup

The `Data` directory, which contains the datasets for this assignment, is not included in this repository and is listed in the `.gitignore` file to avoid publishing sensitive or proprietary information.

To run the script, you must **manually add the `Data` folder** to the root of the project directory. The expected structure is as follows:

```
CPC351_Assignment1/
├── Assignment1.R
├── README.md
└── Data/
    ├── Q1/
    │   ├── Q1_CDS512.txt
    │   ├── Q1_CDS521.txt
    │   └── ... (and other course files)
    ├── Q2/
    │   ├── Q2_Part_1.txt
    │   ├── Q2_Part_2.txt
    │   └── ... (and other text parts)
    ├── Q4/
    │   └── ...
    ├── Q4_split/
    │   └── ...
    └── Q6/
        └── ...
```

### 2. Set Working Directory

The R script assumes that the working directory is set to the root of this project folder. If you run the script from a different context, you may need to set the working directory manually at the beginning of the `Assignment1.R` script.

You can do this by adding the following line at the top of the script, replacing the path with the actual path to this project on your local machine:

```R
setwd("~/Desktop/CPC351-repo/Assignment1")
```

### 3. Execute the Script

Once the data is in place and the working directory is set correctly, you can run the `Assignment1.R` script in your R environment of choice (e.g., RStudio, terminal). The script will print all the results to the console.
