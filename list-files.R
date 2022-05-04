# Script for saving out file names for organizational purposes

library(tidyverse)

data_files <- tibble(data_files = list.files(path = "../data/"))
write_csv(data_files, file = "../data-files.csv")

doc_files <- tibble(data_files = list.files(path = "../doc/"))
write_csv(doc_files, file = "../doc-files.csv")

output_files <- tibble(data_files = list.files(path = "../output/"))
write_csv(output_files, file = "../output-files.csv")