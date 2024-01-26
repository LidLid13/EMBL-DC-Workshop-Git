# Create a "data" directory
dir.create("data_RNAseq")

# Download the data provided by your collaborator, 4 files
# using a for loop to automate this step with the c vector, i changes meaning taking the files names
# remember to define destfile appropriately
]
for(i in c("counts_raw.csv", "counts_transformed.csv", "sample_info.csv", "test_result.csv")){
  download.file(
    url = paste0("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/", i, "?raw=true"),
    destfile = paste0("data_RNAseq/", i)
  )
}
