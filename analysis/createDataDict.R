library(jsonlite)

files <- list.files(".", pattern = "_data\\.csv$", recursive = TRUE)

out <- list()

for (f in files) {
  cols <- names(read.csv(f, nrows = 1, check.names = FALSE))
  for (c in cols) {
    out[[length(out) + 1]] <- list(
      "@type" = "PropertyValue",
      name = c,
      description = NULL,
      measurementTechnique = NULL,
      measurementScale = NULL,
      unitText = NULL,
      propertyID = NULL,
      dataFile = f
    )
  }
}

# Write as a JSON array with comma-separated objects
writeLines(
  toJSON(out, pretty = TRUE, auto_unbox = TRUE, null = "null"),
  "variables.txt"
)
