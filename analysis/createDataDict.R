library(jsonlite)

files <- list.files(".", pattern = "_data\\.csv$", recursive = TRUE)

out <- list()

for (f in files) {
  # Read the raw header line exactly as-is
  header_raw <- readLines(f, n = 1)
  # Split by comma without trimming anything
  cols <- strsplit(header_raw, ",", fixed = TRUE)[[1]]
  for (c in cols) {
    out[[length(out) + 1]] <- list(
      "@type" = "PropertyValue",
      name = c,  # preserved exactly, including trailing spaces
      description = NULL,
      measurementTechnique = NULL,
      unitText = NULL,
      minValue = NULL,
      maxValue = NULL,
      valuePattern = NULL,
      propertyID = NULL,
      alternateName = NULL,
      dataFile = f
    )
  }
}

writeLines(
  toJSON(out, pretty = TRUE, auto_unbox = TRUE, null = "null"),
  "variables.txt"
)