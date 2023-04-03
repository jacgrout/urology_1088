icd10_path <- r"(path\to\ICD10_Edition5_CodesAndTitlesAndMetadata_GB_20160401.txt)"
icd10 <- readr::read_tsv(icd10_path)
# this is what i needed
complication_codes <- icd10 |>
  dplyr::filter(
    stringr::str_detect(ALT_CODE, "T8..") # The complications are all the T8** codes
  ) |>
  dplyr::pull(ALT_CODE)


# Download here

library(ConceptLibraryClient)


# Connect to API

client = connect_to_API(public=TRUE)


# Get codelists of phenotype

codelists = get_phenotype_code_list('PH330', '660', api_client=client)