#' NPPES API
#'
#' Query the NPPES NPI registry.
#'
#' @param query Character string representing a valid query.
#'
#' @return An object of class \code{"nppes_api"}; essentially, an R list with
#' various components depending on the submitted query.
#'
#' @rdname nppes_api
#'
#' @export
#'
#' @examples
#' nppes_api("city=dayton")
#' nppes_api("number=1124021324")

nppes_api <- function(query) {

  # Query the NPPES API
  url <- httr::modify_url("https://npiregistry.cms.hhs.gov/api/", query = query)
  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # Parse the returned JSON file
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  # Turn API errors into R errors
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "NPPES API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  # Return a nppes_api object (a list)
  structure(
    list(
      "content" = parsed,
      "query" = query,
      "response" = resp
    ),
    class = "nppes_api"
  )

}


#' @rdname nppes_api
#'
#' @export
print.nppes_api <- function(x, ...) {
  cat("<NPPES ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}


#' NPPES Fields
#'
#' Print available NPPES field values that can be queried.
#'
#' @references A vector containing the names of the available fields for querying.
#' See https://npiregistry.cms.hhs.gov/registry/help-api for field format requirements.
#' Fields updated March 5th, 2018
#'
#' @export
nppes_fields <- function() {
  c("number", "enumeration_type", "taxonomy_description", "first_name", "last_name", "organization_name",
    "address_purpose", "city", "state", "postal_code", "country_code", "limit", "skip")
}


#' NPI to Specialty
#'
#' Query the NPPES NPI registry with an NPI and return provider's specialty.
#'
#' @param npi Valid 10 digit NPI (National Provider Identifier). Can be a numeric or string.
#'
#' @return An object of class \code{"data.frame"}; containing the NPI queried, specialty
#' and date last updated in NPPES.
#'
#' @rdname npi_specialty
#'
#' @export
#'
#' @examples
#' npi_specialty(1649246182)
#' npi_specialty(1831117050)

npi_specialty <- function(npi) {
  if(nchar(npi) != 10){
    stop(paste0("NPI to query for specialty must be of length 10. NPI ", npi, " is not."))
  }
  nppes_result <- nppes_api(paste0("number=", npi))
  specialty <- nppes_result$content$results[[1]]$taxonomies[[1]]$desc
  last_updated <-  nppes_result$content$results[[1]]$basic$last_updated
  data.frame("number" = npi, "specialty" = specialty, "last_updated" = last_updated)
}


#' NPI Lookup
#'
#' Query the NPPES NPI registry with multiple NPIs and return provider information.
#'
#' @param npis Vector of valid 10 digit NPIs (National Provider Identifier). Can be a numeric, character or mixed vector.
#'
#' @return An object of class \code{"data.frame"}; containing the NPIs queried, and the information requested.
#' One per row. Currently only supports field = speciality.
#'
#' @rdname npi_lookup
#'
#' @export
#'
#' @examples
#' npi_lookup(c(1649246182, 1831117050))

npi_lookup <- function(npis, field = "specialty") {
  # check if field being mapped is one there is a function for
  acceptable_fields <- c("specialty") #, "address")  # will add address later!
  if(!(field %in% acceptable_fields)){
    stop(paste0(field, " field is not one of the acceptable fields. It should be one of these: ",
                paste(acceptable_fields, collapse = ", ")))
  }

  # map according to field specified
  if(field == "specialty"){
    npilist <- lapply(npis, npi_specialty)
  }

  # current plan for this function is a list of ifs, but what if want more than one field?
  # would overwrite it

  # combine all results into a data frame
  do.call(rbind, npilist)
  # note this will error out it any npi returned an error
}
