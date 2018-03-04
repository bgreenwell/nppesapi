#' NPPES API
#'
#' Query the NPPES NPI registry.
#'
#' @param query Character string representing a valid query.
#'
#' @return An bject of class \code{"nppes_api"}; essentially, an R list with
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
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  # Turn API errors into R errors
  if (http_error(resp)) {
    stop(
      sprintf(
        "NPPES API request failed [%s]\n%s\n<%s>",
        status_code(resp),
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
#' @references A vector containing the names of the available for querying.
#'
#' @export
nppes_fields <- function() {
  c("number")
}
