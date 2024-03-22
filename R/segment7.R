#' my_function
#'
#' A that returns a seven-segment display as an SVG
#'
#' @return A character vector
#' @export
#'
#' @examples
#' segment7(8)

segment7 <- function(value = 8,
                     dp = FALSE,
                     color = "#00FF00",
                     stroke = "#004D00",
                     background = "#000000",
                     height = 320){
  all_segments <- letters[1:8]

  width <- height/(1+2/3)

  segments <- list(
    "0" = c("a", "b", "c", "d", "e", "f"),
    "1" = c("b", "c"),
    "2" = c("a", "b", "d", "e", "g"),
    "3" = c("a", "b", "c", "d", "g"),
    "4" = c("b", "c", "f", "g"),
    "5" = c("a", "c", "d", "f", "g"),
    "6" = c("a", "c", "d", "e", "f", "g"),
    "7" = c("a", "b", "c"),
    "8" = c("a", "b", "c", "d", "e", "f", "g"),
    "9" = c("a", "b", "c", "d", "f", "g")
  )

  # validation of input
  if(!(as.character(value) %in% names(segments))){
    stop("The provided value '", value, "' is not valid. Please choose an integer between 0 and 9.", call. = FALSE)
  }
  if(!is.logical(dp)){
    stop("Parameter 'dp' should be TRUE or FALSE.", call. = FALSE)
  }
  validate_color <- function(color) {
    if(!grepl("^#[0-9A-Fa-f]{6}$", color)){
      stop("Color '", color, "' is not a valid hex color code. Please provide a hex code in the format #RRGGBB.", call. = FALSE)
    }
  }
  validate_color(color)
  validate_color(stroke)
  validate_color(background)
  if(!is.numeric(height) || height <= 0){
    stop("Parameter 'height' should be a positive number.", call. = FALSE)
  }


  segments_on <- list(
    a = paste0('<polygon id="a" fill="', color, '" stroke = "', stroke,'" points="2, 2  3, 1  9, 1  10, 2  9, 3  3, 3"/>'),
    b = paste0('<polygon id="b" fill="', color, '" stroke = "', stroke,'" points="10, 2 11, 3 11, 9  10, 10  9, 9  9, 3"/>'),
    c = paste0('<polygon id="c" fill="', color, '" stroke = "', stroke,'" points="10, 10 11,11 11,17  10,18  9,17  9,11"/>'),
    d = paste0('<polygon id="d" fill="', color, '" stroke = "', stroke,'" points="10,18  9,19  3,19  2,18  3,17  9,17"/>'),
    e = paste0('<polygon id="e" fill="', color, '" stroke = "', stroke,'" points="2,18  1,17  1,11  2, 10  3,11  3,17"/>'),
    f = paste0('<polygon id="f" fill="', color, '" stroke = "', stroke,'" points="2, 10  1, 9  1, 3  2, 2  3, 3  3, 9"/>'),
    g = paste0('<polygon id="g" fill="', color, '" stroke = "', stroke,'" points="2, 10  3, 9  9, 9  10, 10  9,11  3,11"/>')
  )


  segments_off <- list(
    a = paste0('<polygon id="a" fill="', background, '" stroke = "', stroke,'" points="2, 2  3, 1  9, 1  10, 2  9, 3  3, 3"/>'),
    b = paste0('<polygon id="b" fill="', background, '" stroke = "', stroke,'" points="10, 2 11, 3 11, 9  10, 10  9, 9  9, 3"/>'),
    c = paste0('<polygon id="c" fill="', background, '" stroke = "', stroke,'" points="10, 10 11,11 11,17  10,18  9,17  9,11"/>'),
    d = paste0('<polygon id="d" fill="', background, '" stroke = "', stroke,'" points="10,18  9,19  3,19  2,18  3,17  9,17"/>'),
    e = paste0('<polygon id="e" fill="', background, '" stroke = "', stroke,'" points="2,18  1,17  1,11  2, 10  3,11  3,17"/>'),
    f = paste0('<polygon id="f" fill="', background, '" stroke = "', stroke,'" points="2, 10  1, 9  1, 3  2, 2  3, 3  3, 9"/>'),
    g = paste0('<polygon id="g" fill="', background, '" stroke = "', stroke,'" points="2, 10  3, 9  9, 9  10, 10  9,11  3,11"/>')
  )

  active_segments <- segments[[as.character(value)]]
  passive_segments <- all_segments[!all_segments %in% active_segments]

  active_segments <- segments_on[active_segments]
  active_segments <- unlist(active_segments)
  active_segments <- paste0(active_segments, collapse = "\n")

  passive_segments <- segments_off[passive_segments]
  passive_segments <- unlist(passive_segments)
  passive_segments <- paste0(passive_segments, collapse = "\n")

  decimal_point <- paste0('<circle r = "1" cx= "12.25" cy = "18" fill = "',ifelse(dp,color,background) ,'" stroke  ="',stroke,'"/>', collapse = "")

  beginning <- paste0('<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"
  width="', width, '" height="', height,' " viewBox="-1 -1 15 20" stroke="',background,'"  stroke-width=".25">
                      <rect width = "100%" height = "100%" fill = "',background,'"/>', collapse = "")
  end <- '</svg>'
  paste0(beginning, active_segments, passive_segments, decimal_point, end, collapse = "\n")
}
