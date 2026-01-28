#' Run ggsem silently (without launching the app) and get the visualization outputs
#'
#' This function processes a saved ggsem workflow metadata file and extracts all
#' visualization data (points, lines, annotations, loops) that would be displayed
#' in the Shiny app. It reproduces both SEM and network visualizations from the
#' saved session state.
#'
#' @param metadata A file path of metadata or a list containing ggsem workflow metadata, typically loaded from
#'   an RDS file saved by the ggsem Shiny app using the "Export Workflow" functionality.
#'   The metadata should contain SEM groups, network groups, visual elements, and
#'   group labels.
#'
#' @return A list of four data frames:
#' \itemize{
#'   \item \code{points} - Data frame containing point coordinates and properties
#'   \item \code{lines} - Data frame containing line coordinates and properties
#'   \item \code{annotations} - Data frame containing text annotations
#'   \item \code{loops} - Data frame containing loop coordinates and properties
#' }
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Load a saved ggsem workflow
#' workflow_metadata <- readRDS("ggsem_workflow_20240101_120000.rds")
#'
#' # Extract visualization data
#' viz_data <- ggsem_silent(workflow_metadata)
#'
#' # Access the different components
#' points <- viz_data$points
#' lines <- viz_data$lines
#' annotations <- viz_data$annotations
#' loops <- viz_data$loops
#' }
ggsem_silent <- function(metadata) {

  if (is.character(metadata) && length(metadata) == 1 && grepl("^https?://", metadata)) {
    message("Downloading metadata from URL: ", metadata)
    tmp <- tempfile(fileext = ".rds")
    tryCatch({
      utils::download.file(metadata, tmp, mode = "wb", timeout = 600, cacheOK = FALSE)
      metadata <- tmp
    }, error = function(e) {
      stop("Failed to download metadata from URL: ", metadata,
           "\nError: ", e$message,
           "\nPlease download the file manually and provide a local path.")
    })
  }

  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop("Metadata file not found: ", metadata)
    }

    tryCatch({
      metadata <- readRDS(metadata)
    }, error = function(e) {
      stop("Error loading metadata file: ", e$message)
    })
  }

  # Now metadata should be a list - validate
  if (!is.list(metadata)) {
    stop("metadata must be either a file path (string) or a list containing ggsem workflow data")
  }

  sem_data <- list(points = data.frame(), lines = data.frame(), annotations = data.frame(), loops = data.frame())
  network_data <- list(points = data.frame(), lines = data.frame(), annotations = data.frame())

  if (length(metadata$sem_groups) > 0)  sem_data <- extract_sem_data(metadata)
  if (length(metadata$network_groups) > 0)  network_data <- extract_network_data(metadata)

  # Others
  points_data <- do.call(rbind, lapply(list(sem_data, network_data), function(x) x$points))
  lines_data <- do.call(rbind, lapply(list(sem_data, network_data), function(x) x$lines))
  annotations_data <- do.call(rbind, lapply(list(sem_data, network_data), function(x) x$annotations))
  loops_data <- do.call(rbind, lapply(list(sem_data), function(x) x$loops))

  if (!is.null(metadata$visual_elements)) {
    independent_points <- metadata$visual_elements$points |> filter(!lavaan & !network)
    independent_lines <- metadata$visual_elements$lines |> filter(!lavaan & !network)
    independent_annotations <- metadata$visual_elements$annotations |> filter(!lavaan & !network)
    independent_loops <- metadata$visual_elements$loops |> filter(!lavaan)

    if (nrow(independent_points) > 0) points_data <- bind_rows(points_data, independent_points)
    if (nrow(independent_lines) > 0) lines_data <- bind_rows(lines_data, independent_lines)
    if (nrow(independent_annotations) > 0) annotations_data <- bind_rows(annotations_data, independent_annotations)
    if (nrow(independent_loops) > 0) loops_data <-  bind_rows(loops_data, independent_loops)

    points_data_sem_network <- metadata$visual_elements$points |> filter(lavaan | network)
    lines_data_sem_network <- metadata$visual_elements$lines |> filter(lavaan | network)
    annotations_data_sem_network <- metadata$visual_elements$annotations |> filter(lavaan | network)
    loops_data_sem <- metadata$visual_elements$loops |> filter(lavaan)

    # If data tables have been edited
    if (nrow(points_data_sem_network) > 0) {
      points_data <- replace_mismatched_rows(points_data, points_data_sem_network)
    }

    if (nrow(lines_data_sem_network) > 0) {
      lines_data <- replace_mismatched_rows(lines_data, lines_data_sem_network)
    }

    if (nrow(annotations_data_sem_network) > 0) {
      annotations_data <- replace_mismatched_rows(annotations_data, annotations_data_sem_network)
    }

    if (nrow(loops_data_sem) > 0) {
      loops_data <- replace_mismatched_rows(loops_data, loops_data_sem)
    }
  }

  res <- list(
    points = points_data,
    lines = lines_data,
    annotations = annotations_data,
    loops = loops_data
  )

  return(res)
}

#' Replace mismatched rows between two data frames
#'
#' Internal helper function that compares two data frames with identical structure
#' and replaces rows in the original data frame where values differ from the
#' new data frame. Only rows with different values are replaced; identical rows
#' are preserved.
#'
#' @param original_df A data frame containing the original data
#' @param new_df A data frame containing updated data with same structure
#'
#' @return A modified version of `original_df` where mismatched rows have been
#'   replaced with corresponding rows from `new_df`
#'
#'
#' @importFrom dplyr anti_join
#' @keywords internal
#' @noRd
replace_mismatched_rows <- function(original_df, new_df) {
  if (nrow(original_df) == 0 || nrow(new_df) == 0) return(original_df)
  if (nrow(original_df) != nrow(new_df)) return(original_df)

  common_cols <- intersect(names(original_df), names(new_df))

  original_df$..row_id.. <- 1:nrow(original_df)
  new_df$..row_id.. <- 1:nrow(new_df)
  different_rows <- dplyr::anti_join(original_df, new_df, by = common_cols)

  if (nrow(different_rows) > 0) {
    diff_indices <- different_rows$..row_id..

    for (idx in diff_indices) {
      if (idx <= nrow(new_df)) {
        original_df[idx, common_cols] <- new_df[idx, common_cols]
      }
    }
  }
  original_df$..row_id.. <- NULL

  return(original_df)
}


#' Extract SEM visualization data from ggsem metadata
#'
#' Internal helper function that processes SEM groups from ggsem workflow metadata
#' and generates the corresponding visualization data (points, lines, annotations).
#'
#' @param metadata A list containing ggsem workflow metadata with SEM groups and
#'   group labels.
#'
#' @return A list with three components:
#' \itemize{
#'   \item \code{points} - Data frame of SEM point coordinates and properties
#'   \item \code{lines} - Data frame of SEM line coordinates and properties
#'   \item \code{annotations} - Data frame of SEM text annotations
#'   \item \code{loops} - Data frame of SEM loops
#' }
#'
#' @keywords internal
#' @noRd
extract_sem_data <- function(metadata) {

  group_list <- names(metadata$sem_groups)

  # Generate network data for all groups
  sem_data <- lapply(1:length(group_list), function(i) {
    reproduce_sem(metadata = metadata, group_id = group_list[[i]])
  })

  # Extract and combine all data types
  points_data <- do.call(rbind, lapply(sem_data, function(x) x$points))
  lines_data <- do.call(rbind, lapply(sem_data, function(x) x$lines))
  annotations_data <- do.call(rbind, lapply(sem_data, function(x) x$annotations))
  loops_data <- do.call(rbind, lapply(sem_data, function(x) x$loops))

  res <- list(
    points = points_data,
    lines = lines_data,
    annotations = annotations_data,
    loops = loops_data
  )
  return(res)
}


#' Extract network visualization data from ggsem metadata
#'
#' Internal helper function that processes network groups from ggsem workflow metadata
#' and generates the corresponding visualization data (points, lines, annotations).
#'
#' @param metadata A list containing ggsem workflow metadata with network groups and
#'   group labels.
#'
#' @return A list with three components:
#' \itemize{
#'   \item \code{points} - Data frame of network point coordinates and properties
#'   \item \code{lines} - Data frame of network line coordinates and properties
#'   \item \code{annotations} - Data frame of network text annotations
#' }
#'
#' @keywords internal
#' @noRd
extract_network_data <- function(metadata) {

  group_list <- names(metadata$network_groups)

  # Generate network data for all groups
  network_data <- lapply(1:length(group_list), function(i) {
    reproduce_network(metadata = metadata, group_id = group_list[[i]])
  })

  # Extract and combine all data types
  points_data <- do.call(rbind, lapply(network_data, function(x) x$points))
  lines_data <- do.call(rbind, lapply(network_data, function(x) x$lines))
  annotations_data <- do.call(rbind, lapply(network_data, function(x) x$annotations))

  res <- list(
    points = points_data,
    lines = lines_data,
    annotations = annotations_data
  )
  return(res)
}
