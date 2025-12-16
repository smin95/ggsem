#' Run ggsem silently (without launching the app) and get the visualization outputs
#'
#' This function processes a saved ggsem workflow metadata file and extracts all
#' visualization data (points, lines, annotations, loops) that would be displayed
#' in the Shiny app. It reproduces both SEM and network visualizations from the
#' saved session state.
#'
#' @param metadata A list containing ggsem workflow metadata, typically loaded from
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
  if (!"group_labels" %in% names(metadata)) {
    stop("metadata must contain 'group_labels'")
  }

  sem_data <- list(points = data.frame(), lines = data.frame(), annotations = data.frame())
  network_data <- list(points = data.frame(), lines = data.frame(), annotations = data.frame())

  if (length(metadata$sem_groups) > 0)  sem_data <- extract_sem_data(metadata)
  if (length(metadata$network_groups) > 0)  network_data <- extract_network_data(metadata)

  # Others
  points_data <- do.call(rbind, lapply(list(sem_data, network_data), function(x) x$points))
  lines_data <- do.call(rbind, lapply(list(sem_data, network_data), function(x) x$lines))
  annotations_data <- do.call(rbind, lapply(list(sem_data, network_data), function(x) x$annotations))

  if (!is.null(metadata$visual_elements)) {
    independent_points <- metadata$visual_elements$points |> filter(!lavaan & !network)
    independent_lines <- metadata$visual_elements$lines |> filter(!lavaan & !network)
    independent_annotations <- metadata$visual_elements$annotations |> filter(!lavaan & !network)

    points_data <- bind_rows(points_data, independent_points)
    lines_data <- bind_rows(lines_data, independent_lines)
    annotations_data <- bind_rows(annotations_data, independent_annotations)
    loops_data <- metadata$visual_elements$loops
  }

  res <- list(
    points = points_data,
    lines = lines_data,
    annotations = annotations_data,
    loops = loops_data
  )

  return(res)
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
#' }
#'
#' @keywords internal
#' @noRd
extract_sem_data <- function(metadata) {
  if (!"group_labels" %in% names(metadata)) {
    stop("metadata must contain 'group_labels'")
  }

  group_list <- metadata$group_labels

  # Generate network data for all groups
  sem_data <- lapply(1:length(group_list), function(i) {
    reproduce_sem(metadata = metadata, group_id = group_list[[i]])
  })

  # Extract and combine all data types
  points_data <- do.call(rbind, lapply(sem_data, function(x) x$points))
  lines_data <- do.call(rbind, lapply(sem_data, function(x) x$lines))
  annotations_data <- do.call(rbind, lapply(sem_data, function(x) x$annotations))

  res <- list(
    points = points_data,
    lines = lines_data,
    annotations = annotations_data
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
  if (!"group_labels" %in% names(metadata)) {
    stop("Metadata must contain 'group_labels'")
  }

  group_list <- metadata$group_labels

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
