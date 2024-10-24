if (!requireNamespace("semPlot", quietly = TRUE)) {
  stop("Package 'semPlot' is required to run the ggsem app. Please install it with install.packages('semPlot').")
}

library(shiny)
library(ggplot2)
library(igraph)
library(DT)
library(colourpicker)
library(grid)
library(svglite)
library(grDevices)
library(lavaan)
library(semPlot)

plot.new()

adjust_endpoint <- function(x1, y1, x2, y2, spacing = 0) {

  dx <- x2 - x1
  dy <- y2 - y1
  distance <- sqrt(dx^2 + dy^2)

  # Adjust the start and end points based on spacing
  if (distance > 0) {
    x1 <- x1 + (spacing * dx / distance)
    y1 <- y1 + (spacing * dy / distance)
    x2 <- x2 - (spacing * dx / distance)
    y2 <- y2 - (spacing * dy / distance)
  }

  return(list(x_start = x1, y_start = y1, x_end = x2, y_end = y2))
}

interpolate_points <- function(x_start, y_start, x_end, y_end, n = 100) {
  t <- seq(0, 1, length.out = n)
  x <- (1 - t) * x_start + t * x_end
  y <- (1 - t) * y_start + t * y_end
  data.frame(x = x, y = y)
}


default_control_point <- function(x_start, y_start, x_end, y_end, offset_ratio = 0.3) {
  mid_x <- (x_start + x_end) / 2
  mid_y <- (y_start + y_end) / 2

  dx <- x_end - x_start
  dy <- y_end - y_start
  offset_x <- -dy * offset_ratio
  offset_y <- dx * offset_ratio

  list(ctrl_x = mid_x + offset_x, ctrl_y = mid_y + offset_y)
}

create_bezier_curve <- function(x_start, y_start, x_end, y_end, ctrl_x, ctrl_y, n_points = 100) {
  t <- seq(0, 1, length.out = n_points)

  bezier_x <- (1 - t)^2 * x_start + 2 * (1 - t) * t * ctrl_x + t^2 * x_end
  bezier_y <- (1 - t)^2 * y_start + 2 * (1 - t) * t * ctrl_y + t^2 * y_end

  data.frame(x = bezier_x, y = bezier_y)
}


generate_graph_from_lavaan <- function(lavaan_string, relative_x_position = 1, relative_y_position = 1,
                                       point_size_latent = 40, point_size_observed = 40,
                                       line_width = 1, text_size = 20, text_font = "serif",
                                       point_color_latent = "black", point_color_observed = 'black',
                                       edge_color = "black", line_endpoint_spacing = 0,
                                       node_border_color = "white",
                                       node_border_width = 1, fontface = "plain",
                                       arrow_type = "open", arrow_size = 0.2,
                                       layout_algorithm = "tree", data = NULL) {

  # Extract the variables from the Lavaan string
  model <- lavaan::lavaanify(lavaan_string)
  latent_vars <- unique(model$lhs[model$op == "=~"])    # Latent variables
  observed_vars <- unique(setdiff(model$rhs[model$op %in% c("=~", "~", "~~")], latent_vars))

  synthetic_data <- as.data.frame(matrix(rnorm(100 * length(observed_vars)), nrow = 100))
  colnames(synthetic_data) <- observed_vars

  fit <- tryCatch({
    lavaan::sem(lavaan_string, data = synthetic_data)
  }, error = function(e) {
    stop("Error in Lavaan model: ", e$message)
  })

  sem_paths <- semPlot::semPaths(fit, layout = layout_algorithm, what = "paths", plot = FALSE)

  # Extract node coordinates and node names
  node_coords <- as.data.frame(sem_paths$layout)
  colnames(node_coords) <- c("x", "y")

  # Normalize coordinates to center the graph
  node_coords$x <- node_coords$x - mean(range(node_coords$x))
  node_coords$y <- node_coords$y - mean(range(node_coords$y))

  # Adjust coordinates based on user-defined scaling factors
  node_coords$x <- node_coords$x * relative_x_position
  node_coords$y <- node_coords$y * relative_y_position

  node_names <- names(sem_paths$graphAttributes$Nodes$labels)
  node_coords$name <- node_names


  # Create the data frames for points and lines

  points_df <- data.frame(x = node_coords$x, y = node_coords$y, stringsAsFactors = FALSE)
  points_df$shape <- ifelse(node_names %in% latent_vars, "circle", "square")
  points_df$color <- ifelse(node_names %in% latent_vars, point_color_latent, point_color_observed)
  points_df$size <- ifelse(node_names %in% latent_vars, point_size_latent, point_size_observed)
  points_df$border_color <- node_border_color
  points_df$border_width <- node_border_width
  points_df$alpha <- 1
  points_df$lavaan <- TRUE
  points_df$locked <- FALSE

  # Create annotations data frame
  annotations <- data.frame(
    text = node_coords$name,
    x = node_coords$x,
    y = node_coords$y,
    font = text_font,
    size = text_size,
    color = "black",
    angle = 0,
    alpha = 1,
    fontface = fontface,
    math_expression = FALSE,
    lavaan = TRUE,
    stringsAsFactors = FALSE
  )

  # Extract and convert edges to lines_df
  lines_df <- data.frame(x_start = numeric(), y_start = numeric(), x_end = numeric(), y_end = numeric(),
                         ctrl_x = numeric(), ctrl_y = numeric(), type = character(),
                         color = character(), end_color = character(), color_type = character(),
                         gradient_position = numeric(), width = numeric(), alpha = numeric(),
                         arrow = logical(), arrow_type = character(), arrow_size = numeric(),
                         two_way = logical(),
                         lavaan = logical(), line_style = character(), stringsAsFactors = FALSE)

  edges_from <- model$rhs[model$op %in% c("=~", "~", "~~")]
  edges_to <- model$lhs[model$op %in% c("=~", "~", "~~")]
  edge_op <- model$op[model$op %in% c("=~", "~", "~~")]

  edges <- data.frame(
    x_start = node_coords[match(edges_from, node_names), "x"],
    y_start = node_coords[match(edges_from, node_names), "y"],
    x_end = node_coords[match(edges_to, node_names), "x"],
    y_end = node_coords[match(edges_to, node_names), "y"],
    operation = edge_op,
    stringsAsFactors = FALSE
  )

  #print(edges)



  for (i in 1:nrow(edges)) {
    x_start <- edges$x_start[i]
    y_start <- edges$y_start[i]
    x_end <- edges$x_end[i]
    y_end <- edges$y_end[i]

    if (is.na(x_start) || is.na(y_start) || is.na(x_end) || is.na(y_end)) {
      warning(paste("Missing coordinates for edge at row", i))
      next
    }

    # Adjust endpoints based on spacing
    adjusted_coords <- adjust_endpoint(x_start, y_start, x_end, y_end, line_endpoint_spacing)

    if (adjusted_coords$x_start == adjusted_coords$x_end && adjusted_coords$y_start == adjusted_coords$y_end) {
      next  # Skip this line if it has zero length
    }

    is_two_way <- edges$operation[i] == "~~"

    # Add arrow and color information using user input for edge color
    new_line <- data.frame(
      x_start = adjusted_coords$x_start,
      y_start = adjusted_coords$y_start,
      x_end = adjusted_coords$x_end,
      y_end = adjusted_coords$y_end,
      ctrl_x = NA,
      ctrl_y = NA,
      type = "Straight Arrow",
      color = edge_color,
      end_color = edge_color,
      color_type = "Single",
      gradient_position = NA,
      width = line_width,
      alpha = 1,
      arrow = TRUE,
      arrow_type = arrow_type,
      arrow_size = arrow_size,
      two_way = is_two_way,
      lavaan = TRUE,
      line_style = 'solid',
      stringsAsFactors = FALSE
    )

    lines_df <- rbind(lines_df, new_line)
  }

  return(list(points = points_df, lines = lines_df, annotations = annotations))
}

auto_generate_edges <- function(points_data, layout_type = "fully_connected", line_color = "black",
                                line_width = 2, line_alpha = 1, random_prob = 0.1, particular_node = NULL,
                                auto_endpoint_spacing = 0) {
  # Filter out locked nodes
  unlocked_points <- points_data[!points_data$locked & !points_data$lavaan, ]

  if (nrow(unlocked_points) < 2) {
    return(NULL)
  }

  # Extract coordinates for unlocked points
  coord_matrix <- as.matrix(unlocked_points[, c("x", "y")])

  edge_list <- NULL

  # Fully connected layout
  if (layout_type == "fully_connected") {
    g <- make_full_graph(nrow(unlocked_points))
    edge_list <- as_edgelist(g)

  } else if (layout_type == "nearest_neighbor") {
    g <- make_empty_graph(n = nrow(unlocked_points))
    dist_matrix <- as.matrix(dist(coord_matrix))
    for (i in 1:nrow(dist_matrix)) {
      dist_matrix[i, i] <- Inf
      nearest_neighbor <- which.min(dist_matrix[i, ])
      if (!is.na(nearest_neighbor)) {
        g <- add_edges(g, c(i, nearest_neighbor))
      }
    }
    edge_list <- as_edgelist(g)

  } else if (layout_type == "connect_to_central_node") {
    center_point <- colMeans(coord_matrix)
    dist_to_center <- apply(coord_matrix, 1, function(row) sqrt(sum((row - center_point)^2)))
    central_node_index <- which.min(dist_to_center)
    edge_list <- cbind(central_node_index, setdiff(1:nrow(coord_matrix), central_node_index))
  } else if (layout_type == "connect_to_particular_node") {
    # Connect all unlocked points to the selected particular node
    if (!is.null(particular_node)) {
      selected_node <- as.numeric(particular_node)
      edge_list <- cbind(selected_node, setdiff(1:nrow(coord_matrix), selected_node))
    }

  } else if (layout_type == "random_graph") {
    g <- erdos.renyi.game(nrow(unlocked_points), p.or.m = random_prob, directed = FALSE)
    edge_list <- as_edgelist(g)
  }

  # Check if edge_list is valid and has proper indices
  if (!is.null(edge_list) && nrow(edge_list) > 0 && all(edge_list[, 1] <= nrow(coord_matrix)) && all(edge_list[, 2] <= nrow(coord_matrix))) {
    lines_df <- data.frame(
      x_start = coord_matrix[edge_list[, 1], 1],
      y_start = coord_matrix[edge_list[, 1], 2],
      x_end = coord_matrix[edge_list[, 2], 1],
      y_end = coord_matrix[edge_list[, 2], 2],
      ctrl_x = NA,
      ctrl_y = NA,
      type = "Auto-generated",
      color = line_color,
      end_color = NA,
      color_type = "Single",
      gradient_position = NA,
      width = line_width,
      alpha = line_alpha,
      arrow = FALSE,
      arrow_type = NA,
      arrow_size = NA,
      two_way = FALSE,
      lavaan = FALSE,
      line_style = 'solid',
      stringsAsFactors = FALSE
    )

    for (i in 1:nrow(lines_df)) {
      adjusted_coords <- adjust_endpoint(
        lines_df$x_start[i], lines_df$y_start[i],
        lines_df$x_end[i], lines_df$y_end[i],
        auto_endpoint_spacing
      )
      lines_df$x_start[i] <- adjusted_coords$x_start
      lines_df$y_start[i] <- adjusted_coords$y_start
      lines_df$x_end[i] <- adjusted_coords$x_end
      lines_df$y_end[i] <- adjusted_coords$y_end
    }

    return(lines_df)
  } else {
    return(NULL)
  }
}


auto_layout_points <- function(points_data, layout_type = "layout_in_circle", distance = 1, center_x = 0, center_y = 0) {
  if (!"locked" %in% names(points_data)) {
    points_data$locked <- FALSE
  }

  unlocked_points <- points_data[!points_data$locked & !points_data$lavaan, ]

  if (layout_type == "horizontal_straight") {
    n <- nrow(unlocked_points)
    unlocked_points$x <- seq(center_x - distance * (n - 1) / 2, center_x + distance * (n - 1) / 2, length.out = n)
    unlocked_points$y <- rep(center_y, n)
  } else if (layout_type == "vertical_straight") {
    n <- nrow(unlocked_points)
    unlocked_points$y <- seq(center_y - distance * (n - 1) / 2, center_y + distance * (n - 1) / 2, length.out = n)
    unlocked_points$x <- rep(center_x, n)
  } else {
    g <- make_empty_graph(n = nrow(unlocked_points))
    layout_fun <- match.fun(layout_type)
    layout_coords <- layout_fun(g) * distance
    unlocked_points$x <- layout_coords[, 1] + center_x
    unlocked_points$y <- layout_coords[, 2] + center_y
  }

  points_data[!points_data$locked & !points_data$lavaan, ] <- unlocked_points
  return(points_data)
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #add_point, #add_line, #add_annotation, #add_loop, #generate_graph {
        font-weight: bold;
      }
    "))
  ),
  titlePanel("ggsem: Interactive and Reproducible Visualizations of Networks and Structural Equation Modeling Diagrams"),

  sidebarLayout(
    sidebarPanel(
      h4("Element Selection"),
      # Add 'Self-loop Arrow' to the element selection
      selectInput("element_type", "Choose Element Type:", choices = c("Point", "Line", "Text Annotation", "Self-loop Arrow", "Lavaan Input")),

      # Layer control for ordering elements
      radioButtons("layer_order", "Select Layer Order:",
                   choices = list("Points in front" = "points_front",
                                  "Lines in front" = "lines_front",
                                  "Annotations in front" = "annotations_front",
                                  "Self-loop Arrows in front" = "loops_front"),
                   selected = "points_front"),



      # Zoom control slider
      fluidRow(
        column(4, sliderInput("zoom", "Zoom Level:", min = .2, max = 5, value = 1.2, step = 0.1)),
        column(4, sliderInput("horizontal_shift", "X-Level:", min = -50, max = 50, value = 0, step = 1)),
        column(4, sliderInput("vertical_shift", "Y-Level:", min = -50, max = 50, value = 0, step = 1))
      ),
      fluidRow(
        column(6, actionButton("undo_button", "Undo")),
        column(6, actionButton("redo_button", "Redo"))
      ),
      hr(),
      conditionalPanel(
        condition = "input.element_type == 'Point'",
        shiny::wellPanel(
          h4("Point Inputs"),
          fluidRow(
            column(6, textInput("x_coord", "X Coordinate:", "0")),
            column(6, textInput("y_coord", "Y Coordinate:", "0"))
          ),
          fluidRow(
            column(6, colourInput("point_color", "Point Color:", value = "black")),
            column(6, selectInput("shape", "Select Shape:", choices = c("circle", "triangle", "square", "diamond")))
          ),
          fluidRow(
            column(6, numericInput("point_size", "Point Size:", value = 40, min = 1)),
            column(6, numericInput("border_width", "Border Width:", value = 1, min = 0))
          ),
          fluidRow(
            column(6, colourInput("border_color", "Border Color:", value = "white")),
            column(6, numericInput("point_alpha", "Point Alpha:", value = 1, min = 0, max = 1, step = 0.1))  # Alpha input for points
          ),
          fluidRow(
            column(12, actionButton("add_point", "Add Point"))
          ),
          hr(),
          h4("Draw Networks"),
          fluidRow(
            column(6, selectInput("layout_type", "Layout Type:",
                                  choices = c("Circle" = "layout_in_circle",
                                              "Grid" = "layout_on_grid",
                                              "Random" = "layout_randomly",
                                              "Star" = "layout_as_star",
                                              "Fruchterman-Reingold" = "layout_with_fr",
                                              "Kamada-Kawai" = "layout_with_kk",
                                              "Horizontal Straight Line" = "horizontal_straight",
                                              "Vertical Straight Line" = "vertical_straight"))),
            column(6, numericInput("point_distance", "Point Distance:", value = 10, min = 0.1, step = 0.1))
          ),
          fluidRow(
            column(6, numericInput("center_x", "Center X Position:", value = 0)),
            column(6, numericInput("center_y", "Center Y Position:", value = 0))
          ),
          fluidRow(
            column(6, colourInput("grad_start_color", "Gradient Start Color:", value = "blue")),
            column(6, colourInput("grad_end_color", "Gradient End Color:", value = "red"))
          ),
          fluidRow(
            column(6, actionButton("auto_layout", "Auto-layout Points"))
          ),
          actionButton("apply_gradient", "Apply Gradient"),
          fluidRow(
            column(6, actionButton("lock_points", "Lock Points"))
          ),
        )
      ),

      # Line Inputs in conditionalPanel
      conditionalPanel(
        condition = "input.element_type == 'Line'",
        shiny::wellPanel(
          h4("Coonect Nodes"),
          selectInput("connection_type", "Choose Edge Connection Type:",
                      choices = c("Fully Connected" = "fully_connected",
                                  "Nearest Neighbor" = "nearest_neighbor",
                                  "Connect to Central Node" = "connect_to_central_node",
                                  "Connect to Particular Node" = "connect_to_particular_node",
                                  "Random Graph" = "random_graph"),
                      selected = "connect_to_central_node"),

          conditionalPanel(
            condition = "input.connection_type == 'connect_to_particular_node'",
            selectInput("particular_node", "Select Central Node:", choices = NULL)
          ),


          fluidRow(
            column(6, colourInput("auto_line_color", "Edge Color:", value = "black")),
            column(6, numericInput("auto_endpoint_spacing", "Edge Spacing:", value = 0, min = 0, step = 0.1))
          ),
          fluidRow(
            column(6, numericInput("auto_line_width", "Edge Width:", value = 1, min = 0.1, step = 0.1)),
            column(6, numericInput("auto_line_alpha", "Edge Alpha:", value = 1, min = 0, max = 1, step = 0.1))
          ),


          actionButton("auto_generate_edges_button", "Auto-generate Edges"),
          hr(),
          h4("Line Inputs"),
          fluidRow(
            column(6, textInput("x_start", "Start X Coordinate:", "0")),
            column(6, textInput("y_start", "Start Y Coordinate:", "0"))
          ),
          fluidRow(
            column(6, textInput("x_end", "End X Coordinate:", "5")),
            column(6, textInput("y_end", "End Y Coordinate:", "5"))
          ),
          fluidRow(
            column(6, colourInput("line_color", "Start Color:", value = "black")),
            column(6, selectInput("color_type", "Line Color Type:", choices = c("Single", "Gradient")))
          ),

          conditionalPanel(
            condition = "input.color_type == 'Gradient'",
            fluidRow(
              column(6, colourInput("end_color", "End Color:", value = "white")),
              column(6, sliderInput("gradient_position", "Gradient Intersection:", min = 0.01, max = 0.99, value = 0.5, step = 0.01))
            )
          ),


          fluidRow(
            column(6, numericInput("line_width", "Line Width:", value = 1, min = 1)),
            column(6, numericInput("line_alpha", "Line Alpha:", value = 1, min = 0, max = 1, step = 0.1))
          ),
          fluidRow(
            column(6, selectInput("line_type", "Line Type:", choices = c("Straight Line", "Straight Arrow", "Curved Line", "Curved Arrow"))),
            column(6, selectInput("line_style", "Line Style:", choices = c("solid", "dashed", "dotted")))
          ),

          # Conditional display for curved lines
          conditionalPanel(
            condition = "input.line_type == 'Curved Line' || input.line_type == 'Curved Arrow'",
            fluidRow(
              column(6, numericInput("ctrl_x", "Control Point X", value = 0)),
              column(6, numericInput("ctrl_y", "Control Point Y", value = 0))
            )
          ),

          # Conditional display for arrows
          conditionalPanel(
            condition = "input.line_type == 'Straight Arrow' || input.line_type == 'Curved Arrow'",
            fluidRow(
              column(6, selectInput("arrow_type", "Arrow Type:", choices = c("open", "closed"))),
              column(6, numericInput("arrow_size", "Arrow Size:", value = 0.2, min = 0.1, step = 0.1))
            ),
            fluidRow(
              column(6, checkboxInput("two_way_arrow", "Two-way Arrow", value = FALSE))  # two-way arrows checkbox
            )
          ),

          actionButton("add_line", "Add Line"),
        )
      ),

      # Text Annotation Inputs
      conditionalPanel(
        condition = "input.element_type == 'Text Annotation'",
        shiny::wellPanel(
          h4("Text Annotation Inputs"),
          fluidRow(
            column(12, textInput("annotation_text", "Text:", "Sample Text")),
          ),
          fluidRow(
            column(12, checkboxInput("math_expression", "Use Math Expression", value = FALSE))
          ),
          fluidRow(
            column(6, textInput("annotation_x", "X Coordinate:", "0")),
            column(6, textInput("annotation_y", "Y Coordinate:", "0"))
          ),
          fluidRow(
            column(6, selectInput("font_family", "Font:",
                                  choices = c("Arial", "Courier New", "Georgia", "Times New Roman",
                                              "Mono", "Sans", "Serif"),
                                  selected = "Serif")),
            column(6, numericInput("text_size", "Text Size:", value = 20, min = 1))
          ),
          fluidRow(
            column(6, colourInput("text_color", "Color:", value = "black")),
            column(6, numericInput("text_angle", "Angle (deg):", value = 0))
          ),
          fluidRow(
            column(6, numericInput("text_alpha", "Text Alpha:", value = 1, min = 0, max = 1, step = 0.1)),
            column(6, selectInput("text_typeface", "Typeface:", choices = c("Plain", "Bold", "Italic"))),
            column(6, actionButton("add_annotation", "Add Text"))
          ),
        )
      ),

      # Self-loop Arrow Inputs
      conditionalPanel(
        condition = "input.element_type == 'Self-loop Arrow'",
        shiny::wellPanel(
          h4("Self-loop Arrow Inputs"),
          fluidRow(
            column(6, textInput("x_center", "X Coordinate (Center):", "0")),
            column(6, textInput("y_center", "Y Coordinate (Center):", "0"))
          ),
          fluidRow(
            column(6, numericInput("radius", "Radius:", value = 5, min = 0.1)),
            column(6, numericInput("line_width_loop", "Line Width:", value = 1, min = 0.1))
          ),
          fluidRow(
            column(6, colourInput("line_color_loop", "Line Color:", value = "black")),
            column(6, numericInput("line_alpha_loop", "Line Alpha:", value = 1, min = 0, max = 1, step = 0.1))
          ),
          fluidRow(
            column(6, selectInput("arrow_type_loop", "Arrow Type:", choices = c("open", "closed"))),
            column(6, numericInput("arrow_size_loop", "Arrow Size:", value = 0.2, min = 0.1, step = 0.1))
          ),
          fluidRow(
            column(6, numericInput("width_loop", "Loop Width:", value = 1, min = 0.1)),
            column(6, numericInput("height_loop", "Loop Height:", value = 1, min = 0.1))
          ),
          fluidRow(
            column(12, checkboxInput("two_way_arrow_loop", "Two-way Arrow", value = FALSE))  # checkbox for two-way self-loop arrows
          ),
          actionButton("add_loop", "Add Self-loop Arrow"),
          hr(),
          h4("Change Configurations"),
          fluidRow(
            column(6, numericInput("gap_size_loop", "Gap Size:", value = 0.2, min = 0, max = 1, step = 0.05)),
            column(6, numericInput("orientation_loop", "Loop Orientation (deg):", value = 0, min = -180, max = 180))
          ),
          fluidRow(
            column(12, actionButton("apply_loop_changes", "Apply Changes"))
          ),
          fluidRow(
            column(12, actionButton("lock_loops", "Lock Self-loop Arrows", value = FALSE))  # checkbox for locking self-loops
          )
        )
      ),

      # Conditional panel for Lavaan input
      conditionalPanel(
        condition = "input.element_type == 'Lavaan Input'",
        shiny::wellPanel(
          h4("Lavaan Syntax Input"),

          # Lavaan syntax input
          textAreaInput("lavaan_syntax", "Lavaan Syntax", value = '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
visual ~~ speed
    ', width = "100%", height = "200px"),

          fluidRow(
            column(6, numericInput("relative_x_position", "Relative X Node Position:", value = 15, min = 0.1, step = 0.1)),
            column(6, numericInput("relative_y_position", "Relative Y Node Position:", value = 15, min = 0.1, step = 0.1))
          ),
          fluidRow(
            column(6, numericInput("line_endpoint_spacing",
                                   "Line Endpoint Spacing:",
                                   value = 3, min = 0, step = 0.1))
          ),

          # Point size input

          fluidRow(
            column(6, colourInput("latent_color_input", "Latent Variable Color:", value = "#FC8888")),
            column(6, colourInput("observed_color_input", "Observed Variable Color:", value = "#8888FC"))
          ),
          fluidRow(
            column(6, numericInput("latent_size_input", "Latent Variable Size:", value = 50, min = 1)),
            column(6, numericInput("observed_size_input", "Observed Variable Size:", value = 40, min = 1))
          ),

          selectInput("lavaan_layout", "Choose Layout Algorithm:",
                      choices = c("Tree" = "tree",
                                  "Circle" = "circle",
                                  "Spring" = "spring",
                                  "Tree2" = "tree2",
                                  "Circle2" = "circle2",
                                  "Default" = "default")),

          actionButton("generate_graph", "Draw SEM"),
          actionButton("apply_changes_lavaan", "Apply Changes"),

          hr(),

          fluidRow(
            column(6, colourInput("node_border_color", "Node Border Color:", value = "white")),
            column(6, numericInput("node_border_width", "Border Width:", value = 1, min = 0.1, step = 0.1))
          ),

          fluidRow(
            column(6, numericInput("text_size_input",
                                   "Text Size:",
                                   value = 20, min = 5, step = 1)),
            column(6, selectInput("text_font_input", "Text Font:",
                                  choices = c("Arial", "Courier New", "Georgia", "Times New Roman",
                                              "Mono", "Sans", "Serif"), selected = "Serif"))
          ),

          fluidRow(
            column(6, selectInput("fontface_input", "Fontface:", choices = c("Plain", "Bold", "Italic"))),
            column(6, numericInput("line_width_input",
                                   "Linewidth:",
                                   value = 1, min = 0.1, step = 0.1))
          ),


          fluidRow(
            column(6, selectInput("lavaan_arrow_type", "Arrow Type:", choices = c("open", "closed"), selected = 'closed')),
            column(6, numericInput("lavaan_arrow_size", "Arrow Size:", value = 0.1, min = 0.1, step = 0.1))
          ),


          colourInput("edge_color_input", "Edge Color:", value = "black")
        )
      ),

      # Upload CSV files
      h4("Upload CSV Files"),
      fileInput("points_file", "Upload Points CSV"),
      fileInput("lines_file", "Upload Lines CSV"),
      fileInput("annotations_file", "Upload Annotations CSV"),
      fileInput("self_loop_file", "Upload Self Loop Arrows CSV"),
      hr(),
      # Download CSV dropdown menu
      selectInput("csv_type", "Choose CSV to Download:",
                  choices = c("Points CSV", "Lines CSV", "Annotations CSV", "Self-loop Arrows CSV")),
      downloadButton("download_selected_csv", "Download Selected CSV"),
      hr(),

      # Image export menu
      selectInput("export_format", "Choose Export Format:", choices = c("PNG", "JPEG", "PDF", "SVG")),
      downloadButton("download_plot", "Save the Figure"),

      textOutput("instruction")
    ),

    # Main panel for plot and data tables
    mainPanel(
      plotOutput("plot", hover = hoverOpts(id = "plot_hover")),
      textOutput("hover_info"),

      fluidRow(
        column(12, h4("Points Table")),
        column(6, actionButton("delete_selected_point", "Delete Selected Point")),
        column(6, actionButton("delete_all_points", "Delete All Points")),
        column(12, DTOutput("data_table"))
      ),

      fluidRow(
        column(12, h4("Lines Table")),
        column(6, actionButton("delete_selected_line", "Delete Selected Line")),
        column(6, actionButton("delete_all_lines", "Delete All Lines")),
        column(12, DTOutput("line_table"))
      ),

      fluidRow(
        column(12, h4("Annotations Table")),
        column(6, actionButton("delete_selected_annotation", "Delete Selected Annotation")),
        column(6, actionButton("delete_all_annotations", "Delete All Annotations")),
        column(12, DTOutput("annotation_table"))
      ),

      fluidRow(
        column(12, h4("Self-loop Arrows Table")),
        column(6, actionButton("delete_selected_loop", "Delete Selected Self-loop Arrow")),
        column(6, actionButton("delete_all_loops", "Delete All Self-loop Arrows")),
        column(12, DTOutput("loop_table"))
      ),
      fluidRow(
        column(12, textOutput("axis_info"))  # hover -> XY coord
      )
    )
  )
)

server <- function(input, output, session) {
  options(warn = -1)
  # For undo/redo history
  values <- reactiveValues(
    points = data.frame(x = numeric(), y = numeric(), shape = character(), color = character(), size = numeric(),
                        border_color = character(), border_width = numeric(), alpha = numeric(), locked = logical(), stringsAsFactors = FALSE),
    lines = data.frame(x_start = numeric(), y_start = numeric(), x_end = numeric(), y_end = numeric(),
                       ctrl_x = numeric(), ctrl_y = numeric(), type = character(), color = character(), end_color = character(), color_type = character(),
                       gradient_position = numeric(), width = numeric(), alpha = numeric(), arrow = logical(), arrow_type = character(),
                       arrow_size = numeric(), two_way = logical(), lavaan = logical(), line_style = character(), stringsAsFactors = FALSE),
    annotations = data.frame(text = character(), x = numeric(), y = numeric(), font = character(), size = numeric(), color = character(), angle = numeric(), alpha = numeric(), two_way = logical(), stringsAsFactors = FALSE),
    loops = data.frame(x_center = numeric(), y_center = numeric(), radius = numeric(), color = character(),
                       width = numeric(), alpha = numeric(), arrow_type = character(), arrow_size = numeric(),
                       gap_size = numeric(), loop_width = numeric(), loop_height = numeric(), orientation = numeric(),
                       locked = logical(), stringsAsFactors = FALSE),
    undo_stack = list(),  # Stack for undo
    redo_stack = list()   # Stack for redo
  )
  uploaded_data <- reactiveVal(NULL)
  # Reactive value to store the last valid hover coordinates
  last_hover <- reactiveVal(NULL)
  debounced_hover <- debounce(reactive(input$plot_hover), 10)

  output$hover_info <- renderText({
    hover <- input$plot_hover
    if (!is.null(hover)) {
      last_hover(paste("Hovered at: X =", round(hover$x, 2), "Y =", round(hover$y, 2)))
    }
    last_hover() %||% "Hover over the plot to see X and Y coordinates"
  })

  save_state <- function() {
    values$undo_stack <- append(values$undo_stack, list(list(points = values$points,
                                                             lines = values$lines,
                                                             annotations = values$annotations,
                                                             loops = values$loops)))
    values$redo_stack <- list()
  }

  undo <- function() {
    if (length(values$undo_stack) > 0) {
      values$redo_stack <- append(values$redo_stack, list(list(points = values$points,
                                                               lines = values$lines,
                                                               annotations = values$annotations,
                                                               loops = values$loops)))

      last_state <- tail(values$undo_stack, 1)[[1]]
      values$undo_stack <- values$undo_stack[-length(values$undo_stack)]
      values$points <- last_state$points
      values$lines <- last_state$lines
      values$annotations <- last_state$annotations
      values$loops <- last_state$loops
    }
  }


  redo <- function() {
    if (length(values$redo_stack) > 0) {
      values$undo_stack <- append(values$undo_stack, list(list(points = values$points,
                                                               lines = values$lines,
                                                               annotations = values$annotations,
                                                               loops = values$loops)))
      last_state <- tail(values$redo_stack, 1)[[1]]
      values$redo_stack <- values$redo_stack[-length(values$redo_stack)]
      values$points <- last_state$points
      values$lines <- last_state$lines
      values$annotations <- last_state$annotations
      values$loops <- last_state$loops
    }
  }

  add_new_line <- function(new_line_data) {
    expected_columns <- c("x_start", "y_start", "x_end", "y_end", "ctrl_x", "ctrl_y", "type",
                          "color", "end_color", "color_type", "gradient_position", "width",
                          "alpha", "arrow", "arrow_type", "arrow_size", "two_way", "lavaan", "line_style")

    missing_columns <- setdiff(expected_columns, colnames(new_line_data))
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        new_line_data[[col]] <- NA
      }
    }

    new_line_data <- new_line_data[expected_columns]
    values$lines <- rbind(values$lines, new_line_data)
  }


  observeEvent(input$lock_points, {
    save_state()
    values$points$locked <- TRUE
  })

  observeEvent(input$lock_loops, {
    save_state()
    values$loops$locked <- TRUE
  })


  observeEvent(input$undo_button, {
    undo()
  })

  observeEvent(input$redo_button, {
    redo()
  })


  observeEvent({input$x_start; input$y_start; input$x_end; input$y_end}, {
    req(input$x_start, input$y_start, input$x_end, input$y_end)

    default_ctrl <- default_control_point(as.numeric(input$x_start), as.numeric(input$y_start), as.numeric(input$x_end), as.numeric(input$y_end))

    updateNumericInput(session, "ctrl_x", value = default_ctrl$ctrl_x)
    updateNumericInput(session, "ctrl_y", value = default_ctrl$ctrl_y)
  })

  # Add point
  observeEvent(input$add_point, {
    req(input$x_coord, input$y_coord)
    save_state()  # Save the state before making changes
    new_point <- data.frame(x = as.numeric(input$x_coord),
                            y = as.numeric(input$y_coord),
                            shape = input$shape,
                            color = input$point_color,
                            size = input$point_size,
                            border_color = input$border_color,
                            border_width = input$border_width,
                            alpha = input$point_alpha,
                            locked = FALSE,
                            lavaan = FALSE,
                            stringsAsFactors = FALSE)
    values$points <- rbind(values$points, new_point)
  })

  # Auto layout points
  observeEvent(input$auto_layout, {
    req(nrow(values$points) > 0)
    save_state()

    values$points <- auto_layout_points(values$points,
                                        layout_type = input$layout_type,
                                        distance = input$point_distance,
                                        center_x = input$center_x,
                                        center_y = input$center_y)
  })


  # Add line
  observeEvent(input$add_line, {
    req(input$x_start, input$y_start, input$x_end, input$y_end)
    save_state()

    new_line <- data.frame(
      x_start = as.numeric(input$x_start),
      y_start = as.numeric(input$y_start),
      x_end = as.numeric(input$x_end),
      y_end = as.numeric(input$y_end),
      ctrl_x = if (input$line_type %in% c("Curved Line", "Curved Arrow")) as.numeric(input$ctrl_x) else NA,
      ctrl_y = if (input$line_type %in% c("Curved Line", "Curved Arrow")) as.numeric(input$ctrl_y) else NA,
      type = input$line_type,
      color = input$line_color,
      end_color = if (input$color_type == "Gradient") input$end_color else NA,
      color_type = input$color_type,
      gradient_position = if (input$color_type == "Gradient") input$gradient_position else NA,
      width = input$line_width,
      alpha = input$line_alpha,
      arrow = input$line_type %in% c("Straight Arrow", "Curved Arrow"),
      arrow_type = if (input$line_type %in% c("Straight Arrow", "Curved Arrow")) input$arrow_type else NA,
      arrow_size = if (input$line_type %in% c("Straight Arrow", "Curved Arrow")) input$arrow_size else NA,
      two_way = input$two_way_arrow,
      lavaan = FALSE,
      line_style = input$line_style,
      stringsAsFactors = FALSE
    )
    add_new_line(new_line)
    #values$lines <- rbind(values$lines, new_line)
  })

  # Auto-generate edges
  observeEvent(input$auto_generate_edges_button, {
    req(nrow(values$points) > 1)

    save_state()

    connection_type <- input$connection_type

    new_edges <- auto_generate_edges(
      points_data = values$points,
      layout_type = connection_type,
      line_color = input$auto_line_color,
      line_width = input$auto_line_width,
      line_alpha = input$auto_line_alpha,
      particular_node = input$particular_node,
      auto_endpoint_spacing = input$auto_endpoint_spacing
    )

    if (!is.null(new_edges)) {
      new_edges$width <- input$auto_line_width
      new_edges$alpha <- input$auto_line_alpha
      new_edges$color <- input$auto_line_color

      values$lines <- rbind(values$lines, new_edges)
    } else {
      showModal(modalDialog(
        title = "Error",
        "Not enough points to generate edges.",
        easyClose = TRUE
      ))
    }
  })

  observe({
    if (nrow(values$points) > 0) {
      point_choices <- seq_len(nrow(values$points))
      updateSelectInput(session, "particular_node", choices = point_choices)
    }
  })


  # Add annotation
  observeEvent(input$add_annotation, {
    req(input$annotation_text, input$annotation_x, input$annotation_y)
    save_state()

    fontface <- switch(input$text_typeface,
                       "Bold" = "bold",
                       "Italic" = "italic",
                       "Plain" = "plain")

    new_annotation <- data.frame(
      text = input$annotation_text,
      x = as.numeric(input$annotation_x),
      y = as.numeric(input$annotation_y),
      font = input$font_family,
      size = input$text_size,
      color = input$text_color,
      angle = input$text_angle,
      alpha = input$text_alpha,
      fontface = fontface,
      math_expression = input$math_expression,
      lavaan = FALSE,
      stringsAsFactors = FALSE
    )

    values$annotations <- rbind(values$annotations, new_annotation)
  })


  # Add self-loop arrow
  observeEvent(input$add_loop, {
    req(input$x_center, input$y_center, input$radius, input$gap_size_loop, input$width_loop, input$height_loop, input$orientation_loop)
    save_state()

    t <- seq(0, 2 * pi, length.out = 100)
    gap_angle <- input$gap_size_loop * pi
    loop_start <- t[t < (2 * pi - gap_angle)]

    x_ellipse <- as.numeric(input$x_center) + input$width_loop * input$radius * cos(loop_start)
    y_ellipse <- as.numeric(input$y_center) + input$height_loop * input$radius * sin(loop_start)

    theta <- input$orientation_loop * pi / 180
    x_rotated <- cos(theta) * (x_ellipse - as.numeric(input$x_center)) - sin(theta) * (y_ellipse - as.numeric(input$y_center)) + as.numeric(input$x_center)
    y_rotated <- sin(theta) * (x_ellipse - as.numeric(input$x_center)) + cos(theta) * (y_ellipse - as.numeric(input$y_center)) + as.numeric(input$y_center)

    circle_data <- data.frame(x = x_rotated, y = y_rotated)

    arrow_type <- if (input$arrow_type_loop == "closed") arrow(type = "closed", length = unit(input$arrow_size_loop, "inches")) else arrow(type = "open", length = unit(input$arrow_size_loop, "inches"))

    new_loop <- data.frame(
      x_center = as.numeric(input$x_center),
      y_center = as.numeric(input$y_center),
      radius = input$radius,
      color = input$line_color_loop,
      width = input$line_width_loop,
      alpha = input$line_alpha_loop,
      arrow_type = input$arrow_type_loop,
      arrow_size = input$arrow_size_loop,
      gap_size = input$gap_size_loop,
      loop_width = input$width_loop,
      loop_height = input$height_loop,
      orientation = input$orientation_loop,
      two_way = input$two_way_arrow_loop,
      locked = FALSE,
      stringsAsFactors = FALSE
    )

    values$loops <- rbind(values$loops, new_loop)
  })

  observeEvent(input$apply_loop_changes, {
    req(nrow(values$loops) > 0)

    save_state()

    for (i in 1:nrow(values$loops)) {
      if (!values$loops$locked[i]) {
        values$loops$orientation[i] <- input$orientation_loop
        values$loops$gap_size[i] <- input$gap_size_loop
      }
    }
  })

  observeEvent(input$generate_graph, {
    req(input$lavaan_syntax)

    save_state()

    fontface <- switch(input$fontface_input,
                       "Bold" = "bold",
                       "Italic" = "italic",
                       "Plain" = "plain")


    updateRadioButtons(session, "layer_order", selected = "annotations_front")

    tryCatch({
      graph_data <- generate_graph_from_lavaan(input$lavaan_syntax, data = data,
                                               relative_x_position = input$relative_x_position,
                                               relative_y_position = input$relative_y_position,
                                               point_size_latent = input$latent_size_input,
                                               point_size_observed = input$observed_size_input,
                                               line_width = input$line_width_input,
                                               text_size = input$text_size_input,
                                               text_font = input$text_font_input,
                                               point_color_latent = input$latent_color_input,
                                               point_color_observed = input$observed_color_input,
                                               edge_color = input$edge_color_input,
                                               line_endpoint_spacing = input$line_endpoint_spacing,
                                               node_border_color = input$node_border_color,
                                               node_border_width = input$node_border_width,
                                               fontface = fontface,
                                               arrow_type = input$lavaan_arrow_type,
                                               arrow_size = input$lavaan_arrow_size,
                                               layout_algorithm = input$lavaan_layout)

      values$points <- rbind(values$points, graph_data$points)
      values$lines <- rbind(values$lines, graph_data$lines)
      values$annotations <- rbind(values$annotations, graph_data$annotations)

      output$plot <- renderPlot({
        recreate_plot()
      })
    }, error = function(e) {
      showNotification(paste("Error in Lavaan model:", e$message), type = "error")
    })
  })

  observeEvent(input$apply_changes_lavaan, {
    save_state()

    fontface <- switch(input$fontface_input,
                       "Bold" = "bold",
                       "Italic" = "italic",
                       "Plain" = "plain")

    graph_data <- generate_graph_from_lavaan(input$lavaan_syntax,
                                             relative_x_position = input$relative_x_position,
                                             relative_y_position = input$relative_y_position,
                                             point_size_latent = input$latent_size_input,
                                             point_size_observed = input$observed_size_input,
                                             line_width = input$line_width_input,
                                             text_size = input$text_size_input,
                                             text_font = input$text_font_input,
                                             point_color_latent = input$latent_color_input,
                                             point_color_observed = input$observed_color_input,
                                             edge_color = input$edge_color_input,
                                             line_endpoint_spacing = input$line_endpoint_spacing,
                                             node_border_color = input$node_border_color,
                                             node_border_width = input$node_border_width,
                                             fontface = fontface,
                                             arrow_type = input$lavaan_arrow_type,
                                             arrow_size = input$lavaan_arrow_size,
                                             layout_algorithm = input$lavaan_layout)

    lavaan_points <- which(values$points$lavaan == TRUE)
    if (length(lavaan_points) > 0) {
      values$points$x[lavaan_points] <- graph_data$points$x
      values$points$y[lavaan_points] <- graph_data$points$y
      values$points$size[lavaan_points] <- graph_data$points$size
      values$points$color[lavaan_points] <- graph_data$points$color
      values$points$border_color[lavaan_points] <- graph_data$points$border_color
      values$points$border_width[lavaan_points] <- graph_data$points$border_width
    }

    lavaan_lines <- which(values$lines$lavaan == TRUE)
    if (length(lavaan_lines) > 0) {
      values$lines$x_start[lavaan_lines] <- graph_data$lines$x_start
      values$lines$y_start[lavaan_lines] <- graph_data$lines$y_start
      values$lines$x_end[lavaan_lines] <- graph_data$lines$x_end
      values$lines$y_end[lavaan_lines] <- graph_data$lines$y_end
      values$lines$width[lavaan_lines] <- graph_data$lines$width
      values$lines$color[lavaan_lines] <- graph_data$lines$color
      values$lines$arrow_type[lavaan_lines] <- graph_data$lines$arrow_type
      values$lines$arrow_size[lavaan_lines] <- graph_data$lines$arrow_size
    }

    lavaan_annotations <- which(values$annotations$lavaan == TRUE)
    if (length(lavaan_annotations) > 0) {
      values$annotations$x[lavaan_annotations] <- graph_data$annotations$x
      values$annotations$y[lavaan_annotations] <- graph_data$annotations$y
      values$annotations$size[lavaan_annotations] <- graph_data$annotations$size
      values$annotations$font[lavaan_annotations] <- graph_data$annotations$font
      values$annotations$fontface[lavaan_annotations] <- graph_data$annotations$fontface
    }

    output$plot <- renderPlot({
      recreate_plot()
    })
  })




  observeEvent(input$apply_loop_changes, {
    req(nrow(values$loops) > 0)

    save_state()

    for (i in 1:nrow(values$loops)) {
      if (!values$loops$locked[i]) {
        values$loops$gap_size[i] <- input$gap_size_loop
        values$loops$orientation[i] <- input$orientation_loop
      }
    }

    output$plot <- renderPlot({
      recreate_plot()
    })
  })

  observeEvent(input$lock_loops, {
    save_state()
    values$loops$locked <- TRUE  # Locks all loops
  })

  observeEvent(input$apply_gradient, {
    unlocked_points <- values$points[!values$points$locked & !values$points$lavaan, ]

    if (nrow(unlocked_points) > 1) {
      grad_start_color <- input$grad_start_color
      grad_end_color <- input$grad_end_color
      gradient_colors_layout <- colorRampPalette(c(grad_start_color, grad_end_color))(nrow(unlocked_points))

      values$points[!values$points$locked & !values$points$lavaan, "color"] <- gradient_colors_layout

      output$plot <- renderPlot({
        recreate_plot()
      })
    }
  })

  observeEvent(input$delete_all_points, {
    save_state()
    values$points <- data.frame(x = numeric(), y = numeric(), shape = character(), color = character(), size = numeric(),
                                border_color = character(), border_width = numeric(), alpha = numeric(), locked = logical(), stringsAsFactors = FALSE)
  })

  observeEvent(input$delete_selected_point, {
    selected_row <- input$data_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$points <- values$points[-selected_row, ]
    }
  })


  observeEvent(input$delete_selected_line, {
    selected_row <- input$line_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$lines <- values$lines[-selected_row, ]
    }
  })

  observeEvent(input$delete_all_lines, {
    save_state()
    values$lines <- data.frame(x_start = numeric(), y_start = numeric(), x_end = numeric(), y_end = numeric(),
                               ctrl_x = numeric(), ctrl_y = numeric(), type = character(), color = character(),
                               width = numeric(), alpha = numeric(), arrow = logical(), stringsAsFactors = FALSE)
  })

  observeEvent(input$delete_selected_annotation, {
    selected_row <- input$annotation_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$annotations <- values$annotations[-selected_row, ]
    }
  })

  observeEvent(input$lavaan_file, {
    req(input$lavaan_file)
    user_data <- tryCatch({
      read.csv(input$lavaan_file$datapath)
    }, error = function(e) {
      showNotification("Error reading CSV file. Please upload a valid CSV.", type = "error")
      return(NULL)
    })

    if (!is.null(user_data)) {
      uploaded_data(user_data)
    }
  })

  observeEvent(input$delete_all_annotations, {
    save_state()
    values$annotations <- data.frame(text = character(), x = numeric(), y = numeric(), font = character(),
                                     size = numeric(), color = character(), angle = numeric(), alpha = numeric(), stringsAsFactors = FALSE)
  })

  observeEvent(input$delete_selected_loop, {
    selected_row <- input$loop_table_rows_selected
    if (!is.null(selected_row)) {
      save_state()
      values$loops <- values$loops[-selected_row, ]
    }
  })

  observeEvent(input$delete_all_loops, {
    save_state()
    values$loops <- data.frame(x_center = numeric(), y_center = numeric(), radius = numeric(), color = character(),
                               width = numeric(), alpha = numeric(), arrow_type = character(), arrow_size = numeric(),
                               gap_size = numeric(), loop_width = numeric(), loop_height = numeric(), orientation = numeric(), stringsAsFactors = FALSE)
  })

  # Create the plot output on the plotting space
  recreate_plot <- function() {
    zoom_factor <- input$zoom
    horizontal_shift <- input$horizontal_shift
    vertical_shift <- input$vertical_shift

    x_limits <- c(-20, 20) * zoom_factor + horizontal_shift
    y_limits <- c(-20, 20) * zoom_factor + vertical_shift
    p <- ggplot() +
      coord_fixed(ratio = 1, xlim = x_limits, ylim = y_limits) +  # Ensure square plotting space
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
      )
    #scale_x_continuous(breaks = seq(x_limits[[1]], x_limits[[2]], by = 10)) +
    #scale_y_continuous(breaks = seq(y_limits[[1]], y_limits[[2]], by = 10))

    valid_hex <- function(x) {
      if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x)) {
        return(x)
      } else if (is.na(x)) {
        return(NA)
      } else {
        return("#000000")  # Default to black or another fallback color
      }
    }

    layer_order <- input$layer_order

    draw_points <- function(p, zoom_factor) {
      if (nrow(values$points) > 0) {
        values$points$color <- sapply(values$points$color, valid_hex)
        values$points$border_color <- sapply(values$points$border_color, valid_hex)

        if (length(values$points$color) != nrow(values$points)) {
          values$points$color <- rep(values$points$color[1], nrow(values$points))
        }

        if (length(values$points$border_width) != nrow(values$points)) {
          values$points$border_width <- rep(values$points$border_width[1], nrow(values$points))
        }

        adjusted_size <- (values$points$size / 3) / zoom_factor
        adjusted_stroke <- values$points$border_width / zoom_factor

        shape_mapping <- sapply(values$points$shape, function(shape) {
          if (shape == "circle") {
            return(21)
          } else if (shape == "triangle") {
            return(24)
          } else if (shape == "square") {
            return(22)
          } else {
            return(23)
          }
        })

        p <- p + geom_point(
          aes(x = values$points$x, y = values$points$y),
          size = adjusted_size,
          color = values$points$border_color,
          alpha = values$points$alpha,
          shape = shape_mapping,
          stroke = adjusted_stroke,
          fill = values$points$color
        )
      }
      return(p)
    }


    draw_lines <- function(p, zoom_factor) {
      if (nrow(values$lines) > 0) {
        values$lines$color <- sapply(values$lines$color, valid_hex)
        values$lines$end_color <- sapply(values$lines$end_color, valid_hex)

        for (i in 1:nrow(values$lines)) {
          line_type <- values$lines$type[i]
          start_color <- values$lines$color[i]
          end_color <- if (length(values$lines$color_type[i]) > 0 && values$lines$color_type[i] == "Gradient") {
            values$lines$end_color[i]
          } else {
            start_color
          }
          gradient_position <- if (!is.null(values$lines$gradient_position[i]) && length(values$lines$gradient_position[i]) > 0) {
            values$lines$gradient_position[i]
          } else {
            1
          }

          if (!is.null(line_type) && length(line_type) > 0) {
            adjusted_line_width <- values$lines$width[i] / zoom_factor
            adjusted_arrow_size <- if (!is.na(values$lines$arrow_size[i])) values$lines$arrow_size[i] / zoom_factor else NA


            if (values$lines$lavaan[i] || line_type == "Straight Line" || line_type == "Straight Arrow" || line_type == "Auto-generated") {
              # Lavaan = straight lines only
              if (!is.null(values$lines$x_start[i]) && !is.null(values$lines$x_end[i])) {

                if (values$lines$color_type[i] == "Gradient") {

                  straight_points <- interpolate_points(
                    x_start = values$lines$x_start[i], y_start = values$lines$y_start[i],
                    x_end = values$lines$x_end[i], y_end = values$lines$y_end[i]
                  )

                  n_points <- nrow(straight_points)
                  split_index <- round(gradient_position * n_points)

                  color_interpolator <- colorRampPalette(c(start_color, end_color))
                  intermediate_color <- color_interpolator(3)[2]

                  gradient_colors_start <- colorRampPalette(c(start_color, intermediate_color))(split_index)
                  gradient_colors_end <- colorRampPalette(c(intermediate_color, end_color))(n_points - split_index + 1)


                  # Draw the line segment by segment with interpolated colors
                  for (j in 1:(split_index - 1)) {
                    p <- p + annotate("segment",
                                      x = straight_points$x[j], y = straight_points$y[j],
                                      xend = straight_points$x[j + 1], yend = straight_points$y[j + 1],
                                      color = gradient_colors_start[j],
                                      size = adjusted_line_width, alpha = values$lines$alpha[i])
                  }

                  # Draw the second segment with the end color gradient
                  for (j in split_index:(n_points - 1)) {
                    p <- p + annotate("segment",
                                      x = straight_points$x[j], y = straight_points$y[j],
                                      xend = straight_points$x[j + 1], yend = straight_points$y[j + 1],
                                      color = gradient_colors_end[j - split_index + 1],
                                      size = adjusted_line_width, alpha = values$lines$alpha[i])
                  }
                } else {
                  # For single-color straight lines, use annotate("segment")
                  p <- p + annotate("segment",
                                    x = values$lines$x_start[i], y = values$lines$y_start[i],
                                    xend = values$lines$x_end[i], yend = values$lines$y_end[i],
                                    color = start_color,
                                    size = adjusted_line_width, alpha = values$lines$alpha[i],
                                    linetype = values$lines$line_style[i])
                }

                # Add arrowhead if necessary
                arrow_type <- values$lines$arrow_type[i]
                if (!is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
                  offset_factor <- 0.01

                  dx <- values$lines$x_end[i] - values$lines$x_start[i]
                  dy <- values$lines$y_end[i] - values$lines$y_start[i]
                  norm <- sqrt(dx^2 + dy^2)


                  x_adjust_start <- values$lines$x_start[i] + offset_factor * dx / norm
                  y_adjust_start <- values$lines$y_start[i] + offset_factor * dy / norm

                  x_adjust_end <- values$lines$x_end[i] - offset_factor * dx / norm
                  y_adjust_end <- values$lines$y_end[i] - offset_factor * dy / norm

                  if (values$lines$two_way[i]){
                    # Two-way arrow logic
                    p <- p + annotate("segment",
                                      x = x_adjust_start, y = y_adjust_start,
                                      xend = values$lines$x_start[i], yend = values$lines$y_start[i],
                                      size = adjusted_line_width, alpha = values$lines$alpha[i],
                                      arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                      color = start_color) +
                      annotate("segment",
                               x = x_adjust_end, y = y_adjust_end,
                               xend = values$lines$x_end[i], yend = values$lines$y_end[i],
                               size = adjusted_line_width, alpha = values$lines$alpha[i],
                               arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                               color = end_color)
                  } else {
                    # One-way arrow logic
                    p <- p + annotate("segment",
                                      x = x_adjust_end, y = y_adjust_end,
                                      xend = values$lines$x_end[i], yend = values$lines$y_end[i],
                                      size = adjusted_line_width, alpha = values$lines$alpha[i],
                                      arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                      color = end_color)  # Use solid end color for arrowhead
                  }
                }
              }
            }

            # For curved lines and arrows
            if (line_type == "Curved Line" || line_type == "Curved Arrow") {
              if (!is.null(values$lines$ctrl_x[i]) && !is.null(values$lines$ctrl_y[i])) {
                # Use create_bezier_curve for curved lines
                bezier_points <- create_bezier_curve(
                  x_start = values$lines$x_start[i], y_start = values$lines$y_start[i],
                  x_end = values$lines$x_end[i], y_end = values$lines$y_end[i],
                  ctrl_x = values$lines$ctrl_x[i], ctrl_y = values$lines$ctrl_y[i]
                )

                if (values$lines$color_type[i] == "Gradient") {
                  # Interpolate gradient colors along the Bezier curve
                  n_points <- nrow(bezier_points)
                  split_index <- round(gradient_position * n_points)
                  color_interpolator <- colorRampPalette(c(start_color, end_color))
                  intermediate_color <- color_interpolator(3)[2]

                  gradient_colors_start <- colorRampPalette(c(start_color, intermediate_color))(split_index)
                  gradient_colors_end <- colorRampPalette(c(intermediate_color, end_color))(n_points - split_index + 1)

                  p <- p + annotate("path",
                                    x = bezier_points$x,
                                    y = bezier_points$y,
                                    color = start_color,
                                    size = adjusted_line_width,
                                    alpha = values$lines$alpha[i],
                                    linetype = values$lines$line_style[i])

                  for (j in 1:(split_index - 1)) {
                    p <- p + annotate("path",
                                      x = bezier_points$x[j:(j + 1)],
                                      y = bezier_points$y[j:(j + 1)],
                                      color = gradient_colors_start[j],
                                      size = adjusted_line_width, alpha = values$lines$alpha[i])
                  }

                  for (j in split_index:(n_points - 1)) {
                    p <- p + annotate("path",
                                      x = bezier_points$x[j:(j + 1)],
                                      y = bezier_points$y[j:(j + 1)],
                                      color = gradient_colors_end[j - split_index + 1],
                                      size = adjusted_line_width, alpha = values$lines$alpha[i])
                  }
                } else {

                  p <- p + annotate("path",
                                    x = bezier_points$x,
                                    y = bezier_points$y,
                                    color = start_color,
                                    size = adjusted_line_width, alpha = values$lines$alpha[i],
                                    linetype = values$lines$line_style[i])
                }

                # Add arrowhead for curved lines if necessary
                arrow_type <- values$lines$arrow_type[i]
                if (line_type == "Curved Arrow" && !is.null(arrow_type) && !is.na(adjusted_arrow_size)) {
                  if (isTRUE(input$two_way_arrow)) {

                    dx_start <- bezier_points$x[2] - bezier_points$x[1]
                    dy_start <- bezier_points$y[2] - bezier_points$y[1]

                    dx_end <- bezier_points$x[nrow(bezier_points)] - bezier_points$x[nrow(bezier_points) - 1]
                    dy_end <- bezier_points$y[nrow(bezier_points)] - bezier_points$y[nrow(bezier_points) - 1]

                    norm_start <- sqrt(dx_start^2 + dy_start^2)
                    norm_end <- sqrt(dx_end^2 + dy_end^2)

                    p <- p + annotate("segment",
                                      x = bezier_points$x[1], y = bezier_points$y[1],
                                      xend = bezier_points$x[1] - dx_start / norm_start * 1e-5,
                                      yend = bezier_points$y[1] - dy_start / norm_start * 1e-5,
                                      size = adjusted_line_width,
                                      arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                      color = start_color) +
                      annotate("segment",
                               x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
                               xend = bezier_points$x[nrow(bezier_points)] + dx_end / norm_end * 1e-5,
                               yend = bezier_points$y[nrow(bezier_points)] + dy_end / norm_end * 1e-5,
                               size = adjusted_line_width,
                               arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                               color = end_color)

                  } else {
                    dx_end <- bezier_points$x[nrow(bezier_points)] - bezier_points$x[nrow(bezier_points) - 1]
                    dy_end <- bezier_points$y[nrow(bezier_points)] - bezier_points$y[nrow(bezier_points) - 1]
                    norm_end <- sqrt(dx_end^2 + dy_end^2)

                    p <- p + annotate("segment",
                                      x = bezier_points$x[nrow(bezier_points)], y = bezier_points$y[nrow(bezier_points)],
                                      xend = bezier_points$x[nrow(bezier_points)] + dx_end / norm_end * 1e-5,
                                      yend = bezier_points$y[nrow(bezier_points)] + dy_end / norm_end * 1e-5,
                                      size = adjusted_line_width,
                                      arrow = arrow(type = arrow_type, length = unit(adjusted_arrow_size, "inches")),
                                      color = end_color)
                  }
                }
              }
            }
          }
        }
      }
      return(p)
    }



    draw_annotations <- function(p, zoom_factor) {
      if (nrow(values$annotations) > 0) {
        values$annotations$color <- sapply(values$annotations$color, valid_hex)
        for (i in 1:nrow(values$annotations)) {
          # mathematical annotations (logical)
          annotation_text <- if (input$math_expression) {
            suppressWarnings(tryCatch(parse(text = values$annotations$text[i]), error = function(e) values$annotations$text[i]))
          } else {
            values$annotations$text[i]
          }

          # Add annotation to the plot
          p <- p + annotate("text",
                            x = values$annotations$x[i],
                            y = values$annotations$y[i],
                            label = annotation_text,
                            size = (values$annotations$size[i] / 3) / zoom_factor,
                            color = values$annotations$color[i],
                            alpha = values$annotations$alpha[i],
                            angle = values$annotations$angle[i],
                            family = values$annotations$font[i],
                            fontface = values$annotations$fontface[i])
        }
      }
      return(p)
    }


    draw_self_loops <- function(p, zoom_factor) {
      if (!is.null(values$loops) && nrow(values$loops) > 0) {
        values$loops$color <- sapply(values$loops$color, valid_hex)
        for (i in 1:nrow(values$loops)) {

          t <- seq(0, 2 * pi, length.out = 100)
          gap_angle <- values$loops$gap_size[i] * pi
          loop_start <- t[t < (2 * pi - gap_angle)]

          x_ellipse <- values$loops$x_center[i] + (values$loops$loop_width[i] / zoom_factor) * values$loops$radius[i] * cos(loop_start)
          y_ellipse <- values$loops$y_center[i] + (values$loops$loop_height[i] / zoom_factor) * values$loops$radius[i] * sin(loop_start)

          theta <- values$loops$orientation[i] * pi / 180
          x_rotated <- cos(theta) * (x_ellipse - values$loops$x_center[i]) - sin(theta) * (y_ellipse - values$loops$y_center[i]) + values$loops$x_center[i]
          y_rotated <- sin(theta) * (x_ellipse - values$loops$x_center[i]) + cos(theta) * (y_ellipse - values$loops$y_center[i]) + values$loops$y_center[i]

          # Add arrowhead
          arrow_type <- if (values$loops$arrow_type[i] == "closed") {
            arrow(type = "closed", length = unit(values$loops$arrow_size[i] / zoom_factor, "inches"))
          } else {
            arrow(type = "open", length = unit(values$loops$arrow_size[i] / zoom_factor, "inches"))
          }

          p <- p + annotate("path",
                            x = x_rotated,
                            y = y_rotated,
                            color = values$loops$color[i],
                            size = values$loops$width[i] / zoom_factor,
                            alpha = values$loops$alpha[i],
                            arrow = arrow_type
          )

          # two-way arrows
          if (isTRUE(values$loops$two_way[i])) {
            x_rotated_rev <- rev(x_rotated)
            y_rotated_rev <- rev(y_rotated)

            # reverse loop
            p <- p + annotate("path",
                              x = x_rotated_rev,
                              y = y_rotated_rev,
                              color = values$loops$color[i],
                              size = values$loops$width[i] / zoom_factor,
                              alpha = values$loops$alpha[i],
                              arrow = arrow_type)
          }
        }
      }
      return(p)
    }



    # Order of the elements based on the user's input
    if (input$layer_order == "points_front") {
      # Draw points, lines, annotations, and self-loop arrows
      p <- draw_annotations(p, zoom_factor)
      p <- draw_self_loops(p, zoom_factor)
      p <- draw_lines(p, zoom_factor)
      p <- draw_points(p, zoom_factor)
    } else if (input$layer_order == "lines_front") {
      # Draw lines, annotations, points, and self-loop arrows
      p <- draw_self_loops(p, zoom_factor)
      p <- draw_points(p, zoom_factor)
      p <- draw_annotations(p, zoom_factor)
      p <- draw_lines(p, zoom_factor)
    } else if (input$layer_order == "annotations_front") {
      # Draw annotations, points, lines, and self-loop arrows
      p <- draw_self_loops(p, zoom_factor)
      p <- draw_lines(p, zoom_factor)
      p <- draw_points(p, zoom_factor)
      p <- draw_annotations(p, zoom_factor)
    } else if (input$layer_order == "loops_front") {
      # Draw self-loop arrows, points, lines, and annotations
      p <- draw_points(p, zoom_factor)
      p <- draw_lines(p, zoom_factor)
      p <- draw_annotations(p, zoom_factor)
      p <- draw_self_loops(p, zoom_factor)
    }

    return(p)
  }

  # Render the plot using ggplot engine

  suppressWarnings({
    output$plot <- renderPlot({
      recreate_plot()  # Render the plot safely
    })
  })


  output$data_table <- renderDT({
    datatable(values$points, selection = 'single',
              options = list(pageLength = 5, autoWidth = TRUE,
                             dom = 'ftip',  # This enables the search bar, pagination, and table info
                             paging = TRUE),  # Pagination enabled
              escape = FALSE, editable = TRUE)
  })

  output$uploaded_data_preview <- DT::renderDataTable({
    req(uploaded_data())
    head(uploaded_data(), 10)
  })

  output$processing_indicator <- renderUI({
    if (is.null(uploaded_data())) {
      "No data uploaded. Using default dataset."
    } else {
      "Processing uploaded dataset..."
    }
  })


  # Render line table with clickable rows
  output$line_table <- renderDT({
    datatable(values$lines,
              selection = 'single',
              options = list(pageLength = 5, autoWidth = TRUE,
                             dom = 'ftip',  # This enables the search bar, pagination, and table info
                             paging = TRUE),
              escape = FALSE, editable = TRUE)
  })


  output$annotation_table <- renderDT({
    datatable(values$annotations, selection = 'single',
              options = list(pageLength = 5, autoWidth = TRUE,
                             dom = 'ftip',  # This enables the search bar, pagination, and table info
                             paging = TRUE),
              escape = FALSE, editable = TRUE)
  })


  output$loop_table <- renderDT({
    datatable(values$loops, selection = 'single',
              options = list(pageLength = 5, autoWidth = TRUE,
                             dom = 'ftip',
                             paging = TRUE),
              escape = FALSE, editable = TRUE)
  })

  safe_as_numeric <- function(x) {
    tryCatch(as.numeric(x), warning = function(w) NA, error = function(e) NA)
  }

  safe_as_logical <- function(x) {
    tryCatch(as.logical(x), warning = function(w) NA, error = function(e) NA)
  }

  safe_as_character <- function(x) {
    tryCatch(as.character(x), error = function(e) NA)
  }

  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    save_state()
    if (info$col %in% c("x", "y", "size", "border_width", "alpha")) {
      values$points[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col %in% c("color", "border_color")) {
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", info$value)) {
        values$points[info$row, info$col] <- as.character(info$value)
      } else {
        #showNotification("Invalid color input. Black as default.", type = "error")
        return() # wrong hex code, default to black
      }
    } else {
      values$points[info$row, info$col] <- info$value  # For non-numeric types
    }

    output$plot <- renderPlot({
      recreate_plot()
    })
  })


  observeEvent(input$line_table_cell_edit, {
    info <- input$line_table_cell_edit
    save_state()
    #new_value <- info$value
    if (info$col %in% c("x_start", "y_start", "x_end", "y_end", "width", "alpha", "ctrl_x", "ctrl_y")) {
      values$lines[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col == "line_style") {
      values$lines[info$row, info$col] <- as.character(info$value)
    } else if (info$col %in% c("color", "end_color")) {
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", info$value)) {
        values$lines[info$row, info$col] <- as.character(info$value)
      } else {
        #showNotification("Invalid color input. Black as default.", type = "error")
        return() # wrong hex code, default to black
      }
    } else {
      values$lines[info$row, info$col] <- info$value
    }
  })


  observeEvent(input$annotation_table_cell_edit, {
    info <- input$annotation_table_cell_edit
    save_state()
    #new_value <- info$value

    if (info$col %in% c("x", "y", "size", "angle", "alpha")) {
      values$annotations[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col %in% c("color")){
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", new_value)) {
        values$annotations[info$row, info$col] <- as.character(info$value)
      } else {
        #showNotification("Invalid color input. Black as default.", type = "error")
        return() # wrong hex code, default to black
      }
    } else {
      values$annotations[info$row, info$col] <- info$value
    }

    # Re-trigger the plot update
    output$plot <- renderPlot({
      recreate_plot()
    })
  })


  observeEvent(input$loop_table_cell_edit, {
    info <- input$loop_table_cell_edit
    save_state()
    #new_value <- info$value

    if (info$col %in% c("x_center", "y_center", "radius", "width", "alpha", "gap_size", "loop_width", "loop_height", "orientation")) {
      values$loops[info$row, info$col] <- as.numeric(info$value)
    } else if (info$col %in% c("color")){
      if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", info$value)) {
        values$loops[info$row, info$col] <- as.character(info$value)
      } else {
        #showNotification("Invalid color input. Black as default.", type = "error")
        return() # wrong hex code, default to black
      }
    } else {
      values$loops[info$row, info$col] <- info$value
    }

    # Re-trigger the plot update
    output$plot <- renderPlot({
      recreate_plot()
    })
  })


  output$download_plot <- downloadHandler(
    filename = function() {
      paste("path_diagram-", Sys.Date(), ".", tolower(input$export_format), sep = "")
    },
    content = function(file) {
      ggsave(
        file,
        plot = recreate_plot(),
        device = switch(
          input$export_format,
          "PNG" = "png",
          "JPEG" = "jpeg",
          "PDF" = cairo_pdf,
          "SVG" = svglite
        ),
        width = 8, height = 6, dpi = 300
      )
    }
  )


  output$download_selected_csv <- downloadHandler(
    filename = function() {
      paste0(tolower(input$csv_type), "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_to_save <- switch(input$csv_type,
                             "Points CSV" = values$points,
                             "Lines CSV" = values$lines,
                             "Annotations CSV" = values$annotations,
                             "Self-loop Arrows CSV" = values$loops
      )

      write.csv(data_to_save, file, row.names = FALSE)
    }
  )


  # message about axis limits
  output$axis_info <- renderText({
    "The axis limits are fixed at -10 to 10 for both x and y axes."
  })

  # Download CSV for points
  output$download_points_csv <- downloadHandler(
    filename = function() {
      paste("points_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$points, file, row.names = FALSE)
    }
  )

  # Download CSV for lines
  output$download_lines_csv <- downloadHandler(
    filename = function() {
      paste("lines_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$lines, file, row.names = FALSE)
    }
  )

  # Download CSV for annotations
  output$download_annotations_csv <- downloadHandler(
    filename = function() {
      paste("annotations_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$annotations, file, row.names = FALSE)
    }
  )

  # Download CSV for self-loop arrows
  output$download_loops_csv <- downloadHandler(
    filename = function() {
      paste("loops_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$loops, file, row.names = FALSE)
    }
  )

  observeEvent(input$points_file, {
    req(input$points_file)
    points_data <- read.csv(input$points_file$datapath)
    values$points <- points_data
  })

  observeEvent(input$lines_file, {
    req(input$lines_file)
    lines_data <- read.csv(input$lines_file$datapath)
    values$lines <- lines_data
  })

  observeEvent(input$annotations_file, {
    req(input$annotations_file)
    annotations_data <- read.csv(input$annotations_file$datapath)
    values$annotations <- annotations_data
  })

  observeEvent(input$self_loop_file, {
    req(input$self_loop_file)
    self_loop_data <- read.csv(input$self_loop_file$datapath)
    values$loops <- self_loop_data
  })
}

shinyApp(ui = ui, server = server)
