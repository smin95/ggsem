#' Convert OpenMx model to lavaan syntax
#'
#' Converts an OpenMx model object to lavaan model syntax string. Handles
#' both single-group and multi-group models, including submodels.
#'
#' @param mxModel An OpenMx model object to convert
#' @param group_prefix Character string prefix for group names in multi-group models
#'   (used internally for recursion)
#'
#' @return Character string containing lavaan model syntax
#' @keywords internal
#' @noRd
openmx_to_lavaan <- function(mxModel, group_prefix = NULL) {
  model_string <- c()

  # If model has submodels, recurse
  if (length(mxModel@submodels) > 0) {
    for (submodel in mxModel@submodels) {
      sub_prefix <- if (is.null(group_prefix)) submodel$name else paste0(group_prefix, ".", submodel$name)
      sub_syntax <- openmx_to_lavaan(submodel, group_prefix = sub_prefix)
      model_string <- c(model_string, paste0("# Submodel: ", sub_prefix), sub_syntax)
    }
    return(paste(model_string, collapse = "\n"))
  }

  # Extract matrices and variables
  A <- mxModel$A$values
  S <- mxModel$S$values
  manifests <- mxModel@manifestVars
  latents <- mxModel@latentVars

  # 1. Latent variable loadings
  for (latent in latents) {
    loading_vars <- c()
    for (manifest in manifests) {
      if (!is.na(A[manifest, latent]) && A[manifest, latent] != 0) {
        loading_vars <- c(loading_vars, manifest)
      }
    }
    if (length(loading_vars) > 0) {
      line <- paste0(latent, " =~ ", paste(loading_vars, collapse = " + "))
      model_string <- c(model_string, line)
    }
  }

  # 2. Residual variances for manifests
  for (manifest in manifests) {
    var_value <- S[manifest, manifest]
    if (!is.na(var_value) && var_value != 0) {
      line <- paste0(manifest, " ~~ ", round(var_value, 3), "*", manifest)
      model_string <- c(model_string, line)
    }
  }

  # 3. Latent variances
  for (latent in latents) {
    var_value <- S[latent, latent]
    if (!is.na(var_value) && var_value != 0) {
      line <- paste0(latent, " ~~ ", round(var_value, 3), "*", latent)
      model_string <- c(model_string, line)
    }
  }


  # Add group label if applicable
  if (!is.null(group_prefix)) {
    model_string <- c(paste0("# Group: ", group_prefix), model_string)
  }

  return(paste(model_string, collapse = "\n"))
}

#' Convert Mplus model to lavaan object
#'
#' Converts an Mplus model object to a fitted lavaan model. Automatically detects
#' model type (CFA, SEM, growth, path analysis) and uses appropriate lavaan function.
#'
#' @param mplus_object An Mplus model object containing model syntax and data
#' @param data Optional data frame to use instead of data from Mplus object
#'
#' @return A fitted lavaan model object
#' @importFrom lavaan cfa sem growth
#' @keywords internal
#' @noRd
convert_mplus_to_lavaan <- function(mplus_object, data = NULL) {
  if (!is.null(mplus_object$MODEL)) {
    mplus_syntax <- mplus_object$MODEL
  } else if (!is.null(mplus_object$model)) {
    mplus_syntax <- mplus_object$model
  } else {
    stop("Could not find model syntax in Mplus object")
  }

  if (is.null(data)) {
    if (!is.null(mplus_object$rdata)) {
      data <- mplus_object$rdata
    } else if (!is.null(mplus_object$data)) {
      data <- mplus_object$data
    } else {
      stop("No data provided and no data found in Mplus object")
    }
  }

  model_syntax <- extract_mplus_syntax(mplus_syntax)

  model_type <- detect_mplus_model_type(mplus_syntax)

  switch(model_type,
         "cfa" = {
           fit_lav <- lavaan::cfa(model_syntax, data = data)
         },
         "growth" = {
           fit_lav <- lavaan::growth(model_syntax, data = data)
         },
         "sem" = {
           fit_lav <- lavaan::sem(model_syntax, data = data)
         },
         "path" = {
           fit_lav <- lavaan::sem(model_syntax, data = data)
         },
         {
           fit_lav <- try_auto_detect_model(model_syntax, data, mplus_syntax)
         }
  )

  return(fit_lav)
}


#' Detect Mplus model type from syntax
#'
#' Analyzes Mplus model syntax to determine the appropriate lavaan model type
#' (CFA, SEM, growth, or path analysis).
#'
#' @param mplus_syntax Character string containing Mplus model syntax
#'
#' @return Character string indicating model type: "cfa", "growth", "sem", or "path"
#' @keywords internal
#' @noRd
detect_mplus_model_type <- function(mplus_syntax) {
  mplus_upper <- toupper(mplus_syntax)

  if (grepl("\\|", mplus_syntax) || grepl("S\\s+BY|I\\s+BY", mplus_upper)) {
    return("growth")
  }

  if (grepl("BY", mplus_upper) && !grepl("ON", mplus_upper) && !grepl("WITH", mplus_upper)) {
    return("cfa")
  }

  if (grepl("BY", mplus_upper) && grepl("ON", mplus_upper)) {
    return("sem")
  }

  if (grepl("ON", mplus_upper) && !grepl("BY", mplus_upper)) {
    return("path")
  }

  # Default to SEM
  return("sem")
}


#' Auto-detect model type by trying multiple lavaan functions
#'
#' Attempts to fit the model using different lavaan functions (cfa, sem, growth)
#' and returns the first successful fit. Used as fallback when model type detection fails.
#'
#' @param model_syntax Character string containing lavaan model syntax
#' @param data Data frame containing the model data
#' @param mplus_syntax Original Mplus syntax (for error messages)
#'
#' @return A fitted lavaan model object
#' @importFrom lavaan cfa sem growth
#' @keywords internal
#' @noRd
try_auto_detect_model <- function(model_syntax, data, mplus_syntax) {
  models <- list(
    cfa = try(lavaan::cfa(model_syntax, data = data), silent = TRUE),
    sem = try(lavaan::sem(model_syntax, data = data), silent = TRUE),
    growth = try(lavaan::growth(model_syntax, data = data), silent = TRUE)
  )

  for (model_name in names(models)) {
    if (!inherits(models[[model_name]], "try-error")) {
      return(models[[model_name]])
    }
  }

  warning("Could not auto-detect model type, using sem() as default")
  return(lavaan::sem(model_syntax, data = data))
}

#' Extract and convert Mplus syntax to lavaan syntax
#'
#' Parses Mplus model syntax and converts it to lavaan-compatible syntax.
#' Handles factor loadings (BY), regressions (ON), covariances (WITH),
#' and intercepts.
#'
#' @param mplus_syntax Character string containing raw Mplus model syntax
#'
#' @return Character string containing converted lavaan model syntax
#' @keywords internal
#' @noRd
extract_mplus_syntax <- function(mplus_syntax) {
  mplus_syntax <- gsub("!.*", "", mplus_syntax)
  mplus_syntax <- gsub("\\s+", " ", mplus_syntax)
  mplus_syntax <- trimws(mplus_syntax)

  lines <- strsplit(mplus_syntax, ";")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  lavaan_lines <- character()

  for (line in lines) {
    line_upper <- toupper(line)

    if (grepl("^(TITLE|DATA|VARIABLE|DEFINE|MONTECARLO|ANALYSIS|OUTPUT|SAVEDATA|PLOT)", line_upper)) {
      next
    }

    if (grepl("^MODEL:$", line_upper)) {
      next
    }

    line_clean <- gsub("\\*\\([^\\)]+\\)", "", line)
    line_clean <- gsub("\\*", "", line_clean)
    line_clean <- gsub("@\\d+", "", line_clean)  #

    if (grepl(" BY ", line_upper)) {
      parts <- strsplit(line_clean, "\\s+[Bb][Yy]\\s+")[[1]]
      if (length(parts) == 2) {
        factor <- trimws(parts[1])
        indicators <- strsplit(trimws(parts[2]), "\\s+")[[1]]
        indicators <- indicators[indicators != ""]
        lavaan_lines <- c(lavaan_lines, paste(factor, "=~", paste(indicators, collapse = " + ")))
      }
      next
    }

    # ON statements (regressions)
    if (grepl(" ON ", line_upper)) {
      parts <- strsplit(line_clean, "\\s+[Oo][Nn]\\s+")[[1]]
      if (length(parts) == 2) {
        outcome <- trimws(parts[1])
        predictors <- strsplit(trimws(parts[2]), "\\s+")[[1]]
        predictors <- predictors[predictors != ""]
        lavaan_lines <- c(lavaan_lines, paste(outcome, "~", paste(predictors, collapse = " + ")))
      }
      next
    }

    # WITH statements (covariances)
    if (grepl(" WITH ", line_upper)) {
      parts <- strsplit(line_clean, "\\s+[Ww][Ii][Tt][Hh]\\s+")[[1]]
      if (length(parts) == 2) {
        var1 <- trimws(parts[1])
        var2 <- trimws(parts[2])
        lavaan_lines <- c(lavaan_lines, paste(var1, "~~", var2))
      }
      next
    }

    # Intercepts
    if (grepl("^\\[.*\\]$", line)) {
      var_name <- gsub("\\[|\\]", "", line)
      var_name <- trimws(var_name)
      lavaan_lines <- c(lavaan_lines, paste(var_name, "~ 1"))
      next
    }

    # Handle means (alternative intercept syntax)
    if (grepl("^\\s*\\w+\\s*\\[", line)) {
      var_name <- gsub("\\[.*", "", line)
      var_name <- trimws(var_name)
      lavaan_lines <- c(lavaan_lines, paste(var_name, "~ 1"))
      next
    }
  }

  unique_lines <- unique(lavaan_lines)
  clean_syntax <- paste(unique_lines, collapse = "\n")
  clean_syntax <- gsub("^MODEL:\\s*", "", clean_syntax)

  return(clean_syntax)
}




#' Extract lavaan syntax from OpenMx model
#'
#' Converts an OpenMx MxRAMModel object to lavaan model syntax by extracting
#' parameters from A (asymmetric), S (symmetric), and M (means) matrices.
#' Handles factor loadings, regressions, covariances, and intercepts.
#'
#' @param mx_model An OpenMx MxRAMModel object
#'
#' @return Character string containing lavaan model syntax
#' @keywords internal
#' @noRd
extract_mx_syntax <- function(mx_model) {
  if (!inherits(mx_model, "MxRAMModel")) {
    stop("Input must be an MxRAMModel object")
  }

  A <- tryCatch(
    mx_model$A$values,  # FINAL estimates
    error = function(e) stop("Could not extract A matrix from MxRAMModel")
  )
  S <- tryCatch(
    mx_model$S$values,  # FINAL estimates
    error = function(e) stop("Could not extract S matrix from MxRAMModel")
  )
  M <- if (!is.null(mx_model$M)) mx_model$M$values else NULL

  latents <- tryCatch(
    mx_model$latentVars,
    error = function(e) character(0)  # Return empty if not found
  )

  manifests <- tryCatch(
    mx_model$manifestVars,
    error = function(e) character(0)  # Return empty if not found
  )

  if (length(manifests) == 0) {
    manifests <- setdiff(rownames(A), latents)
  }

  if (length(manifests) == 0) {
    manifests <- rownames(A)
  }

  if (length(manifests) == 0) stop("No manifest variables found in model")
  if (is.null(dim(A)) || is.null(dim(S))) stop("A or S matrices are not properly formed")

  lines <- character()
  all_vars <- c(manifests, latents)

  # 1. Handle models WITH latent variables (measurement model)
  if (length(latents) > 0) {
    # Factor loadings (latent -> manifest)
    for (latent in latents) {
      indicators <- character()

      for (manifest in manifests) {
        # Check if this path exists and get FINAL value
        value <- A[manifest, latent]
        if (!is.na(value) && value != 0) {
          # Include the FINAL parameter value in syntax
          indicators <- c(indicators, paste0(value, "*", manifest))
        }
      }

      if (length(indicators) > 0) {
        lines <- c(lines, paste(latent, "=~", paste(indicators, collapse = " + ")))
      }
    }

    # Structural paths (latent -> latent)
    for (lhs in latents) {
      predictors <- character()

      for (rhs in latents) {
        if (lhs != rhs) { # Avoid self-regression
          value <- A[lhs, rhs]
          if (!is.na(value) && value != 0) {
            # Include the FINAL parameter value in syntax
            predictors <- c(predictors, paste0(value, "*", rhs))
          }
        }
      }

      if (length(predictors) > 0) {
        lines <- c(lines, paste(lhs, "~", paste(predictors, collapse = " + ")))
      }
    }
  }

  # 2. Handle ALL regression paths (manifest -> manifest, manifest -> latent, latent -> manifest)
  for (lhs in all_vars) {
    predictors <- character()

    for (rhs in all_vars) {
      if (lhs != rhs) { # Avoid self-regression
        value <- A[lhs, rhs]
        if (!is.na(value) && value != 0) {
          # Include the FINAL parameter value in syntax
          predictors <- c(predictors, paste0(value, "*", rhs))
        }
      }
    }

    if (length(predictors) > 0) {
      lines <- c(lines, paste(lhs, "~", paste(predictors, collapse = " + ")))
    }
  }

  # 3. Variances and covariances (for ALL variables)
  for (i in 1:length(all_vars)) {
    for (j in i:length(all_vars)) {  # Include diagonal (i <= j)
      var1 <- all_vars[i]
      var2 <- all_vars[j]

      # Get FINAL covariance value
      value <- S[var1, var2]

      if (!is.na(value) && value != 0) {
        if (i == j) {
          # Variance
          lines <- c(lines, paste(var1, "~~", paste0(value, "*", var1)))
        } else {
          # Covariance
          lines <- c(lines, paste(var1, "~~", paste0(value, "*", var2)))
        }
      }
    }
  }

  # 4. Means/intercepts
  if (!is.null(M)) {
    for (var in all_vars) {
      value <- M[1, var]
      if (!is.na(value) && value != 0) {
        # Include the FINAL intercept value in syntax
        lines <- c(lines, paste(var, "~", paste0(value, "*1")))
      }
    }
  }

  # Remove any duplicates and return
  unique_lines <- unique(lines)

  # If no lines were generated (empty model), return empty string
  if (length(unique_lines) == 0) {
    return("")
  }

  return(paste(unique_lines, collapse = "\n"))
}

#' Detect model type from OpenMx model
#'
#' Analyzes the structure of an OpenMx MxRAMModel to determine the appropriate
#' model type (CFA, SEM, growth, path analysis, EFA, etc.) based on matrix
#' patterns and variable relationships.
#'
#' @param mx_model An OpenMx MxRAMModel object
#'
#' @return Character string indicating model type: "cfa", "efa", "esem",
#'   "growth", "sem", "path", "multigroup", or "correlation"
#' @keywords internal
#' @noRd
detect_mx_model_type <- function(mx_model) {
  if (!inherits(mx_model, "MxRAMModel")) {
    stop("Input must be an MxRAMModel object")
  }

  A <- mx_model$A$values
  S <- mx_model$S$values
  latents <- mx_model$latentVars
  manifests <- mx_model$manifestVars

  # Basic counts
  n_manifests <- length(manifests)
  n_latents <- length(latents)

  # Path counts
  measurement_paths <- sum(A[manifests, latents] != 0 | !is.na(mx_model$A$free[manifests, latents]))
  structural_paths <- sum(A[latents, latents] != 0 | !is.na(mx_model$A$free[latents, latents]))
  manifest_regressions <- sum(A[manifests, manifests] != 0 | !is.na(mx_model$A$free[manifests, manifests]))

  # Check model characteristics
  has_means <- !is.null(mx_model$M)
  is_multigroup <- !is.null(mx_model$groups) && length(mx_model$groups) > 1

  # Growth model detection
  growth_indicators <- c("intercept", "slope", "linear", "quadratic", "growth")
  is_growth_latents <- any(grepl(paste(growth_indicators, collapse = "|"), latents, ignore.case = TRUE))

  time_indicators <- c("time", "age", "wave", "t[0-9]", "occasion", "session", "visit")
  has_time_vars <- any(grepl(paste(time_indicators, collapse = "|"),
                             c(latents, manifests), ignore.case = TRUE))

  # EFA vs CFA detection
  cross_loadings <- 0
  if (n_latents > 1 && n_manifests > 0) {
    loading_matrix <- A[manifests, latents] != 0 | !is.na(mx_model$A$free[manifests, latents])
    cross_loadings <- sum(rowSums(loading_matrix) > 1)
    cross_loading_ratio <- cross_loadings / n_manifests
  } else {
    cross_loading_ratio <- 0
  }

  # Decision tree
  if (n_latents == 0) {
    if (manifest_regressions > 0) return("path")
    else return("correlation")
  }
  else if (is_multigroup) {
    return("multigroup")
  }
  else if ((is_growth_latents || has_time_vars) && has_means) {
    return("growth")
  }
  else if (measurement_paths > 0 && structural_paths == 0 && manifest_regressions == 0) {
    if (cross_loading_ratio > 0.4) return("efa")
    else if (cross_loading_ratio > 0.1) return("esem")  # Exploratory SEM
    else return("cfa")
  }
  else if (measurement_paths > 0 && (structural_paths > 0 || manifest_regressions > 0)) {
    return("sem")
  }
  else if (structural_paths > measurement_paths) {
    return("path")
  }
  else {
    return("sem")  # Default
  }
}

#' Convert OpenMx model to fitted lavaan object
#'
#' Converts an OpenMx MxRAMModel to a fitted lavaan model by extracting the
#' syntax, detecting the model type, and using the appropriate lavaan fitting
#' function with provided data.
#'
#' @param mx_model An OpenMx MxRAMModel object
#' @param data Data frame containing the observed variables
#'
#' @return A fitted lavaan model object
#' @keywords internal
#' @noRd
convert_mx_to_lavaan <- function(mx_model, original_data = NULL) {
  if (!inherits(mx_model, "MxRAMModel")) {
    stop("Input must be an MxRAMModel object")
  }

  model_syntax <- extract_mx_syntax(mx_model)
  model_type <- detect_mx_model_type(mx_model)

  # Get data information from the OpenMx model
  mx_data <- mx_model$data

  if (is.null(mx_data)) {
    stop("No data found in the OpenMx model")
  }

  # Determine data type and extract appropriate data
  if (mx_data$type == "cov" || mx_data$type == "cor") {
    # Handle covariance/correlation matrix
    cov_matrix <- as.matrix(mx_data$observed)
    n_obs <- mx_data$numObs

    if (is.null(n_obs) || is.na(n_obs)) {
      # Try to extract N from the data if available
      n_obs <- tryCatch({
        if (!is.null(original_data) && is.data.frame(original_data)) {
          nrow(original_data)
        } else if (!is.null(mx_data$numObs)) {
          mx_data$numObs
        } else {
          NA
        }
      }, error = function(e) NA)
    }

    if (is.na(n_obs)) {
      warning("Number of observations not found, using N = 1000")
      n_obs <- 1000
    }

    # For covariance/correlation data
    switch(model_type,
           "cfa" = {
             fit_lav <- cfa(model_syntax, sample.cov = cov_matrix,
                            sample.nobs = n_obs)
           },
           "efa" = {
             fit_lav <- cfa(model_syntax, sample.cov = cov_matrix,
                            sample.nobs = n_obs, estimator = "ML")
           },
           "sem" = {
             fit_lav <- sem(model_syntax, sample.cov = cov_matrix,
                            sample.nobs = n_obs)
           },
           "path" = {
             fit_lav <- sem(model_syntax, sample.cov = cov_matrix,
                            sample.nobs = n_obs)
           },
           {
             # Default case for covariance data
             fit_lav <- sem(model_syntax, sample.cov = cov_matrix,
                            sample.nobs = n_obs)
           }
    )

  } else if (mx_data$type == "raw") {
    # Handle raw data
    raw_data <- as.data.frame(mx_data$observed)

    switch(model_type,
           "cfa" = {
             fit_lav <- cfa(model_syntax, data = raw_data)
           },
           "efa" = {
             fit_lav <- cfa(model_syntax, data = raw_data, estimator = "ML")
           },
           "growth" = {
             fit_lav <- growth(model_syntax, data = raw_data)
           },
           "sem" = {
             fit_lav <- sem(model_syntax, data = raw_data)
           },
           "path" = {
             fit_lav <- sem(model_syntax, data = raw_data)
           },
           {
             # Default case for raw data
             fit_lav <- sem(model_syntax, data = raw_data)
           }
    )
  } else {
    stop(sprintf("Unsupported data type: %s", mx_data$type))
  }

  return(fit_lav)
}

#' Convert OpenMx model to lavaan with fixed parameter estimates
#'
#' Internal function to convert an OpenMx RAM model to a lavaan model,
#' preserving parameter estimates as fixed values or starting values.
#' Automatically handles different data types (raw, covariance, correlation).
#'
#' @param mx_model An OpenMx MxRAMModel object to convert
#' @param data Optional data input. Can be: raw data frame, covariance matrix,
#'   correlation matrix, or the full OpenMx data object (mx_model$data).
#'   If NULL, data is extracted from mx_model$data$observed.
#'
#' @return A fitted lavaan model with parameters set to OpenMx estimates.
#'   Fixed parameters in OpenMx remain fixed in lavaan; free parameters
#'   use OpenMx estimates as starting values.
#'
#' @keywords internal
#' @noRd
convert_openmx_to_lavaan <- function(mx_model, data = NULL) {
  # Extract parameter estimates from OpenMx
  params <- get_openmx_parameters(mx_model)

  # Determine data type and source
  mx_data_info <- mx_model$data

  # Check if user passed the full OpenMx data object instead of just the data
  if (!is.null(data) && !is.null(data$observed)) {
    observed_data <- data$observed
    if (!is.null(data$type)) {
      data_type <- data$type
    } else {
      # Try to determine type
      if (is.matrix(observed_data) && nrow(observed_data) == ncol(observed_data)) {
        data_type <- "cov"
      } else if (is.data.frame(observed_data)) {
        data_type <- "raw"
      } else {
        stop("Unrecognized data format in data$observed")
      }
    }
  } else if (!is.null(data)) {
    # Data provided by user - determine type
    observed_data <- data
    if (is.matrix(data) && nrow(data) == ncol(data)) {
      data_type <- "cov"
    } else if (is.data.frame(data)) {
      data_type <- "raw"
    } else {
      stop("Unrecognized data format")
    }
  } else if (!is.null(mx_data_info) && !is.null(mx_data_info$observed)) {
    # Use data from OpenMx model
    observed_data <- mx_data_info$observed
    data_type <- mx_data_info$type  # "raw", "cov", or "cor"
  } else {
    stop("No data provided and cannot extract data from OpenMx object")
  }

  # Extract model syntax
  model_syntax <- extract_mx_syntax(mx_model)

  if (model_syntax == "" || nchar(model_syntax) == 0) {
    warning("Empty model syntax generated. Creating intercept-only model.")

    if (data_type == "raw") {
      var_names <- names(observed_data)
    } else {
      var_names <- colnames(observed_data)
    }

    if (is.null(var_names) || length(var_names) == 0) {
      stop("Cannot determine variable names from data")
    }

    intercept_lines <- paste(var_names, "~", "1")
    model_syntax <- paste(intercept_lines, collapse = "\n")

    params <- data.frame(
      lhs = var_names,
      op = "~1",
      rhs = "",
      est = rep(0, length(var_names)),
      se = rep(NA, length(var_names)),
      free = rep(1, length(var_names)),
      stringsAsFactors = FALSE
    )
  }

  lav_model <- create_lavaan_with_fixed_params(model_syntax, params, observed_data, data_type)

  return(lav_model)
}

#' Create lavaan model with parameters fixed to OpenMx estimates
#'
#' Internal function that takes OpenMx parameter estimates and applies them
#' as fixed values or starting values in a lavaan model. Handles covariance,
#' correlation, and raw data types.
#'
#' @param model_syntax Character string of lavaan model syntax
#' @param params Data frame of parameter estimates from OpenMx with columns:
#'   lhs, op, rhs, est, free (logical)
#' @param data Data for model fitting: data frame (raw), covariance matrix,
#'   or correlation matrix
#' @param data_type Optional: "raw", "cov", or "cor". Auto-detected if NULL
#'
#' @return A fitted lavaan model with parameters constrained to OpenMx estimates
#' @importFrom lavaan sem parTable lavaan
#' @keywords internal
#' @noRd
#'
#' @importFrom lavaan sem parTable lavaan
create_lavaan_with_fixed_params <- function(model_syntax, params, data, data_type = NULL) {
  # Auto-detect data type if not provided
  if (is.null(data_type)) {
    if (is.matrix(data) && nrow(data) == ncol(data)) {
      data_type <- "cov"
    } else {
      data_type <- "raw"
    }
  }

  # Parse the model to get parameter table
  if (data_type == "cov") {
    # For covariance data
    n_obs <- attr(data, "n_obs")
    if (is.null(n_obs)) {
      warning("Number of observations not specified for covariance matrix. Using N=1000.")
      n_obs <- 1000
    }

    # Initial lavaan model without fitting
    lav_model <- lavaan::sem(model_syntax,
                             sample.cov = data,
                             sample.nobs = n_obs,
                             do.fit = FALSE)

  } else if (data_type == "cor") {
    # For correlation data
    n_obs <- attr(data, "n_obs")
    if (is.null(n_obs)) {
      warning("Number of observations not specified for correlation matrix. Using N=1000.")
      n_obs <- 1000
    }

    lav_model <- lavaan::sem(model_syntax,
                             sample.cov = data,
                             sample.nobs = n_obs,
                             do.fit = FALSE)

  } else {
    # For raw data
    lav_model <- lavaan::sem(model_syntax,
                             data = data,
                             do.fit = FALSE)
  }

  # Get parameter table
  par_table <- lavaan::parTable(lav_model)

  # If params is a data frame (from get_openmx_parameters), process it
  if (is.data.frame(params) && nrow(params) > 0) {
    # Match parameters and set estimates and constraints
    for (i in 1:nrow(par_table)) {
      # Find matching parameter
      match_idx <- which(
        params$lhs == par_table$lhs[i] &
          params$op == par_table$op[i] &
          params$rhs == par_table$rhs[i]
      )

      if (length(match_idx) > 0) {
        # Set starting value to the OpenMx estimate
        par_table$ustart[i] <- params$est[match_idx[1]]

        # If parameter was fixed in OpenMx, fix it in lavaan
        if (!params$free[match_idx[1]]) {
          par_table$free[i] <- 0
        }
      }
    }
  }

  # Refit the model with updated parameter table
  if (data_type == "cov") {
    fit <- lavaan::lavaan(par_table,
                          sample.cov = data,
                          sample.nobs = n_obs,
                          fixed.x = FALSE,
                          estimator = "ML",
                          warn = FALSE)
  } else if (data_type == "cor") {
    fit <- lavaan::lavaan(par_table,
                          sample.cov = data,
                          sample.nobs = n_obs,
                          fixed.x = FALSE,
                          estimator = "ML",
                          warn = FALSE)
  } else {
    fit <- lavaan::lavaan(par_table,
                          data = data,
                          fixed.x = FALSE,
                          estimator = "ML",
                          warn = FALSE)
  }

  return(fit)
}


#' Extract lavaan syntax from fitted model
#'
#' Extracts the original lavaan model syntax from a fitted lavaan object.
#' Attempts to retrieve the syntax from the model call first, then falls back
#' to reconstruction from the parameter table if needed.
#'
#' @param fit A fitted lavaan object
#'
#' @return Character string containing the original lavaan model syntax
#' @keywords internal
#' @noRd
fit_to_lavstring <- function(fit) {
  if (!inherits(fit, 'lavaan')) {
    stop("Input must be a lavaan object (class 'lavaan').")
  }

  # Try to extract from call first (preserves original formatting)
  model_string <- tryCatch({
    if (is.name(fit@call$model)) {
      # Case 1: Model stored in a variable
      eval(fit@call$model, envir = environment(fit@call))
    } else if (is.character(fit@call$model)) {
      # Case 2: Model passed directly as string
      as.character(fit@call$model)
    } else {
      # Case 3: Fall back to reconstruction
      stop("Cannot extract from call")
    }
  }, error = function(e) {
    # Fallback: reconstruct from parameter table (works with tidySEM)
    reconstruct_from_parTable(fit)
  })

  return(model_string)
}

#' Reconstruct lavaan syntax from parameter table
#'
#' Fallback method to reconstruct lavaan syntax from a fitted model's parameter
#' table when the original syntax cannot be extracted from the call. Handles
#' factor loadings, regressions, covariances, intercepts, and variances.
#'
#' @param fit A fitted lavaan object
#'
#' @return Character string containing reconstructed lavaan syntax
#' @keywords internal
#' @importFrom lavaan parTable
#' @noRd
reconstruct_from_parTable <- function(fit) {
  param_table <- lavaan::parTable(fit)
  lines <- character()

  # Get all observed variables to check for duplicates
  all_observed <- unique(c(
    param_table$rhs[param_table$op == "=~"],  # Indicators in measurement model
    param_table$lhs[param_table$op == "~"],   # Dependent variables in regressions
    param_table$rhs[param_table$op == "~"],   # Independent variables in regressions
    param_table$lhs[param_table$op == "~~" & param_table$lhs == param_table$rhs],  # Variances
    param_table$lhs[param_table$op == "~1"]   # Variables with intercepts
  ))

  # Remove empty strings and latent variables (those with =~ operator)
  latent_vars <- unique(param_table$lhs[param_table$op == "=~"])
  all_observed <- setdiff(all_observed, c("", latent_vars))

  # Check for duplicates
  if (any(duplicated(all_observed))) {
    warning("Duplicate observed variables found: ",
            paste(unique(all_observed[duplicated(all_observed)]), collapse = ", "))
  }

  # 1. Measurement model (==)
  loadings <- param_table[param_table$op == "=~", ]
  if (nrow(loadings) > 0) {
    factors <- unique(loadings$lhs)
    for (factor in factors) {
      indicators <- loadings$rhs[loadings$lhs == factor]
      indicators <- unique(indicators)
      lines <- c(lines, paste(factor, "=~", paste(indicators, collapse = " + ")))
    }
  }

  # 2. Structural model (~)
  regressions <- param_table[param_table$op == "~", ]
  if (nrow(regressions) > 0) {
    regressions <- regressions[!duplicated(regressions[, c("lhs", "rhs")]), ]
    for (i in 1:nrow(regressions)) {
      lines <- c(lines, paste(regressions$lhs[i], "~", regressions$rhs[i]))
    }
  }

  # 3. Covariances (~~) - with duplicate removal
  covariances <- param_table[param_table$op == "~~" & param_table$lhs != param_table$rhs, ]
  if (nrow(covariances) > 0) {
    unique_covs <- character()
    for (i in 1:nrow(covariances)) {
      pair <- sort(c(covariances$lhs[i], covariances$rhs[i]))
      pair_str <- paste(pair[1], "~~", pair[2])
      if (!pair_str %in% unique_covs) {
        unique_covs <- c(unique_covs, pair_str)
      }
    }
    lines <- c(lines, unique_covs)
  }

  # 4. Intercepts (~1)
  intercepts <- param_table[param_table$op == "~1" & !param_table$lhs %in% c("", ".p."), ]
  if (nrow(intercepts) > 0) {
    intercepts <- intercepts[!duplicated(intercepts$lhs), ]
    for (i in 1:nrow(intercepts)) {
      lines <- c(lines, paste(intercepts$lhs[i], "~ 1"))
    }
  }

  # 5. Variances (~~ with same variable) - only if user-specified or fixed
  variances <- param_table[param_table$op == "~~" & param_table$lhs == param_table$rhs, ]
  if (nrow(variances) > 0) {
    variances <- variances[!duplicated(variances$lhs), ]
    for (i in 1:nrow(variances)) {
      lines <- c(lines, paste(variances$lhs[i], "~~", variances$rhs[i]))
    }
  }

  # Remove any duplicate lines that might have been created
  lines <- unique(lines)

  return(paste(lines, collapse = "\n"))
}

#' Extract lavaan syntax from blavaan model
#'
#' Extracts lavaan syntax from a fitted blavaan (Bayesian lavaan) object
#' by reconstructing from the parameter table.
#'
#' @param fit A fitted blavaan object
#'
#' @return Character string containing the lavaan model syntax
#' @keywords internal
#' @noRd
blavaan_to_lavstring <- function(fit) {
  if (!inherits(fit, 'blavaan')) {
    stop("Input must be a blavaan object")
  }

  return(reconstruct_from_parTable(fit))
}


#' Convert blavaan model to semPaths with enhanced Bayesian options
#'
#'
#' @param fit Fitted blavaan model object or list of two blavaan models for comparison
#' @param data_file Optional data frame used for fitting the model
#' @param layout_algorithm Layout algorithm for semPaths (default: 'tree2')
#' @param intercepts Logical, whether to include intercepts in the visualization (default: TRUE)
#' @param annotate_estimates Logical, whether to annotate edges with parameter
#'   estimates (default: TRUE)
#' @param standardized Logical, whether to show standardized estimates (default: FALSE)
#' @param unstandardized Logical, whether to show unstandardized estimates (default: TRUE)
#' @param p_val Logical, whether to add asterisks for significant parameters based on
#'   HPD intervals (default: FALSE)
#' @param conf_int Logical, whether to show HPD credible intervals (default: FALSE)
#' @param multi_group Logical, whether the model is multi-group (default: FALSE)
#' @param group_var Character, name of grouping variable for multi-group models
#' @param group_level Character, specific group level to visualize in multi-group models
#' @param combine Logical, whether to combine two SEMs for comparison (default: FALSE)
#' @param group1 Character, group level of group1 for comparison
#' @param group2 Character, group level of group2 for comparison
#' @param sep_by Character, separator for combined estimates (default: " | ")
#' @param residuals Logical, show residuals or not
#'
#' @return A semPaths qgraph object for visualization with Bayesian parameter estimates
#'
#'
#' @importFrom blavaan blavInspect
#' @importFrom lavaan parameterEstimates standardizedSolution
#' @importFrom semPlot semPaths
#' @importFrom dplyr select mutate across filter left_join
#' @importFrom rlang .data
#' @importFrom methods is
#'
#' @keywords internal
#' @noRd
blavaan_to_sempaths <- function(fit, data_file = NULL, layout_algorithm = 'tree2',
                                intercepts = TRUE,
                                annotate_estimates = TRUE,
                                standardized = FALSE,
                                unstandardized = TRUE,
                                p_val = FALSE,
                                conf_int = FALSE,
                                multi_group = FALSE,
                                group_var = NULL,
                                group_level = NULL,
                                combine = FALSE,
                                group1 = NULL,
                                group2 = NULL,
                                sep_by = " | ",
                                residuals = FALSE) {

  if (is.list(fit)) {
    combine <- FALSE
    fake_combine <- TRUE
  } else {
    fake_combine <- FALSE
  }

  if (is.null(group1) || is.null(group2)) combine <- FALSE

  if (combine) {
    params <- get_comparison_table_bayes(fit = fit, rope = c(-0.1,0.1), group1 = group1, group2 = group2, sep_by = sep_by)

    group_info <- blavaan::blavInspect(fit, "group.label")

    edge_params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]
    self_loop_indices <- which(edge_params$lhs == edge_params$rhs)

    if (!residuals) {
      if (length(self_loop_indices) > 0) {
        params1 <- edge_params[-self_loop_indices, ]
      } else {
        params1 <- edge_params
      }
    } else {
      params1 <- edge_params
    }

    if (!intercepts) {
      params1 <- params1[params1$op != "~1", ]
    }

    unstd <- params1$est  # Unstandardized comparison
    std <- params1$std   # Standardized comparison

    significant <- params1$significant
    significant[is.na(significant)] <- FALSE

    if (p_val == TRUE) {
      std[significant] <- paste0(std[significant], "*")
      unstd[significant] <- paste0(unstd[significant], "*")
    }

    ci_labels <- if (conf_int) {
      ci_col <- NULL
      if ("confint_unstd" %in% names(params1)) {
        ci_col <- params1$confint_unstd
      } else if ("credible_interval" %in% names(params1)) {
        ci_col <- params1$credible_interval
      } else if ("confint_combined" %in% names(params1)) {
        ci_col <- params1$confint_combined
      }
      if (!is.null(ci_col)) {
        ifelse(is.na(ci_col) | grepl("NA", ci_col), "", paste0("\n", ci_col))
      } else {
        ""
      }
    } else {
      ""
    }

    if (standardized == TRUE && unstandardized == TRUE) {
      base_labels <- paste0(unstd, " (", std, ")")
    } else if (standardized == TRUE && unstandardized == FALSE) {
      base_labels <- std
    } else if (standardized == FALSE && unstandardized == TRUE) {
      base_labels <- unstd
    } else {
      base_labels <- NULL
    }

    edgeLabels <- if (conf_int && !is.null(base_labels)) {
      paste0(base_labels, ci_labels)
    } else {
      base_labels
    }

    sem_paths0 <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                    whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                    edge.color = ifelse(significant, "#000000", "#BEBEBE"))

    if (inherits(sem_paths0, 'list')) {
      sem_paths <- sem_paths0[[1]]
    } else if (inherits(sem_paths0, 'qgraph')) {
      sem_paths <- sem_paths0
    }

  } else if (fake_combine) {
    params <- combine_model_parameters_bayes(fit1 = fit[[1]], fit2 = fit[[2]], group1 = group1, group2 = group2, sep_by = sep_by)

    ref <- lavaan::parameterEstimates(fit[[1]])
    ref <- ref |> mutate(across(c(lhs, op, rhs), as.character))

    params <- ref |>
      select(lhs, op, rhs) |>
      left_join(params, by = c("lhs", "op", "rhs"))

    edge_params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]
    self_loop_indices <- which(edge_params$lhs == edge_params$rhs)

    if (!residuals) {
      if (length(self_loop_indices) > 0) {
        params1 <- edge_params[-self_loop_indices, ]
      } else {
        params1 <- edge_params
      }
    } else {
      params1 <- edge_params
    }

    # Create fake significance column for compatibility
    params1$significant <- FALSE

    if (!intercepts) {
      params1 <- params1[params1$op != "~1", ]
    }

    unstd <- params1$est  # Unstandardized comparison
    std <- params1$std   # Standardized comparison

    significant <- params1$significant
    significant[is.na(significant)] <- FALSE

    if (p_val == TRUE) {
      std[significant] <- paste0(std[significant], "*")
      unstd[significant] <- paste0(unstd[significant], "*")
    }

    ci_labels <- if (conf_int) {
      ci_col <- NULL
      if ("confint_unstd" %in% names(params1)) {
        ci_col <- params1$confint_unstd
      } else if ("credible_interval" %in% names(params1)) {
        ci_col <- params1$credible_interval
      } else if ("confint_combined" %in% names(params1)) {
        ci_col <- params1$confint_combined
      }
      if (!is.null(ci_col)) {
        ifelse(is.na(ci_col) | grepl("NA", ci_col), "", paste0("\n", ci_col))
      } else {
        ""
      }
    } else {
      ""
    }

    if (standardized == TRUE && unstandardized == TRUE) {
      base_labels <- paste0(unstd, " (", std, ")")
    } else if (standardized == TRUE && unstandardized == FALSE) {
      base_labels <- std
    } else if (standardized == FALSE && unstandardized == TRUE) {
      base_labels <- unstd
    } else {
      base_labels <- NULL
    }

    edgeLabels <- if (conf_int && !is.null(base_labels)) {
      paste0(base_labels, ci_labels)
    } else {
      base_labels
    }

    sem_paths <- semPlot::semPaths(fit[[1]], layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                   whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                   edge.color = ifelse(significant, "#000000", "#BEBEBE"))

  } else {
    params <- lavaan::parameterEstimates(fit)
    edge_params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]

    hpd_intervals <- as.data.frame(blavaan::blavInspect(fit, "hpd"))
    hpd_names <- rownames(hpd_intervals)

    self_loop_indices <- which(edge_params$lhs == edge_params$rhs)

    if (multi_group == TRUE && !is.null(group_level)) {
      group_info <- blavaan::blavInspect(fit, "group.label")

      if (!is.null(group_level)) {
        group_number <- which(group_info == group_level)
        if (length(group_number) == 0) {
          stop("Group level '", group_level, "' not found. Available groups: ",
               paste(group_info, collapse = ", "))
        }
      } else {
        group_number <- 1
        group_level <- group_info[1]
      }

      is_blavaan_format <- any(grepl("^\\.p[0-9]+\\.$", hpd_names)) ||
        any(grepl("\\.\\.", hpd_names))

      if (!residuals) {
        if (length(self_loop_indices) > 0) {
          params1 <- edge_params[-self_loop_indices, ]
          std_est1 <- standardizedSolution(fit)[-self_loop_indices, ]
        } else {
          params1 <- edge_params
          std_est1 <- standardizedSolution(fit)
        }
      } else {
        params1 <- edge_params
        std_est1 <- standardizedSolution(fit)
      }

      params1 <- params1[params1$group == group_number, ]
      std_est1 <- std_est1[std_est1$group == group_number, ]

      if (is_blavaan_format) {
        if (group_number == 1) {
          # Group 1: parameters without .gX suffix or ..1 suffix
          group_hpd_indices <- which(!grepl("\\.g[0-9]+$", hpd_names) &
                                       !grepl("\\.\\.1$", hpd_names) &
                                       !grepl("\\.1\\.g[0-9]+$", hpd_names))
        } else {
          # Group 2+: parameters with .gX or ..1 suffix
          group_hpd_pattern <- paste0("(\\.g", group_number, "$|\\.\\.", group_number, "$)")
          group_hpd_indices <- grep(group_hpd_pattern, hpd_names)
        }

        if (length(group_hpd_indices) > 0) {
          hpd_intervals <- hpd_intervals[group_hpd_indices, ]
          hpd_names <- rownames(hpd_intervals)
        }
      }

    } else {
      # Single group case
      group_number <- 1

      if (!residuals) {
        if (length(self_loop_indices) > 0) {
          params1 <- edge_params[-self_loop_indices, ]
          std_est1 <- standardizedSolution(fit)[-self_loop_indices, ]
        } else {
          params1 <- edge_params
          std_est1 <- standardizedSolution(fit)
        }
      } else {
        params1 <- edge_params
        std_est1 <- standardizedSolution(fit)
      }
    }

    if (!intercepts) {
      params1 <- params1[params1$op != "~1", ]
      std_est1 <- std_est1[std_est1$op != "~1", ]
    }

    unstd <- round(params1$est, 2)  # Unstandardized
    std <- round(std_est1$est.std, 2)   # Standardized

    significant <- rep(FALSE, nrow(params1))
    ci_labels <- character(nrow(params1))

    for(i in 1:nrow(params1)) {
      hpd_name <- NULL
      if (multi_group == TRUE &&!is.null(group_level)) {
        if (!is.na(params1$label[i]) && params1$label[i] != "") {
          base_hpd_name <- params1$label[i]

          if ((multi_group == TRUE && !is.null(group_level)) &&
              any(grepl("^\\.p[0-9]+\\.$", hpd_names))) {

            if (group_number == 1) {
              hpd_name <- base_hpd_name
            } else {
              possible_names <- c(
                paste0(base_hpd_name, "..", group_number),
                paste0(base_hpd_name, ".g", group_number),
                base_hpd_name
              )

              for (possible_name in possible_names) {
                if (possible_name %in% hpd_names) {
                  hpd_name <- possible_name
                  break
                }
              }
            }
          } else {
            hpd_name <- base_hpd_name
          }

        } else if (params1$op[i] == "~~") {
          lhs <- params1$lhs[i]
          rhs <- params1$rhs[i]

          if (any(grepl("\\.\\.", hpd_names))) {
            base_hpd_name <- paste0(lhs, "..", rhs)

            if ((multi_group == TRUE && !is.null(group_level)) && group_number > 1) {
              # Try with .g2 suffix first
              possible_names <- c(
                paste0(base_hpd_name, ".g", group_number),
                base_hpd_name  # Also try without suffix
              )

              for (possible_name in possible_names) {
                if (possible_name %in% hpd_names) {
                  hpd_name <- possible_name
                  break
                }
              }
            } else {
              hpd_name <- base_hpd_name
            }
          } else {
            # Human-readable format: x1~~x1
            hpd_name <- paste0(lhs, "~~", rhs)
          }

        } else if (params1$op[i] == "~1") {
          if ((multi_group == TRUE && !is.null(group_level)) &&
              group_number > 1 &&
              params1$lhs[i] %in% c("visual", "textual", "speed")) {
            hpd_name <- paste0(params1$lhs[i], ".1.g", group_number)
          } else if (!is.na(params1$label[i]) && params1$label[i] != "") {
            base_hpd_name <- params1$label[i]

            if ((multi_group == TRUE && !is.null(group_level)) && group_number > 1) {
              possible_names <- c(
                paste0(base_hpd_name, "..", group_number),
                paste0(base_hpd_name, ".g", group_number),
                base_hpd_name
              )

              for (possible_name in possible_names) {
                if (possible_name %in% hpd_names) {
                  hpd_name <- possible_name
                  break
                }
              }
            } else {
              hpd_name <- base_hpd_name
            }
          }
        }
      }


      if (is.null(hpd_name) || !(hpd_name %in% hpd_names)) {
        human_readable_name <- paste0(params1$lhs[i], params1$op[i],
                                      ifelse(params1$op[i] == "~1", "", params1$rhs[i]))
        if (human_readable_name %in% hpd_names) {
          hpd_name <- human_readable_name
        }
      }

      if (!is.null(hpd_name) && hpd_name %in% hpd_names) {
        match_idx <- which(hpd_names == hpd_name)
        significant[i] <- hpd_intervals$lower[match_idx] > 0 | hpd_intervals$upper[match_idx] < 0
        ci_labels[i] <- paste0("[",
                               round(hpd_intervals$lower[match_idx], 2), ", ",
                               round(hpd_intervals$upper[match_idx], 2), "]")
      }
    }

    if (p_val == TRUE) {
      std[significant] <- paste0(std[significant], "*")
      unstd[significant] <- paste0(unstd[significant], "*")
    }

    if (standardized == TRUE && unstandardized == TRUE) {
      labels <- paste0(unstd, " (", std, ")")
    } else if (standardized == TRUE && unstandardized == FALSE) {
      labels <- std
    } else if (standardized == FALSE && unstandardized == TRUE) {
      labels <- unstd
    } else {
      labels <- NULL
    }

    edgeLabels <- switch(
      paste(standardized, unstandardized, conf_int, sep = "-"),
      "FALSE-TRUE-FALSE"  = unstd,
      "TRUE-FALSE-FALSE"  = std,
      "TRUE-TRUE-FALSE"   = labels,
      "FALSE-FALSE-FALSE" = labels,
      "FALSE-TRUE-TRUE"  = paste0(unstd, "\n", ci_labels),
      "TRUE-FALSE-TRUE"  = paste0(std, "\n",  ci_labels),
      "TRUE-TRUE-TRUE"   = paste0(labels, "\n",  ci_labels),
      "FALSE-FALSE-TRUE" = paste0(labels, ci_labels),
    )

    edge_colors <- ifelse(significant, "#000000", "#BEBEBE")

    if (multi_group == TRUE) {
      sem_paths0 <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                      whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                      edge.color = edge_colors)

      if (inherits(sem_paths0, 'list')) {
        names(sem_paths0) <- group_info
        sem_paths <- sem_paths0[[1]]
      } else {
        sem_paths <- sem_paths0
      }

    } else {
      if (!is.null(data_file)) {
        sem_paths <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                       whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                       edge.color = edge_colors)
      } else {
        sem_paths <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts,
                                       what = "paths", whatLabels = "par", edgeLabels = edgeLabels,
                                       residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                       edge.color = edge_colors)
      }
    }
  }

  return(sem_paths)
}

#' Convert lavaan model to semPaths with enhanced options
#'
#'
#' @param fit Fitted lavaan model object
#' @param data_file Optional data frame used for fitting the model
#' @param layout_algorithm Layout algorithm for semPaths (default: 'tree2')
#' @param intercepts Logical, whether to include intercepts in the visualization (default: TRUE)
#' @param annotate_estimates Logical, whether to annotate edges with parameter
#'   estimates (default: TRUE)
#' @param standardized Logical, whether to show standardized estimates (default: FALSE)
#' @param unstandardized Logical, whether to show unstandardized estimates (default: TRUE)
#' @param conf_int Logical, whether to show confidence intervals (default: FALSE)
#' @param p_val Logical, whether to add asterisks for significant parameters (default: FALSE)
#' @param multi_group Logical, whether the model is multi-group (default: FALSE)
#' @param group_var Character, name of grouping variable for multi-group models
#' @param group_level Character, specific group level to visualize in multi-group models
#' @param p_val_alpha alpha level of p-value significance, double
#' @param combine combine two SEMs, logical
#' @param group1 group level of group1, character
#' @param group2 group level of group2, character
#' @param sep_by character separation, no space
#' @param residuals incldue residuals or not, logical
#'
#' @return A semPaths qgraph object for visualization
#' @importFrom lavaan parameterEstimates standardizedSolution lavInspect
#' @importFrom semPlot semPaths
#' @importFrom dplyr select mutate across filter left_join
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
lavaan_to_sempaths <- function(fit, data_file = NULL, layout_algorithm = 'tree2',
                               intercepts = TRUE,
                               annotate_estimates = TRUE,
                               standardized = FALSE,
                               unstandardized = TRUE,
                               conf_int = FALSE,
                               p_val = FALSE,
                               multi_group = FALSE,
                               group_var = NULL,
                               group_level = NULL,
                               p_val_alpha = 0.05,
                               combine = FALSE,
                               group1 = NULL,
                               group2 = NULL,
                               sep_by = " |",
                               residuals = FALSE) {

  if (is.list(fit)) {
    combine <- FALSE
    fake_combine <- TRUE
  } else {
    fake_combine <- FALSE
  }

  if (is.null(group1) || is.null(group2)) combine <- FALSE

  if (combine) {
    params <- get_comparison_table(fit = fit, alpha = p_val_alpha, group1 = group1, group2 = group2, sep_by = sep_by)

    group_info <- lavInspect(fit, "group.label")
    group_number <- which(group_info == group_level)

    edge_params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]
    self_loop_indices <- which(edge_params$lhs == edge_params$rhs)

    if (!residuals) {
      if (length(self_loop_indices) > 0) {
        params1 <- edge_params[-self_loop_indices, ]
        std_est1 <- standardizedSolution(fit)[-self_loop_indices, ]
      } else {
        params1 <- edge_params
      }
    } else {
      params1 <- edge_params
      std_est1 <- standardizedSolution(fit)
    }

    if (!intercepts) params1 <- params1[params1$op != "~1", ]

    unstd <- params1$est  # Unstandardized
    std <- params1$std   # Standardized

    params1$pvalue[is.na(params1$pvalue)] <- 1
    std_est1$pvalue[is.na(std_est1$pvalue)] <- 1
    # pval_idx <- which(params1$pvalue < p_val_alpha)

    if (p_val == TRUE) {
      std[which(std_est1$pvalue < p_val_alpha)] <- paste0(std[which(std_est1$pvalue < p_val_alpha)], "*")
      unstd[which(params1$pvalue < p_val_alpha)] <- paste0(unstd[which(params1$pvalue < p_val_alpha)], "*")
    }

    ci_labels <- if (conf_int) {
      # Determine which CI to show
      ci_to_show <- if (standardized == TRUE && unstandardized == FALSE) {
        params1$confint_std
      } else {
        params1$confint_unstd
      }

      # Add \n only if we're also showing point estimates
      if (standardized == FALSE && unstandardized == FALSE) {
        ci_to_show  # No \n when only showing CI
      } else {
        paste0("\n", ci_to_show)  # Add \n when showing with point estimates
      }
    } else {
      ""
    }


    if (standardized == TRUE && unstandardized == TRUE) {
      base_labels <- paste0(unstd, " (", std, ")")
    } else if (standardized == TRUE && unstandardized == FALSE) {
      base_labels <- std
    } else if (standardized == FALSE && unstandardized == TRUE) {
      base_labels <- unstd
    } else {
      base_labels <- NULL
    }

    edgeLabels <- if (conf_int && !is.null(base_labels)) {
      paste0(base_labels, ci_labels)
    } else {
      base_labels
    }

    if (standardized == TRUE && unstandardized == FALSE) {
      edge_colors <- ifelse(std_est1$pvalue < p_val_alpha, "#000000", "#BEBEBE")
    } else {
      edge_colors <- ifelse(params1$pvalue < p_val_alpha, "#000000", "#BEBEBE")
    }


    sem_paths0 <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                    whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                    edge.color = edge_colors)


    if (inherits(sem_paths0, 'list')) {
      sem_paths <- sem_paths0[[1]]
    } else if (is(sem_paths0) == 'qgraph') {
      sem_paths <- sem_paths0
    }


  } else if (fake_combine) {

    params <- combine_model_parameters(fit1 = fit[[1]], fit2 = fit[[2]], group1 = group1, group2 = group2, sep_by = sep_by)

    edge_params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]
    self_loop_indices <- which(edge_params$lhs == edge_params$rhs)

    if (!residuals) {
      if (length(self_loop_indices) > 0) {
        params1 <- edge_params[-self_loop_indices, ]
        # std_est1 <- standardizedSolution(fit)[-self_loop_indices, ]
      } else {
        params1 <- edge_params
      }
    } else {
      params1 <- edge_params
      # std_est1 <- standardizedSolution(fit)
    }

    params1$pvalue <- 1 # fake column

    if (!intercepts) params1 <- params1[params1$op != "~1", ]

    unstd <- params1$est  # Unstandardized
    std <- params1$std   # Standardized

    ci_labels <- if (conf_int) {
      if (standardized == FALSE && unstandardized == FALSE) {
        params1$confint_unstd
      } else if (standardized == TRUE && unstandardized == TRUE) {
        paste0("\n", params1$confint_unstd)
      } else if (standardized == TRUE && unstandardized == FALSE) {
        paste0("\n", params1$confint_std)
      } else {
        paste0("\n", params1$confint_unstd)
      }
    } else {
      ""
    }
    if (standardized == TRUE && unstandardized == TRUE) {
      base_labels <- paste0(unstd, " (", std, ")")
    } else if (standardized == TRUE && unstandardized == FALSE) {
      base_labels <- std
    } else if (standardized == FALSE && unstandardized == TRUE) {
      base_labels <- unstd
    } else {
      base_labels <- NULL
    }


    edgeLabels <- if (conf_int && !is.null(base_labels)) {
      paste0(base_labels, ci_labels)
    } else {
      base_labels
    }

    if (multi_group == TRUE) {
      sem_paths0 <- semPlot::semPaths(fit[[1]], layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                      whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                      edge.color = ifelse(params1$pvalue < p_val_alpha, "#000000", "#BEBEBE"))

      names(sem_paths0) <- group_info
      sem_paths <- sem_paths0[[1]]

    } else if (multi_group == FALSE) {
      if (!is.null(data_file)) {
        sem_paths <- semPlot::semPaths(fit[[1]], layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                       whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                       edge.color = ifelse(params1$pvalue < p_val_alpha, "#000000", "#BEBEBE"))
      } else {
        sem_paths <- semPlot::semPaths(fit[[1]], layout = layout_algorithm, intercepts = intercepts,
                                       what = "paths", whatLabels = "par", edgeLabels = edgeLabels,
                                       residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                       edge.color = ifelse(params1$pvalue < p_val_alpha, "#000000", "#BEBEBE"))
      }
    }

  } else {
    params <- lavaan::parameterEstimates(fit)
    edge_params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]

    self_loop_indices <- which(edge_params$lhs == edge_params$rhs)

    # Filter parameters based on multi-group setting
    if (multi_group == TRUE && !is.null(group_level)) {
      # Get the group number for the specified group level
      group_info <- lavInspect(fit, "group.label")
      group_number <- which(group_info == group_level)

      if (length(group_number) == 0) {
        stop("Group level '", group_level, "' not found in the model. Available groups: ",
             paste(group_info, collapse = ", "))
      }

      if (!residuals) {
        if (length(self_loop_indices) > 0) {
          params1 <- edge_params[-self_loop_indices, ]
          std_est1 <- standardizedSolution(fit)[-self_loop_indices, ]
        } else {
          params1 <- edge_params
          std_est1 <- standardizedSolution(fit)
        }
      } else {
        params1 <- edge_params
        std_est1 <- standardizedSolution(fit)
      }

      # Filter parameters for the specific group
      params1 <- params1[params1$group == group_number, ]
      std_est1 <- std_est1[std_est1$group == group_number, ]

    } else {
      # Single group case
      if (!residuals) {
        if (length(self_loop_indices) > 0) {
          params1 <- edge_params[-self_loop_indices, ]
          std_est1 <- standardizedSolution(fit)[-self_loop_indices, ]
        } else {
          params1 <- edge_params
          std_est1 <- standardizedSolution(fit)
        }
      } else {
        params1 <- edge_params
        std_est1 <- standardizedSolution(fit)
      }
    }

    if (!intercepts) {
      params1 <- params1[params1$op != "~1", ]
      std_est1 <- std_est1[std_est1$op != "~1", ]
    }

    unstd <- round(params1$est, 2)  # Unstandardized
    std <- round(std_est1$est.std, 2)   # Standardized

    # Handle NA p-values
    params1$pvalue[is.na(params1$pvalue)] <- 1
    std_est1$pvalue[is.na(std_est1$pvalue)] <- 1

    # Apply significance stars based on which values are shown
    if (p_val == TRUE) {
      std[which(std_est1$pvalue < p_val_alpha)] <- paste0(std[which(std_est1$pvalue < p_val_alpha)], "*")
      unstd[which(params1$pvalue < p_val_alpha)] <- paste0(unstd[which(params1$pvalue < p_val_alpha)], "*")
    }

    if (standardized == TRUE && unstandardized == TRUE) {
      labels <- paste0(unstd, " (", std, ")")
    } else if (standardized == TRUE && unstandardized == FALSE) {
      labels <- std
    } else if (standardized == FALSE && unstandardized == TRUE) {
      labels <- unstd
    } else {
      labels <- NULL
    }

    if (conf_int) {
      if (standardized == TRUE && unstandardized == FALSE) {
        if (standardized == FALSE && unstandardized == FALSE) {
          ci_labels <- paste0("[",
                              round(std_est1$ci.lower, 2), ", ",
                              round(std_est1$ci.upper, 2), "]")
        } else {
          ci_labels <- paste0("\n[",
                              round(std_est1$ci.lower, 2), ", ",
                              round(std_est1$ci.upper, 2), "]")
        }
      } else {
        if (standardized == FALSE && unstandardized == FALSE) {
          ci_labels <- paste0("[",
                              round(params1$ci.lower, 2), ", ",
                              round(params1$ci.upper, 2), "]")
        } else {
          ci_labels <- paste0("\n[",
                              round(params1$ci.lower, 2), ", ",
                              round(params1$ci.upper, 2), "]")
        }
      }
    } else {
      ci_labels <- ""
    }

    edgeLabels <- switch(
      paste(standardized, unstandardized, conf_int, sep = "-"),
      "FALSE-TRUE-FALSE"  = unstd,
      "TRUE-FALSE-FALSE"  = std,
      "TRUE-TRUE-FALSE"   = labels,
      "FALSE-FALSE-FALSE" = labels,
      "FALSE-TRUE-TRUE"  = paste0(unstd, ci_labels),
      "TRUE-FALSE-TRUE"  = paste0(std, ci_labels),
      "TRUE-TRUE-TRUE"   = paste0(labels, ci_labels),
      "FALSE-FALSE-TRUE" = paste0(labels, ci_labels),
    )

    if (standardized == TRUE && unstandardized == FALSE) {
      edge_colors <- ifelse(std_est1$pvalue < p_val_alpha, "#000000", "#BEBEBE")
    } else {
      edge_colors <- ifelse(params1$pvalue < p_val_alpha, "#000000", "#BEBEBE")
    }

    if (multi_group == TRUE) {
      sem_paths0 <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                      whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals,
                                      plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                      edge.color = edge_colors)

      names(sem_paths0) <- group_info
      sem_paths <- sem_paths0[[1]]

    } else if (multi_group == FALSE) {
      if (!is.null(data_file)) {
        sem_paths <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts, what = "paths",
                                       whatLabels = "par", edgeLabels = edgeLabels, residuals = residuals,
                                       plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                       edge.color = edge_colors)
      } else {
        sem_paths <- semPlot::semPaths(fit, layout = layout_algorithm, intercepts = intercepts,
                                       what = "paths", whatLabels = "par", edgeLabels = edgeLabels,
                                       residuals = residuals, plot = FALSE, title = FALSE, DoNotPlot = TRUE,
                                       edge.color = edge_colors)
      }
    }

  }

  return(sem_paths)
}

#' Convert lavaan syntax to fitted model
#'
#' Fits structural equation models using lavaan syntax with support for custom
#' model fitting code, multi-group analysis, and various model types including
#' CFA, SEM, growth models, and EFA.
#'
#' @param lavaan_string Character string containing lavaan model syntax
#' @param sem_code Character string containing custom SEM fitting code (optional).
#'   If provided, this code will be executed instead of standard lavaan functions.
#'   Must include `lavaan_string` and `data` variables.
#' @param data_file Data frame containing the observed variables for model fitting
#' @param multi_group Logical, whether to perform multi-group analysis (default: FALSE)
#' @param group_var Character, name of grouping variable for multi-group analysis
#' @param invariance_level Character, level of measurement invariance for multi-group
#'   analysis: "configural", "metric", "scalar", or "strict" (default: NULL)
#' @param custom_sem Logical, whether to use custom SEM code when provided (default: TRUE)
#' @param model_type Character, type of model to fit: "sem", "cfa", "growth", or "efa"
#'   (default: "sem")
#' @param nfactors Integer, number of factors for EFA models (required for EFA,
#'   default: NULL)
#'
#' @return A fitted lavaan model object
#'
#' @details
#' This function provides a flexible interface for fitting SEM models with several
#' key features:
#'
#' \strong{Model Type Support:}
#' \itemize{
#'   \item \strong{SEM}: Structural Equation Modeling with latent variables
#'   \item \strong{CFA}: Confirmatory Factor Analysis
#'   \item \strong{Growth}: Latent growth curve models
#'   \item \strong{EFA}: Exploratory Factor Analysis (requires nfactors parameter)
#' }
#'
#' \strong{Multi-Group Analysis:}
#' \itemize{
#'   \item Configural invariance (no constraints)
#'   \item Metric invariance (equal loadings)
#'   \item Scalar invariance (equal loadings and intercepts)
#'   \item Strict invariance (equal loadings, intercepts, and residuals)
#' }
#'
#' \strong{Custom SEM Code:}
#' When `sem_code` is provided and `custom_sem = TRUE`, the function evaluates
#' the custom code instead of using standard lavaan functions. The custom code must:
#' \itemize{
#'   \item Include `lavaan_string` and `data` variables
#'   \item Use valid lavaan functions (sem, cfa, growth, efa, lavaan)
#'   \item Return a fitted lavaan model object
#' }
#'
#' \strong{Error Handling:}
#' The function includes comprehensive error checking for:
#' \itemize{
#'   \item Missing data files
#'   \item Invalid custom code syntax
#'   \item Missing required parameters (group_var for multi-group, nfactors for EFA)
#'   \item Model fitting errors with informative messages
#' }
#'
#' @examples
#' \dontrun{
#' # Basic SEM
#' model_syntax <- "
#'   # Measurement model
#'   visual =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed =~ x7 + x8 + x9
#'
#'   # Structural model
#'   visual ~ textual + speed
#' "
#' fit <- lavstring_to_fit(model_syntax, data_file = HolzingerSwineford1939)
#'
#' # Multi-group CFA with metric invariance
#' fit <- lavstring_to_fit(
#'   model_syntax,
#'   data_file = data,
#'   multi_group = TRUE,
#'   group_var = "school",
#'   invariance_level = "metric",
#'   model_type = "cfa"
#' )
#'
#' # Custom SEM code
#' custom_code <- "cfa(lavaan_string, data = data, estimator = 'MLR')"
#' fit <- lavstring_to_fit(model_syntax, data_file = data, sem_code = custom_code)
#'
#' # EFA with 3 factors
#' efa_syntax <- "f1 =~ x1 + x2 + x3 + x4 + x5"
#' fit <- lavstring_to_fit(efa_syntax, data_file = data, model_type = "efa", nfactors = 3)
#' }
#'
#' @importFrom lavaan lavaanify sem cfa growth efa
#' @keywords internal
#' @noRd
lavstring_to_fit <- function(lavaan_string, sem_code = NULL, data_file = NULL, multi_group = FALSE,
                             group_var = NULL, invariance_level = NULL, custom_sem = TRUE,
                             model_type = "sem", nfactors = NULL) {

  if (multi_group) custom_sem = FALSE

  if (is.null(data_file)) {
    stop("Data file must be provided")
  }

  model <- lavaan::lavaanify(lavaan_string)
  latent_vars <- unique(model$lhs[model$op == "=~"])    # Latent variables
  observed_vars <- unique(setdiff(model$rhs[model$op %in% c("=~", "~", "~~")], model$lhs[model$op == "=~"]))

  data <- data_file

  if (!is.null(sem_code) && custom_sem) {
    # Check for required components in custom code
    if (!grepl("lavaan_string", sem_code) || !grepl("data", sem_code)) {
      stop("Custom SEM code must include `lavaan_string` and `data`.")
    }

    allowed_functions <- c("sem", "cfa", "growth", "efa", "lavaan")
    function_found <- sapply(allowed_functions, function(fun) grepl(paste0("\\b", fun, "\\("), sem_code))

    if (!any(function_found)) {
      stop("Custom SEM code must use a valid lavaan function: ", paste(allowed_functions, collapse = ", "))
    }
  }

  fit <- tryCatch({
    if (custom_sem && !is.null(sem_code)) {
      # Use custom SEM code
      eval(parse(text = sem_code))
    } else {
      if (multi_group) {
        if (is.null(group_var)) {
          stop("group_var must be specified for multi-group analysis")
        }

        group_equal_args <- switch(invariance_level,
                                   "configural" = NULL,
                                   "metric" = "loadings",
                                   "scalar" = c("loadings", "intercepts"),
                                   "strict" = c("loadings", "intercepts", "residuals"),
                                   NULL)

        # Choose appropriate function based on model type
        switch(model_type,
               "sem" = sem(lavaan_string, data = data, group = group_var, group.equal = group_equal_args),
               "cfa" = cfa(lavaan_string, data = data, group = group_var, group.equal = group_equal_args),
               "growth" = growth(lavaan_string, data = data, group = group_var, group.equal = group_equal_args),
               "efa" = {
                 warning("EFA typically doesn't support multi-group analysis. Using CFA instead.")
                 cfa(lavaan_string, data = data, group = group_var, group.equal = group_equal_args)
               },
               # Default to sem
               sem(lavaan_string, data = data, group = group_var, group.equal = group_equal_args)
        )
      } else {
        # Single-group analysis
        switch(model_type,
               "sem" = sem(lavaan_string, data = data),
               "cfa" = cfa(lavaan_string, data = data),
               "growth" = growth(lavaan_string, data = data),
               "efa" = {
                 if (is.null(nfactors)) {
                   stop("nfactors must be specified for EFA models")
                 }
                 efa(lavaan_string, data = data, nfactors = nfactors)
               },
               sem(lavaan_string, data = data)
        )
      }
    }
  }, error = function(e) {
    stop("Error in SEM model fitting: ", e$message)
  })

  return(fit)
}

#' Extract Parameters from OpenMx Model
#'
#' This internal function extracts parameter estimates from an OpenMx model object
#' and converts them into a standardized data frame format similar to lavaan's
#' parameter table structure.
#'
#' @param fit An OpenMx model object from which to extract parameters.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{lhs}: Left-hand side variable name
#'   \item \code{op}: Operator symbol ("~" for regressions, "~~" for covariances, "~1" for means)
#'   \item \code{rhs}: Right-hand side variable name (empty for means/intercepts)
#'   \item \code{est}: Parameter estimate value
#'   \item \code{free}: Logical indicating whether the parameter is free (TRUE) or fixed (FALSE)
#'   \item \code{group}: Group identifier (always 1 for single-group models)
#' }
#'
#' @details
#' The function processes three main OpenMx matrices:
#' \itemize{
#'   \item \code{A} matrix: Contains regression coefficients (operator "~")
#'   \item \code{S} matrix: Contains variances and covariances (operator "~~")
#'   \item \code{M} matrix: Contains means and intercepts (operator "~1")
#' }
#'
#' For covariance matrices (\code{S}), the function avoids duplicate entries by
#' only including the lower triangle (excluding the diagonal for covariances).
#' The function extracts both free and fixed parameters that have non-zero values.
#'
#' @note
#' This function is designed for internal use within the package to convert
#' OpenMx model parameters into a standardized format compatible with other
#' SEM visualization functions. It currently supports single-group models only.
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Assuming mx_fit is a fitted OpenMx model
#' params <- get_openmx_parameters(mx_fit)
#' }
#'
get_openmx_parameters <- function(fit) {
  params <- data.frame(
    lhs = character(),
    op = character(),
    rhs = character(),
    est = numeric(),
    free = logical(),
    stringsAsFactors = FALSE
  )

  add_params <- function(mat, op_symbol) {
    if (!mat %in% names(fit$matrices)) return()

    mat_values <- fit[[mat]]$values
    mat_labels <- fit[[mat]]$labels
    mat_free <- fit[[mat]]$free

    non_zero <- which(mat_values != 0 | mat_free, arr.ind = TRUE)

    if (length(non_zero) > 0) {
      row_names <- rownames(mat_values)
      col_names <- colnames(mat_values)

      for (i in 1:nrow(non_zero)) {
        row_idx <- non_zero[i, 1]  # Fixed: row index first
        col_idx <- non_zero[i, 2]  # Fixed: column index second

        # Skip symmetric duplicates for covariance matrices
        if (mat == "S" && row_idx != col_idx && row_idx > col_idx) next

        # For means/intercepts, adjust indexing
        if (mat == "M") {
          lhs <- col_names[col_idx]
          rhs <- ""
        } else {
          lhs <- row_names[row_idx]
          rhs <- col_names[col_idx]
        }

        # Determine if parameter is free
        is_free <- isTRUE(mat_free[row_idx, col_idx])

        new_row <- data.frame(
          lhs = lhs,
          op = op_symbol,
          rhs = rhs,
          est = mat_values[row_idx, col_idx],
          free = is_free,
          stringsAsFactors = FALSE
        )
        params <<- rbind(params, new_row)
      }
    }
  }

  # Process matrices
  add_params("A", "~")   # Regressions
  add_params("S", "~~")  # Variances/covariances
  add_params("M", "~1")  # Means/intercepts

  # Add group information
  if (nrow(params) > 0) {
    params$group <- 1
  }

  return(params)
}

#' Combine tidySEM Groups for Multi-Group Analysis
#'
#' This internal function combines parameter estimates from multiple groups within
#' a single tidySEM object. It merges estimates, confidence intervals, and
#' significance indicators for specified groups into a unified visualization object
#' for frequentist multi-group SEM models.
#'
#' @param tidysem_obj A tidySEM graph object containing multiple groups.
#' @param group1 Character string specifying the name of the first group to compare.
#' @param group2 Character string specifying the name of the second group to compare.
#' @param sep_by Character string used to separate group values in the output
#'   (default: " | ").
#' @param standardized Logical indicating whether to include standardized estimates
#'   in the output (default: FALSE).
#' @param unstandardized Logical indicating whether to include unstandardized estimates
#'   in the output (default: TRUE).
#' @param p_val Logical indicating whether to include significance indicators
#'   in the output (default: TRUE).
#' @param conf_int Logical indicating whether to include confidence intervals
#'   in the output (default: FALSE).
#'
#' @return A modified tidySEM object with combined group information
#'
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Assuming tidysem_obj contains multiple groups
#' combined <- combine_tidysem_groups(
#'   tidysem_obj,
#'   group1 = "Group1", group2 = "Group2",
#'   standardized = TRUE, unstandardized = TRUE,
#'   p_val = TRUE, conf_int = TRUE
#' )
#' }
#'
#'
#' @importFrom dplyr mutate across group_by summarise filter arrange select distinct first left_join case_when row_number
#' @importFrom stats na.omit
#' @importFrom rlang .data
combine_tidysem_groups <- function(tidysem_obj, group1 = "", group2 = "",
                                   sep_by = " | ", standardized = FALSE, unstandardized = TRUE,
                                   p_val = TRUE, conf_int = FALSE) {

  # Ensure numeric columns are actually numeric
  tidysem_obj$edges <- tidysem_obj$edges |>
    mutate(across(c(est, est_std), ~ as.numeric(as.character(.x))))

  tidysem_obj$nodes <- tidysem_obj$nodes |>
    mutate(across(c(est, est_std), ~ as.numeric(as.character(.x))))

  original_edges_order <- tidysem_obj$edges |>
    distinct(lhs, op, rhs) |>
    mutate(original_order = row_number())

  # Combine edges
  edges_combined <- tidysem_obj$edges |>
    group_by(lhs, op, rhs) |>
    summarise(
      est_combined = if (all(c(group1, group2) %in% group)) {
        paste(
          ifelse(is.na(est[group == group1]), "NA", round(est[group == group1], 2)),
          ifelse(is.na(est[group == group2]), "NA", round(est[group == group2], 2)),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      est_sig_combined = if (all(c(group1, group2) %in% group)) {
        paste(
          ifelse(is.na(est_sig[group == group1]), "NA", est_sig[group == group1]),
          ifelse(is.na(est_sig[group == group2]), "NA", est_sig[group == group2]),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      # Add standardized estimates
      est_std_combined = if (all(c(group1, group2) %in% group)) {
        paste(
          ifelse(is.na(est_std[group == group1]), "NA", round(est_std[group == group1], 2)),
          ifelse(is.na(est_std[group == group2]), "NA", round(est_std[group == group2], 2)),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      est_sig_std_combined = if (all(c(group1, group2) %in% group)) {
        paste(
          ifelse(is.na(est_sig_std[group == group1]), "NA", est_sig_std[group == group1]),
          ifelse(is.na(est_sig_std[group == group2]), "NA", est_sig_std[group == group2]),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      # Add confidence intervals
      confint_combined = if (all(c(group1, group2) %in% group)) {
        paste(
          ifelse(is.na(confint[group == group1]), "NA", confint[group == group1]),
          ifelse(is.na(confint[group == group2]), "NA", confint[group == group2]),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      confint_std_combined = if (all(c(group1, group2) %in% group)) {
        paste(
          ifelse(is.na(confint_std[group == group1]), "NA", confint_std[group == group1]),
          ifelse(is.na(confint_std[group == group2]), "NA", confint_std[group == group2]),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      across(c(from, to, arrow, connect_from, connect_to, curvature,
               linetype, block, show, label_results), first),
      .groups = 'drop'
    ) |>
    filter(!is.na(est_combined)) |>
    mutate(
      across(c(est_combined, est_sig_combined, est_std_combined, est_sig_std_combined,
               confint_combined, confint_std_combined),
             ~ ifelse(.x == paste0("NA", sep_by, "NA"), "", .x))
    ) |>
    mutate(
      label = case_when(
        # Case 1: Both standardized and unstandardized are FALSE
        standardized == FALSE & unstandardized == FALSE ~
          if (conf_int) {
            # Show unstandardized confidence interval without "\n"
            confint_combined
          } else {
            ""
          },

        # Case 2: Both standardized and unstandardized
        standardized == TRUE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")\n", confint_combined)
            } else {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")")
            }
          } else {
            if (conf_int) {
              paste0(est_combined, " (", est_std_combined, ")\n", confint_combined)
            } else {
              paste0(est_combined, " (", est_std_combined, ")")
            }
          },

        # Case 3: Only standardized
        standardized == TRUE & unstandardized == FALSE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_std_combined, "\n", confint_std_combined)
            } else {
              est_sig_std_combined
            }
          } else {
            if (conf_int) {
              paste0(est_std_combined, "\n", confint_std_combined)
            } else {
              est_std_combined
            }
          },

        # Case 4: Only unstandardized
        standardized == FALSE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_combined, "\n", confint_combined)
            } else {
              est_sig_combined
            }
          } else {
            if (conf_int) {
              paste0(est_combined, "\n", confint_combined)
            } else {
              est_combined
            }
          },

        TRUE ~ ""  # Fallback (shouldn't be reached)
      )
    ) |>
    # Reorder to match original order
    left_join(original_edges_order, by = c("lhs", "op", "rhs")) |>
    arrange(original_order) |>
    select(-original_order)

  original_nodes_order <- tidysem_obj$nodes |>
    distinct(name, op, rhs) |>
    mutate(original_order = row_number())

  # Combine nodes
  nodes_combined <- tidysem_obj$nodes |>
    group_by(name, op, rhs) |>
    summarise(
      est_combined = if (all(c(group1, group2) %in% group)) {
        paste(round(est[group == group1], 2),
              round(est[group == group2], 2), sep = sep_by)
      } else {
        NA_character_
      },
      est_sig_combined = if (all(c(group1, group2) %in% group)) {
        paste(est_sig[group == group1],
              est_sig[group == group2], sep = sep_by)
      } else {
        NA_character_
      },
      # Add standardized estimates for nodes
      est_std_combined = if (all(c(group1, group2) %in% group)) {
        paste(round(est_std[group == group1], 2),
              round(est_std[group == group2], 2), sep = sep_by)
      } else {
        NA_character_
      },
      est_sig_std_combined = if (all(c(group1, group2) %in% group)) {
        paste(est_sig_std[group == group1],
              est_sig_std[group == group2], sep = sep_by)
      } else {
        NA_character_
      },
      # Add confidence intervals for nodes
      confint_combined = if (all(c(group1, group2) %in% group)) {
        paste(confint[group == group1],
              confint[group == group2], sep = sep_by)
      } else {
        NA_character_
      },
      confint_std_combined = if (all(c(group1, group2) %in% group)) {
        paste(confint_std[group == group1],
              confint_std[group == group2], sep = sep_by)
      } else {
        NA_character_
      },
      across(c(x, y, shape, node_xmin, node_xmax, node_ymin, node_ymax,
               lhs, block, show, label_results), first),
      .groups = 'drop'
    ) |>
    filter(!is.na(est_combined)) |>
    # Set label based on standardized/unstandardized parameters with p_val and conf_int
    mutate(
      label = case_when(
        # Case 1: Both standardized and unstandardized are FALSE
        standardized == FALSE & unstandardized == FALSE ~
          if (conf_int) {
            # Show unstandardized confidence interval without "\n"
            confint_combined
          } else {
            ""
          },

        # Case 2: Both standardized and unstandardized
        standardized == TRUE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")\n", confint_combined)
            } else {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")")
            }
          } else {
            if (conf_int) {
              paste0(est_combined, " (", est_std_combined, ")\n", confint_combined)
            } else {
              paste0(est_combined, " (", est_std_combined, ")")
            }
          },

        # Case 3: Only standardized
        standardized == TRUE & unstandardized == FALSE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_std_combined, "\n", confint_std_combined)
            } else {
              est_sig_std_combined
            }
          } else {
            if (conf_int) {
              paste0(est_std_combined, "\n", confint_std_combined)
            } else {
              est_std_combined
            }
          },

        # Case 4: Only unstandardized
        standardized == FALSE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_combined, "\n", confint_combined)
            } else {
              est_sig_combined
            }
          } else {
            if (conf_int) {
              paste0(est_combined, "\n", confint_combined)
            } else {
              est_combined
            }
          },

        TRUE ~ ""  # Fallback (shouldn't be reached)
      )
    ) |>
    left_join(original_nodes_order, by = c("name", "op", "rhs")) |>
    arrange(original_order) |>
    select(-original_order)

  # Update the object
  tidysem_obj$edges <- edges_combined
  tidysem_obj$nodes <- nodes_combined

  return(tidysem_obj)
}

#' Combine tidySEM Groups for Bayesian Multi-Group Analysis
#'
#' This internal function combines parameter estimates from multiple groups within
#' a single Bayesian SEM tidySEM object. It merges estimates, HPD intervals, and
#' significance indicators for specified groups into a unified visualization object.
#'
#' @param tidysem_obj A tidySEM graph object containing multiple groups.
#' @param blavaan_fit A fitted Bayesian SEM object from blavaan containing the multi-group model.
#' @param group1 Character string specifying the name of the first group to compare.
#' @param group2 Character string specifying the name of the second group to compare.
#' @param sep_by Character string used to separate group values in the output
#'   (default: " | ").
#' @param standardized Logical indicating whether to include standardized estimates
#'   in the output (default: FALSE).
#' @param unstandardized Logical indicating whether to include unstandardized estimates
#'   in the output (default: TRUE).
#' @param p_val Logical indicating whether to include significance stars based on
#'   HPD intervals (default: FALSE).
#' @param conf_int Logical indicating whether to include HPD intervals
#'   in the output (default: FALSE).
#' @param ci_level Numeric value specifying the confidence level for HPD intervals
#'   (default: 0.95).
#'
#' @return A modified tidySEM object with combined group information
#'
#'
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Assuming tidysem_obj contains multiple groups and blavaan_fit is a multi-group model
#' combined <- combine_tidysem_groups_bayes(
#'   tidysem_obj, blavaan_fit,
#'   group1 = "Group1", group2 = "Group2",
#'   standardized = TRUE, unstandardized = TRUE,
#'   p_val = TRUE, conf_int = TRUE
#' )
#' }
#'
#'
#' @importFrom blavaan blavInspect
#' @importFrom dplyr distinct mutate left_join select group_by filter summarise across arrange case_when n first any_of row_number
#' @importFrom stats na.omit
#' @importFrom rlang .data
combine_tidysem_groups_bayes <- function(tidysem_obj, blavaan_fit, group1 = "", group2 = "",
                                         sep_by = " | ", standardized = FALSE, unstandardized = TRUE,
                                         p_val = FALSE, conf_int = FALSE, ci_level = 0.95) {

  # Get HPD intervals from blavaan for credible intervals
  hpd_intervals <- as.data.frame(blavaan::blavInspect(blavaan_fit, "hpd", level = ci_level))
  hpd_intervals$parameter <- rownames(hpd_intervals)

  add_significance_stars <- function(est_values, lower_values, upper_values) {
    significant <- !is.na(lower_values) & !is.na(upper_values) &
      (lower_values > 0 | upper_values < 0)
    ifelse(significant, paste0(est_values, "*"), est_values)
  }

  original_edges_order <- tidysem_obj$edges |>
    distinct(lhs, op, rhs) |>
    mutate(original_order = row_number())

  tidysem_obj$edges <- tidysem_obj$edges |>
    mutate(
      # Create parameter names for matching with HPD intervals
      param_name = case_when(
        op == "=~" ~ paste0(lhs, op, rhs),
        op == "~~" & lhs != rhs ~ paste0(lhs, op, rhs),
        op == "~~" & lhs == rhs ~ paste0("Variances.", lhs),
        op == "~1" ~ paste0("Means.", lhs),
        TRUE ~ paste0(lhs, op, rhs)
      ),
      group_num = match(group, unique(group)),
      param_name_full = ifelse(group_num == 1,
                               param_name,
                               paste0(param_name, ".g", group_num))
    ) |>
    left_join(hpd_intervals |> select(parameter, lower, upper),
              by = c("param_name_full" = "parameter")) |>
    mutate(
      confint = ifelse(!is.na(lower),
                       paste0("[", round(lower, 2), ", ", round(upper, 2), "]"),
                       NA_character_),
      confint_std = confint,
      est_sig = if (p_val) add_significance_stars(est, lower, upper) else est,
      est_sig_std = if (p_val) add_significance_stars(est_std, lower, upper) else est_std
    ) |>
    select(-param_name, -param_name_full, -group_num, -lower, -upper)

  tidysem_obj$edges <- tidysem_obj$edges |>
    mutate(across(c(est, est_std), ~ as.numeric(as.character(.x))))

  if (!is.null(tidysem_obj$nodes)) {
    tidysem_obj$nodes <- tidysem_obj$nodes |>
      mutate(across(c(est, est_std), ~ as.numeric(as.character(.x))))
  }


  edges_combined <- tidysem_obj$edges |>
    group_by(lhs, op, rhs) |>
    summarise(
      est_combined = paste(
        ifelse(is.na(est[group == group1]), "NA", round(est[group == group1], 2)),
        ifelse(is.na(est[group == group2]), "NA", round(est[group == group2], 2)),
        sep = sep_by
      ),
      est_sig_combined = paste(
        ifelse(is.na(est_sig[group == group1]), "NA", est_sig[group == group1]),
        ifelse(is.na(est_sig[group == group2]), "NA", est_sig[group == group2]),
        sep = sep_by
      ),
      # Add standardized estimates
      est_std_combined = if ("est_std" %in% names(tidysem_obj$edges)) {
        paste(
          ifelse(is.na(est_std[group == group1]), "NA", round(est_std[group == group1], 2)),
          ifelse(is.na(est_std[group == group2]), "NA", round(est_std[group == group2], 2)),
          sep = sep_by
        )
      },
      est_sig_std_combined = if ("est_sig_std" %in% names(tidysem_obj$edges)) {
        paste(
          ifelse(is.na(est_sig_std[group == group1]), "NA", est_sig_std[group == group1]),
          ifelse(is.na(est_sig_std[group == group2]), "NA", est_sig_std[group == group2]),
          sep = sep_by
        )
      } else if ("est_std" %in% names(tidysem_obj$edges)) {
        paste(
          ifelse(is.na(est_std[group == group1]), "NA", round(est_std[group == group1], 2)),
          ifelse(is.na(est_std[group == group2]), "NA", round(est_std[group == group2], 2)),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      confint_combined = paste(
        ifelse(is.na(confint[group == group1]), "NA", confint[group == group1]),
        ifelse(is.na(confint[group == group2]), "NA", confint[group == group2]),
        sep = sep_by
      ),
      confint_std_combined = paste(
        ifelse(is.na(confint_std[group == group1]), "NA", confint_std[group == group1]),
        ifelse(is.na(confint_std[group == group2]), "NA", confint_std[group == group2]),
        sep = sep_by
      ),
      across(c(from, to, arrow, connect_from, connect_to, curvature,
               linetype, block, show, label_results), first),
      .groups = 'drop'
    ) |>
    filter(!is.na(est_combined)) |>
    # CLEANUP: Replace "NA | NA" and NA with empty strings in confint columns
    mutate(
      across(c(est_combined, est_sig_combined, est_std_combined, est_sig_std_combined,
               confint_combined, confint_std_combined),
             ~ ifelse(.x == paste0("NA", sep_by, "NA"), "", .x))
    ) |>
    mutate(
      label = case_when(
        # Both standardized and unstandardized
        standardized == TRUE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_sig_combined, " (", est_sig_std_combined, ")\n", confint_combined),
                     paste0(est_sig_combined, " (", est_sig_std_combined, ")"))
            } else {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")")
            }
          } else {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_combined, " (", est_std_combined, ")\n", confint_combined),
                     paste0(est_combined, " (", est_std_combined, ")"))
            } else {
              paste0(est_combined, " (", est_std_combined, ")")
            }
          },

        # Only standardized
        standardized == TRUE & unstandardized == FALSE ~
          if (p_val) {
            if (conf_int) {
              ifelse(confint_std_combined != "",
                     paste0(est_sig_std_combined, "\n", confint_std_combined),
                     est_sig_std_combined)
            } else {
              est_sig_std_combined
            }
          } else {
            if (conf_int) {
              ifelse(confint_std_combined != "",
                     paste0(est_std_combined, "\n", confint_std_combined),
                     est_std_combined)
            } else {
              est_std_combined
            }
          },

        # Only unstandardized
        standardized == FALSE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_sig_combined, "\n", confint_combined),
                     est_sig_combined)
            } else {
              est_sig_combined
            }
          } else {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_combined, "\n", confint_combined),
                     est_combined)
            } else {
              est_combined
            }
          },

        # Neither standardized nor unstandardized
        standardized == FALSE & unstandardized == FALSE ~
          if (conf_int) {
            # Show unstandardized confidence interval without "\n"
            confint_combined
          } else {
            ""
          },

        TRUE ~ ""  # Fallback
      )
    ) |>
    left_join(original_edges_order, by = c("lhs", "op", "rhs")) |>
    arrange(original_order) |>
    select(-original_order)

  original_nodes_order <- tidysem_obj$nodes |>
    distinct(name, op, rhs) |>
    mutate(original_order = row_number())

  # Combine nodes (if nodes exist)
  if (!is.null(tidysem_obj$nodes)) {
    tidysem_obj$nodes <- tidysem_obj$nodes |>
      mutate(
        param_name = ifelse(op == "~1", paste0("Means.", name), NA_character_),
        group_num = match(group, unique(group)),
        param_name_full = ifelse(group_num == 1,
                                 param_name,
                                 paste0(param_name, ".g", group_num))
      ) |>
      left_join(hpd_intervals |> select(parameter, lower, upper),
                by = c("param_name_full" = "parameter")) |>
      mutate(
        confint = ifelse(!is.na(lower),
                         paste0("[", round(lower, 2), ", ", round(upper, 2), "]"),
                         NA_character_),
        confint_std = confint,
        est_sig = if (p_val) add_significance_stars(est, lower, upper) else est,
        est_sig_std = if (p_val) add_significance_stars(est_std, lower, upper) else est_std
      ) |>
      select(-param_name, -param_name_full, -group_num, -lower, -upper)

    nodes_combined <- tidysem_obj$nodes |>
      group_by(name, op, rhs) |>
      summarise(
        est_combined = if (all(c(group1, group2) %in% group)) {
          paste(round(est[group == group1], 2),
                round(est[group == group2], 2), sep = sep_by)
        } else {
          NA_character_
        },
        est_sig_combined = if (all(c(group1, group2) %in% group)) {
          paste(est_sig[group == group1],
                est_sig[group == group2], sep = sep_by)
        } else {
          NA_character_
        },
        est_std_combined = if (all(c(group1, group2) %in% group)) {
          paste(round(est_std[group == group1], 2),
                round(est_std[group == group2], 2), sep = sep_by)
        } else {
          NA_character_
        },
        est_sig_std_combined = if (all(c(group1, group2) %in% group)) {
          paste(est_sig_std[group == group1],
                est_sig_std[group == group2], sep = sep_by)
        } else {
          NA_character_
        },
        confint_combined = if (all(c(group1, group2) %in% group)) {
          paste(confint[group == group1],
                confint[group == group2], sep = sep_by)
        } else {
          NA_character_
        },
        confint_std_combined = if (all(c(group1, group2) %in% group)) {
          paste(confint_std[group == group1],
                confint_std[group == group2], sep = sep_by)
        } else {
          NA_character_
        },
        across(c(x, y, shape, node_xmin, node_xmax, node_ymin, node_ymax,
                 lhs, block, show, label_results), first),
        .groups = 'drop'
      ) |>
      filter(!is.na(est_combined)) |>
      # CLEANUP: Replace "NA | NA" and NA with empty strings in confint columns for nodes too
      mutate(
        confint_combined = ifelse(is.na(confint_combined) | confint_combined == paste0("NA ", sep_by, " NA"), "", confint_combined),
        confint_std_combined = ifelse(is.na(confint_std_combined) | confint_std_combined == paste0("NA ", sep_by, " NA"), "", confint_std_combined)
      ) |>
      mutate(
        label = case_when(
          standardized == TRUE & unstandardized == TRUE ~
            if (p_val) {
              if (conf_int) {
                ifelse(confint_combined != "",
                       paste0(est_sig_combined, " (", est_sig_std_combined, ")\n", confint_combined),
                       paste0(est_sig_combined, " (", est_sig_std_combined, ")"))
              } else {
                paste0(est_sig_combined, " (", est_sig_std_combined, ")")
              }
            } else {
              if (conf_int) {
                ifelse(confint_combined != "",
                       paste0(est_combined, " (", est_std_combined, ")\n", confint_combined),
                       paste0(est_combined, " (", est_std_combined, ")"))
              } else {
                paste0(est_combined, " (", est_std_combined, ")")
              }
            },

          # Only standardized
          standardized == TRUE & unstandardized == FALSE ~
            if (p_val) {
              if (conf_int) {
                ifelse(confint_std_combined != "",
                       paste0(est_sig_std_combined, "\n", confint_std_combined),
                       est_sig_std_combined)
              } else {
                est_sig_std_combined
              }
            } else {
              if (conf_int) {
                ifelse(confint_std_combined != "",
                       paste0(est_std_combined, "\n", confint_std_combined),
                       est_std_combined)
              } else {
                est_std_combined
              }
            },

          # Only unstandardized
          standardized == FALSE & unstandardized == TRUE ~
            if (p_val) {
              if (conf_int) {
                ifelse(confint_combined != "",
                       paste0(est_sig_combined, "\n", confint_combined),
                       est_sig_combined)
              } else {
                est_sig_combined
              }
            } else {
              if (conf_int) {
                ifelse(confint_combined != "",
                       paste0(est_combined, "\n", confint_combined),
                       est_combined)
              } else {
                est_combined
              }
            },

          # Neither standardized nor unstandardized
          standardized == FALSE & unstandardized == FALSE ~
            if (conf_int) {
              # Show unstandardized confidence interval without "\n"
              confint_combined
            } else {
              ""
            },

          TRUE ~ ""  # Fallback
        )
      )  |>
      left_join(original_nodes_order, by = c("name", "op", "rhs")) |>
      arrange(original_order) |>
      select(-original_order)

    tidysem_obj$nodes <- nodes_combined
  }

  # Update the object
  tidysem_obj$edges <- edges_combined

  return(tidysem_obj)
}

#' Combine tidySEM Objects for Group Comparison
#'
#' This internal function combines two tidySEM graph objects into a single object
#' for group comparison. It merges parameter estimates, confidence intervals,
#' and significance indicators from both groups for visualization.
#'
#' @param tidysem_obj1 A tidySEM graph object for the first group.
#' @param tidysem_obj2 A tidySEM graph object for the second group.
#' @param group1 Character string specifying the name for the first group
#'   (default: "Group1").
#' @param group2 Character string specifying the name for the second group
#'   (default: "Group2").
#' @param sep_by Character string used to separate group values in the output
#'   (default: " | ").
#' @param standardized Logical indicating whether to include standardized estimates
#'   in the output (default: FALSE).
#' @param unstandardized Logical indicating whether to include unstandardized estimates
#'   in the output (default: TRUE).
#' @param p_val Logical indicating whether to include significance indicators
#'   in the output (default: TRUE).
#' @param conf_int Logical indicating whether to include confidence intervals
#'   in the output (default: FALSE).
#'
#' @return A combined tidySEM object
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Assuming tidysem_obj1 and tidysem_obj2 exist
#' combined <- combine_tidysem_objects(
#'   tidysem_obj1, tidysem_obj2,
#'   group1 = "Control", group2 = "Treatment",
#'   standardized = TRUE, unstandardized = TRUE,
#'   p_val = TRUE, conf_int = TRUE
#' )
#' }
#'
#'
#' @importFrom dplyr distinct mutate left_join select group_by filter summarise across arrange bind_rows n first any_of row_number
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom methods is
combine_tidysem_objects <- function(tidysem_obj1, tidysem_obj2, group1 = "Group1", group2 = "Group2",
                                    sep_by = " | ", standardized = FALSE, unstandardized = TRUE,
                                    p_val = TRUE, conf_int = FALSE) {

  if (!all(c("edges", "nodes") %in% names(tidysem_obj1)) ||
      !all(c("edges", "nodes") %in% names(tidysem_obj2))) {
    stop("Both objects must be tidySEM graph objects with 'edges' and 'nodes' components")
  }

  tidysem_obj1$edges$group <- group1
  tidysem_obj1$nodes$group <- group1

  tidysem_obj2$edges$group <- group2
  tidysem_obj2$nodes$group <- group2

  original_edges_order <- tidysem_obj1$edges |>
    distinct(lhs, op, rhs) |>
    mutate(original_order = row_number())

  edges_combined <- bind_rows(tidysem_obj1$edges, tidysem_obj2$edges)

  nodes_combined <- tidysem_obj1$nodes

  edges_combined <- edges_combined |>
    mutate(across(any_of(c("est", "est_std")), ~ as.numeric(as.character(.x))))

  nodes_combined <- nodes_combined |>
    mutate(across(any_of(c("est", "est_std")), ~ as.numeric(as.character(.x))))

  edges_final <- edges_combined |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>  # Only keep parameters that exist in both groups
    summarise(
      est_combined = paste(
        round(est[group == group1], 2),
        round(est[group == group2], 2),
        sep = sep_by
      ),
      est_sig_combined = if ("est_sig" %in% names(edges_combined)) {
        paste(
          est_sig[group == group1],
          est_sig[group == group2],
          sep = sep_by
        )
      } else {
        paste(
          round(est[group == group1], 2),
          round(est[group == group2], 2),
          sep = sep_by
        )
      },
      est_std_combined = if ("est_std" %in% names(edges_combined)) {
        paste(
          round(est_std[group == group1], 2),
          round(est_std[group == group2], 2),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      est_sig_std_combined = if (all(c("est_sig_std") %in% names(edges_combined))) {
        paste(
          est_sig_std[group == group1],
          est_sig_std[group == group2],
          sep = sep_by
        )
      } else if ("est_std" %in% names(edges_combined)) {
        paste(
          round(est_std[group == group1], 2),
          round(est_std[group == group2], 2),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      confint_combined = if ("confint" %in% names(edges_combined)) {
        paste(
          confint[group == group1],
          confint[group == group2],
          sep = sep_by
        )
      } else {
        NA_character_
      },
      confint_std_combined = if ("confint_std" %in% names(edges_combined)) {
        paste(
          confint_std[group == group1],
          confint_std[group == group2],
          sep = sep_by
        )
      } else {
        NA_character_
      },
      across(any_of(c("from", "to", "arrow", "connect_from", "connect_to",
                      "curvature", "linetype", "block", "show", "label_results")),
             ~ first(na.omit(.x))),
      .groups = 'drop'
    ) |>
    mutate(
      label = case_when(
        # Case 1: Both standardized and unstandardized are FALSE
        standardized == FALSE & unstandardized == FALSE ~
          if (conf_int) {
            # Show unstandardized confidence interval without "\n"
            confint_combined
          } else {
            ""
          },

        # Case 2: Both standardized and unstandardized
        standardized == TRUE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")\n", confint_combined)
            } else {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")")
            }
          } else {
            if (conf_int) {
              paste0(est_combined, " (", est_std_combined, ")\n", confint_combined)
            } else {
              paste0(est_combined, " (", est_std_combined, ")")
            }
          },

        # Case 3: Only standardized
        standardized == TRUE & unstandardized == FALSE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_std_combined, "\n", confint_std_combined)
            } else {
              est_sig_std_combined
            }
          } else {
            if (conf_int) {
              paste0(est_std_combined, "\n", confint_std_combined)
            } else {
              est_std_combined
            }
          },

        # Case 4: Only unstandardized
        standardized == FALSE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              paste0(est_sig_combined, "\n", confint_combined)
            } else {
              est_sig_combined
            }
          } else {
            if (conf_int) {
              paste0(est_combined, "\n", confint_combined)
            } else {
              est_combined
            }
          },

        # Fallback (shouldn't be reached)
        TRUE ~ ""
      )
    ) |>
    left_join(original_edges_order, by = c("lhs", "op", "rhs")) |>
    arrange(original_order) |>
    select(-original_order)

  nodes_combined <- nodes_combined |> select(-group)

  combined_tidysem <- list(
    edges = edges_final,
    nodes = nodes_combined
  )

  if (!is.null(attr(tidysem_obj1, "class"))) {
    class(combined_tidysem) <- class(tidysem_obj1)
  }

  return(combined_tidysem)
}

#' Combine tidySEM Objects for Bayesian Group Comparison
#'
#' This internal function combines two tidySEM graph objects from Bayesian SEM models
#' into a single object for group comparison. It merges parameter estimates,
#' HPD intervals, and significance indicators from both groups.
#'
#' @param tidysem_obj1 A tidySEM graph object for the first group.
#' @param tidysem_obj2 A tidySEM graph object for the second group.
#' @param blavaan_fit1 A fitted Bayesian SEM object from blavaan for the first group.
#' @param blavaan_fit2 A fitted Bayesian SEM object from blavaan for the second group.
#' @param group1 Character string specifying the name for the first group
#'   (default: "Group1").
#' @param group2 Character string specifying the name for the second group
#'   (default: "Group2").
#' @param sep_by Character string used to separate group values in the output
#'   (default: " | ").
#' @param standardized Logical indicating whether to include standardized estimates
#'   in the output (default: FALSE).
#' @param unstandardized Logical indicating whether to include unstandardized estimates
#'   in the output (default: TRUE).
#' @param p_val Logical indicating whether to include significance stars based on
#'   HPD intervals (default: TRUE).
#' @param conf_int Logical indicating whether to include confidence intervals
#'   in the output (default: FALSE).
#' @param ci_level Numeric value specifying the confidence level for HPD intervals
#'   (default: 0.95).
#'
#' @return A combined tidySEM object with bayesian stats
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Assuming tidysem_obj1, tidysem_obj2, blavaan_fit1, and blavaan_fit2 exist
#' combined <- combine_tidysem_objects_bayes(
#'   tidysem_obj1, tidysem_obj2, blavaan_fit1, blavaan_fit2,
#'   group1 = "Control", group2 = "Treatment",
#'   standardized = TRUE, unstandardized = TRUE,
#'   p_val = TRUE, conf_int = TRUE
#' )
#' }
#'
#' @importFrom blavaan blavInspect
#' @importFrom dplyr distinct mutate left_join select group_by filter summarise across arrange bind_rows n first any_of row_number
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom methods is
combine_tidysem_objects_bayes <- function(tidysem_obj1, tidysem_obj2, blavaan_fit1, blavaan_fit2,
                                          group1 = "Group1", group2 = "Group2",
                                          sep_by = " | ", standardized = FALSE, unstandardized = TRUE,
                                          p_val = TRUE, conf_int = FALSE, ci_level = 0.95) {

  if (!all(c("edges", "nodes") %in% names(tidysem_obj1)) ||
      !all(c("edges", "nodes") %in% names(tidysem_obj2))) {
    stop("Both objects must be tidySEM graph objects with 'edges' and 'nodes' components")
  }

  hpd_intervals1 <- as.data.frame(blavaan::blavInspect(blavaan_fit1, "hpd", level = ci_level))
  hpd_intervals1$parameter <- rownames(hpd_intervals1)

  hpd_intervals2 <- as.data.frame(blavaan::blavInspect(blavaan_fit2, "hpd", level = ci_level))
  hpd_intervals2$parameter <- rownames(hpd_intervals2)

  add_significance_stars <- function(est_values, lower_values, upper_values) {
    significant <- !is.na(lower_values) & !is.na(upper_values) &
      (lower_values > 0 | upper_values < 0)
    ifelse(significant, paste0(est_values, "*"), est_values)
  }

  original_edges_order <- tidysem_obj1$edges |>
    distinct(lhs, op, rhs) |>
    mutate(original_order = row_number())

  tidysem_obj1$edges <- tidysem_obj1$edges |>
    mutate(
      param_name = case_when(
        op == "=~" ~ paste0(lhs, op, rhs),
        op == "~~" & lhs != rhs ~ paste0(lhs, op, rhs),
        op == "~~" & lhs == rhs ~ paste0("Variances.", lhs),
        op == "~1" ~ paste0("Means.", lhs),
        TRUE ~ paste0(lhs, op, rhs)
      )
    ) |>
    left_join(hpd_intervals1 |> select(parameter, lower, upper),
              by = c("param_name" = "parameter")) |>
    mutate(
      confint = ifelse(!is.na(lower),
                       paste0("[", round(lower, 2), ", ", round(upper, 2), "]"),
                       NA_character_),
      confint_std = confint,
      est_sig = if (p_val) add_significance_stars(est, lower, upper) else as.character(est),
      est_sig_std = if (p_val) add_significance_stars(est_std, lower, upper) else as.character(est_std),
      group = group1
    ) |>
    select(-param_name, -lower, -upper)

  tidysem_obj2$edges <- tidysem_obj2$edges |>
    mutate(
      param_name = case_when(
        op == "=~" ~ paste0(lhs, op, rhs),
        op == "~~" & lhs != rhs ~ paste0(lhs, op, rhs),
        op == "~~" & lhs == rhs ~ paste0("Variances.", lhs),
        op == "~1" ~ paste0("Means.", lhs),
        TRUE ~ paste0(lhs, op, rhs)
      )
    ) |>
    left_join(hpd_intervals2 |> select(parameter, lower, upper),
              by = c("param_name" = "parameter")) |>
    mutate(
      confint = ifelse(!is.na(lower),
                       paste0("[", round(lower, 2), ", ", round(upper, 2), "]"),
                       NA_character_),
      confint_std = confint,
      est_sig = if (p_val) add_significance_stars(est, lower, upper) else as.character(est),
      est_sig_std = if (p_val) add_significance_stars(est_std, lower, upper) else as.character(est_std),
      group = group2
    ) |>
    select(-param_name, -lower, -upper)

  edges_combined <- bind_rows(tidysem_obj1$edges, tidysem_obj2$edges)

  edges_combined <- edges_combined |>
    mutate(across(any_of(c("est", "est_std")), ~ as.numeric(as.character(.x))))
  edges_final <- edges_combined |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>  # Only keep parameters that exist in both groups
    summarise(
      est_combined = paste(
        ifelse(is.na(est[group == group1]), "NA", round(est[group == group1], 2)),
        ifelse(is.na(est[group == group2]), "NA", round(est[group == group2], 2)),
        sep = sep_by
      ),
      est_sig_combined = paste(
        ifelse(is.na(est_sig[group == group1]), "NA", est_sig[group == group1]),
        ifelse(is.na(est_sig[group == group2]), "NA", est_sig[group == group2]),
        sep = sep_by
      ),
      est_std_combined = if ("est_std" %in% names(edges_combined)) {
        paste(
          ifelse(is.na(est_std[group == group1]), "NA", round(est_std[group == group1], 2)),
          ifelse(is.na(est_std[group == group2]), "NA", round(est_std[group == group2], 2)),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      est_sig_std_combined = if ("est_sig_std" %in% names(edges_combined)) {
        paste(
          ifelse(is.na(est_sig_std[group == group1]), "NA", est_sig_std[group == group1]),
          ifelse(is.na(est_sig_std[group == group2]), "NA", est_sig_std[group == group2]),
          sep = sep_by
        )
      } else if ("est_std" %in% names(edges_combined)) {
        paste(
          ifelse(is.na(est_std[group == group1]), "NA", round(est_std[group == group1], 2)),
          ifelse(is.na(est_std[group == group2]), "NA", round(est_std[group == group2], 2)),
          sep = sep_by
        )
      } else {
        NA_character_
      },
      confint_combined = paste(
        ifelse(is.na(confint[group == group1]), "NA", confint[group == group1]),
        ifelse(is.na(confint[group == group2]), "NA", confint[group == group2]),
        sep = sep_by
      ),
      confint_std_combined = paste(
        ifelse(is.na(confint_std[group == group1]), "NA", confint_std[group == group1]),
        ifelse(is.na(confint_std[group == group2]), "NA", confint_std[group == group2]),
        sep = sep_by
      ),
      across(any_of(c("from", "to", "arrow", "connect_from", "connect_to",
                      "curvature", "linetype", "block", "show", "label_results")),
             ~ first(na.omit(.x))),
      .groups = 'drop'
    ) |>
    # Clean up NA values in confidence intervals
    mutate(
      across(c(est_combined, est_sig_combined, est_std_combined, est_sig_std_combined,
               confint_combined, confint_std_combined),
             ~ ifelse(.x == paste0("NA", sep_by, "NA"), "", .x))
    ) |>
    mutate(
      label = case_when(
        # Case 1: Both standardized and unstandardized are FALSE
        standardized == FALSE & unstandardized == FALSE ~
          if (conf_int) {
            # Show unstandardized confidence interval without "\n"
            confint_combined
          } else {
            ""
          },

        # Case 2: Both standardized and unstandardized
        standardized == TRUE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_sig_combined, " (", est_sig_std_combined, ")\n", confint_combined),
                     paste0(est_sig_combined, " (", est_sig_std_combined, ")"))
            } else {
              paste0(est_sig_combined, " (", est_sig_std_combined, ")")
            }
          } else {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_combined, " (", est_std_combined, ")\n", confint_combined),
                     paste0(est_combined, " (", est_std_combined, ")"))
            } else {
              paste0(est_combined, " (", est_std_combined, ")")
            }
          },

        # Case 3: Only standardized
        standardized == TRUE & unstandardized == FALSE ~
          if (p_val) {
            if (conf_int) {
              ifelse(confint_std_combined != "",
                     paste0(est_sig_std_combined, "\n", confint_std_combined),
                     est_sig_std_combined)
            } else {
              est_sig_std_combined
            }
          } else {
            if (conf_int) {
              ifelse(confint_std_combined != "",
                     paste0(est_std_combined, "\n", confint_std_combined),
                     est_std_combined)
            } else {
              est_std_combined
            }
          },

        # Case 4: Only unstandardized
        standardized == FALSE & unstandardized == TRUE ~
          if (p_val) {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_sig_combined, "\n", confint_combined),
                     est_sig_combined)
            } else {
              est_sig_combined
            }
          } else {
            if (conf_int) {
              ifelse(confint_combined != "",
                     paste0(est_combined, "\n", confint_combined),
                     est_combined)
            } else {
              est_combined
            }
          },

        # Fallback (shouldn't be reached)
        TRUE ~ ""
      )
    ) |>
    left_join(original_edges_order, by = c("lhs", "op", "rhs")) |>
    arrange(original_order) |>
    select(-original_order)

  nodes_final <- tidysem_obj1$nodes

  # Create the combined tidySEM object
  combined_tidysem <- list(
    edges = edges_final,
    nodes = nodes_final
  )

  if (!is.null(attr(tidysem_obj1, "class"))) {
    class(combined_tidysem) <- class(tidysem_obj1)
  }

  return(combined_tidysem)
}

#' Combine Bayesian SEM Model Parameters for Group Comparison
#'
#' This internal function combines parameter estimates, standardized solutions,
#' and HPD intervals from two Bayesian structural equation models for group comparison.
#' It creates a comparison table showing parameter estimates and credible intervals
#' for both groups side by side.
#'
#' @param fit1 A fitted Bayesian SEM object from blavaan for the first group.
#' @param fit2 A fitted Bayesian SEM object from blavaan for the second group.
#' @param group1 Character string specifying the name for the first group
#'   (default: "Group1").
#' @param group2 Character string specifying the name for the second group
#'   (default: "Group2").
#' @param sep_by Character string used to separate group values in the output
#'   (default: "|").
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{lhs}: Left-hand side variable in the SEM parameter
#'   \item \code{op}: Operator type ("=~" for loadings, "~~" for covariances, "~1" for intercepts)
#'   \item \code{rhs}: Right-hand side variable in the SEM parameter
#'   \item \code{est}: Unstandardized parameter estimates formatted as "group1|group2"
#'   \item \code{std}: Standardized parameter estimates formatted as "group1|group2"
#'   \item \code{confint_unstd}: HPD credible intervals formatted as "[lower1,upper1]|[lower2,upper2]"
#' }
#'
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Assuming fit1 and fit2 are fitted Bayesian SEM models
#' comparison <- combine_model_parameters_bayes(fit1, fit2,
#'                                              group1 = "Control",
#'                                              group2 = "Treatment")
#' print(comparison)
#' }
#' @importFrom lavaan parameterEstimates standardizedSolution
#' @importFrom blavaan blavInspect
#' @importFrom dplyr group_by filter summarise mutate rowwise ungroup select inner_join n case_when
#' @importFrom rlang .data
combine_model_parameters_bayes <- function(fit1, fit2, group1 = "Group1", group2 = "Group2", sep_by = "|") {
  # Get parameter estimates for both models
  params1 <- lavaan::parameterEstimates(fit1)
  params2 <- lavaan::parameterEstimates(fit2)

  # Get standardized solutions for both models
  std_sol1 <- lavaan::standardizedSolution(fit1)
  std_sol2 <- lavaan::standardizedSolution(fit2)

  # Get HPD intervals for both models
  hpd1 <- as.data.frame(blavaan::blavInspect(fit1, "hpd"))
  hpd2 <- as.data.frame(blavaan::blavInspect(fit2, "hpd"))

  params1$group_name <- group1
  params2$group_name <- group2
  std_sol1$group_name <- group1
  std_sol2$group_name <- group2

  combined_params <- rbind(params1, params2)
  combined_std <- rbind(std_sol1, std_sol2)

  combined_params <- combined_params[combined_params$op %in% c("=~", "~~", "~1", "~"), ]
  combined_std <- combined_std[combined_std$op %in% c("=~", "~~", "~1", "~"), ]

  # combined_params <- combined_params[!(combined_params$op == "~~" & combined_params$lhs == combined_params$rhs), ]
  # combined_std <- combined_std[!(combined_std$op == "~~" & combined_std$lhs == combined_std$rhs), ]

  create_param_name <- function(lhs, op, rhs) {
    dplyr::case_when(
      op == "=~" ~ paste0(lhs, "=~", rhs),
      op == "~~" & lhs != rhs ~ paste0(lhs, "~~", rhs),
      op == "~~" & lhs == rhs ~ paste0(lhs, "~~", rhs),
      op == "~1" ~ paste0(lhs, "~1"),
      TRUE ~ paste0(lhs, op, rhs)
    )
  }

  comparison_unstd <- combined_params |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>
    summarise(
      est = paste0(
        round(est[group_name == group1], 2), sep_by,
        round(est[group_name == group2], 2)
      ),
      group1_est = est[group_name == group1],
      group2_est = est[group_name == group2],
      .groups = 'drop'
    )

  comparison_unstd <- comparison_unstd |>
    mutate(
      param_name_group1 = create_param_name(lhs, op, rhs),
      param_name_group2 = create_param_name(lhs, op, rhs)
    ) |>
    rowwise() |>
    mutate(
      group1_hpd_lower = ifelse(param_name_group1 %in% rownames(hpd1),
                                hpd1[param_name_group1, "lower"], NA_real_),
      group1_hpd_upper = ifelse(param_name_group1 %in% rownames(hpd1),
                                hpd1[param_name_group1, "upper"], NA_real_),
      group2_hpd_lower = ifelse(param_name_group2 %in% rownames(hpd2),
                                hpd2[param_name_group2, "lower"], NA_real_),
      group2_hpd_upper = ifelse(param_name_group2 %in% rownames(hpd2),
                                hpd2[param_name_group2, "upper"], NA_real_)
    ) |>
    ungroup() |>
    mutate(
      confint_unstd = paste0(
        "[", round(group1_hpd_lower, 2), ",", round(group1_hpd_upper, 2), "]",
        sep_by,
        "[", round(group2_hpd_lower, 2), ",", round(group2_hpd_upper, 2), "]"
      )
    )

  comparison_std <- combined_std |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>
    summarise(
      std = paste0(
        round(est.std[group_name == group1], 2), sep_by,
        round(est.std[group_name == group2], 2)
      ),
      group1_std = est.std[group_name == group1],
      group2_std = est.std[group_name == group2],
      .groups = 'drop'
    )

  comparison_table <- comparison_unstd |>
    inner_join(comparison_std, by = c("lhs", "op", "rhs")) |>
    select(lhs, op, rhs, est, std, confint_unstd)

  return(comparison_table)
}

#' Combine SEM Model Parameters for Group Comparison
#'
#' This internal function combines parameter estimates, standardized solutions,
#' and confidence intervals from two structural equation models for group comparison.
#' It creates a comprehensive comparison table showing both unstandardized and
#' standardized parameter estimates with confidence intervals for both groups.
#'
#' @param fit1 A fitted SEM object from lavaan for the first group.
#' @param fit2 A fitted SEM object from lavaan for the second group.
#' @param group1 Character string specifying the name for the first group
#'   (default: "Group1").
#' @param group2 Character string specifying the name for the second group
#'   (default: "Group2").
#' @param sep_by Character string used to separate group values in the output
#'   (default: "|").
#' @param ci_level Numeric value specifying the confidence level for intervals
#'   (default: 0.95, for 95% confidence intervals).
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{lhs}: Left-hand side variable in the SEM parameter
#'   \item \code{op}: Operator type ("=~" for loadings, "~~" for covariances, "~1" for intercepts)
#'   \item \code{rhs}: Right-hand side variable in the SEM parameter
#'   \item \code{est}: Unstandardized parameter estimates formatted as "group1|group2"
#'   \item \code{std}: Standardized parameter estimates formatted as "group1|group2"
#'   \item \code{confint_unstd}: Unstandardized confidence intervals formatted as "[lower1,upper1]|[lower2,upper2]"
#'   \item \code{confint_std}: Standardized confidence intervals formatted as "[lower1,upper1]|[lower2,upper2]"
#' }
#'
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Assuming fit1 and fit2 are fitted SEM models from lavaan
#' comparison <- combine_model_parameters(fit1, fit2,
#'                                       group1 = "Control",
#'                                       group2 = "Treatment",
#'                                       ci_level = 0.95)
#' }
#'
#'
#' @importFrom lavaan parameterEstimates standardizedSolution lavInspect
#' @importFrom dplyr  group_by filter summarise mutate select inner_join n bind_rows rename
#' @importFrom rlang .data
combine_model_parameters <- function(fit1, fit2, group1 = "Group1", group2 = "Group2", sep_by = "|", ci_level = 0.95) {
  extract_params <- function(fit, target_group) {
    group_info <- lavInspect(fit, "group.label")

    extract_and_label <- function(extract_func) {
      df <- extract_func(fit, ci = TRUE, level = ci_level)

      if (length(group_info) > 0) {
        group_mapping <- setNames(group_info, seq_along(group_info))
        df <- df |>
          mutate(group = ifelse(group %in% names(group_mapping),
                                group_mapping[as.character(group)], group)) |>
          filter(group == target_group)
      } else {
        df$group <- target_group
      }

      df
    }

    list(
      unstd = extract_and_label(parameterEstimates),
      std = extract_and_label(standardizedSolution)
    )
  }

  params1 <- extract_params(fit1, group1)
  params2 <- extract_params(fit2, group2)

  combine_and_filter <- function(sol1, sol2) {
    combined <- bind_rows(sol1, sol2)
    combined[combined$op %in% c("=~", "~~", "~1", "~"), ]
  }

  combined_unstd <- combine_and_filter(params1$unstd, params2$unstd)
  combined_std <- combine_and_filter(params1$std, params2$std)

  create_comparison <- function(combined_df, value_col, round_digits = 2) {
    combined_df |>
      group_by(lhs, op, rhs) |>
      filter(n() == 2) |>
      summarise(
        value = paste0(
          round(.data[[value_col]][group == group1], round_digits),
          sep_by,
          round(.data[[value_col]][group == group2], round_digits)
        ),
        confint = paste0(
          "[", round(ci.lower[group == group1], round_digits), ",",
          round(ci.upper[group == group1], round_digits), "]", sep_by,
          "[", round(ci.lower[group == group2], round_digits), ",",
          round(ci.upper[group == group2], round_digits), "]"
        ),
        .groups = 'drop'
      )
  }

  comparison_unstd <- create_comparison(combined_unstd, "est") |>
    rename(est = value, confint_unstd = confint)

  comparison_std <- create_comparison(combined_std, "est.std") |>
    rename(std = value, confint_std = confint)

  inner_join(comparison_unstd, comparison_std, by = c("lhs", "op", "rhs")) |>
    select(lhs, op, rhs, est, std, confint_unstd, confint_std)
}

# Get estimate differences (frequentist)
#' @importFrom lavaan lavInspect parameterEstimates
#' @importFrom dplyr mutate group_by filter n summarise ungroup bind_rows
#' @importFrom stats pnorm setNames
#' @keywords internal
#' @noRd
get_est_differences <- function(fit,
                                alpha = 0.05,
                                p_adjust = "none",
                                test_type = "pairwise",
                                group1 = "",
                                group2 = "") {

  group_info <- lavInspect(fit, "group.label")
  group_mapping <- setNames(group_info, 1:length(group_info))

  params <- parameterEstimates(fit) |>
    mutate(group = ifelse(group %in% names(group_mapping),
                          group_mapping[as.character(group)],
                          group))

  params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]

  pairwise_results <- data.frame()

  multi_group_params <- params |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() >= 2) |>
    ungroup()

  if (test_type == "pairwise") {
    if (group1 != "" && group2 != "") {
      all_groups <- unique(multi_group_params$group)
      if (!group1 %in% all_groups) {
        stop("Group '", group1, "' not found. Available groups: ", paste(all_groups, collapse = ", "))
      }
      if (!group2 %in% all_groups) {
        stop("Group '", group2, "' not found. Available groups: ", paste(all_groups, collapse = ", "))
      }

      group_levels <- c(group1, group2)
    } else {
      # Use all groups if none specified
      group_levels <- unique(multi_group_params$group)
    }

    for (i in 1:(length(group_levels)-1)) {
      for (j in (i+1):length(group_levels)) {
        current_group1 <- group_levels[i]
        current_group2 <- group_levels[j]

        pair_diffs <- multi_group_params |>
          group_by(lhs, op, rhs) |>
          filter(group %in% c(current_group1, current_group2)) |>
          filter(dplyr::n() == 2) |>
          summarise(
            comparison = paste(current_group1, "vs", current_group2),
            p_value = 2 * (1 - pnorm(abs(diff(est)) / sqrt(sum(se^2)))),
            .groups = 'drop'
          )


        pairwise_results <- bind_rows(pairwise_results, pair_diffs)
      }
    }
  }

  pairwise_results$significant <- pairwise_results$p_value < alpha

  results <- pairwise_results

  return(results)
}

# Get estimate differences (Bayesian)
#' @importFrom lavaan parameterEstimates
#' @importFrom blavaan blavInspect
#' @importFrom dplyr mutate group_by filter n group_map bind_rows
#' @importFrom stats setNames
#' @keywords internal
#' @noRd
get_est_differences_bayes <- function(fit,
                                      p_adjust = "none",
                                      test_type = "pairwise",
                                      group1 = "",
                                      group2 = "",
                                      rope = c(-0.1, 0.1)) {

  group_info <- blavaan::blavInspect(fit, "group.label")
  group_mapping <- setNames(group_info, 1:length(group_info))

  # Get parameter estimates and HPD intervals
  params <- lavaan::parameterEstimates(fit)
  hpd_intervals <- as.data.frame(blavaan::blavInspect(fit, "hpd"))

  params$group <- ifelse(params$group %in% names(group_mapping),
                         group_mapping[as.character(params$group)],
                         params$group)

  params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]

  pairwise_results <- data.frame()


  multi_group_params <- params |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() >= 2) |>
    ungroup()

  if (test_type == "pairwise") {
    if (group1 != "" && group2 != "") {
      all_groups <- unique(multi_group_params$group)
      if (!group1 %in% all_groups) {
        stop("Group '", group1, "' not found. Available groups: ", paste(all_groups, collapse = ", "))
      }
      if (!group2 %in% all_groups) {
        stop("Group '", group2, "' not found. Available groups: ", paste(all_groups, collapse = ", "))
      }
      group_levels <- c(group1, group2)
    } else {
      group_levels <- unique(multi_group_params$group)
    }

    for (i in 1:(length(group_levels)-1)) {
      for (j in (i+1):length(group_levels)) {
        current_group1 <- group_levels[i]
        current_group2 <- group_levels[j]

        group1_num <- which(group_info == current_group1)
        group2_num <- which(group_info == current_group2)

        pair_diffs <- multi_group_params |>
          group_by(lhs, op, rhs) |>
          filter(group %in% c(current_group1, current_group2)) |>
          filter(dplyr::n() == 2) |>
          group_map(~ {
            diff_est <- .x$est[1] - .x$est[2]

            # Get parameter names for HPD intervals
            param_name_base <- paste0(.x$lhs[1], .x$op[1], .x$rhs[1])
            param_name1 <- if (group1_num == 1) param_name_base else paste0(param_name_base, ".g", group1_num)
            param_name2 <- if (group2_num == 1) param_name_base else paste0(param_name_base, ".g", group2_num)

            # Get HPD intervals
            hpd_idx1 <- which(rownames(hpd_intervals) == param_name1)
            hpd_idx2 <- which(rownames(hpd_intervals) == param_name2)

            if (length(hpd_idx1) > 0 && length(hpd_idx2) > 0) {
              hpd1_lower <- hpd_intervals$lower[hpd_idx1]
              hpd1_upper <- hpd_intervals$upper[hpd_idx1]
              hpd2_lower <- hpd_intervals$lower[hpd_idx2]
              hpd2_upper <- hpd_intervals$upper[hpd_idx2]

              # Conservative bounds for difference
              diff_lower <- hpd1_lower - hpd2_upper
              diff_upper <- hpd1_upper - hpd2_lower

              # Bayesian significance tests
              credible_excludes_zero <- diff_lower > 0 | diff_upper < 0
              excludes_rope <- diff_lower > rope[2] | diff_upper < rope[1]

              data.frame(
                lhs = .x$lhs[1],
                op = .x$op[1],
                rhs = .x$rhs[1],
                comparison = paste(current_group1, "vs", current_group2),
                posterior_mean_diff = diff_est,
                credible_interval = paste0("[", round(diff_lower, 2), ", ", round(diff_upper, 2), "]"),
                excludes_zero = credible_excludes_zero,
                excludes_rope = excludes_rope,
                rope_lower = rope[1],
                rope_upper = rope[2],
                stringsAsFactors = FALSE
              )
            } else {
              data.frame(
                lhs = .x$lhs[1],
                op = .x$op[1],
                rhs = .x$rhs[1],
                comparison = paste(current_group1, "vs", current_group2),
                posterior_mean_diff = diff_est,
                credible_interval = NA,
                excludes_zero = NA,
                excludes_rope = NA,
                rope_lower = rope[1],
                rope_upper = rope[2],
                stringsAsFactors = FALSE
              )
            }
          }, .keep = TRUE) |>
          bind_rows()

        pairwise_results <- bind_rows(pairwise_results, pair_diffs)
      }
    }
  }

  if (nrow(pairwise_results) > 0) {
    pairwise_results$significant <- pairwise_results$excludes_zero
  }

  results <- pairwise_results
  return(results)
}

#' Generate Bayesian model comparison table
#'
#' Creates a comparison table for Bayesian structural equation models, including
#' parameter estimates, credible intervals, and significance tests for group differences.
#' This function extracts and compares parameter estimates between two groups in a
#' Bayesian SEM framework, incorporating Highest Posterior Density (HPD) intervals
#' and Region of Practical Equivalence (ROPE) tests.
#'
#' @param fit A blavaan object containing the fitted Bayesian SEM model
#' @param rope Numeric vector of length 2 specifying the Region of Practical Equivalence
#'   bounds. Parameters with credible intervals entirely outside this range are
#'   considered practically significant. Default is c(-0.1, 0.1)
#' @param group1 Character string specifying the first group for comparison.
#'   Must match a group label in the fitted model
#' @param group2 Character string specifying the second group for comparison.
#'   Must match a group label in the fitted model
#' @param sep_by Character separator used in the comparison columns to distinguish
#'   between group1 and group2 values. Default is "|"
#'
#' @return A data frame containing the comparison table
#'
#' @importFrom blavaan blavInspect
#' @importFrom lavaan parameterEstimates standardizedSolution
#' @importFrom dplyr filter mutate distinct group_by summarise rowwise ungroup arrange select rename inner_join left_join case_when row_number
#' @importFrom rlang .data
#'
#' @keywords internal
#' @noRd
get_comparison_table_bayes <- function(fit, rope = c(-0.1, 0.1), group1 = "", group2 = "", sep_by = "|") {
  sig_diffs <- get_est_differences_bayes(fit = fit, rope = rope, group1 = group1, group2 = group2)

  if (nrow(sig_diffs) == 0) {
    return(data.frame())
  }

  # Get group information for blavaan
  group_info <- blavaan::blavInspect(fit, "group.label")
  group_mapping <- setNames(group_info, 1:length(group_info))

  # Get parameter estimates and HPD intervals
  params <- lavaan::parameterEstimates(fit)
  hpd_intervals <- as.data.frame(blavaan::blavInspect(fit, "hpd"))

  params$group <- ifelse(params$group %in% names(group_mapping),
                         group_mapping[as.character(params$group)],
                         params$group)

  params <- params[params$op %in% c("=~", "~1", "~~", "~"), ]

  # Get standardized solutions for blavaan
  std_solution <- lavaan::standardizedSolution(fit)
  std_solution <- std_solution |>
    filter(op %in% c("=~", "~1", "~~", "~")) |>
    mutate(group = ifelse(group %in% 1:length(group_info),
                          group_info[group],
                          group))

  group1_order <- params |>
    filter(group == group1) |>
    distinct(lhs, op, rhs) |>
    mutate(order = row_number())

  # Get unstandardized comparisons with HPD intervals
  group_comparisons <- params |>
    filter(group %in% c(group1, group2)) |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>
    summarise(
      group1_est = est[group == group1],
      group2_est = est[group == group2],
      # Get group numbers for HPD interval lookup
      group1_num = which(group_info == group1),
      group2_num = which(group_info == group2),
      .groups = 'drop'
    ) |>
    mutate(
      # Create parameter names for HPD lookup
      param_base = dplyr::case_when(
        op == "=~" ~ paste0(lhs, op, rhs),
        op == "~~" & lhs != rhs ~ paste0(lhs, op, rhs),
        op == "~~" & lhs == rhs ~ paste0("Variances.", lhs),
        op == "~1" ~ paste0("Means.", lhs),
        TRUE ~ paste0(lhs, op, rhs)
      ),
      # Create parameter names with group suffixes
      param_group1 = if (first(group1_num) == 1) param_base else paste0(param_base, ".g", first(group1_num)),
      param_group2 = if (first(group2_num) == 1) param_base else paste0(param_base, ".g", first(group2_num))
    ) |>
    rowwise() |>
    mutate(
      # Get HPD intervals for each group
      group1_hpd_lower = ifelse(param_group1 %in% rownames(hpd_intervals),
                                hpd_intervals[param_group1, "lower"], NA_real_),
      group1_hpd_upper = ifelse(param_group1 %in% rownames(hpd_intervals),
                                hpd_intervals[param_group1, "upper"], NA_real_),
      group2_hpd_lower = ifelse(param_group2 %in% rownames(hpd_intervals),
                                hpd_intervals[param_group2, "lower"], NA_real_),
      group2_hpd_upper = ifelse(param_group2 %in% rownames(hpd_intervals),
                                hpd_intervals[param_group2, "upper"], NA_real_)
    ) |>
    ungroup() |>
    mutate(
      # Create formatted credible intervals
      confint_unstd = paste0(
        "[", round(group1_hpd_lower, 2), ",", round(group1_hpd_upper, 2), "]",
        sep_by,
        "[", round(group2_hpd_lower, 2), ",", round(group2_hpd_upper, 2), "]"
      )
    )

  std_comparisons <- std_solution |>
    filter(group %in% c(group1, group2)) |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>
    summarise(
      group1_std = est.std[group == group1],
      group2_std = est.std[group == group2],
      .groups = 'drop'
    )

  comparison_table <- sig_diffs |>
    inner_join(group_comparisons, by = c("lhs", "op", "rhs")) |>
    left_join(std_comparisons, by = c("lhs", "op", "rhs")) |>
    left_join(group1_order, by = c("lhs", "op", "rhs")) |>
    mutate(
      comparison_unstd = paste0(round(group1_est, 2), sep_by, round(group2_est, 2)),
      comparison_std = paste0(round(group1_std, 2), sep_by, round(group2_std, 2)),
    ) |>
    arrange(order) |>
    select(lhs, op, rhs, comparison_unstd, comparison_std, confint_unstd,
           group1_est, group2_est, group1_std, group2_std,
           posterior_mean_diff, credible_interval, excludes_zero, excludes_rope,
           rope_lower, rope_upper) |>
    rename(est = comparison_unstd, std = comparison_std, significant = excludes_zero) |>
    distinct(lhs, op, rhs, .keep_all = TRUE)

  return(comparison_table)
}

#' Generate frequentist model comparison table
#'
#' Creates a comparison table for frequentist structural equation models, including
#' parameter estimates, confidence intervals, and significance tests for group differences.
#' This function extracts and compares parameter estimates between two groups in a
#' frequentist SEM framework, incorporating Wald tests for group differences.
#'
#' @param fit A lavaan object containing the fitted SEM model with multiple groups
#' @param alpha Numeric significance level for determining significant differences.
#'   Default is 0.05
#' @param group1 Character string specifying the first group for comparison.
#'   Must match a group label in the fitted model
#' @param group2 Character string specifying the second group for comparison.
#'   Must match a group label in the fitted model
#' @param sep_by Character separator used in the comparison columns to distinguish
#'   between group1 and group2 values. Default is "|"
#'
#' @return A data frame containing the comparison table
#'
#' @importFrom lavaan lavInspect standardizedSolution
#' @importFrom dplyr filter mutate distinct group_by summarise arrange select rename inner_join left_join row_number
#' @importFrom rlang .data
#'
#' @keywords internal
#' @noRd
get_comparison_table <- function(fit, alpha = 0.05, group1 = "", group2 = "", sep_by = "|") {
  sig_diffs <- get_est_differences(fit = fit, alpha = alpha, group1 = group1, group2 = group2)

  if (nrow(sig_diffs) == 0) {
    return(data.frame())
  }

  params <- get_params_with_group_names(fit)

  # Get standardized solutions
  std_solution <- standardizedSolution(fit)
  std_solution <- std_solution |>
    filter(op %in% c("=~", "~1", "~~")) |>
    mutate(group = ifelse(group %in% 1:length(lavInspect(fit, "group.label")),
                          lavInspect(fit, "group.label")[group],
                          group))

  group1_order <- params |>
    filter(group == group1) |>
    distinct(lhs, op, rhs) |>
    mutate(order = row_number())

  # Get unstandardized comparisons with confidence intervals
  group_comparisons <- params |>
    filter(group %in% c(group1, group2)) |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>
    summarise(
      group1_est = est[group == group1],
      group2_est = est[group == group2],
      group1_ci_lower = ci.lower[group == group1],
      group1_ci_upper = ci.upper[group == group1],
      group2_ci_lower = ci.lower[group == group2],
      group2_ci_upper = ci.upper[group == group2],
      .groups = 'drop'
    )

  # Get standardized comparisons with confidence intervals
  std_comparisons <- std_solution |>
    filter(group %in% c(group1, group2)) |>
    group_by(lhs, op, rhs) |>
    filter(dplyr::n() == 2) |>
    summarise(
      group1_std = est.std[group == group1],
      group2_std = est.std[group == group2],
      group1_std_ci_lower = ci.lower[group == group1],
      group1_std_ci_upper = ci.upper[group == group1],
      group2_std_ci_lower = ci.lower[group == group2],
      group2_std_ci_upper = ci.upper[group == group2],
      .groups = 'drop'
    )

  comparison_table <- sig_diffs |>
    inner_join(group_comparisons, by = c("lhs", "op", "rhs")) |>
    left_join(std_comparisons, by = c("lhs", "op", "rhs")) |>
    left_join(group1_order, by = c("lhs", "op", "rhs")) |>
    mutate(
      comparison_unstd = paste0(round(group1_est, 2),  sep_by , round(group2_est, 2)),
      comparison_std = paste0(round(group1_std, 2), sep_by, round(group2_std, 2)),
      # Add confidence intervals
      confint_unstd = paste0(
        "[", round(group1_ci_lower, 2), ",", round(group1_ci_upper, 2), "]",
        sep_by,
        "[", round(group2_ci_lower, 2), ",", round(group2_ci_upper, 2), "]"
      ),
      confint_std = paste0(
        "[", round(group1_std_ci_lower, 2), ",", round(group1_std_ci_upper, 2), "]",
        sep_by,
        "[", round(group2_std_ci_lower, 2), ",", round(group2_std_ci_upper, 2), "]"
      )
    ) |>
    arrange(order) |>
    select(lhs, op, rhs, comparison_unstd, comparison_std, confint_unstd, confint_std,
           group1_est, group2_est, group1_std, group2_std,
           group1_ci_lower, group1_ci_upper, group2_ci_lower, group2_ci_upper,
           group1_std_ci_lower, group1_std_ci_upper, group2_std_ci_lower, group2_std_ci_upper,
           p_value, significant) |>
    rename(pvalue = p_value, est = comparison_unstd, std = comparison_std) |>
    distinct(lhs, op, rhs, .keep_all = TRUE)

  return(comparison_table)
}

#' Extract parameters with group names from lavaan model
#'
#' Extracts parameter estimates from a lavaan model and replaces numeric group indices
#' with meaningful group labels. This function simplifies working with multi-group
#' SEM models by providing human-readable group identifiers in the parameter table.
#'
#' @param fit A lavaan object containing the fitted SEM model
#'
#' @return A data frame containing parameter estimates

#' @examples
#' \dontrun{
#' # Assuming 'fit' is a multi-group lavaan model
#' params <- get_params_with_group_names(fit)
#'
#' # View parameters with group names
#' head(params)
#'
#' # Filter parameters for a specific group
#' params_group1 <- params |> filter(group == "Group1")
#' }
#'
#' @importFrom lavaan lavInspect parameterEstimates
#' @importFrom dplyr filter mutate
#' @importFrom stats setNames
#' @keywords internal
#' @noRd
get_params_with_group_names <- function(fit) {
  group_info <- lavInspect(fit, "group.label")
  group_mapping <- setNames(group_info, 1:length(group_info))

  params <- parameterEstimates(fit) |>
    filter(op %in% c("=~", "~1", "~~")) |>
    mutate(group = ifelse(group %in% names(group_mapping),
                          group_mapping[as.character(group)],
                          group))
  return(params)
}

#' Update tidysem object labels with statistical information
#'
#' This function updates the edge and node labels in a tidysem object with
#' formatted statistical information including estimates, significance stars,
#' and confidence intervals based on user specifications.
#'
#' @param tidysem_obj A tidysem object containing edges and optionally nodes
#'   data frames with parameter estimates.
#' @param standardized Logical. Whether to include standardized estimates
#'   in the labels. Default is FALSE.
#' @param unstandardized Logical. Whether to include unstandardized estimates
#'   in the labels. Default is TRUE.
#' @param p_val Logical. Whether to include significance stars (*) for
#'   significant parameters (p < 0.05). Default is TRUE.
#' @param conf_int Logical. Whether to include confidence intervals in the
#'   labels. Default is FALSE.
#'
#' @return The modified tidysem object with updated `label` columns in the
#'   edges and nodes data frames containing formatted statistical information.
#'
#' @importFrom dplyr case_when mutate
#'
#'
#' @examples
#' \dontrun{
#' # Update labels with standardized estimates and significance stars
#' tidysem_obj <- update_tidysem_labels(tidysem_obj,
#'                                      standardized = TRUE,
#'                                      p_val = TRUE)
#'
#' # Show only unstandardized estimates with confidence intervals
#' tidysem_obj <- update_tidysem_labels(tidysem_obj,
#'                                      standardized = FALSE,
#'                                      unstandardized = TRUE,
#'                                      conf_int = TRUE)
#' }
#'
#' @keywords internal
#' @noRd
update_tidysem_labels <- function(tidysem_obj, standardized = FALSE, unstandardized = TRUE,
                                  p_val = TRUE, conf_int = FALSE) {

  create_label <- function(row, standardized, unstandardized, p_val, conf_int) {
    # Check which columns exist
    has_std <- all(c("est_sig_std", "est_std", "confint_std") %in% names(row))
    has_unstd <- all(c("est_sig", "est", "confint") %in% names(row))

    if (standardized == FALSE & unstandardized == FALSE) {
      if (conf_int & has_unstd) {
        return(ifelse(!is.na(row$confint), row$confint, ""))
      } else {
        return("")
      }
    }

    if (standardized == TRUE & !has_std) {
      # Fall back to unstandardized if available
      if (unstandardized == FALSE) {
        unstandardized <- TRUE
      }
      standardized <- FALSE
    }

    if (standardized == TRUE & unstandardized == TRUE) {
      if (p_val & conf_int) {
        return(paste0(row$est_sig, " (", row$est_sig_std, ")\n", row$confint))
      } else if (p_val & !conf_int) {
        return(paste0(row$est_sig, " (", row$est_sig_std, ")"))
      } else if (!p_val & conf_int) {
        return(paste0(row$est, " (", row$est_std, ")\n", row$confint))
      } else {
        return(paste0(row$est, " (", row$est_std, ")"))
      }
    }

    if (standardized == TRUE & unstandardized == FALSE) {
      if (p_val & conf_int) {
        return(paste0(row$est_sig_std, "\n", row$confint_std))
      } else if (p_val & !conf_int) {
        return(row$est_sig_std)
      } else if (!p_val & conf_int) {
        return(paste0(row$est_std, "\n", row$confint_std))
      } else {
        return(as.character(row$est_std))
      }
    }

    if (standardized == FALSE & unstandardized == TRUE) {
      if (p_val & conf_int) {
        return(paste0(row$est_sig, "\n", row$confint))
      } else if (p_val & !conf_int) {
        return(row$est_sig)
      } else if (!p_val & conf_int) {
        return(paste0(row$est, "\n", row$confint))
      } else {
        return(as.character(row$est))
      }
    }

    return("")
  }

  if ("edges" %in% names(tidysem_obj)) {
    tidysem_obj$edges$label <- apply(tidysem_obj$edges, 1, function(row) {
      create_label(as.list(row), standardized, unstandardized, p_val, conf_int)
    })
  }

  if ("nodes" %in% names(tidysem_obj) && !is.null(tidysem_obj$nodes)) {
    if (nrow(tidysem_obj$nodes) > 0 && all(c("est_sig", "est", "confint") %in% names(tidysem_obj$nodes))) {
      tidysem_obj$nodes$label <- apply(tidysem_obj$nodes, 1, function(row) {
        create_label(as.list(row), standardized, unstandardized, p_val, conf_int)
      })
    }
  }

  return(tidysem_obj)
}

#' Update tidysem object labels with Bayesian statistical information
#'
#' This function updates edge and node labels in a tidysem object with
#' formatted Bayesian statistical information including estimates, significance
#' indicators based on HPD intervals, and credible intervals.
#'
#' @param tidysem_obj A tidysem object containing edges and optionally nodes
#'   data frames with parameter estimates.
#' @param blavaan_fit A blavaan model object from which to extract HPD intervals.
#' @param standardized Logical. Whether to include standardized estimates
#'   in the labels. Default is FALSE.
#' @param unstandardized Logical. Whether to include unstandardized estimates
#'   in the labels. Default is TRUE.
#' @param p_val Logical. Whether to include significance indicators (*) for
#'   parameters whose HPD intervals do not contain zero. Default is FALSE.
#' @param conf_int Logical. Whether to include HPD credible intervals in the
#'   labels. Default is FALSE.
#' @param ci_level Numeric. The credible interval level for HPD intervals.
#'   Default is 0.95 (95% credible intervals).
#'
#' @return The modified tidysem object with updated `label` columns in the
#'   edges and nodes data frames containing formatted Bayesian statistical
#'   information.
#' @examples
#' \dontrun{
#' # Fit Bayesian SEM with blavaan
#' fit <- bsem(model, data = mydata)
#'
#' # Convert to tidysem format
#' tidysem_obj <- tidy(fit)
#'
#' # Update labels with HPD intervals
#' tidysem_obj <- update_tidysem_labels_bayes(tidysem_obj, fit,
#'                                            conf_int = TRUE,
#'                                            ci_level = 0.89)
#'
#' # Show standardized estimates with significance based on HPD
#' tidysem_obj <- update_tidysem_labels_bayes(tidysem_obj, fit,
#'                                            standardized = TRUE,
#'                                            p_val = TRUE)
#' }
#' @importFrom dplyr mutate case_when select left_join
#' @importFrom blavaan blavInspect
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
update_tidysem_labels_bayes <- function(tidysem_obj, blavaan_fit,
                                        standardized = FALSE, unstandardized = TRUE,
                                        p_val = FALSE, conf_int = FALSE,
                                        ci_level = 0.95) {

  hpd_intervals <- as.data.frame(blavaan::blavInspect(blavaan_fit, "hpd", level = ci_level))
  hpd_intervals$parameter <- rownames(hpd_intervals)

  group_info <- blavaan::blavInspect(blavaan_fit, "group.label")

  convert_to_hpd_name <- function(lhs, op, rhs, group, label = NULL) {
    group_num <- match(group, group_info)
    if (is.na(group_num)) group_num <- 1

    if (!is.null(label) && label != "" && grepl("^\\.p[0-9]+\\.$", label)) {
      # Has a lavaan label (like .p2., .p3., etc.)
      base_name <- label

      if (group_num == 1) {
        return(base_name)
      } else {
        possible_names <- c(
          paste0(base_name, "..", group_num),  # .p2..1
          paste0(base_name, ".g", group_num),  # .p2.g2
          base_name  # Also try without suffix as fallback
        )
        return(possible_names)
      }
    } else if (op == "~~") {
      base_name <- paste0(lhs, "..", rhs)

      if (group_num == 1) {
        return(base_name)
      } else {
        return(paste0(base_name, ".g", group_num))
      }
    } else if (op == "~1" && lhs %in% c("visual", "textual", "speed")) {
      if (group_num > 1) {
        return(paste0(lhs, ".1.g", group_num))
      }
    }

    return(NULL)
  }

  add_significance_stars <- function(est_values, lower_values, upper_values) {
    significant <- !is.na(lower_values) & !is.na(upper_values) &
      (lower_values > 0 | upper_values < 0)
    ifelse(significant, paste0(est_values, "*"), est_values)
  }

  add_significance_indicator <- function(lower_values, upper_values) {
    significant <- !is.na(lower_values) & !is.na(upper_values) &
      (lower_values > 0 | upper_values < 0)
    ifelse(significant, 0, NA_real_)  # 0 for significant, NA for not
  }

  hpd_names <- hpd_intervals$parameter
  is_blavaan_format <- any(grepl("^\\.p[0-9]+\\.$", hpd_names))

  if (is_blavaan_format) {
    edges_with_hpd <- tidysem_obj$edges |>
      rowwise() |>
      mutate(
        hpd_name = {
          if (!is.na(lavaan_label) && lavaan_label != "" && grepl("^\\.p[0-9]+\\.$", lavaan_label)) {
            label <- lavaan_label
            group_num <- match(group, group_info)
            if (is.na(group_num)) group_num <- 1

            if (group_num == 1) {
              # Group 1
              label
            } else {
              # Group 2 - try different patterns
              name1 <- paste0(label, "..", group_num)
              name2 <- paste0(label, ".g", group_num)

              if (name1 %in% hpd_names) name1
              else if (name2 %in% hpd_names) name2
              else label  # fallback
            }
          } else if (op == "~~") {
            # Variance or covariance
            base_name <- paste0(lhs, "..", rhs)
            group_num <- match(group, group_info)
            if (is.na(group_num)) group_num <- 1

            if (group_num == 1) {
              base_name
            } else {
              paste0(base_name, ".g", group_num)
            }
          } else {
            NA_character_
          }
        }
      ) |>
      ungroup() |>
      left_join(hpd_intervals |> select(parameter, lower, upper),
                by = c("hpd_name" = "parameter")) |>
      select(-hpd_name)

  } else {
    edges_with_hpd <- tidysem_obj$edges |>
      mutate(
        param_name = case_when(
          op == "=~" ~ paste0(lhs, op, rhs),
          op == "~~" ~ paste0(lhs, op, rhs),
          op == "~1" ~ paste0(lhs, op),
          TRUE ~ paste0(lhs, op, rhs)
        )
      ) |>
      left_join(hpd_intervals |> select(parameter, lower, upper),
                by = c("param_name" = "parameter")) |>
      select(-param_name)
  }

  tidysem_obj$edges <- edges_with_hpd |>
    mutate(
      confint = ifelse(!is.na(lower),
                       paste0("[", round(lower, 2), ", ", round(upper, 2), "]"),
                       NA_character_),
      confint_std = confint,

      pval = add_significance_indicator(lower, upper),

      est_sig = if (p_val) add_significance_stars(est, lower, upper) else est,
      est_sig_std = if (p_val) add_significance_stars(est_std, lower, upper) else est_std
    ) |>
    select(-lower, -upper)

  tidysem_obj$edges <- tidysem_obj$edges |>
    mutate(
      label = case_when(
        # Case 1: Both standardized and unstandardized are FALSE
        standardized == FALSE & unstandardized == FALSE ~
          if (conf_int) {
            # Show unstandardized confidence interval without "\n"
            confint
          } else {
            ""
          },

        # Case 2: Show both standardized and unstandardized
        standardized == TRUE & unstandardized == TRUE ~
          if (p_val & conf_int) {
            ifelse(!is.na(confint),
                   paste0(est_sig, " (", est_sig_std, ")\n", confint),
                   paste0(est_sig, " (", est_sig_std, ")"))
          } else if (p_val & !conf_int) {
            paste0(est_sig, " (", est_sig_std, ")")
          } else if (!p_val & conf_int) {
            ifelse(!is.na(confint),
                   paste0(est, " (", est_std, ")\n", confint),
                   paste0(est, " (", est_std, ")"))
          } else {
            paste0(est, " (", est_std, ")")
          },

        # Case 3: Show only standardized
        standardized == TRUE & unstandardized == FALSE ~
          if (p_val & conf_int) {
            ifelse(!is.na(confint_std),
                   paste0(est_sig_std, "\n", confint_std),
                   est_sig_std)
          } else if (p_val & !conf_int) {
            est_sig_std
          } else if (!p_val & conf_int) {
            ifelse(!is.na(confint_std),
                   paste0(est_std, "\n", confint_std),
                   est_std)
          } else {
            as.character(est_std)
          },

        # Case 4: Show only unstandardized
        standardized == FALSE & unstandardized == TRUE ~
          if (p_val & conf_int) {
            ifelse(!is.na(confint),
                   paste0(est_sig, "\n", confint),
                   est_sig)
          } else if (p_val & !conf_int) {
            est_sig
          } else if (!p_val & conf_int) {
            ifelse(!is.na(confint),
                   paste0(est, "\n", confint),
                   est)
          } else {
            as.character(est)
          },

        # Fallback (shouldn't be reached)
        TRUE ~ ""
      )
    )

  if (!is.null(tidysem_obj$nodes) && "est" %in% names(tidysem_obj$nodes)) {

    if (is_blavaan_format) {
      nodes_with_hpd <- tidysem_obj$nodes |>
        mutate(
          # For latent intercepts in group 2
          hpd_name = ifelse(name %in% c("visual", "textual", "speed") &
                              match(group, group_info) > 1,
                            paste0(name, ".1.g", match(group, group_info)),
                            NA_character_)
        ) |>
        left_join(hpd_intervals |> select(parameter, lower, upper),
                  by = c("hpd_name" = "parameter")) |>
        select(-hpd_name)
    } else {
      nodes_with_hpd <- tidysem_obj$nodes |>
        mutate(
          param_name = paste0(name, "~1")
        ) |>
        left_join(hpd_intervals |> select(parameter, lower, upper),
                  by = c("param_name" = "parameter")) |>
        select(-param_name)
    }

    tidysem_obj$nodes <- nodes_with_hpd |>
      mutate(
        confint = ifelse(!is.na(lower),
                         paste0("[", round(lower, 2), ", ", round(upper, 2), "]"),
                         NA_character_),
        confint_std = confint,

        pval = add_significance_indicator(lower, upper),

        est_sig = if (p_val) add_significance_stars(est, lower, upper) else est,
        est_sig_std = if (p_val) add_significance_stars(est_std, lower, upper) else est_std
      ) |>
      select(-lower, -upper)

    tidysem_obj$nodes <- tidysem_obj$nodes |>
      mutate(
        label = case_when(
          # Case 1: Both standardized and unstandardized are FALSE
          standardized == FALSE & unstandardized == FALSE ~
            if (conf_int) {
              # Show unstandardized confidence interval without "\n"
              confint
            } else {
              ""
            },

          # Case 2: Show both standardized and unstandardized
          standardized == TRUE & unstandardized == TRUE ~
            if (p_val & conf_int) {
              ifelse(!is.na(confint),
                     paste0(est_sig, " (", est_sig_std, ")\n", confint),
                     paste0(est_sig, " (", est_sig_std, ")"))
            } else if (p_val & !conf_int) {
              paste0(est_sig, " (", est_sig_std, ")")
            } else if (!p_val & conf_int) {
              ifelse(!is.na(confint),
                     paste0(est, " (", est_std, ")\n", confint),
                     paste0(est, " (", est_std, ")"))
            } else {
              paste0(est, " (", est_std, ")")
            },

          # Case 3: Show only standardized
          standardized == TRUE & unstandardized == FALSE ~
            if (p_val & conf_int) {
              ifelse(!is.na(confint_std),
                     paste0(est_sig_std, "\n", confint_std),
                     est_sig_std)
            } else if (p_val & !conf_int) {
              est_sig_std
            } else if (!p_val & conf_int) {
              ifelse(!is.na(confint_std),
                     paste0(est_std, "\n", confint_std),
                     est_std)
            } else {
              as.character(est_std)
            },

          # Case 4: Show only unstandardized
          standardized == FALSE & unstandardized == TRUE ~
            if (p_val & conf_int) {
              ifelse(!is.na(confint),
                     paste0(est_sig, "\n", confint),
                     est_sig)
            } else if (p_val & !conf_int) {
              est_sig
            } else if (!p_val & conf_int) {
              ifelse(!is.na(confint),
                     paste0(est, "\n", confint),
                     est)
            } else {
              as.character(est)
            },

          # Fallback (shouldn't be reached)
          TRUE ~ ""
        )
      )
  }

  return(tidysem_obj)
}

