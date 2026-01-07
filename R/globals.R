utils::globalVariables(
  c(
    # ===== PIPE OPERATOR =====
    "%>%",

    # ===== NETWORK PACKAGE OPERATORS =====
    "%n%", "%v%<-", "E<-",

    # ===== LAYOUT COLUMNS =====
    "PC1", "PC2", "V1", "V2",

    # ===== NODE AESTHETICS =====
    "x", "y", "angle", "angle_value",
    "shape", "color", "size", "border_color", "border_width",
    "width_height_ratio", "orientation", "locked", "group",
    "fill", "font", "fontface", "math_expression", "group_label",
    "loop_label", "name", "network",

    # ===== EDGE AESTHETICS =====
    "target", "edge_id", "edge_pair", "weight", "scaled_width", "two_way",
    "x_start", "y_start", "x_end", "y_end", "ctrl_x", "ctrl_y", "ctrl_x2", "ctrl_y2",
    "curvature", "curvature_magnitude", "curvature_asymmetry", "rotate_curvature",
    "type", "end_color", "color_type", "gradient_position", "width", "line_style",
    "linetype", "to", "from",

    # ===== TEXT/LABEL COLUMNS =====
    "text_x", "text_y", "label_x", "label_y", "label_results",
    "hjust", "vjust",  # Added from error messages

    # ===== NETWORK PACKAGE SPECIFIC =====
    "attr_val", "attrs", "block", "connect_from", "connect_to",
    "node_xmax", "node_xmin", "node_ymax", "node_ymin",
    "network_edges_curvature_asymmetry",

    # ===== LAVAAN/SEM PARAMETER COLUMNS =====
    "op", "lhs", "rhs", "est", "est.std", "std", "p_value", "significant",
    "ci.lower", "ci.upper", "group", "group_name", "group_num",
    "comparison_unstd", "comparison_std", "confint_unstd", "confint_std",
    "confint_combined", "confint_std_combined",
    "lavaan_label", "hpd_name", "se",

    # ===== MULTI-GROUP COMPARISON COLUMNS =====
    "group1_est", "group2_est", "group1_std", "group2_std",
    "group1_ci_lower", "group1_ci_upper", "group2_ci_lower", "group2_ci_upper",
    "group1_std_ci_lower", "group1_std_ci_upper", "group2_std_ci_lower", "group2_std_ci_upper",
    "group1_hpd_lower", "group1_hpd_upper", "group2_hpd_lower", "group2_hpd_upper",
    "group1_num", "group2_num",

    # ===== BAYESIAN ANALYSIS COLUMNS =====
    "credible_interval", "posterior_mean_diff", "excludes_zero", "excludes_rope",
    "rope_lower", "rope_upper", "param_base", "param_group1", "param_group2",

    # ===== TIDYSEM COLUMNS =====
    "param_name", "param_name_full", "parameter", "lower", "upper", "est_std",
    "param_name_group1", "param_name_group2",

    # ===== TIDYSEM ESTIMATE COLUMNS =====
    "est_sig", "est_sig_std", "est_combined", "est_sig_combined",
    "est_std_combined", "est_sig_std_combined",

    # ===== METHODS FUNCTIONS =====
    "is", "is<-",

    # ===== DPLYR FUNCTIONS =====
    "case_when", "row_number",
    "calculate_control_points", "calculate_curvature_magnitude",
    "rescale_values", "get_est_differences", "get_est_differences_bayes",
    "original_order", "weight", "value"
  )
)
