# Normalise Attributes
normalise_S_v <- function(S_v) {
  S_v %>% 
    group_by(exercise) %>%
    dplyr::mutate(L_1 = (L_1 - min(L_1))/(max(L_1) - min(L_1))) %>%
    ungroup()
}

# Add points to time distance data
points_fun_filter <- function(data, point_data, min) {
  index_filter <- point_data %>% 
    group_by(STUDI_ID) %>%
    dplyr::summarise(sum = sum(points)) %>%
    dplyr::filter(sum >= min) %>%
    dplyr::pull(STUDI_ID)
  
  data %<>%
    dplyr::filter(v_1 %in% index_filter & v_2 %in% index_filter)
  
  data_points <- data %>%
    dplyr::left_join(point_data, 
                     by = c("v_1" = "STUDI_ID", "exercise")) %>%
    dplyr::rename("points_v_1" = points) %>%
    dplyr::mutate(points_v_1 = dplyr::coalesce(points_v_1, 0))
  
  data_points %<>%
    dplyr::left_join(point_data, 
                     by = c("v_2" = "STUDI_ID", "exercise")) %>%
    dplyr::rename("points_v_2" = points) %>%
    dplyr::mutate(points_v_2 = dplyr::coalesce(points_v_2, 0))
  
  data_points %>%
    dplyr::mutate(
      points_L = abs(points_v_1 - points_v_2) 
    ) %>%
    dplyr::select(-c(points_v_1, points_v_2))
}

# Normalise Attributes
normalise_S_v_time_points <- function(S_v) {
  S_v %>% 
    group_by(exercise) %>%
    dplyr::mutate(points_L = (points_L - min(points_L))/(max(points_L) - min(points_L))) %>%
    ungroup()
}

# cluster functions
L1_cluster_fun <- function(dist_mat, method = "complete") {
  dist_mat_L1 <- dist_mat %>% 
    pivot_wider(names_from = v_2, 
                values_from = dist_L1, 
                id_cols = v_1) %>%
    dplyr::select(-v_1) %>%
    as.dist()
  
  hclust(dist_mat_L1, method = method)
}


#---- Organize hclust objects in dendlist ----
set_custom <- function(hcl) {
  hcl %>%
    as.dendrogram() %>% 
    set("branches_lwd", 0.7) %>%
    set("labels_col", "white")
}
