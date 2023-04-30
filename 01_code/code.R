################################################################################
####################### Load Data, Functions and Packages ######################
################################################################################
# load packages
source(here::here('01_code/packages.R'))
# load functions 
source(here::here('01_code/functions.R'))
# load data
load("00_data/data.Rdata")

################################################################################
################################ data wrangling ################################
################################################################################

# normalize time distance value
time_data %<>% normalise_S_v()

# weighted mean of the time distances 
dist_time_data <-  time_data %>% 
  group_by(v_1, v_2) %>%
  dplyr::summarise(dist_L1 = weighted.mean(L_1, 
                                           c(rep(2/30, 13), rep(1/30, 4))), 
                   .groups = "drop")

# Add points to time distance data and remove students with less than 25% of points
s_v_20_21 <- time_data %>% points_fun_filter(point_data,
                                             min = 401)

# Normalise Attributes
s_v_20_21 %<>% normalise_S_v_time_points()

# Add up to get a global measure
dist_mat_global_s_v_20_21 <-  s_v_20_21 %>%   
  pivot_longer(cols = c(points_L, L_1)) %>%
  group_by(v_1, v_2) %>%
  plyr::arrange(v_1, v_2, name) %>%
  dplyr::summarise(dist_L1 = weighted.mean(value[name != "L_2"], 
                                           c(rep(4/77, 13), rep(2/77, 4), rep(1/77, 17))), 
                   .groups = "drop")

# Clustering
L1_20_21_av <- L1_cluster_fun(dist_mat = dist_mat_global_s_v_20_21, method = "average")

L1_20_21_dendlist <- dendextend::dendlist(L1_20_21_av %>% 
                                            set_custom())
names(L1_20_21_dendlist) <- c("average")

# creating the data for the tree
L1_20_21_av_0.2 <- L1_20_21_dendlist$average %>% 
  dendextend::cutree(h = c(0.163), 
                     use_labels_not_values = T, 
                     order_clusters_as_data = F)

L_1_20_21_av_0.2_index <- L1_20_21_av_0.2[duplicated(L1_20_21_av_0.2)] %>%
  unique()
clust_ID <- L1_20_21_av_0.2[L1_20_21_av_0.2 %in% L_1_20_21_av_0.2_index]
leaf_xy <- tibble(
  y = L1_20_21_dendlist$average %>% 
    hang.dendrogram(hang = 0.1) %>% 
    dendextend::get_leaves_attr("height") -0.015, 
  x = 1:attr(L1_20_21_dendlist$average, "member")
)
clust_xy <- leaf_xy %>% dplyr::slice(which(duplicated(L1_20_21_av_0.2)))


# Fix letters of the groups -----------------------------------------------
clust_xy_backup <- clust_xy
clust_xy <- clust_xy[clust_xy$y %in% sort(clust_xy$y)[1:6], ]


################################################################################
################################### plotting ###################################
################################################################################

L1_20_21_dendlist$average %>%
  hang.dendrogram(hang = 0.1) %>%
  set("labels_cex", 0.55) %>%
  plot(ylim = c(-0.15, 0.45), 
       axes = F, 
       ylab = "proximity")
axis(side = 2, at = seq(0, 0.4, 0.1))
for (i in 1:6) {
  text(x = clust_xy$x[i], 
       y = clust_xy$y[i], 
       labels = LETTERS[i])
}