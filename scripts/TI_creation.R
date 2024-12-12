# Setup ----
side_length <- 5

# Functions ----
pm <- function(num)c(-num, num)
rotate_yz <- function(data, theta){
  ys = data[,"y"] * cos(theta) - data[,"z"] * sin(theta)
  zs = data[,"z"] * cos(theta) + data[,"y"] * sin(theta)
  return(cbind(y=ys, z=zs))
}

# TI calculations ----
### Calculate vertices ----
a <- (3+sqrt(5))/6
b <- (sqrt(5)-1)/3
c <- sqrt(5)/3
TI_vertices <- setNames(rbind(
  expand.grid(pm(1/3), pm(a), pm(b)),
  expand.grid(pm(b), pm(1/3), pm(a)),
  expand.grid(pm(a), pm(b), pm(1/3)),
  expand.grid(pm(2/3), pm(c), pm(b/2)),
  expand.grid(pm(b/2), pm(2/3), pm(c)),
  expand.grid(pm(c), pm(b/2), pm(2/3)),
  expand.grid(pm(1), pm(b/2), 0),
  expand.grid(0, pm(1), pm(b/2)),
  expand.grid(pm(b/2), 0, pm(1))
), c("x", "y", "z"))

# Rotate to put hex face on top and reorganize points
golden_phi <- (1+sqrt(5))/2
TI_vertices[, c("y", "z")] <- rotate_yz(
  TI_vertices[,c("y", "z")], theta = atan((golden_phi-1)/golden_phi)
)
TI_vertices <- TI_vertices[
  order(TI_vertices$z, TI_vertices$y, decreasing = TRUE),
]*side_length*2.4
rownames(TI_vertices) <- NULL
TI_vertices <- cbind(id=1:60, TI_vertices)

# Calculate phi and theta
TI_vertices$compass_angle <- -atan2(x = TI_vertices$y, y = TI_vertices$x)/pi*180
TI_vertices$elevation_angle <- -(atan(TI_vertices$z/sqrt(TI_vertices$x^2+TI_vertices$y^2))/pi*180-90)

# Calculate nearest vertices for each vertex
nearest_verts <- apply(TI_vertices, 1, function(x){
  x_delta <- abs(TI_vertices$x-as.numeric(x["x"]))
  y_delta <- abs(TI_vertices$y-as.numeric(x["y"]))
  z_delta <- abs(TI_vertices$z-as.numeric(x["z"]))
  dist_vec <- sqrt(x_delta^2+y_delta^2+z_delta^2)
  TI_vertices$id[order(dist_vec)][2:4]
}, simplify = FALSE)


### Calculate faces ----
TI_faces <- data.frame(face_number=1:32)
# Manually determine which points belong to which faces
TI_faces$vertex_rows <- list(
  c(1, 3, 4, 2, 6, 5),
  c(4, 8, 19, 21, 10, 2),
  c(2, 10, 15, 12, 6),
  c(6, 12, 17, 16, 11, 5),
  c(5, 11, 14, 9, 1),
  c(1, 9, 20, 18, 7, 3),
  c(3, 7, 13, 8, 4),
  c(12, 15, 24, 34, 30, 17),
  c(16, 17, 30, 39, 29),
  c(11, 16, 29, 33, 23, 14),
  c(9, 14, 23, 35, 25, 20),
  c(20, 25, 37, 27, 18),
  c(7, 18, 27, 31, 22, 13),
  c(8, 13, 22, 32, 28, 19),
  c(19, 28, 38, 26, 21),
  c(10, 15, 24, 36, 26, 21), #Reversed ???
  c(24, 36, 41, 43, 34),
  c(30, 34, 43, 54, 48, 39),
  c(29, 39, 48, 53, 42, 33),
  c(23, 33, 42, 40, 35),
  c(25, 35, 40, 51, 46, 37),
  c(27, 37, 46, 49, 44, 31),
  c(22, 31, 44, 45, 32),
  c(28, 32, 45, 50, 47, 38),
  c(26, 38, 47, 52, 41, 36),
  c(44, 49, 55, 56, 50, 45),
  c(47, 50, 56, 60, 52),
  c(41, 52, 60, 58, 54, 43),
  c(48, 54, 58, 57, 53),
  c(42, 53, 57, 59, 51, 40),
  c(46, 51, 59, 55, 49),
  c(55, 59, 57, 58, 60, 56)
)

# Calculate face centers and associated data
center_points <- t(sapply(TI_faces$vertex_rows, function(given_vertices){
  colMeans(TI_vertices[given_vertices,c("x", "y", "z")])
}))
TI_faces <- cbind(TI_faces, center_points)
TI_faces$n_vertices <- lengths(TI_faces$vertex_rows)
TI_faces$compass_angle <- -atan2(x = TI_faces$y, y = TI_faces$x)/pi*180
TI_faces$elevation_angle <- -(atan(TI_faces$z/sqrt(TI_faces$x^2+TI_faces$y^2))/pi*180-90)
TI_faces$id <- 151:182

# Calculate nearest faces for each vertex
nearest_faces <- apply(TI_vertices, 1, function(x){
  x_delta <- abs(TI_faces$x-as.numeric(x["x"]))
  y_delta <- abs(TI_faces$y-as.numeric(x["y"]))
  z_delta <- abs(TI_faces$z-as.numeric(x["z"]))
  dist_vec <- sqrt(x_delta^2+y_delta^2+z_delta^2)
  TI_faces$id[order(dist_vec)][1:3]
}, simplify = FALSE)



### Calculate edges ----
side_mat <- cbind(TI_vertices$id, do.call(rbind, nearest_verts)[T])
side_mat <- side_mat[side_mat[,1]<side_mat[,2],]

TI_edges <- t(apply(side_mat, 1, function(given_vertices){
  colMeans(TI_vertices[given_vertices,c("x", "y", "z")])
}))
TI_edges <- cbind(id=61:150, as.data.frame(TI_edges))

# Calculate phi and theta
TI_edges$compass_angle <- -atan2(x = TI_edges$y, y = TI_edges$x)/pi*180
TI_edges$elevation_angle <- -(atan(TI_edges$z/sqrt(TI_edges$x^2+TI_edges$y^2))/pi*180-90)

# Calculate nearby edges
### Four edges near to each edge
nearest_edges_per_edge <- apply(TI_edges, 1, function(x){
  x_delta <- abs(TI_edges$x-as.numeric(x["x"]))
  y_delta <- abs(TI_edges$y-as.numeric(x["y"]))
  z_delta <- abs(TI_edges$z-as.numeric(x["z"]))
  dist_vec <- sqrt(x_delta^2+y_delta^2+z_delta^2)
  TI_edges$id[order(dist_vec)][2:5]
}, simplify = FALSE)
### Three edges near to each vertex
nearest_edges <- apply(TI_vertices, 1, function(x){
  x_delta <- abs(TI_edges$x-as.numeric(x["x"]))
  y_delta <- abs(TI_edges$y-as.numeric(x["y"]))
  z_delta <- abs(TI_edges$z-as.numeric(x["z"]))
  dist_vec <- sqrt(x_delta^2+y_delta^2+z_delta^2)
  TI_edges$id[order(dist_vec)][1:3]
}, simplify = FALSE)



# Combine all ----
TI_structure <- list(
  vertices = TI_vertices,
  edges = TI_edges,
  faces = TI_faces[,c("id", "x", "y", "z", "compass_angle", "elevation_angle")]
)

cart2sphere <- function(cart_vec){
  x <- cart_vec[1]
  y <- cart_vec[2]
  z <- cart_vec[3]
  r <- sqrt(x^2+y^2+z^2)
  phi <- atan2(y, x)
  theta <- acos(z/r)
  return(c(r, theta, phi))
}
sphere2cart <- function(sphere_vec){
  r <- sphere_vec[1]
  theta <- sphere_vec[2]
  phi <- sphere_vec[3]
  x <- r*sin(theta)*cos(phi)
  y <- r*sin(theta)*sin(phi)
  z <- r*cos(theta)
  return(c(x, y, z))
}
# Create initial data for interactable markers
marker_data_unmoved  <- rbind(
  cbind(TI_structure$vertices, lab="vertex"),
  cbind(TI_structure$edges, lab="edge"),
  cbind(TI_structure$faces, lab="face")
)
# Move them slightly outside the globe to improve interactivity
marker_data_all <- marker_data_unmoved
marker_data_all[c("x", "y", "z")] <- marker_data_all[c("x", "y", "z")] %>%
  apply(1, cart2sphere) %>% `+`(c(0.5, 0, 0)) %>% apply(2, sphere2cart) %>%
  t() %>% setNames(c("x", "y", "z"))

# Create network of nearby structures
nearby_structures <- list(
  verts = as.data.frame(cbind(
    id=TI_vertices$id,
    nearest_verts,
    nearest_edges,
    nearest_faces
  )),
  edges = as.data.frame(cbind(
    id=TI_edges$id,
    nearest_verts=apply(side_mat, 1, c, simplify = FALSE),
    nearest_edges=nearest_edges_per_edge,
    nearest_faces=NA
  )),
  faces = as.data.frame(cbind(
    id=TI_faces$id,
    nearest_verts=TI_faces$vertex_rows,
    nearest_edges=NA,
    nearest_faces=NA
  ))
)
nearby_structures$verts$id <- unlist(nearby_structures$verts$id)
nearby_structures$edges$id <- unlist(nearby_structures$edges$id)
nearby_structures$faces$id <- unlist(nearby_structures$faces$id)

# QC ----
# library(plotly)
# plot_ly() %>%
#   add_trace(type="scatter3d", mode="markers",
#             x=~x, y=~y, z=~z, data=TI_vertices,
#             marker=list(color="green", size=5)) %>%
#   add_trace(type="scatter3d", mode="markers",
#             x=~x, y=~y, z=~z, data=TI_faces,
#             marker=list(color="black", size=10)) %>%
#   add_trace(type="scatter3d", mode="markers",
#             x=~x, y=~y, z=~z, data=TI_edges,
#             marker=list(color="steelblue", size=2))
# 
# plot_ly(TI_vertices, type = "scatter3d", mode="text",
#         x=~x, y=~y, z=~z, text=~id)
# 
# mapply(cbind, TI_structure, type=names(TI_structure), SIMPLIFY = FALSE) %>%
#   do.call(what = rbind) %>%
#   plot_ly(type="scatter3d", mode="text", text=~id,
#           x=~x, y=~y, z=~z, color=~type)

# Cleanup for sourcing ----
# rm(list = setdiff(ls(), c("TI_structure", "nearby_structures", "side_length")))



