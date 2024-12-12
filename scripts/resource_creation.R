
# Setup ----
source("scripts/TI_creation.R")

# Hex colors and counts ----
hex_border_width <- 0.05 # as a fraction of side_length
hex_border_height <- 0.05 # as a fraction of side_length
hex_border_color <- "black"

forest_type <- "mixed" # Mixed, pine, or deci
ntrees <- 20
forest_floor_color <- "#1a580a"
tree_bark_color <- "#5a3a23"

nmounts <- 15
mountain_base_color <- "grey50"
mountain_default_color <- "#666666"
snowcap_color <- "white"
snow_default_color <- "#EEEEEE"

nsheep <- 10
meadow_color <- "#6f9e10"
sheep_coat_color <- "white"
sheep_head_color <- "black"

light_wheat_color <- "#e2a902"
wheat_color <- "#f8ac1b"
dark_wheat_color <- "#e69a11"
dirt_color <- "#986e4a"

brick_base_color <- "#b17223"
dark_brick <- "#bd3a0b"
light_brick <- "#ba4b18"

npuffs <- 10
nflakes <- 50
cloud_color <- "white"


# Functions ----
colorParser <- function(color, pepper=0){
  if(pepper){
    colvals <- t(col2rgb(color))
    colvals <- rbind(colvals, colvals[rep(1, pepper-1), ])
    # colvals <- round(sweep(colvals, 1, rnorm(pepper, sd=10), `+`))
    colvals <- round(sweep(colvals, 1, runif(pepper, min = -50, max = 50), `+`))
    colvals[colvals>255] <- 255
    colvals[colvals<0] <- 0
    rgb(colvals, maxColorValue = 255)
  } else {
    rgb(t(col2rgb(color)), maxColorValue = 255)
  }
}

render_dots <- function(dots_data_row, side_length_factor=10){
  edge_points <- if(dots_data_row$pip_number==1){
    data.frame(x=dots_data_row$x, y=dots_data_row$y)
  } else if(dots_data_row$pip_number==2){
    data.frame(x=dots_data_row$x, y=dots_data_row$y+side_length/side_length_factor*c(-1,1))
  } else {
    interior_angle <- (dots_data_row$pip_number-2)*180 / dots_data_row$pip_number
    radius_length <- side_length/side_length_factor / cos(interior_angle/2 * pi/180)
    outer_pts <- t(sapply(1:dots_data_row$pip_number, function(i){
      radius_length*c(cos(2*i*pi/dots_data_row$pip_number), sin(2*i*pi/dots_data_row$pip_number))
    }))
    data.frame(x=outer_pts[,1]+dots_data_row$x, y=outer_pts[,2]+dots_data_row$y)
  }
  edge_points$z <- dots_data_row$z
  
  vert_list <- list(vertices=edge_points) %>%
    rotate_geom(y_deg = dots_data_row$elevation_angle) %>%
    rotate_geom(z_deg = -dots_data_row$compass_angle) %>%
    .[["vertices"]]
  vert_list$id <- dots_data_row$id
  vert_list
}
render_gon <- function(n_sides, center_x, center_y, side_length, color, pepper=FALSE, z=0){
  interior_angle <- (n_sides-2)*180 / n_sides
  radius_length <- side_length/2 / cos(interior_angle/2 * pi/180)
  outer_pts <- t(sapply(1:n_sides, function(i){
    radius_length*c(cos(2*i*pi/n_sides), sin(2*i*pi/n_sides))
  }))
  vertices <- rbind(c(center_x, center_y), data.frame(x=outer_pts[,1]+center_x, 
                                                      y=outer_pts[,2]+center_y))
  vertices$z <- z
  faces <- data.frame(i=0,
                      j=c(1:n_sides),
                      k=c(2:n_sides, 1),
                      color=colorParser(color, ifelse(pepper, n_sides, 0)))
  list(vertices=round(vertices, digits = 10), faces=faces)
}
render_4gram <- function(height, length_top, length_bottom, center_x, center_y,
                         color, pepper=FALSE){
  x_top_vert <- rev(center_x+length_top*c(-1, 1)/2)
  x_bottom_vert <- center_x+length_bottom*c(-1, 1)/2
  x_vert <- c(x_top_vert, x_bottom_vert)
  y_vert <- rep(center_y+height*c(-1, 1)/2, 2)[c(1, 3, 2, 4)]
  vertices <- as.data.frame(rbind(c(x=center_x, y=center_y), cbind(x_vert, y_vert)))
  vertices$z <- 0
  
  faces <- data.frame(i=0, j=1:4, k=c(2:4,1),
                      color=colorParser(color, ifelse(pepper, 4, 0)))
  list(vertices=vertices, faces=faces)
}

gon_2_point <- function(poly_obj, elevation, height, color, pepper=TRUE){
  base <- poly_obj$vertices[,1:2]
  
  top_point <- data.frame(x=mean(base$x),
                          y=mean(base$y),
                          z=height+elevation)
  vertices <- rbind(cbind(base, z=elevation), top_point)
  new_faces <- cbind(
    i=nrow(base), 
    poly_obj$faces[, c("j", "k")],
    color=colorParser(color, pepper = ifelse(pepper, nrow(poly_obj$faces), 0))
  )
  faces <- rbind(poly_obj$faces, new_faces)
  
  list(vertices=vertices, faces=faces)
}
colShift <- function(mat, n_sides=NULL, spec_vert=NULL){
  new_mat <- mat[,c("j", "k")]
  colnames(new_mat) <- c("i", "j")
  if(!is.null(n_sides)){
    new_mat[,"k"] <- new_mat[,"i"]+n_sides
  } else if(!is.null(spec_vert)){
    new_mat[,"k"] <- spec_vert
  } else {
    stop("Either n_sides or spec_vert must not be NULL")
  }
  new_mat
}
gon_2_prism <- function(poly_obj, elevation, height, color, pepper=TRUE){
  init_vertices <- poly_obj$vertices
  new_vertices <- init_vertices[c(2:nrow(init_vertices), 1),]
  new_vertices[,"z"] <- height
  all_verts <- rbind(init_vertices, new_vertices)
  all_verts[,"z"] <- all_verts[,"z"] + elevation
  
  n_sides <- nrow(poly_obj$faces)
  lower_tri_faces <- colShift(poly_obj$faces, n_sides = n_sides)
  upper_tri_faces <- colShift(lower_tri_faces, n_sides = n_sides)
  cap_faces <- colShift(upper_tri_faces, spec_vert = n_sides*2+1)
  upper_tri_faces <- upper_tri_faces[,c(1,3,2)]
  names(upper_tri_faces) <- c("i", "j", "k")
  
  all_faces <- rbind(poly_obj$faces[,c("i", "j", "k")], lower_tri_faces, 
                     upper_tri_faces, cap_faces)
  if(pepper){
    wall_colors <- replicate(nrow(lower_tri_faces), colorParser(color=color, pepper=TRUE))
    wall_colors <- rep(wall_colors, length.out=nrow(lower_tri_faces)*2)
    all_faces[,"color"] <- c(poly_obj$faces$color, wall_colors, poly_obj$faces$color)
  } else {
    all_faces[,"color"] <- color
  }
  
  list(vertices=all_verts, faces=all_faces)
}

move_geom <- function(geom, delta_x=0, delta_y=0, delta_z=0){
  geom$vertices$x <- geom$vertices$x+delta_x
  geom$vertices$y <- geom$vertices$y+delta_y
  geom$vertices$z <- geom$vertices$z+delta_z
  geom
}
grow_geom <- function(geom, growth_factor, toward_center=FALSE, vertical_only=FALSE){
  growth_factor <- max(growth_factor, 0)
  if(vertical_only){
    center_z <- ifelse(toward_center, mean(geom$vertices$z), min(geom$vertices$z))
    centered_z <- geom$vertices$z-center_z
    geom$vertices$z <- centered_z*growth_factor+center_z
  } else {
    center_x <- mean(geom$vertices$x)
    center_y <- mean(geom$vertices$y)
    center_z <- ifelse(toward_center, mean(geom$vertices$z), min(geom$vertices$z))
    centered_vertices <- sweep(geom$vertices, 2, c(center_x, center_y, center_z), `-`)
    geom$vertices$x <- centered_vertices$x*growth_factor+center_x
    geom$vertices$y <- centered_vertices$y*growth_factor+center_y
    geom$vertices$z <- centered_vertices$z*growth_factor+center_z
  }
  geom
}
rotate_geom <- function(geom, x_deg=0, y_deg=0, z_deg=0, manual_center=NULL){
  geom_verts <- as.matrix(geom$vertices)
  
  if(!is.null(manual_center)){
    geom_center <- manual_center
  } else {
    geom_center <- colMeans(geom_verts)
  }
  
  geom_centered <- sweep(geom_verts, 2, geom_center, `-`)
  
  # Switching wikipedia entry bc observation
  x_rot <- matrix(c(1, 0, 0, 
                    0, cos(y_deg*2*pi/360), -sin(y_deg*2*pi/360), 
                    0, sin(y_deg*2*pi/360), cos(y_deg*2*pi/360)), 
                  ncol = 3, byrow = TRUE)
  y_rot <- matrix(c(cos(x_deg*2*pi/360), 0, sin(x_deg*2*pi/360), 
                    0, 1, 0, 
                    -sin(x_deg*2*pi/360), 0, cos(x_deg*2*pi/360)), 
                  ncol = 3, byrow = TRUE)
  z_rot <- matrix(c(cos(z_deg*2*pi/360), -sin(z_deg*2*pi/360), 0, 
                    sin(z_deg*2*pi/360), cos(z_deg*2*pi/360), 0, 
                    0, 0, 1), 
                  ncol = 3, byrow = TRUE)
  
  geom_x <- geom_centered%*%x_rot
  geom_y <- geom_x%*%y_rot
  geom_z <- geom_y%*%z_rot
  geom_replaced <- sweep(geom_z, 2, geom_center, `+`)
  rot_df <- as.data.frame(geom_replaced)
  
  names(rot_df) <- names(geom$vertices)
  geom$vertices <- rot_df
  geom
}
combine_geoms <- function(geom_list){ #Where each geom in geom_list has both faces and vertices
  if(nrow(geom_list[[1]]$vertices)==0){
    return(geom_list[[2]])
  }
  vert_out_len <- sum(sapply(geom_list, function(geom)nrow(geom$vertices)))
  na_vert_fill <- rep(NA_real_, vert_out_len)
  face_out_len <- sum(sapply(geom_list, function(geom)nrow(geom$faces)))
  na_face_fill <- rep(NA_real_, face_out_len)
  out_geom <- list(vertices = data.frame(x=na_vert_fill, 
                                         y=na_vert_fill, 
                                         z=na_vert_fill),
                   faces = data.frame(i=na_face_fill,
                                      j=na_face_fill,
                                      k=na_face_fill,
                                      color=na_face_fill))
  for(i in seq_along(geom_list)){
    geom <- geom_list[[i]]
    vert_fill_start <- ifelse(i==1, 1, min(which(is.na(out_geom$vertices$x))))
    vert_fill_end <- vert_fill_start+nrow(geom$vertices)-1
    out_geom$vertices$x[vert_fill_start:vert_fill_end] <- geom$vertices$x
    out_geom$vertices$y[vert_fill_start:vert_fill_end] <- geom$vertices$y
    out_geom$vertices$z[vert_fill_start:vert_fill_end] <- geom$vertices$z
    face_fill_start <- ifelse(i==1, 1, min(which(is.na(out_geom$faces$i))))
    face_fill_end <- face_fill_start+nrow(geom$faces)-1
    out_geom$faces$i[face_fill_start:face_fill_end] <- geom$faces$i+vert_fill_start-1
    out_geom$faces$j[face_fill_start:face_fill_end] <- geom$faces$j+vert_fill_start-1
    out_geom$faces$k[face_fill_start:face_fill_end] <- geom$faces$k+vert_fill_start-1
    out_geom$faces$color[face_fill_start:face_fill_end] <- geom$faces$color
  }
  
  out_geom
}

make_base <- function(center_x, center_y, n_sides,
                      tile_color, border_color){
  interior_angle <- ((n_sides-2)*180)/n_sides
  border_apothem <- sin(interior_angle/360*2*pi/2)*hex_border_width*side_length
  border_corner_diff <- cos(interior_angle/360*2*pi/2)*hex_border_width*side_length
  central_hex <- render_gon(n_sides = n_sides, center_x = 0, center_y = 0,
                            side_length = side_length, color = tile_color)
  side_centers <- data.frame(x=rep(NA, n_sides), y=rep(NA, n_sides))
  side_vertices <- central_hex$vertices[c(2:nrow(central_hex$vertices), 2),]
  for(i in 1:(nrow(side_vertices)-1)){
    side_centers[i,c("x", "y")] <- colMeans(side_vertices[i:(i+1),c("x", "y")])
  }
  scale_factor=(side_length-hex_border_width*side_length/2)/side_length
  border_geoms <- apply(side_centers, 1, function(row_xy){
    poly_obj <- render_4gram(height=border_apothem, length_top = side_length, 
                             length_bottom = side_length-border_corner_diff*2, 
                             center_x = row_xy["x"]*scale_factor, 
                             center_y = row_xy["y"]*scale_factor,
                             color = border_color)
    gon_2_prism(poly_obj, elevation = 0, height = hex_border_height*side_length, color = border_color)
  })
  if(n_sides==6){
    side_rotations <- rev(1:n_sides*360/n_sides)+180
  } else if(n_sides==5){
    side_rotations <- rev(1:n_sides*360/n_sides)+162
  } else {
    stop("That number of sides hasn't been supported yet, sorry!")
  }
  correct_geoms <- mapply(rotate_geom, border_geoms, x_deg=0, y_deg=0, 
                          z_deg=side_rotations, SIMPLIFY = FALSE)
  made_base <- combine_geoms(c(list(central_hex), correct_geoms))
  move_geom(made_base, center_x, center_y)
}
hex_maker <- function(hex_type, n_sides, center_x, center_y, center_z, 
                      compass_angle, elevation_angle, rotation_angle){
  center_vec <- c(center_x, center_y, center_z)
  hex_fun <- get(paste0(hex_type, "_hex"))
  hex_fun(center_x = center_x, center_y=center_y, n_sides = n_sides) %>%
    move_geom(delta_z = center_z) %>%
    rotate_geom(z_deg = rotation_angle, manual_center = center_vec) %>%
    rotate_geom(y_deg = elevation_angle, manual_center = center_vec) %>%
    rotate_geom(z_deg = -compass_angle, manual_center = center_vec)
}


### Wood hex functions ----
wood_hex <- function(center_x, center_y, n_sides){
  wood_base <- make_base(center_x = center_x, center_y = center_y, n_sides = n_sides,
                         tile_color = forest_floor_color, border_color = hex_border_color)
  forest_add <- make_forest(center_x=center_x, center_y = center_y,
                            n_sides = n_sides)
  combine_geoms(list(wood_base, forest_add))
}
make_forest <- function(center_x, center_y, n_sides){
  if(forest_type=="mixed"){
    tree_df <- data.frame(tree_type=sample(c("pine", "deci"), size = ntrees, replace = TRUE))
  } else if(forest_type=="pine"){
    tree_df <- data.frame(tree_type=rep("pine", ntrees))
  } else if(forest_type=="deci"){
    tree_df <- data.frame(tree_type=rep("deci", ntrees))
  } else {
    stop(paste("Forest type", forest_type, "not supported"))
  }
  tree_df$tree_height <- runif(ntrees, min=side_length/4, max=side_length/2)
  tree_df$tree_height <- ifelse(tree_df$tree_height=="pine", 
                                tree_df$tree_height*1.2, tree_df$tree_height*0.9)
  tree_df$tree_color <- ifelse(tree_df$tree_type=="pine",
                               hsv(h = 0.3333, s = 1, v=runif(ntrees, min = 0.25, max = 0.55)),
                               hsv(h = runif(ntrees, min = 0.15, max = 0.4), s = 1, 
                                   v=runif(ntrees, min = 0.45, max = 0.75)))
  
  interior_angle <- (n_sides-2)*180 / n_sides
  apothem_length <- side_length/2 * tan(interior_angle/2 * pi/180)
  r <- rbeta(ntrees, shape1 = 3, shape2 = 3)*apothem_length*0.9
  theta <- runif(ntrees)*2*pi
  tree_df$x <- r*cos(theta)
  tree_df$y <- r*sin(theta)
  
  many_trees <- mapply(make_tree, type=tree_df$tree_type, height=tree_df$tree_height,
                       leafcolor=tree_df$tree_color, center_x=tree_df$x, 
                       center_y=tree_df$y, n_sides=n_sides, SIMPLIFY = FALSE)
  new_forest <- combine_geoms(
    many_trees
  )
  move_geom(new_forest, delta_x = center_x, delta_y = center_y)
}
make_tree <- function(center_x, center_y, n_sides, type, height, leafcolor){
  shadow_color <- rgb2hsv(col2rgb(forest_floor_color))-c(0,0,0.2)
  shadow_color["v",] <- max(0, shadow_color["v",])
  shadow_color <- hsv(h = shadow_color["h",], s = shadow_color["s",], shadow_color["v",])
  tree_shadow <- gon_2_prism(
    render_gon(n_sides=n_sides, center_x = center_x, center_y = center_y,
               side_length = height/3, color = shadow_color),
    elevation = 0, height = 0.001*height, color = shadow_color, pepper = FALSE
  )
  if(type=="deci"){
    trunk <- gon_2_prism(
      render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, 
                 side_length = height/8, color = "black"),
      height = height/2, elevation = 0, color = tree_bark_color
    )
    crown_bottom <- gon_2_point(
      render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, 
                 side_length = height/3, color = leafcolor),
      height = -height/8, elevation = height/2, color = leafcolor
    )
    crown_middle <- gon_2_prism(
      render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, side_length = height/3, color = leafcolor),
      height = height*3/8, elevation = height/2, color = leafcolor
    )
    crown_top <- gon_2_point(
      render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, side_length = height/3, color = leafcolor),
      height = height/8, elevation = height*7/8, color = leafcolor
    )
    return(combine_geoms(list(trunk, crown_bottom, crown_middle, crown_top, tree_shadow)))
  } else if(type=="pine"){
    trunk = gon_2_prism(
      render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, side_length = height/10, color = "black"),
      height = height/4, elevation = 0, color = tree_bark_color
    )
    crown = gon_2_point(
      render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, side_length = height/3, color = leafcolor),
      height = height*7/8, elevation = height/8, color = leafcolor
    )
    return(combine_geoms(list(trunk, crown, tree_shadow)))
  } else {
    stop("Only pine and deciduous (deci) trees supported currently, sorry!")
  }
}



### Ore hex functions ----
ore_hex <- function(center_x, center_y, n_sides){
  ore_base <- make_base(center_x = center_x, center_y = center_y, 
                        n_sides = n_sides,
                        tile_color = mountain_base_color, border_color = hex_border_color)
  ore_add <- make_range(center_x=center_x, center_y = center_y, n_sides = n_sides)
  return(combine_geoms(list(ore_base, ore_add)))
}
make_range <- function(center_x, center_y, n_sides){
  max_height <- side_length/2
  central_mount <- make_mountain(center_x=0, center_y=0, n_sides=n_sides, 
                                 height = max_height, width=max_height*0.7,
                                 snowcapped = TRUE)
  
  mount_df <- data.frame(height=runif(nmounts, min = max_height*0.5, max=max_height*0.8))
  mount_df$snowcap <- ifelse(mount_df$height>max_height*0.7, TRUE, FALSE)
  
  interior_angle <- (n_sides-2)*180 / n_sides
  apothem_length <- side_length/2 * tan(interior_angle/2 * pi/180)
  r <- rbeta(nmounts, shape1 = 3, shape2 = 3)*apothem_length*0.9
  theta <- runif(nmounts)*2*pi
  mount_df$x <- r*cos(theta)
  mount_df$y <- r*sin(theta)
  
  range <- mapply(make_mountain, center_x=mount_df$x, center_y=mount_df$y,
                  height=mount_df$height, width=mount_df$height*0.7, 
                  n_sides=n_sides, snowcapped=mount_df$snowcap, 
                  SIMPLIFY = FALSE)
  range <- combine_geoms(c(list(central_mount), range))
  range <- move_geom(range, delta_x = center_x, delta_y = center_y)
  range
}
make_mountain <- function(center_x, center_y, height, width, n_sides, 
                          snowcapped=FALSE){
  mount = gon_2_point(
    render_gon(n_sides = n_sides, center_x=center_x, center_y=center_y, side_length=width, 
               color=mountain_default_color),
    height = height, elevation = 0, color=mountain_default_color)
  if(snowcapped){
    snow_cap = gon_2_point(
      render_gon(n_sides = n_sides, center_x=center_x, center_y=center_y, side_length=0.9*width, 
                 color=snowcap_color), 
      height = 1.1*height, elevation=0, color=snow_default_color)
    mount <- combine_geoms(list(mount, snow_cap))
  }
  return(mount)
}



### Sheep hex functions ----
wool_hex <- function(center_x, center_y, n_sides){
  wool_base <- make_base(center_x = center_x, center_y = center_y, 
                         n_sides = n_sides,
                         tile_color = meadow_color, border_color = hex_border_color)
  wool_add <- make_flock(center_x=center_x, center_y = center_y,
                         n_sides = n_sides)
  return(combine_geoms(list(wool_base, wool_add)))
}
make_flock <- function(center_x, center_y, n_sides){
  sheep_df <- data.frame(size=runif(nsheep, min = side_length/10, max=side_length/5))
  
  interior_angle <- (n_sides-2)*180 / n_sides
  apothem_length <- side_length/2 * tan(interior_angle/2 * pi/180)
  r <- runif(nsheep)*apothem_length*0.7
  theta <- runif(nsheep)*2*pi
  sheep_df$x <- r*cos(theta)
  sheep_df$y <- r*sin(theta)
  sheep_df$angle <- runif(nsheep, min = 0, max = 360)
  
  flock <- mapply(make_sheep, center_x=sheep_df$x, center_y=sheep_df$y,
                  height=sheep_df$size, width=sheep_df$size,
                  n_sides=n_sides, SIMPLIFY = FALSE)
  flock <- mapply(rotate_geom, flock, x_deg=0, y_deg=0, 
                  z_deg=sheep_df$angle, SIMPLIFY = FALSE)
  flock <- combine_geoms(flock)
  flock <- move_geom(flock, delta_x = center_x, delta_y = center_y)
  flock
}

make_sheep <- function(center_x, center_y, height, width, n_sides, 
                       color=sheep_coat_color, headcolor=sheep_head_color){
  body = gon_2_prism(
    render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, 
               side_length = width, color = color),
    height = height, color = color, elevation = 0
  )
  head = gon_2_prism(
    render_gon(n_sides = n_sides, center_x = 0.9*width+center_x, 
               center_y = center_y, side_length = 0.4*width, color = headcolor),
    height = 0.6*height, color = headcolor, elevation=0.2*height
  )
  
  shadow_color <- rgb2hsv(col2rgb(meadow_color))-c(0,0,0.2)
  shadow_color["v",] <- max(0, shadow_color["v",])
  shadow_color <- hsv(h = shadow_color["h",], s = shadow_color["s",], shadow_color["v",])
  head_shadow <- gon_2_prism(
    render_gon(n_sides = n_sides, center_x = 0.9*width+center_x, 
               center_y = center_y, side_length = 0.4*width, color = shadow_color),
    height = 0.001*height, color = shadow_color, elevation=0
  )
  combine_geoms(list(body, head, head_shadow))
}


### Wheat hex functions ----
wheat_hex <- function(center_x, center_y, n_sides){
  wheat_base <- make_base(center_x = center_x, center_y = center_y, 
                          n_sides = n_sides,tile_color = dirt_color, 
                          border_color = hex_border_color)
  wheat_field <- make_field(center_x=center_x, center_y = center_y, n_sides = n_sides)
  wheat_sheaves <- make_sheaves(center_x = center_x, center_y = center_y,
                                n_sides = n_sides)
  return(combine_geoms(list(wheat_base, wheat_field, wheat_sheaves)))
}
make_field <- function(center_x, center_y, n_sides){
  harvested_part <- render_gon(n_sides=n_sides, center_x = center_x, 
                               center_y = center_y, side_length = side_length, 
                               color = dirt_color, pepper = FALSE)
  
  interior_angle <- (n_sides-2)*180 / n_sides
  apothem_length <- side_length/2 * tan(interior_angle/2 * pi/180)
  wheat_part <- gon_2_prism(
    render_4gram(height = apothem_length*0.95, length_top = side_length*0.95,
                 length_bottom = side_length+2*apothem_length*0.95*
                   tan((interior_angle-90)/180*pi),
                 center_x = center_x, center_y = center_y, 
                 color = light_wheat_color, pepper = FALSE),
    elevation = 0, height = side_length/21, color = light_wheat_color, pepper = TRUE
  )
  if(n_sides==5){
    wheat_part <- rotate_geom(wheat_part, z_deg = 90)
    wheat_part <- move_geom(wheat_part, delta_x = -apothem_length/2)
  } else if(n_sides==6){
    wheat_part <- move_geom(wheat_part, delta_y = -apothem_length/2)
  }
  
  combine_geoms(list(harvested_part, wheat_part))
}

make_sheaves <- function(center_x, center_y, n_sides){
  sheaf_df <- data.frame(id=seq_len(sample(2:5, 1)))
  
  interior_angle <- (n_sides-2)*180 / n_sides
  apothem_length <- side_length/2 * tan(interior_angle/2 * pi/180)
  r <- runif(nrow(sheaf_df))*apothem_length*0.8
  theta <- runif(nrow(sheaf_df))*2*pi
  sheaf_df$x <- r*cos(theta)
  sheaf_df$y <- r*sin(theta)
  if(n_sides==5){
    sheaf_df$x <- abs(sheaf_df$x)
  } else {
    sheaf_df$y <- abs(sheaf_df$y)
  }
  sheaf_df$angle <- runif(nrow(sheaf_df), min = 0, max = 360)
  sheaf_df$height <- runif(nrow(sheaf_df), min = side_length/5, max = side_length/3)
  sheaf_df$width <- runif(nrow(sheaf_df), min = side_length/8, max = side_length/6)
  
  sheaves <- mapply(make_sheaf, center_x = sheaf_df$x, center_y = sheaf_df$y,
                    height = sheaf_df$height, width=sheaf_df$width, 
                    n_sides=n_sides, SIMPLIFY = FALSE)
  sheaves <- mapply(rotate_geom, sheaves, x_deg=0, y_deg=0, 
                    z_deg=sheaf_df$angle, SIMPLIFY = FALSE)
  sheaves <- combine_geoms(sheaves)
  sheaves <- move_geom(sheaves, delta_x = center_x, delta_y = center_y)
  sheaves
}

make_sheaf <- function(center_x, center_y, height, width, n_sides){
  lower = gon_2_point(
    render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, 
               side_length = width, color = dark_wheat_color),
    height = height, color = dark_wheat_color, elevation = 0
  )
  upper = gon_2_point(
    render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, 
               side_length = width, color = wheat_color),
    height = -height, color = wheat_color, elevation = height
  )
  upper <- rotate_geom(upper, z_deg = (n_sides-2)*45/n_sides)
  
  combine_geoms(list(upper, lower))
}


### Brick hex functions ----
brick_hex <- function(center_x, center_y, n_sides){
  brick_base <- make_base(center_x = center_x, center_y = center_y, 
                          n_sides = n_sides,
                          tile_color = brick_base_color, 
                          border_color = hex_border_color)
  wall <- make_wall(center_x = center_x, center_y = center_y)
  return(combine_geoms(list(brick_base, wall)))
}
make_wall <- function(center_x, center_y){
  brick_df <- data.frame(x=c(0, side_length/5, -side_length/5, side_length/5, 
                             -side_length/5, 0, 0, -side_length/10, side_length/10,
                             side_length/2.5, -side_length/2.5))
  brick_df$y <- c(0, side_length/5, side_length/5, -side_length/5, 
                  -side_length/5, side_length/2.5, -side_length/2.5,
                  -side_length/10, side_length/10, 0, 0)
  brick_df$rotation <- c(0, 0, 0, 0, 0, 0, 0, 45, 135, 0, 0)
  brick_df$elevation <- c(0, 0, 0, 0, 0, 0, 0, side_length/5, side_length/5, 0, 0)
  wall <- mapply(make_brick, center_x = brick_df$x, center_y = brick_df$y,
                 elevation=brick_df$elevation,
                 rotation = brick_df$rotation, SIMPLIFY = FALSE)
  wall <- move_geom(combine_geoms(wall), delta_x = center_x, delta_y = center_y)
  rotate_geom(wall, z_deg = runif(1, 0, 360))
}
make_brick <- function(center_x, center_y, elevation, rotation){
  brick <- gon_2_prism(
    render_4gram(height = side_length/5.5, length_top = side_length/3,
                 length_bottom = side_length/3, center_x = center_x,
                 center_y = center_y, color = light_brick),
    elevation = elevation, height = side_length/5, color = dark_brick
  )
  rotate_geom(brick, z_deg = rotation)
}

### Snow hex functions ----
snow_hex <- function(center_x, center_y, n_sides){
  snow_base <- make_base(center_x = center_x, center_y = center_y, 
                         n_sides = n_sides,
                         tile_color = snow_default_color, 
                         border_color = hex_border_color)
  cloud <- make_cloud(center_x = center_x, center_y = center_y, n_sides=n_sides)
  storm <- make_snowfall(center_x = center_x, center_y = center_y, n_sides=n_sides)
  return(combine_geoms(list(snow_base, cloud, storm)))
}
make_cloud <- function(center_x, center_y, elevation, n_sides){
  big_puff <- make_puff(center_x = 0, center_y = 0, n_sides=n_sides,
                        size = side_length/2, base_elevation = side_length/3)
  r <- max(abs(big_puff$vertices$x), abs(big_puff$vertices$y))*0.9
  theta <- runif(npuffs)*2*pi
  puff_df <- data.frame(x=r*cos(theta))
  puff_df$y <- r*sin(theta)
  puff_df$size <- runif(npuffs, min = side_length/5, max = side_length/3)
  r <- max(abs(big_puff$vertices$x), abs(big_puff$vertices$y))*1.5
  theta <- runif(npuffs)*2*pi
  plus_df <- data.frame(x=r*cos(theta))
  plus_df$y <- r*sin(theta)
  plus_df$size <- runif(npuffs, min = side_length/10, max = side_length/6)
  puff_df <- rbind(plus_df, puff_df)
  
  other_puffs <- mapply(make_puff, center_x=puff_df$x, center_y=puff_df$y,
                        size=puff_df$size, n_sides=n_sides,
                        base_elevation = side_length/3, SIMPLIFY = FALSE)
  cloud <- combine_geoms(c(list(big_puff), other_puffs))
  move_geom(cloud, delta_x = center_x, delta_y = center_y)
}
make_puff <- function(center_x, center_y, size, base_elevation, n_sides){
  gon_2_prism(
    render_gon(n_sides = n_sides, center_x = center_x, center_y = center_y, 
               side_length = size, color = cloud_color),
    elevation = base_elevation, height = size, color = cloud_color, pepper = TRUE
  )
}
make_snowfall <- function(center_x, center_y, n_sides){
  r <- runif(nflakes, -side_length/2, side_length/2)*0.9
  theta <- runif(nflakes)*2*pi
  flake_df <- data.frame(x=r*cos(theta))
  flake_df$y <- r*sin(theta)
  flake_df$rotation <- runif(nflakes, min = 0, max = 360)
  flake_df$z <- runif(nflakes, min=0, max = side_length/3)
  
  flakes <- mapply(render_gon, n_sides=n_sides, center_x = flake_df$x, center_y = flake_df$y,
                   side_length=side_length/30, color=snowcap_color, z=flake_df$z, SIMPLIFY = FALSE)
  flakes <- mapply(rotate_geom, flakes, x_deg = 90, z_deg = flake_df$rotation, SIMPLIFY = FALSE)
  flakes <- combine_geoms(flakes)
  move_geom(flakes, delta_x = center_x, delta_y = center_y)
}

### Gamepiece functions ----
tower_maker <- function(center_x, center_y, size, color="grey50"){
  gon <- render_gon(n_sides = 4, center_x, center_y, 
                    side_length = size/3, color = "black")
  tower <- gon_2_prism(gon, elevation = 0, height = size, 
                       color = color, pepper = TRUE)
  top <- gon_2_point(gon, elevation = size, height = size/4, 
                     color = color)
  combine_geoms(c(list(tower), list(top)))
}
city_piece <- function(center_x, center_y, size=side_length/3, color="grey50"){
  castle_center <- render_gon(n_sides = 4, center_x = center_x, center_y = center_y, 
                              side_length = size, color = "black") %>%
    gon_2_prism(elevation = 0, height = size*2/3, color = color, pepper = TRUE)
  castle_top <- render_gon(n_sides = 4, center_x = center_x, center_y = center_y,
                           size, color = "black") %>%
    gon_2_point(elevation = size*2/3, height = size/2, color = color)
  tower_coords <- data.frame(x=c(0, 0, sqrt(2)/2, -sqrt(2)/2)*size*0.9+center_x,
                             y=c(sqrt(2)/2, -sqrt(2)/2, 0, 0)*size*0.9+center_y)
  tower_list <- mapply(tower_maker, tower_coords$x, tower_coords$y, size=size,
                       color=color, SIMPLIFY = FALSE)
  castle_obj <- combine_geoms(c(tower_list, list(castle_center, castle_top)))
  castle_obj$vertices["z"] <- castle_obj$vertices["z"]*0.8
  castle_obj
}

settlement_piece <- function(center_x, center_y, size=side_length/4, color="grey50"){
  wall_gon <- render_gon(n_sides = 4, center_x, center_y, size, color)
  settlement_height <- size
  settle_walls <- gon_2_prism(wall_gon, elevation = 0, height = size*2/3,
                              color = color, pepper = TRUE)
  top_gon <- render_gon(n_sides = 4, center_x, center_y, size/sqrt(2), 
                        color=color)
  settle_top <- gon_2_prism(top_gon, elevation = size/2-size/3, height = size,
                            color=color, pepper = TRUE)
  settle_top <- rotate_geom(settle_top, x_deg = 90, z_deg = 45)
  settlement <- combine_geoms(list(settle_walls, settle_top))
  # settlement$vertices[11:20,] <- settlement$vertices[11:20,]*0.999
}

# piece_maker <- function(piece_type, center_x, center_y, center_z,
#                         compass_angle, elevation_angle, color='grey50'){
#   center_vec <- c(center_x, center_y, center_z)
#   if(piece_type=="city"){
#     game_piece <- city_piece(center_x = center_x, center_y=center_y, color=color)
#   } else {
#     game_piece <- settlement_piece(center_x, center_y, color = color)
#   }
#   game_piece %>%
#     move_geom(delta_z = center_z) %>%
#     rotate_geom(z_deg = compass_angle, manual_center = center_vec) %>%
#     rotate_geom(y_deg = elevation_angle, manual_center = center_vec) %>%
#     rotate_geom(z_deg = -compass_angle, manual_center = center_vec)
# }
piece_maker <- function(piece_type, data_df_row, color='grey50'){
  center_vec <- unlist(data_df_row[,c("x", "y", "z")])
  if(piece_type=="city"){
    game_piece <- city_piece(center_x = data_df_row$x, 
                             center_y = data_df_row$y, 
                             color=color) %>%
      move_geom(delta_z = data_df_row$z) %>%
      rotate_geom(z_deg = data_df_row$compass_angle, manual_center = center_vec) %>%
      rotate_geom(y_deg = data_df_row$elevation_angle, manual_center = center_vec) %>%
      rotate_geom(z_deg = -data_df_row$compass_angle, manual_center = center_vec)
  } else if(piece_type=="settlement") {
    game_piece <- settlement_piece(center_x = data_df_row$x, 
                                   center_y = data_df_row$y, 
                                   color=color) %>%
      move_geom(delta_z = data_df_row$z) %>%
      rotate_geom(z_deg = data_df_row$compass_angle, manual_center = center_vec) %>%
      rotate_geom(y_deg = data_df_row$elevation_angle, manual_center = center_vec) %>%
      rotate_geom(z_deg = -data_df_row$compass_angle, manual_center = center_vec)
  } else if(piece_type=="road"){
    nearest_verts <- nearby_structures$edges[
      nearby_structures$edges$id==data_df_row$id, "nearest_verts"
    ]
    data_df_row <- TI_structure$vertices[nearest_verts[[1]][1],]
    data_df_row2 <- TI_structure$vertices[nearest_verts[[1]][2],]
    game_piece <- road_maker(coords_1 = unlist(data_df_row[,c("x", "y", "z")]),
                             coords_2 = unlist(data_df_row2[,c("x", "y", "z")]),
                             road_color = color)
  } else {
    stop(paste("Piece type", piece_type, "not supported"))
  }
  game_piece
}

post_maker <- function(coords_vec, post_color){
  scoords_vec <- cart2sphere(coords_vec)
  endcap_2 <- render_gon(n_sides = 3, center_x = coords_vec[1], 
                         center_y = coords_vec[2], 
                         side_length = side_length/5, post_color, TRUE) %>%
    gon_2_point(elevation = 0, height = side_length/8, post_color, FALSE) %>%
    move_geom(delta_z = coords_vec[3]) %>%
    rotate_geom(y_deg = scoords_vec[2]*180/pi, manual_center = coords_vec) %>%
    rotate_geom(z_deg = 360-scoords_vec[3]*180/pi+90, manual_center = coords_vec)
}
road_maker <- function(coords_1, coords_2, road_color){
  post1 <- post_maker(coords_1, road_color)
  post2 <- post_maker(coords_2, road_color)
  post2$faces[,1:3] <- post2$faces[,1:3]+5
  road_faces <- data.frame(
    i=4, k=c(1:3, 6:8), j=9, color=road_color
  )
  road_combo <- list(
    vertices=rbind(post1$vertices, post2$vertices),
    faces=rbind(post1$faces, post2$faces, road_faces)
  )
}


# Allocate resources and make manual rotations ----
getRandomGlobeLayout <- function(){
  globe_layout <- cbind(
    TI_structure$faces, n_vertices=lengths(nearby_structures$faces$nearest_verts)
  )
  globe_layout$rotation_angle <- ifelse(globe_layout$n_vertices==5, 
                                        globe_layout$compass_angle*6/5-90, 
                                        globe_layout$compass_angle)
  globe_layout$rotation_angle <- ifelse(globe_layout$n_vertices==5 & 
                                          globe_layout$elevation_angle > 90,
                                        globe_layout$rotation_angle+180, 
                                        globe_layout$rotation_angle)
  globe_layout$rotation_angle <- ifelse(globe_layout$id%in%c(158, 161, 164),
                                        globe_layout$rotation_angle+45,
                                        globe_layout$rotation_angle)
  globe_layout$rotation_angle <- ifelse(globe_layout$id%in%c(160, 163, 166),
                                        globe_layout$rotation_angle-45,
                                        globe_layout$rotation_angle)
  resources <- c("wood", "brick", "wool", "wheat", "ore")
  globe_layout$hex_resources <- c("snow", sample(rep(resources, 6)), "snow")
  globe_layout$pip <- c(0, sample(c(rep(2:6, 3), rep(8:12, 3))), 0)
  globe_layout
}

# Build world ----
worldbuilder <- function(globe_layout){
  combine_geoms(pbapply::pbmapply(
    hex_maker,
    hex_type=globe_layout$hex_resources,
    n_sides = globe_layout$n_vertices,
    center_x=globe_layout$x,
    center_y=globe_layout$y,
    center_z=globe_layout$z,
    compass_angle=globe_layout$compass_angle,
    elevation_angle=globe_layout$elevation_angle,
    rotation_angle=globe_layout$rotation_angle,
    SIMPLIFY = FALSE
  ))
}
# Generate a globe for debugging purposes
# globe_layout <- getRandomGlobeLayout()
# saveRDS(worldbuilder(globe_layout), file="debug_globe_plates.rds")
# saveRDS(globe_layout, file="debug_globe_layout.rds")
# globe_plates <- readRDS("debug_globe_plates.rds")
# globe_layout <- readRDS("debug_globe_layout.rds")



# Debug ----
# set_axis <- list(range=max(abs(globe_plates$vertices))*c(-1, 1),
#                  autorange=FALSE, showspikes=FALSE, fixedrange=TRUE,
#                  showgrid=FALSE, zeroline=FALSE, visible=FALSE)
# 
# base_globe <- plot_ly() %>%
#   add_trace(type="mesh3d",
#             x=globe_plates$vertices$x,
#             y=globe_plates$vertices$y,
#             z=globe_plates$vertices$z,
#             i=globe_plates$faces$i,
#             j=globe_plates$faces$j,
#             k=globe_plates$faces$k,
#             facecolor=rgb(t(col2rgb(globe_plates$faces$color)), maxColorValue = 255),
#             lighting=list(diffuse=1),
#             hoverinfo="none") %>%
#   layout(scene=list(
#     xaxis=set_axis, yaxis=set_axis, zaxis=set_axis,
#     aspectmode='cube',
#     camera=list(eye=list(x=0.8, y=0.8,z=0.8)),
#     bgcolor="black"),
#     margin=list(l=0, r=0, b=0, t=0, pad=0)) %>%
#   config(displayModeBar = FALSE, scrollZoom=FALSE)
# 
# base_globe

# Cleanup for sourcing ----
# rm(list = setdiff(ls(), 
#   c("TI_structure", "nearby_structures", "side_length", "hex_resources", 
#     "base_globe", "render_4gram", "render_gon", "sphere2cart", "cart2sphere", 
#     "colorParser", "colShift", "gon_2_point", "gon_2_prism", "globe_plates",
#     "piece_maker", "post_maker", "")))
