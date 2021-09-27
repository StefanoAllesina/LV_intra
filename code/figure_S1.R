rm(list = ls())

source("general_settings.R")
library(RColorBrewer)

get_figure_and_latex <- function(filepars, labelfile){
  general_model <- function(t, z, pars = NULL){
    z[z < THRESH] <- 0
    dzdt <- P %*% (r * z) + Q %*% (diag(z) %*% A %*% z)
    return(list(as.vector(dzdt)))
  }
  
  # load parameters
  print("loading pars")
  load(filepars)
  # integrate dynamics
  int_time <- seq(0, 1500, by = 0.05) # integration time
  model_out <- ode(y = z0, times = int_time, func = general_model, parms = NULL, method = "ode45")
  # prepare figure
  trajectories <- tibble()
  # make into tidy form for plotting
  model_out <- model_out %>% as.data.frame()
  member <- as.numeric(as.factor(member))
  npoints <- nrow(model_out)
  species <- unique(member)
  # create labels
  phenotype <- rep(1, length(member))
  for (i in 2:length(member)){
    if (member[i-1] == member[i]) phenotype[i] <- phenotype[i - 1] + 1
  }
  labels <- character(0)
  for (i in 1:length(member)){
    if (typesp[i] == 1) lb <- " producer"
    if (typesp[i] == 2) lb <- " consumer"
    lb <- paste0("population ", member[i], ":", lb, "_", phenotype[i])
    labels <- c(labels, lb)
  }
  # rename columns
  colnames(model_out) <- c("time", labels)
  #create color scheme
  pl <- c("Blues", "Oranges", "Purples", "Greens", "Reds","Greys")
  mycols <- character(0)
  for (i in 1:length(species)){
    mycols <- c(mycols, 
              rev(brewer.pal(6, name=pl[i]))[1:sum(member == i)])
  }
  # make in tidy form and plot
  mp_out <- model_out %>% as_tibble(.name_repair = "minimal") %>% 
    pivot_longer(-time) %>% 
    separate(col = name, into = c("species", "pheno"), sep = "_", remove = FALSE)
  pl_si <- ggplot(data = mp_out) + aes(x = time, y = value, colour = name) + 
    geom_line() + scale_y_sqrt("density") + scale_color_manual(values = mycols) + 
    theme_bw() + theme(legend.position = "none") + facet_wrap(~species, scales = "free")
  show(pl_si)

  # write the latex
  lt <- "\\begin{equation*}
  \\begin{aligned}
  "
  # r
  lt <- paste0(lt,
               "r &= \\begin{pmatrix}
  ")
  m <- length(member)
  for (i in 1:m) lt <- paste0(lt, r[i], "\\\\
  ")
  lt <- paste0(lt,
               "\\end{pmatrix} \\quad
  ")
  
  # P
  lt <- paste0(lt,
               "P &= \\begin{pmatrix}
  ")
  m <- length(member)
  for (i in 1:m) {
    for (j in 1:(m - 1)) lt <- paste0(lt, round(P[i,j], 2), " & ")
    lt <- paste0(lt, round(P[i,j + 1], 2), " \\\\
  ")
  }
  lt <- paste0(lt,
               "\\end{pmatrix} \\quad \\\\
  ")
  
  # Q
  lt <- paste0(lt,
               "Q &= \\begin{pmatrix}
  ")
  m <- length(member)
  for (i in 1:m) {
    for (j in 1:(m - 1)) lt <- paste0(lt, round(Q[i,j], 2), " & ")
    lt <- paste0(lt, round(Q[i,j + 1], 2), " \\\\
  ")
  }
  lt <- paste0(lt,
               "\\end{pmatrix} \\quad
  ")
  
  # A
  lt <- paste0(lt,
               "A &= \\begin{pmatrix}
  ")
  m <- length(member)
  for (i in 1:m) {
    for (j in 1:(m - 1)) lt <- paste0(lt, round(A[i,j], 2), " & ")
    lt <- paste0(lt, round(A[i,j + 1], 2), " \\\\
  ")
  }
  lt <- paste0(lt,
               "\\end{pmatrix}
  ")
  
  
  lt <- paste0(lt,
               "\\end{aligned}
  \\end{equation*}")
  
  # save latex file
  fileConn <- file(paste0("../figures/", labelfile, ".tex"))
  writeLines(lt, fileConn)
  close(fileConn)
  # save the plot
  ggsave(pl_si, filename = paste0("../figures/", labelfile, ".pdf"), width = 8.5 / 2, height = 8.5 / 2)
}

get_figure_and_latex("S1_par_5505.RData", "figure_S1")
