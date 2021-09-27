source("general_settings.R")
general_model <- function(t, z, pars = NULL){
  z[z < THRESH] <- 0
  dzdt <- P %*% (r * z) + Q %*% (diag(z) %*% A %*% z)
  return(list(as.vector(dzdt)))
}
set.seed(12) #2

int_time <- seq(0, 100, by = 0.01) # integration time
# store data for figure
# trajectories
trajectories <- tibble()
# initial conditions
initial_conditions <- tibble()
# equilibria
equilibria <- tibble()

model <- "ii) Consumer-resource model"
# model parameters
n <- 4
r <- c(1,1,-1.1,-1)
Q <- diag(rep(1, n))
P <- matrix(c(
  1/2, 1/2, 0, 0,
  1/2, 1/2, 0, 0,
  0, 0, 1, 0,
  0, 0, 0, 1), n, n, byrow = TRUE)
Ai <- -(round(matrix(runif(4, 0, 2), 2, 2), 1))

print(Ai)

A <- matrix(c(
  0, 0, Ai[1,1], Ai[1,2],
  0, 0, Ai[2,1], Ai[2,2],
  -Ai[1,1], -Ai[2,1], 0, 0, 
  -Ai[1,2], -Ai[2,2], 0, 0), n, n, byrow = TRUE)
# initial conditions
z0 <- c(0.5, 0.5, 1, 1)
model_out <- ode(y = z0, times = int_time, func = general_model, parms = NULL, method = "ode45")

# make into tidy form for plotting
model_out <- model_out %>% as.data.frame()
npoints <- nrow(model_out)
colnames(model_out) <- c("time", "prey1", "prey2", "predator1", "predator2")
mp_out <- model_out %>% as_tibble(.name_repair = "minimal") %>% 
  pivot_longer(-time) %>% add_column(model = model)

pl2 <- ggplot(data = mp_out) + aes(x = time, y = value, colour = name) + 
  geom_line() + scale_y_sqrt("density") + scale_color_manual(values = rev(p2p2_colors)) + 
  theme_bw() + theme(legend.position = "none") + facet_wrap(~model)
show(pl2)
ggsave(pl2, filename = "../figures/figure_3b_base.pdf", width = 8.5 / 2, height = 8.5 / 2)
