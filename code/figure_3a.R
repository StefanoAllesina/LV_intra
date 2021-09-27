source("general_settings.R")
general_model <- function(t, z, pars = NULL){
  z[z < THRESH] <- 0
  dzdt <- P %*% (r * z) + Q %*% (diag(z) %*% A %*% z)
  return(list(as.vector(dzdt)))
}

int_time <- seq(0, 200, by = 0.01) # integration time
# store data for figure
# trajectories
trajectories <- tibble()
# initial conditions
initial_conditions <- tibble()
# equilibria
equilibria <- tibble()

model <- "i) Predator with two heritable phenotypes"
# model parameters
n <- 3
d <- 1/3
a <- 75/100
r <- c(1, -(1 + d), -(1 - d))
P <- diag(rep(1, n))
Q <- matrix(c(
  1, 0, 0,
  0, a, (1-a),
  0, (1-a), a), n, n, byrow = TRUE)
A <- matrix(c(
  0, -1, -1,
  1, 0, 0,
  1, 0, 0), n, n, byrow = TRUE)
# initial conditions
z0 <- c(0.5, 5.75, 1.25)
initial_conditions <- tibble(model = model, 
       prey = z0[1], predator = z0[2:3],  
       phenotype = c("1", "2"))
equilibria<- tibble(model = model, 
             prey = (9 -sqrt(17))/6, 
             predator = c((5 - sqrt(17))/4, (-1 + sqrt(17))/4), 
             phenotype = c("1", "2"))

model_out <- ode(y = z0, times = int_time, func = general_model, parms = NULL, method = "ode45")

# make into tidy form for plotting
model_out <- model_out %>% as.data.frame()
npoints <- nrow(model_out)
colnames(model_out) <- c("time", "prey", "predator", "predator")
model_out <- rbind(model_out[, -4],
                   model_out[, -3])
colnames(model_out) <- c("time", "prey", "predator")
model_out <- model_out %>% mutate(model = model, phenotype = c(rep("1", npoints), rep("2", npoints)))
trajectories <- model_out

pl1 <- ggplot(data = trajectories %>% filter(time < 0.25)) + 
  aes(x = prey, y = predator, colour = phenotype) + 
  geom_path(arrow = arrow(type = "closed", length = unit(0.1, "inches"))) + 
  geom_path(data = trajectories %>% filter(time >= 0.2)) + 
  geom_point(data = initial_conditions, size = 2) + 
  geom_point(data = equilibria, size = 2, shape = 8, col = "black") + 
  facet_wrap(~model, scales = "free") + 
  scale_color_manual(values = three_colors[-1]) + 
  theme_bw() + theme(legend.position = "none")

show(pl1)
ggsave(pl1, filename = "../figures/figure_3a_base.pdf", width = 8.5 / 2, height = 8.5 / 2)


