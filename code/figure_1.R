source("general_settings.R")
int_time <- seq(0, 150, by = 0.05) # integration time
# store data for figure
# trajectories
trajectories <- tibble()
# initial conditions
initial_conditions <- tibble()
# equilibria
equilibria <- tibble()

# original Lotka-Volterra model
LV <- function(t, z, pars = NULL){
  z[z < THRESH] <- 0
  x <- z[1]
  y <- z[2]
  dzdt <- list(c(
    x * (1 - y), # dx/dt
    y * (x - 1) # dy/dt
  ))
}
# two prey phenotypes
two_prey <- function(t, z, pars = NULL){
  z[z < THRESH] <- 0
  x1 <- z[1]
  x2 <- z[2]
  y <- z[3]
  dzdt <- list(c(
    (x1 + x2) / 2 - e1 * x1 * y, # dx1/dt
    (x1 + x2) / 2 - e2 * x2 * y, # dx2/dt
    y * (e1 * x1 + e2 * x2 - 1) # dy/dt
  ))
}
# two predator phenotypes
two_pred <- function(t, z, pars = NULL){
  z[z < THRESH] <- 0
  x <- z[1]
  y1 <- z[2]
  y2 <- z[3]
  dzdt <- list(c(
    x * (1 - y1 - y2), # dx/dt
    -d1 * y1 + (y1 + y2) * x / 2, # dy1/dy
    -d2 * y2 + (y1 + y2) * x / 2 # dy2/dy
  ))
}

# model (for panel)
model <- "i) Lotka-Volterra"
# initial conditions
z0 <- c(1.5,1.5)
initial_conditions <- rbind(initial_conditions, 
                            tibble(model = model, 
                                   prey = z0[1], predator = z0[2], 
                                   phenotype = "0"))
equilibria <- rbind(equilibria, 
                    tibble(model = model, 
                           prey = 1, predator = 1, 
                           phenotype = "0"))

lv_out <- ode(y = z0, times = int_time, func = LV, parms = NULL, method = "ode45")
# make into tidy form for plotting
lv_out <- lv_out %>% as.data.frame()
colnames(lv_out) <- c("time", "prey", "predator")
lv_out <- lv_out %>% add_column(phenotype = "0", model = model)
trajectories <- rbind(trajectories, lv_out)

model <- "ii) Two predator phenotypes"
# model parameter (death rates)
d <- 1/2
d1 <- 1 + d
d2 <- 1 - d
# initial conditions
z0 <- c(1.5, 0.75, 0.75)
initial_conditions <- rbind(initial_conditions, 
                            tibble(model = model, 
                                   prey = z0[1], predator = z0[2:3],  
                                   phenotype = c("1", "2")))
equilibria <- rbind(equilibria, 
                    tibble(model = model, 
                           prey = c(1 - d^2), predator = c(d2 / 2, d1 / 2), 
                           phenotype = c("1", "2")))

two_pred_out <- ode(y = z0, times = int_time, func = two_pred, parms = NULL, method = "ode45")
# make into tidy form for plotting
two_pred_out <- two_pred_out %>% as.data.frame()
npoints <- nrow(two_pred_out)
colnames(two_pred_out) <- c("time", "prey", "predator", "predator")
two_pred_out <- rbind(two_pred_out[, -4],
                      two_pred_out[, -3])
colnames(two_pred_out) <- c("time", "prey", "predator")
two_pred_out <- two_pred_out %>% mutate(model = model, phenotype = c(rep("1", npoints), rep("2", npoints)))
trajectories <- rbind(trajectories, two_pred_out)

model <- "iii) Two prey phenotypes"
# model parameter (predation rates)
e <- 1/4
e1 <- 1 + e
e2 <- 1 - e
# initial conditions
z0 <- c(0.75, 0.75, 1.5)
initial_conditions <- rbind(initial_conditions, 
                            tibble(model = model, 
                                   prey = z0[1:2], predator = z0[3], 
                                   phenotype = c("1", "2")))
equilibria <- rbind(equilibria, 
                    tibble(model = model, 
                           prey = c(1 / (2 * e1), 1 / (2 * e2)), predator = 1 / (e1 * e2), 
                           phenotype = c("1", "2")))

two_prey_out <- ode(y = z0, times = int_time, func = two_prey, parms = NULL, method = "ode45")
# make into tidy form for plotting
two_prey_out <- two_prey_out %>% as.data.frame()
npoints <- nrow(two_prey_out)
colnames(two_prey_out) <- c("time", "prey", "prey", "predator")
two_prey_out <- rbind(two_prey_out[, -3],
                      two_prey_out[, -2])
colnames(two_prey_out) <- c("time", "prey", "predator")
two_prey_out <- two_prey_out %>% mutate(model = model, phenotype = c(rep("1", npoints), rep("2", npoints)))
trajectories <- rbind(trajectories, two_prey_out)

pl1 <- ggplot(data = trajectories %>% filter(time < 0.5)) + 
  aes(x = prey, y = predator, colour = phenotype) + 
  geom_path(arrow = arrow(type = "closed", length = unit(0.1, "inches"))) + 
  geom_path(data = trajectories %>% filter(time >= 0.45)) + 
  geom_point(data = initial_conditions, size = 2) + 
  geom_point(data = equilibria, size = 2, shape = 8, col = "black") + 
  facet_wrap(~model, scales = "free") + 
  scale_color_manual(values = three_colors) + 
  theme_bw() + theme(legend.position = "none")

show(pl1)

ggsave(pl1, filename = "../figures/figure_1.pdf", width = 8.5, height = 8.5 / 3)