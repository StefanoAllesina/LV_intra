source("general_settings.R")
int_time <- seq(0, 150, by = 0.05) # integration time
# store data for figure
# trajectories
trajectories <- tibble()
# initial conditions
initial_conditions <- tibble()
# equilibria
equilibria <- tibble()

# many predator phenotypes
many_preds <- function(t, z, pars = NULL){ 
  z[z < THRESH] <- 0
  x <- z[1]
  y <- z[-1]
  avgy <- mean(y)
  dzdt <- list(c(
    x * (1 - sum(y)), # dx/dt
    -y * di + avgy * x # dy_i/dt
  ))
}

model <- "Many predator phenotypes"
set.seed(2) # for reproducibility
# take 10 phenotypes, some with the same death rate
di <- sample(c(0.75, 1.5, 0.5), 10, replace = TRUE) 
di <- di / mean(di)# the average is 1
set.seed(13)
z0 <- runif(11) * 2


mp_out <- ode(y = z0, times = int_time, func = many_preds, parms = NULL, method = "ode45")
# make into tidy form for plotting
mp_out <- mp_out %>% as.data.frame()
colnames(mp_out) <- c("time", "prey", paste("predator", di))

mp_out <- mp_out %>% as_tibble(.name_repair = "minimal") %>% 
  pivot_longer(-time) %>% group_by(time, name) %>% summarise(value = sum(value))

pl2 <- ggplot(data = mp_out) + aes(x = time, y = value, colour = name) + 
  geom_line() + scale_y_sqrt("density") + scale_color_manual(values = rev(four_colors)) + 
  theme_bw() + theme(legend.position = "none")

for_histo <- table(di) %>% as_tibble()

pl_inset <- ggplot(for_histo) + 
  aes(x = as.numeric(di), y = n / sum(n), fill = di) + 
  geom_col() + scale_fill_manual(values = rev(four_colors)) + 
  theme_bw() + theme(legend.position = "none") + 
  xlab("mortality rate") + ylab("probability")

figure_2 <- pl2 + 
  annotation_custom(
    ggplotGrob(pl_inset), 
    xmin = 100, xmax = 150, ymin = 1.25, ymax = 2.7
  )

ggsave(figure_2, filename = "../figures/figure_2.pdf", width = 6.5, height = 3)
show(figure_2)