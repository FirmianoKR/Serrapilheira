## fauna associada à serripilheria acumulada em E. brasiliensis

serrap <- read.csv("./data/serrapilheira.csv")

biomassa <- serrap$biomassa_serrapilheira_g
folhas <- serrap$num_folhas
altura <- serrap$altura_total_cm

# mod1
mod_1 <- glm(biomassa ~ folhas)
anova(mod_1,test = "Chisq")
# mod2
mod_2 <- glm(biomassa ~ altura)
anova(mod_2,test = "Chisq")

# plot1
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1))
plot(
  biomassa ~ folhas,
  pch = 16,
  las = 1,
  xlab = "Número de folhas",
  ylab = "Serrapilheira acumulada (g)"
)
abline(mod_1, lty = 2, col = "red")

# plot2
plot(
  biomassa ~ altura,
  pch = 16,
  las = 1,
  xlab = "Altura (cm)",
  ylab = "Serrapilheira acumulada (g)"
)
abline(mod_2, lty = 2, col = "red")
dev.off()
