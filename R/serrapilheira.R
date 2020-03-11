## fauna associada à serripilheria acumulada em E. brasiliensis

# lendo os dados
serrap <- read.csv("./data/serrapilheira.csv")

# H1
# biomassa=glm(biomassa~folhas*altura)
serrap$biomassa_serrapilheira_g = glm(serrap$biomassa_serrapilheira_g ~ serrap$num_folhas*serrap$altura_total_cm)
anova(serrap$biomassa_serrapilheira_g,test="Chisq")

# aqui, plotar as 2 paradas
