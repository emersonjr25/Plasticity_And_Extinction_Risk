##### trabalho victoria ####

dados <- read.csv2('data/base_de_dados_risco_de_extincao.csv')

dados$Resume[which(dados$Resume == "Nameacado")] <- "Nao Ameacado"
dados$Resume[which(dados$Resume == "Ameacado")] <- "Ameacado"

### ORGANIZING DATA ###

plasticity <- dados$Plasticity[which(dados$Plasticity != 0)]
extinctionrisk <- dados$Resume[which(dados$Plasticity != 0)]

pos <- which(extinctionrisk != 0)

plasticity <- plasticity[pos]
plasticity <- as.integer(plasticity)
extinctionrisk <- extinctionrisk[pos]

### ANOVA ###

#plot(plasticity ~ as.factor(extinctionrisk))

### ANOVA LOG ###

#posicao <- which(plasticity != 0)
#plasticity[posicao] <- log(plasticity[posicao])
#plot(plasticity ~ as.factor(extinctionrisk))
#summary(aov(plasticity ~ extinctionrisk))


##### CHI SQUARE ####

plasticity[which(plasticity != 0)] <- "Muita Plasticidade"
plasticity[which(plasticity == 0)] <- "Pouca Plasticidade"
#sum(plasticity == "Without plasticity")

plasticity <- factor(plasticity, levels = c("Pouca Plasticidade", "Muita Plasticidade"))

count <- table(extinctionrisk, plasticity)

count

chisq.test(count)

barplot(count, legend.text = extinctionrisk[c(12, 13)],
        xlab = "Plasticidade", ylab = "Frequ?ncia",
        args.legend = list(x = 2.4,
                           y = 47))

