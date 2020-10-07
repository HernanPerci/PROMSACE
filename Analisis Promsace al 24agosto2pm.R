
## Tomaremos como variables de analisis a las variables
## provenientes de las preguntas 2 y 3, luego procederemos
## a encontrar un perfil similar entre los individuos que
## responden similares y trataremos de predecir a cuales 
## categorias de las variables organo de linea, sexo, años 
## trabajando para el estado y nivel de gobierno 
## estan mas asociadas.

library(readxl)
promsace_28agosto2am <- read_excel("promsace 28agosto2am.xlsx", 
                    sheet = "R", range = "C1:V10089")

promsace_28agosto2am[] <- lapply(promsace_28agosto2am, 
                                 as.factor)

promsace_28agosto2am$P5 <- factor(promsace_28agosto2am$P5, levels = c("Órgano de Apoyo (recursos humanos, administración, tecnologías de la información, otros)", "Órgano de Asesoría (jurídica, planeamiento, presupuesto, otros)", "Órgano de Línea", "Otros"), labels = c("apoyo", "asesoria", "linea", "Otros"))

promsace_28agosto2am$P7  <- factor(promsace_28agosto2am$P7 , levels = c("Menos de dos años", "De dos a cinco años", "De seis a diez años", "Más de diez años"), labels = c("[0 - 2>", "[2 - 5]", "[6 - 10]", "[11 a más>"))

summary(promsace_28agosto2am)

## Las principales fuentes de repspuestas positivas y 
## que no distorcionan el analisis provienen de las variables
## P2.1 P2.2 P2.9 P3.1 P3.2 P3.3 P3.4 y P3.5

## analisis multivariado inicial
## ----------------

library(FactoMineR)

res <- MCA(promsace_28agosto2am, quali.sup = c(3, 4, 5, 6, 7, 8, 10, 11, 17, 18, 19, 20), graph = FALSE)

library(factoextra)

fviz_screeplot(res, addlabels = TRUE, ylim = c(0, 60))

fviz_mca_var(res, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_mca_var(res,
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_mca_var(res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())

fviz_cos2(res, choice = "var", axes = 1:2)

fviz_mca_var(res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)

fviz_contrib(res, choice = "var", axes = 1:2)

fviz_mca_ind(res, col.ind = "cos2", label = "none",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_ellipses(res, c(1,2),
              geom = "point")

fviz_ellipses(res, c(3,4),
              geom = "point")

fviz_ellipses(res, c(5,6),
              geom = "point")

fviz_ellipses(res, c(7,8),
              geom = "point")

fviz_ellipses(res, c(9,10),
              geom = "point")

fviz_ellipses(res, c(11,12),
              geom = "point")

fviz_ellipses(res, c(13,14),
              geom = "point")

fviz_ellipses(res, c(15,16),
              geom = "point")

fviz_ellipses(res, c(17,18),
              geom = "point")

fviz_ellipses(res, c(19,20),
              geom = "point")

## mejorar MCA con NA como nuevas categorias
## ---------------
## imputar la data

library(missMDA)
complete <- imputeMCA(promsace_28agosto2am, quali.sup = c(3, 4, 5, 6, 7, 8, 10, 11, 17, 18, 19, 20), ncp = 5)
names(complete)
head(complete$tab.disj)

## mejoramos MCA con la tabla disyuntiva completa

res <- MCA(promsace_28agosto2am, quali.sup = c(3, 4, 5, 6, 7, 8, 10, 11, 17, 18, 19, 20), tab.disj = complete$tab.disj, graph = FALSE)
res$eig

fviz_screeplot(res, addlabels = TRUE, ylim = c(0, 20))

fviz_mca_var(res, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_mca_var(res,
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_mca_var(res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())

fviz_cos2(res, choice = "var", axes = 1:2)

fviz_mca_var(res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)

fviz_contrib(res, choice = "var", axes = 1:2)

fviz_mca_ind(res, col.ind = "cos2", label = "none",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_ellipses(res, c(1,2),
              geom = "point")

fviz_ellipses(res, c(3,4),
              geom = "point")

fviz_ellipses(res, c(5,6),
              geom = "point")

fviz_ellipses(res, c(7,8),
              geom = "point")

fviz_ellipses(res, c(9,10),
              geom = "point")

fviz_ellipses(res, c(11,12),
              geom = "point")

fviz_ellipses(res, c(13,14),
              geom = "point")

fviz_ellipses(res, c(15,16),
              geom = "point")

fviz_ellipses(res, c(17,18),
              geom = "point")

fviz_ellipses(res, c(19,20),
              geom = "point")

## cluster herarquico ascendente
## ---------------------

res.hcpc <- HCPC(res, kk = 100, min = 3, max = 10, consol = FALSE, graph = FALSE)

# Dendrograma

fviz_dend(res.hcpc, show_labels = FALSE)

# mapa de factores de individuos

fviz_cluster(res.hcpc, geom = "point", main = "Factor map")

## salidas
## --------------------

## Resultados mas importantes

names(res.hcpc)
res.hcpc$call$t
res.hcpc$data.clust
res.hcpc$desc.var
res.hcpc$desc.ind
