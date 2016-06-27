# PCA Economy

library("FactoMineR")
library("ggplot2")
library("factoextra")

head(trEco)

cor.mat <- round(cor(trRule))

library(corrplot)
corrplot(cor.mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

library("PerformanceAnalytics")
chart.Correlation(trEco, histogram = T, pch = 19)

res.pca <- PCA(trEco, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()
res.pca$var$contrib
res.pca$var$cor

FinaceStatus <- Q102674*0.69741563 + Q106388*0.45840428 + Q108343*0.67475749
        + Q116441*0.56444927 + Q123621*0.54434040


# Education
res.pca <- PCA(trEdu, scale.unit = T, ncp = 2, quali.sup = 1, graph = F)
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$eig
res.pca$var$contrib
res.pca$var$cor

EducationStatus <- Q120379*0.6497771 + Q122771*-0.4915218 + Q119851*0.5922486

# Rules
res.pca <- PCA(trRule, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Rule <- Q98578*0.49115409  + Q102687*0.47049496 + Q116197*0.45420927 + Q116797*0.46253573

## Family
res.pca <- PCA(trFam, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Fam <- Q98059*-0.68715014 + Q120650*-0.49784237 +  Q108617*0.66863893 +  Q99716*0.63392069


## Social
res.pca <- PCA(trSoc, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Soc <- Q106997*-0.65758066 + Q108856*-0.66264616 + Q113584*-0.51027050

## Attitude
res.pca <- PCA(trAtt, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Att <- Q119650*-0.5884656 + Q98869*-0.6552226 + Q114961*0.5760039 + Q116881*-0.4754082

## Conservative
res.pca <- PCA(trCnsv, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Con <- Q118117*-0.58626035 + Q121700*-0.62746999 + Q116448*-0.58817316

## Self-efficiency
res.pca <- PCA(trSlf, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Slf <- Q106993*-0.58365703 + Q107869*-0.40823785 + Q112512*-0.45264414
       + Q120014*-0.47422437 + Q122120*0.53987832

## Frustration
res.pca <- PCA(trFrt, scale.unit = T, ncp = 1, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Frst <- Q105840*-0.578957504 + Q100680*-0.527635468 + Q120012*-0.420185112
        + Q102906*-0.562846897 + Q118237*-0.569155604

## Unknown
res.pca <- PCA(trUnk, scale.unit = T, ncp = 5, quali.sup = 1, graph = F)
res.pca$eig
print(res.pca)
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
        scale_color_gradient2(low="white", mid="blue", 
                              high="red", midpoint=55)+theme_bw()

res.pca$var$contrib
res.pca$var$cor

Frst <- Q105840*-0.578957504 + Q100680*-0.527635468 + Q120012*-0.420185112
+ Q102906*-0.562846897 + Q118237*-0.569155604

