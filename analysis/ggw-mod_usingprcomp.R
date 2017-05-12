library(stats)
library(pracma)

ncomp <- 5
# ncomp <- 2
# ncomp <- 1

pca_temp        <- prcomp(d1)
rawLoadings     <- pca_temp$rotation[,1:ncomp] %*% diag(pca_temp$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(d1) %*% invLoadings
print(scores)                   # Scores computed via rotated loadings

fa.sort(rawLoadings)
fa.sort(rotatedLoadings)
