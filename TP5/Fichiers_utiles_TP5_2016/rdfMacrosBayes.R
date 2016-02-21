rdfBinarise <- function (image, seuil) {
  (image - seuil) >= 0
}

rdfProbaAPriori <- function (hbis, h, nbins) {
  sum(hbis$counts[0:(nbins-1)])/ sum(h$counts[0:(nbins-1)])
}

rdfProbaConditionnelle <- function (x, h, nbins) {
  h$counts[x]/sum(h$counts[0:(nbins-1)])
}