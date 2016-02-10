# -----------------------------------------------------------------------
# Extraction d'attributs de pixels pour la classification,
# Module RdF, reconnaissance de formes
# Copyleft (C) 2014, Universite Lille 1
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# -----------------------------------------------------------------------

# Chargement des fonctions externes
library ("EBImage")
source ("rdfSegmentation.R")

# Chargement d'une image
nom <- "2classes_100_100_8bits_2016.png"
image <- rdfReadGreyImage (nom)

#nomBinaire <- "rdf-masque-ronds.png"
#imageBinaire <- rdfReadGreyImage (nomBinaire)

# Calcul et affichage de son histogramme
nbins <- 1024
#h <- hist (as.vector (image), breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation
seuil <- 0.47
binaire <- 1*(image - seuil) >= 0
pourcentageErreur <- (sum(image != binaire) / length(binaire)) *100

# segmentation texture
#texture <- rdfTextureEcartType(image, 2)
#h <- hist (as.vector (texture), breaks = seq (0, 1, 1 / nbins))
#seuil <- 0.41
#binaire <- -1*(texture - seuil) >= 0
#pourcentageErreur <- (sum(imageBinaire != binaire) / length(binaire)) *100

# histogramme conjoint
#texture <- rdfTextureEcartType(image, 2)
#h <- rdfCalculeHistogramme2D(image, 256, texture, 256);
#print(pourcentageErreur)

# Affichage des deux images
#if (interactive ()) {
  #display (texture, nom)
  #display (binaire, "image binaire")
#  display(h, "histogramme2D");
#}
