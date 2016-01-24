# -----------------------------------------------------------------------
# Extraction d'attributs de contours,
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
source ("rdfContours.R")

# Chargement d'un contour
nom <- "rdf-carre-80.txt"
cont <- rdfChargeFichierContour (nom)

# Afficher le contour
plot (cont, main = nom, type = "o", asp = 1, col = "red",
      ylim = rev (range (Im (cont))))
fourier = fft(cont, FALSE)/length(cont)

#on modifie fourier[1] pour modifier le Z0 et ainsi le centre de gravité
#fourier[1]= 3+3i

#con4 = cont[c(TRUE, FALSE, FALSE, FALSE)]
#con8 = con4[c(TRUE,FALSE)]
#lines(con4,type = "o", col="blue")
#lines(con8, type = "o", col= "green")
fourier03 = rdfAnnuleDescFourier(fourier, 0.10)
lines(fft(fourier, TRUE), type = "o", col= "yellow")
lines(fft(fourier03, TRUE), type = "o", col= "blue")
lines(fft(rdfAnnuleDescFourier(fourier, 0.5), TRUE), type = "o", col= "green")
