# -----------------------------------------------------------------------
# Extraction d'attributs de forme,
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

library( "EBImage" )
library( abind );

# Chargement d'une image en niveaux de gris
rdfReadGreyImage <- function (nom) {
  image <- readImage (nom)
  if (length (dim (image)) == 2) {
    image
  } else {
    channel (image, 'red')
  }
}

splitImageArray <- function( images, rows, cols, height, width ) {
	result <- array( , dim = c( height, width, rows * cols ) );
	for ( i in 1:rows ) {
		for ( j in 1:cols ) {
			result[ , , (j-1)*rows+i ] <- images[((i-1)*height+1):(i*height), ((j-1)*width+1):(j*width)];
		}
	}
	return (result);
}

# read USPS digits files
# Files must be stored in a single folder and their names must have the
# following format : usps_digit.jpg, where digit is in [0;9].
# Image data is stored in a 16x16x11000 array.
# Image matrices are non-transposed 16x16 matrices.
# Image labels is a vector of integers
#
# params: folder - the folder where the files are stored
#
# returns: a list containing image data and image labels (in [0;9])
#
readUSPSdata <- function( folder ) {

	# output data: image matrix and labels vectors
	data <- array(, dim=c(16, 16, 0) );
	labels <- vector( , length=0 );

	# read the image files 
	for ( i in 0:9 ) {
		file <- sprintf( "%s/usps_%d.png", folder, i );
		image <- rdfReadGreyImage( file );
		image <- t( imageData( image ) );
		images <- splitImageArray( image, 34, 33, 16, 16 );
		images <- images[ , , 1:1100 ];
		data <- abind( data, images, along=3 );
		labels <- c( labels, rep ( i, 1100 ) );
	}

	return ( list(data, labels) );
}

