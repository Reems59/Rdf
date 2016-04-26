# enlarge image to ensure all borders are 0
addBorder <- function( image ) {
	output = matrix( 0, nrow=(nrow(image)+2), ncol=(ncol(image)+2) );
	output[ 2:(nrow(output)-1), 2:(ncol(output)-1) ] = image;
	return (output);
}

# true iff pixel [i,j] is background (i.e. 0) or out of bounds
isBackground <- function( image, i, j ) {
	if ( i < 1 | j < 1 | i > nrow(image) | j > ncol(image) ) {
		return (FALSE);
	} else {
		return (image[i,j] == 0);
	}
}

# true iff pixel is a contour, i.e. is connected (1 of 4 ways) to the background
isContour <- function( image, i, j ) {
	return ( !isBackground( image, i, j ) & ( isBackground( image, i+1, j) | isBackground( image, i-1, j )
		| isBackground( image, i, j+1 ) | isBackground( image, i, j-1 ) ) );
}

# get contour based on simple 4-connectivity to background (0) pixels
getContour <- function( image ) {
	contour <- matrix( , nrow=nrow(image), ncol=ncol(image) );
	for ( i in 1:nrow(image) ) {
		for ( j in 1:ncol(image) ) {
			if ( isContour( image, i, j ) ) {
				contour[i,j] <- 1;
			} else {
				contour[i,j] <- 0;
			}
		}
	}
	return (contour);
}

getFirstNonBackgroundPixel <- function( image, i ) {
	for ( j in 1:ncol(image) ) {
		if ( image[i,j] != 0 ) {
			return (j);
		}
	}
	return (-1);
}

flood <- function( image, i, j ) {
	if ( i < 1 | j < 1 | i > nrow(image) | j > ncol(image) ) {
		return (image);
	}

	if ( image[i,j] == 0 ) {
		image[i,j] <- -1;
		image <- flood( image, i+1, j );
		image <- flood( image, i-1, j );
		image <- flood( image, i, j+1 );
		image <- flood( image, i, j-1 );
	}

	return (image);
}

getSilhouette <- function( image ) {
	image <- addBorder( image );
	silhouette <- flood( image, 1, 1 );
	for ( i in 1:nrow(silhouette) ) {
		for ( j in 1:ncol(silhouette) ) {
			if ( silhouette[i,j] == -1 ) {
				silhouette[i,j] <- 0;
			} else {
				silhouette[i,j] <- 1;
			}
		}
	}
	return (silhouette);
}

nextMove <- function( m ) {
	return ( (m + 1)%%8 );
}

moveToCoordinates <- function( i, j, m ) {
	if ( m == 0 ) {
		i <- i - 1;
	}
	if ( m == 1 ) {
		i <- i - 1;
		j <- j + 1;
	}
	if ( m == 2 ) {
		j <- j + 1;
	}
	if ( m == 3 ) {
		i <- i + 1;
		j <- j + 1;
	}
	if ( m == 4 ) {
		i <- i + 1;
	}
	if ( m == 5 ) {
		i <- i + 1;
		j <- j - 1;
	}
	if ( m == 6 ) {
		j <- j - 1;
	}
	if ( m == 7 ) {
		i <- i - 1;
		j <- j - 1;
	}
	return ( c( i, j ) );
}

isValidPosition <- function( image, i, j ) {
	if ( i > 0 & j > 0 & i <= nrow(image) & j <= ncol(image) ) {
		return ( image[i,j] == 1 );
	}
	return (FALSE);
}

nextValidMove <- function( image, i, j, lastMove ) {
	backwardsMove <- ( lastMove + 4 ) %% 8;
	move <- nextMove( backwardsMove ) ;
	coordinates <- moveToCoordinates( i, j, move );
	#print( paste( "current = [", i, ", ", j, "] ; move = ", move, "next = [", coordinates[1], ", ", coordinates[2], "]" ) );
	while( move != backwardsMove & !isValidPosition( image, coordinates[1], coordinates[2] ) ) {
		move <- nextMove( move );
		coordinates <- moveToCoordinates( i, j, move );
	}
	if ( move != backwardsMove ) {
		return (move);
	} else {
		return (-1);
	}
}

getFreemanChainCode <- function( image ) {

	# freakin' EBImage coding...
	image <- t(image);

	# look for the starting coordinates
	coordinates <- vector( , length=2 );
	for ( i in 1:nrow(image) ) {
		first <- getFirstNonBackgroundPixel( image, i );
		if ( first != -1 ) {
			coordinates[1] <- i;
			coordinates[2] <- first;
			break;
		}
	}

	# clockwise move
	path <- vector(, length=0 );
	move <- nextValidMove( image, coordinates[1], coordinates[2], 2 );

	#print( paste( "Starting coordinates: ", coordinates ) );
	while( move != -1 ) {
		#print( paste( "current move: ", move ) );
		path <- append( path, move );
		image[coordinates[1], coordinates[2]] <- -1;
		coordinates <- moveToCoordinates( coordinates[1], coordinates[2], move );
		move <- nextValidMove( image, coordinates[1], coordinates[2], move );
	}

	return (path);
}

freeman <- function( image ) {
	silhouette <- getSilhouette( image );
	contour <- getContour( silhouette );
	return ( getFreemanChainCode( contour ) );
}

