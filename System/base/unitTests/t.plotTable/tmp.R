# TODO: Add comment
# 
# Author: claudio
###############################################################################


plot.table.param <- function
		(
		myMatrix,
		smain = '',
		myMatrix.cex,
		myMatrix_bg.col,
		frame.cell = T,
		keep.all.same.cex = FALSE
)
{
	n = nrow(myMatrix)
	pages = unique(c(seq(0, n, by = 120), n))
	for(p in 1:(len(pages)-1)) {
		rindex = (pages[p]+1) : pages[p+1]
		temp.table = matrix('', nr = len(rindex)+1, nc = ncol(myMatrix)+1)
		temp.table[-1, -1] = myMatrix[rindex,]
		temp.table[1, -1] = colnames(myMatrix)
		temp.table[-1, 1] = rownames(myMatrix)[rindex]
		temp.table[1, 1] = smain
		nr = nrow(temp.table)
		nc = ncol(temp.table)
		par(mar = c(0, 0, 0, 0), cex = 0.5)
		oldpar = make.table(nr, nc)
		text.cex = myMatrix.cex[c(1, 1 + rindex), ]
		text.cex = plot.table.helper.auto.adjust.cex(temp.table, keep.all.same.cex)
		bg.col = myMatrix_bg.col[c(1, 1 + rindex), ]
		for(r in 1:nr) {
			for(c in 1:nc) {
				draw.cell( paste('', temp.table[r,c], '', sep=' '), r, c,
						text.cex = text.cex[r,c], bg.col = bg.col[r,c], frame.cell = frame.cell)
			}
		}
	}
}
plot.table.helper.auto.adjust.cex <- function
		(
		temp.table,
		keep.all.same.cex = FALSE
)
{

	nr = nrow(temp.table)
	nc = ncol(temp.table)
	all.xrange = diff(par()$usr[1:2]) / nc
	xrange = matrix( strwidth(paste('  ', temp.table), units = 'user', cex = 1), nc = nc)
	all.yrange = diff(par()$usr[3:4]) / nr
	yrange = matrix( 5/3 * strheight(temp.table, units = 'user', cex = 1), nc = nc)
	myMatrix.cex = pmin( round(all.yrange / yrange, 2) , round(all.xrange / xrange, 2) )
	header.col.cex = min(myMatrix.cex[1,-1])
	header.row.cex = min(myMatrix.cex[-1,1])
	title.cex = myMatrix.cex[1, 1]
	data.cex = min(myMatrix.cex[-1, -1])
	if ( keep.all.same.cex ) {
		myMatrix.cex[] = min(myMatrix.cex)
	} else {
		myMatrix.cex[1,-1] = min(c(header.col.cex, header.row.cex))
		myMatrix.cex[-1,1] = min(c(header.col.cex, header.row.cex))
		myMatrix.cex[-1,-1]= min(c(header.col.cex, header.row.cex, data.cex))
		myMatrix.cex[1,1]= min(c(header.col.cex, header.row.cex, data.cex, title.cex))
		myMatrix.cex[1,-1] = min(c(header.col.cex))
		myMatrix.cex[-1,1] = min(c(header.row.cex))
		myMatrix.cex[-1,-1]= min(c(data.cex))
		myMatrix.cex[1,1]= min(c(title.cex))
	}
	return(myMatrix.cex)
}


plot.table <- function
		(
		myMatrix,
		smain = '',
		text.cex = 1,
		frame.cell = T,
		highlight = F,
		colorbar = FALSE,
		keep_all.same.cex = FALSE
)
{

	if( is.null(rownames(myMatrix)) & is.null(colnames(myMatrix)) ) {
		temp.matrix = myMatrix
		if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
		if( ncol(temp.matrix) == 1 ) temp.matrix = cbind('', temp.matrix)
		myMatrix = temp.matrix[-1, -1, drop = FALSE]
		colnames(myMatrix) = temp.matrix[1, -1]
		rownames(myMatrix) = temp.matrix[-1, 1]
		smain = temp.matrix[1, 1]
	} else if( is.null(rownames(myMatrix)) ) {
		temp.matrix = myMatrix
		if( ncol(myMatrix) == 1 ) temp.matrix = cbind('', temp.matrix)
		myMatrix = temp.matrix[, -1, drop = FALSE]
		colnames(myMatrix) = colnames(temp.matrix)[-1]
		rownames(myMatrix) = temp.matrix[,1]
		smain = colnames(temp.matrix)[1]
	} else if( is.null(colnames(myMatrix)) ) {
		temp.matrix = myMatrix
		if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
		myMatrix = temp.matrix[-1, , drop = FALSE]
		rownames(myMatrix) = rownames(temp.matrix)[-1]
		colnames(myMatrix) = temp.matrix[1, ]
		smain = rownames(temp.matrix)[1]
	}
	myMatrix[which(trim(myMatrix) == 'NA')] = ''
	myMatrix[which(trim(myMatrix) == 'NA%')] = ''
	myMatrix[which(is.na(myMatrix))] = ''
	if(colorbar) {
		myMatrix = cbind(myMatrix, '')
		if(!is.null(highlight)) if(!is.logical(highlight)) { highlight = cbind(highlight, NA) }
	}
	nr = nrow(myMatrix) + 1
	nc = ncol(myMatrix) + 1
	is_highlight = T
	if(is.logical(highlight)) {
		is_highlight = highlight
		if(highlight) highlight = plot.table.helper.color(myMatrix)
	}
	if(!is_highlight) {
		myMatrix.cex = matrix(1, nr = nr, nc = nc )
		myMatrix_bg.col = matrix('white', nr = nr, nc = nc )
		myMatrix_bg.col[seq(1, nr, 2), ] = 'yellow'
		myMatrix_bg.col[1,] = 'gray';
		plot.table.param( myMatrix, smain, myMatrix.cex, myMatrix_bg.col,
				frame.cell, keep_all.same.cex)
	} else {
		myMatrix.cex = matrix(1, nr = nr, nc = nc )
		myMatrix_bg.col = matrix('white', nr = nr, nc = nc )
		myMatrix_bg.col[1,] = 'gray'
		myMatrix_bg.col[2:nr,2:nc] = highlight
		plot.table.param(myMatrix, smain, myMatrix.cex, myMatrix_bg.col,
				frame.cell, keep_all.same.cex)
	}

	if(colorbar) plot.table.helper.colorbar(myMatrix);
}



plot.table.helper.color <- function
		(
		temp
){
	temp = matrix(as.double(gsub('[%,$]', '', temp)), nrow(temp), ncol(temp))
	highlight = as.vector(temp)
	cols = rep(NA, len(highlight))
	ncols = len(highlight[!is.na(highlight)])
	cols[1:ncols] = rainbow(ncols, start = 0, end = 0.3)
	o = sort.list(highlight, na.last = TRUE, decreasing = FALSE)
	o1 = sort.list(o, na.last = TRUE, decreasing = FALSE)
	highlight = matrix(cols[o1], nrow = nrow(temp))
	highlight[is.na(temp)] = NA
	return(highlight)
}
plot.table.helper.colorbar <- function
		(
		myMatrix
)
{
	nr = nrow(myMatrix) + 1
	nc = ncol(myMatrix) + 1
	c = nc
	r1 = 1
	r2 = nr
	rect((2*(c - 1) + .5), -(r1 - .5), (2*c + .5), -(r2 + .5), col='white', border='white')
	rect((2*(c - 1) + .5), -(r1 - .5), (2*(c - 1) + .5), -(r2 + .5), col='black', border='black')
	y1= c( -(r2) : -(r1) )
	graphics::image(x = c(  (2*(c - 1) + 1.5) : (2*c + 0.5) ),
			y   = y1,
			z   = t(matrix(  y1  , ncol = 1)),
			col = t(matrix( rainbow(len( y1  ), start = 0, end = 0.3) , ncol = 1)),
			add = T)
}
trim <- function
		(
		s
)
{
	s = sub(pattern = '^ +', replacement = '', x = s)
	s = sub(pattern = ' +$', replacement = '', x = s)
	return(s)
}


len <- function
		(
		x
)
{
	return(length(x))
}