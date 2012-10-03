# TODO: Add comment
# 
# Author: claudio
###############################################################################


groupBy <- function(positions,groupCriteria) {
	## positions: a variable of class positions
	## groupCriteria: a character vector of criteria to be used to aggregate
	## at the moment only "securityId" implemented
	if (length(positions)<=1) return(positions)
	
	if (groupCriteria=="securityId") {
		extractId <- function(position) {
			return(position@security@id)
		}
		ids <- lapply(positions,extractId)
		ids <- unlist(lapply(ids,idForGroupBy))
		
		# create a list of vectors containing the indices of the same assets
		indices.l <- split(1:length(ids),as.factor(ids))
		
		groupPositionsBySecurityId <- function(indices,positions) {
			# indices: the vector of the indices of the positions to be grouped
			# positions: the corresponding list of positions
			
			if (length(indices)==1) {
				
				return(positions[[indices[[1]]]])
			
			}
			result <- positions[[indices[[1]]]]
			for (i in indices[-1]) result <- groupBySecurityId(result,positions[[i]])
			return(result)
		}
		
		result <- lapply(indices.l,groupPositionsBySecurityId,positions)
		return(new("Positions",unname(result)))
	}
}
