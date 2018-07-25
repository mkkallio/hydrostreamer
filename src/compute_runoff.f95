subroutine compute_runoff(nriv, nseg, ng, nts, wrIDs, wgIDs, &
						weights, gridareas, runoffTS, QTS, convert, debug, debug2)
	implicit none
	
	! input variables
	integer, intent(inout) :: nriv			! number of rivers
	integer, intent(inout) :: nseg 			! number of weighted segments
	integer, intent(in) :: ng				! number of grid cells
	integer, intent(in) :: nts				! number of timesteps
	integer, intent(in) :: wrIDs(nseg) 		! vector of column indices weight refers to
	integer, intent(in) :: wgIDs(nseg)		! vector of column indices weight refers to
	
	real(kind=8), intent(inout) :: weights(nseg)	! vector of weights
	real(kind=8), intent(in) :: gridareas(ng)		! vector of grid surface areas 
	real(kind=8), intent(in) :: runoffTS(ng,nts)	! array of runoff
	real(kind=8), intent(inout) :: QTS(nriv,nts)	! array of downscaled runoff
	
	integer, intent(in) :: convert			! whether to convert mm/s to m3/s
	
	
	! looping variables
	integer:: tstep, seg, i, j, rindex, gindex
	real(kind=8) :: runoff, debug(nseg)
	integer :: debug2(nseg)
	! loop over timesteps, and number of weighted segments
	do tstep = 1, nts
		do seg = 1, nseg	
		
			rindex = wrIDs(seg)
			gindex = wgIDs(seg)
			
			runoff = 0.

			if (convert == 1) then
				runoff = weights(seg) * runoffTS(gindex,tstep) * gridareas(gindex) / 1000
			else
				runoff = weights(seg) * runoffTS(gindex,tstep)
			end if

			QTS(rindex, tstep) = QTS(rindex, tstep) + runoff 
			debug(seg) = runoffTS(gindex, tstep)
			debug2(seg) = gindex
		end do
	end do	
end subroutine