subroutine routing_instant(nriv, nts, rID, next, runoffTS, output)
	implicit none
	
	! input variables
	integer, intent(in) :: nriv			! number of rivers
	integer, intent(in) :: nts				! number of timesteps
	
	integer, intent(in) :: rID(nriv)		! vector of river IDs
	integer, intent(in) :: next(nriv)		! vector of IDs of the next river segment

	real(kind=8), intent(in) :: runoffTS(nts,nriv)	! array of runoff
	real(kind=8), intent(inout) :: output(nts,nriv)	! array of accumulated (routed) runoff
		
	
	! looping variables
	integer:: tstep, riv, n, i, rindex
	real(kind=8) :: runoff
	
	! loop over timesteps, and rivers
	do tstep = 1, nts
		do riv = 1, nriv	
		
			runoff = runoffTS(tstep,riv)
			
			n = next(riv)
			
			do while (n /= -9999)	
				if (n == -9999) then
					exit
				end if
				
				rindex = 0
				do i = 1, nriv
					if (rID(i) == n) then
						rindex = i
						exit 
					end if
				end do 
			
				if (rindex == 0) then
					exit 
				end if
			
				output(tstep,rindex) = output(tstep,rindex) + runoff
				n = next(rindex)
			end do
		end do 
	end do 
end subroutine