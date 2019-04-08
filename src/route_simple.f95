subroutine routesimple(nt, ncol, intervals, duration, inmat, outmat)
	implicit none
	
	integer, intent(in) :: nt(*), ncol(*)
	real(kind=8), intent(in) :: intervals(*), duration(*)
	real(kind=8), intent(inout) :: inmat(nt(1),ncol(1)) 
	real(kind=8), intent(inout) :: outmat(nt(1),ncol(1)) 
	
	integer :: t, col
	real(kind=8) :: outflow, inflow
	
	! initialize
	
	
	do col = 1, ncol(1)
		do t = 1, (nt(1)-1)
			!if(t == 0) then
			!	outflow = inmat(1,col) * (1-(duration(1) / intervals(1)))
			!	inflow = inmat(t+1,col) + (inmat(1,col) - outflow)
			!	inmat(1,col) = inflow
			!	outmat(1,col) = outflow
			!end if
			
			!if(t > 0) then
				outflow = inmat(t,col) * (1-(duration(1) / intervals(t)))
				inflow = inmat(t+1,col) + (inmat(t,col) - outflow)
				inmat(t+1,col) = inflow
				outmat(t,col) = outflow
			!end if
		end do
		
	end do
	
end subroutine routesimple

