subroutine routesimple2(nts, padn, nseg, nds, seg, ds, ints, cumul, & 
						inmat, outmat, debug)
	implicit none
	
	! input arguments
	integer, intent(in) :: nts(*), nseg(*), nds(*), seg(*), padn(*), ds(*)
	double precision, intent(in) :: ints(*), cumul(*)
	double precision, intent(in) :: inmat(nts(1)+padn(1),nseg(1)) 
	double precision, intent(inout) :: outmat(nts(1)+padn(1),nseg(1)) 
	
	!helper arguments
	integer :: i, ii, t, tuni, cnt, f, l
	integer, allocatable :: dsinds(:)
	logical, allocatable :: tind(:)
	double precision :: q
	double precision :: durations(nds(1)), tf(nds(1))
	double precision, allocatable :: outflow(:,:)
	
	! debug
	double precision, intent(out) :: debug(nds(1))
	
	! initialize
	debug(:) = -1
	
	! loop through timesteps
	do t = 1, nts(1), 1
		! inflow at time t
		q = inmat(t, seg(1))
		
		if(q == 0) then
			cycle
		end if
		
		!downstream reaches and timesteps
		durations(1:nds(1)) = cumul(1:nds(1)) / ints(t)
		tf = floor(durations)
		tuni = maxval(tf(:))+1

		! allocate downstream indices and timestep index
		allocate(dsinds(tuni +1))
		allocate(tind(nds(1)))
		
		!first element of downstream indices = 0
		dsinds(1) = 0
		! consecutive elements of dsinds are the number of times each unique
		! value appears in tf
		do i = 1, tuni
			tind = tf(:) == (i-1)
			cnt = count(tind)
			dsinds(i+1) = dsinds(i) + cnt
		end do
		
		! allocate outflow for computation and initialize it
		allocate(outflow(tuni+1, nds(1)))
		outflow = 0.
		
		! add outflow to all downstream segments at the timestep flow passes
		do i = 2, size(dsinds)
			f = dsinds(i-1)+1
			l = dsinds(i)
				
			do ii = f, l
				!flow through downstream segments at timestep 
				outflow(i-1, ii) = q*(1-(durations(ii)- tf(ii)))
				
				!flow through downstream segments at the next timestep
				outflow(i, ii) = q-outflow(i-1,ii)
			end do
		end do

		!update outflow matrix
		do i = 1, tuni+1
			do ii = 1, nds(1)
				outmat(t+i-1, ds(ii)) = outmat(t+i-1, ds(ii)) + outflow(i, ii)
			end do
		
		end do
		
		!deallocate before processing next timestep
		deallocate(dsinds)
		deallocate(tind)
		deallocate(outflow)
	end do
	
	
end subroutine routesimple2