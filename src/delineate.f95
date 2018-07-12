subroutine delineate(nx, ny, nseeds, seeds, basID, drdir, delbas)
	implicit none
	
	integer, intent(in) :: nx(*), ny(*), nseeds(*)
	integer, intent(in) :: seeds(*), basID(*)
	integer, intent(in) :: drdir(nx(1)*ny(1)) 
	
	integer, intent(out) :: delbas(nx(1)*ny(1))
	
	integer :: vsize, c2vi, check, currcell, cell, i, arrsize, vpos
    logical :: notdone
	integer, allocatable :: c2v(:)

    integer :: neigh(8)
	
	!print *, nx(1), ny(1), nseeds(1)
	!print *, seeds
	! initialize
	
	
	allocate(c2v(nx(1)*ny(1)))
	c2v = 0
	

	do i = 1, nseeds(1)
		c2v = 0
		c2v(1) = seeds(i)
		c2vi = 1
        vpos = 0
		currcell = c2v(1)
		
		do while (currcell > 0)
            vpos = vpos + 1
			notdone = .true.
			currcell = c2v(vpos)
			delbas(currcell) = basID(i)

            if (vpos /= 1) then
                if ( ANY(seeds(1:nseeds(1)) == currcell) ) then
                    cycle
                end if
            end if

            neigh = [ currcell-1-nx(1), currcell-nx(1), currcell+1-nx(1), currcell-1, &
                      currcell+1, currcell-1+nx(1), currcell+nx(1), currcell+1+nx(1) ]

            if ( ANY(neigh <= 0) .or. ANY(neigh > (nx(1)*ny(1))) ) then
				exit
            end if
			
			if (drdir(neigh(1)) == 2) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(1)
                notdone = .false.
            end if
				
			if (drdir(neigh(2)) == 4) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(2)
                notdone = .false.
			end if

			if ( drdir(neigh(3)) == 8) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(3)
                notdone = .false.
			end if	
			
            if ( drdir(neigh(4)) == 1) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(4)
                notdone = .false.
			end if	
			
            if ( drdir(neigh(5)) == 16) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(5)
                notdone = .false.
			end if	
			
            if ( drdir(neigh(6)) == 128) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(6)
                notdone = .false.
			end if	
			
            if ( drdir(neigh(7)) == 64) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(7)
                notdone = .false.
			end if	
			
            if ( drdir(neigh(8)) == 32) then
				c2vi = c2vi + 1
				c2v(c2vi) = neigh(8)
                notdone = .false.
            end if
            
			!if (notdone) then
            !    c2vi = c2vi + 1
			!end if
            !print *, cell
		end do
        !print *, i
	end do
	
end subroutine delineate