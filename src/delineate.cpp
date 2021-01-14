#include <Rcpp.h>

using namespace Rcpp;
//[[Rcpp::export]]
IntegerVector delineatecpp(IntegerVector seeds,
                           IntegerVector basID,
                           IntegerVector drdir,
                           IntegerVector delbas,
                           int nx,
                           int ny) {
    
    
    // delineate basins of each seed
    for(int i = 0; i < seeds.size(); i++) {
        IntegerVector c2v(drdir.size()); // c2v = cells to visit

        c2v[0] = seeds[i]; 
        
        // c2v is a vector, this keeps track which position of c2v we add to
        int c2vi = 0;  
        
        // visit position (of the c2v vector)
        int vpos = -1; 
        
        // current cell - which cell we're working at?
        int currcell = c2v[0];
        
        // loop until no further cells to visit (c2v(vpos) == 0)
        bool keep = true;
        while(keep) { 
            vpos = vpos + 1;
            currcell = c2v[vpos];
            
            if(currcell == 0) {
                keep = false;
                break;
            }

            //if current cell is one of seeds, go to the next cell
            bool test = false;
            if(vpos > 0) {
                for(int ii = 0; ii < seeds.size(); ii++) {
                    if(currcell == seeds[ii]) {
                        test = true;
                        break;
                    }
                }
            }
            
            if(test == true) {
                continue;
            }
            
            delbas[currcell] = basID[i]; // record ID for current cell
            
            // neighbour locations
            IntegerVector neigh(8);
            neigh = { currcell-1-nx, currcell-nx, currcell+1-nx, 
                      currcell-1,                 currcell+1, 
                      currcell-1+nx, currcell+nx, currcell+1+nx };
            
            // test that no cells are outside the vector
            bool test2 = false;
            for(int ii = 0; ii < 8; ii++) {
                if(neigh[ii] < 0) {
                    test2 = true;
                } 
                
                if(neigh[ii] > delbas.size()) {
                    test2 = true;
                }
            }
            if(test2 == true) {
                break;
            }
            

            // check each neighbour, and if it flows into to the current cell,
            // add that cell to c2v 
            if(drdir(neigh[0]) == 2) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[0];
            } 
            
            if(drdir[neigh[1]] == 4) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[1];
            }
            
            if(drdir[neigh[2]] == 8) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[2];
            }
            
            if(drdir[neigh[3]] == 1) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[3];
            }
            
            if(drdir[neigh[4]] == 16) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[4];
            }
            
            if(drdir[neigh[5]] == 128) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[5];
            }
            
            if(drdir[neigh[6]] == 64) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[6];
            }
            
            if(drdir[neigh[7]] == 32) {
                c2vi = c2vi + 1;
                c2v[c2vi] = neigh[7];
            } 
            
        }
    }
    return(delbas);
}

// OLD WORKING FORTRAN SUBROUTINE
/*
subroutine delineate(nx, ny, nseeds, seeds, basID, drdir, delbas)
implicit none

integer, intent(in) :: nx(*), ny(*), nseeds(*)
integer, intent(in) :: seeds(*), basID(*)
integer, intent(in) :: drdir(nx(1)*ny(1)) 

integer, intent(out) :: delbas(nx(1)*ny(1))

integer :: c2vi, currcell, i, vpos
integer, allocatable :: c2v(:)

integer :: neigh(8)

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
            
        end if
        
        if (drdir(neigh(2)) == 4) then
            c2vi = c2vi + 1
            c2v(c2vi) = neigh(2)
            
        end if
        
        if ( drdir(neigh(3)) == 8) then
            c2vi = c2vi + 1
            c2v(c2vi) = neigh(3)
            
        end if	
        
        if ( drdir(neigh(4)) == 1) then
            c2vi = c2vi + 1
            c2v(c2vi) = neigh(4)
            
        end if	
        
        if ( drdir(neigh(5)) == 16) then
            c2vi = c2vi + 1
            c2v(c2vi) = neigh(5)
            
        end if	
        
        if ( drdir(neigh(6)) == 128) then
            c2vi = c2vi + 1
            c2v(c2vi) = neigh(6)
            
        end if	
        
        if ( drdir(neigh(7)) == 64) then
            c2vi = c2vi + 1
            c2v(c2vi) = neigh(7)
            
        end if	
        
        if ( drdir(neigh(8)) == 32) then
            c2vi = c2vi + 1
            c2v(c2vi) = neigh(8)
            
        end if
    end do
end do
    
! make sure that seed cells belong to a basin of the seed
do i=1, nseeds(1)
delbas(seeds(i)) = basID(i)
end do 

end subroutine delineate
 */