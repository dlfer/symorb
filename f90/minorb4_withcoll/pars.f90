module pars 
use initdata 
implicit none 
contains 
subroutine init_values
implicit none
integer :: i,j,k
character :: endofchunk

read (*,*) omega(1)
read (*,*) omega(2)
read (*,*) omega(3)
sqomega=dot_product(omega,omega)

read (*,*) action_type
read (*,*) NOB
read (*,*) dim


NTOTALVAR =  NOB*dim*(steps+2) 
!! NEQ =  NOB*dim*(steps+2) 
NOP = steps*2 !!! TEMP

allocate  ( m( 1 :  NOB  ) )
!! do i=1,NOB
!! read (*,*) m(i)
!! end do


read (*,*)  sizekerT             ! size of kernelTchar 


allocate( perm(1:sizekerT,1:NOB)) 		
allocate ( matrix (1:sizekerT,1:dim,1:dim) )	


do i=1,sizekerT
	do j=1,NOB
		read (*,*) perm( i , j ) 
	end do
	do j=1,dim
		do k=1,dim
			read (*,*) matrix (i,j,k)
		end do
	end do	

end do


read (*,*) cyclic_order 


allocate (cyclic_perm(1:cyclic_order,1:NOB))
allocate (cyclic_matrix(1:cyclic_order,1:dim,1:dim))

do i=1,cyclic_order
	do j=1,NOB
		read (*,*) cyclic_perm( i , j )
	end do
	do j=1,dim
		do k=1,dim
		read (*,*) cyclic_matrix( i, j , k ) 
		end do
	end do
end do

if (action_type > 0 ) then
	allocate (first_perm(1:NOB))
	allocate (second_perm(1:NOB))
	allocate (first_matrix(1:dim,1:dim))
	allocate (second_matrix(1:dim,1:dim))

	do j=1,NOB
		read (*,*) first_perm( j ) 
	end do
	do j=1,dim
		do k=1,dim
			read (*,*) first_matrix( j , k )
		end do
	end do
	do j=1,NOB
		read (*,*) second_perm( j ) 
	end do
	do j=1,dim
		do k=1,dim
			read (*,*) second_matrix( j , k )
		end do
	end do
end if
!! write (unit=0,fmt=*) "# reading masses..."

do i=1,NOB
read (*,*) m(i)
end do


!!! check that there is a correct flag
read (*,*) endofchunk

if ( endofchunk /= ':' ) then
  write(unit=0,fmt=*) "endofchunk ", endofchunk, " not correct!"
  stop
!! else
!!   write(unit=0,fmt=*) ""
end if

!! write (unit=0,fmt=*) "endofchunk = ", endofchunk
!! stop
!! __HERE__
end subroutine init_values



end module pars



!!!=====================================================================
!!!
!!!=-- END OF pars.f90
!!!
!!!=====================================================================
