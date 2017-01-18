module mod_starting_path

use initdata
use functions
use mod_project

implicit none

contains

subroutine linearstartingpath(mygamma)
implicit none
real(precis), dimension (NOB,dim,0:steps+1), intent(OUT) :: mygamma
mygamma = 0.0d0
call random_number(mygamma(:,:,0))
call random_number(mygamma(:,:,steps+1))
mygamma = mygamma * scale
return 
end subroutine linearstartingpath

subroutine randompath(mygamma)
 implicit none
 real(precis), dimension (NOB,dim,0:steps+1), intent(OUT) :: mygamma
 integer :: i,j,k

do i=1,NOB; do j=1,dim; do k=0,steps+1
 call random_number(mygamma(i,j,k))
 mygamma(i,j,k) = (mygamma(i,j,k)-0.5d0)
end do; end do; end do;
mygamma(:,:,0) = mygamma(:,:,0) * 10.d0 
mygamma(:,:,steps+1) = mygamma(:,:,steps+1) * 10.d0 
 return
end subroutine randompath

subroutine cerchio(mygamma)
  implicit none
  real(precis), dimension (NOB,dim,0:steps+1), intent(OUT) :: mygamma
  real(precis), dimension (NOB,dim,0:NOP+1) :: punti
  
  integer :: i,h
  
  mygamma = 0.0d0
  punti = 0.0d0
  
  do i=1,3
    do h=0,NOP+1
!     punti(i,1,h)=cos(pi/(real(NOP+1,precis))*h +real(i-1,precis)*2.0d0*pi/3.0d0)   !!!SEMICERCHIO!!!
!     punti(i,2,h)=sin(pi/(real(NOP+1,precis))*h +real(i-1,precis)*2.0d0*pi/3.0d0)    
     punti(i,1,h)=cos(2.0d0*pi/(real(NOP+1,precis))*real(h,precis) +real(i-1,precis)*2.0d0*pi/3.0d0)  !!!CERCHIO!!!
     punti(i,2,h)=sin(2.0d0*pi/(real(NOP+1,precis))*real(h,precis) +real(i-1,precis)*2.0d0*pi/3.0d0)
    end do
  end do 
  mygamma=mycoeff(punti)


end subroutine cerchio

subroutine cerchio2(mygamma)
  implicit none
  real(precis), dimension (NOB,dim,0:steps+1), intent(OUT) :: mygamma
  real(precis), dimension (NOB,dim,0:NOP+1) :: punti
  
  integer :: i,h
  
  mygamma = 0.0d0
  punti = 0.0d0
  
  do i=1,3
    do h=0,NOP+1
     punti(i,1,h)=cos(1.0d0*pi/(real(NOP+1,precis))*real(h,precis) +real(i-1,precis)*2.0d0*pi/3.0d0)  !!!CERCHIO!!!
     punti(i,2,h)=sin(1.0d0*pi/(real(NOP+1,precis))*real(h,precis) +real(i-1,precis)*2.0d0*pi/3.0d0)
    end do
  end do 
  mygamma=mycoeff(punti)

return
end subroutine cerchio2

subroutine stdin_starting_path(mygamma)
implicit none
real(precis), dimension (NOB,dim,0:steps+1), intent(OUT) :: mygamma
integer :: i,k
real(precis) :: tmp
integer, dimension(3) :: dims
real(precis), dimension(3) :: thisomega


read (*,*) dims
if ( (dims(1) /= NOB) .OR. (dims(2) /= dim) .OR. (dims(3) /= STEPS) ) then
	write (unit=0,fmt=*) "FATAL: dims do not coincide!", dims
	stop
end if

read (*,*) thisomega


if (thisomega(1) /= omega(1) .OR. thisomega(2) /= omega(2) .OR. thisomega(3) /= omega(3) ) then
	!! write (unit=0,fmt=*) "WARNING: omega has changed"
	!! write (unit=0,fmt=*) "omega     = ", omega
	!! write (unit=0,fmt=*) "old omega = ", thisomega
	omega=thisomega	
end if

read (*,*) tmp
if (tmp .NE. ALPHA) then
	write (unit=0,fmt=*) "WARNING: ALPHA has changed"
	!! ALPHA=tmp
end if

do i=1,NOB
 	read (*,*) tmp
 	if (tmp /= m(i) ) then
 		write (unit=0,fmt=*) "WARNING: changing m(",i,")"
 		!! m(i)=tmp	 !! they have to be the same
 	end if
end do

do i=1,NOB
do k=0,steps+1
read (*,*) mygamma(i,:,k)
end do; end do;
return
!! format specification
1234 format(f10.3)
end subroutine stdin_starting_path



subroutine starting_path(mygamma)
 implicit none
 real(precis), dimension (NOB,dim,0:steps+1), intent(OUT) :: mygamma
 
 if (SPMETHOD .EQ. 0) then
    write (unit=0,fmt=*) "with linear SP"
    call linearstartingpath(mygamma)
    call project(mygamma) !!!DA AGGIUNGERE!!!
 else if (SPMETHOD .EQ. 1) then
    write (unit=0,fmt=*) "# with random SP"
    call randompath(mygamma)
    call project(mygamma) !!!DA AGGIUNGERE!!!
 else if (SPMETHOD .EQ. 2) then
    write (unit=0,fmt=*) "# with circle SP"
    call cerchio(mygamma)
    call project(mygamma) !!!DA AGGIUNGERE!!!
 else if (SPMETHOD .EQ. 3) then
    write (unit=0,fmt=*) "# with stdin SP"
    call stdin_starting_path(mygamma)
 else  
    write(unit=0,fmt=*) 'FATAL ERROR: SPMETHOD ', SPMETHOD, ' NOT FOUND'
    stop
 end if
 
 
return
end subroutine starting_path

end module mod_starting_path


