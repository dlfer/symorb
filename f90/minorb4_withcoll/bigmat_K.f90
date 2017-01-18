module mod_bigmat_K
use initdata
use mod_mat_omega
use integrali
implicit none
!! bigmat_K is already allocated at this stage...

contains

subroutine def_bigmat_K
implicit none
real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: bigmatrice
integer :: i,j,k,h,ip,jp,kp
!! real(precis) :: tmpvar

!! write (unit=0,fmt=*) "... defining bigmat_K..."
call def_mat_omegas
!! write (unit=0,fmt=*) "... defined bigmat_K..."

bigmatrice = 0.0d0
bigmatrice = bigmatrice + bigmat_kinet() 
!! write (unit=0,fmt=*) "... defining bigmat_K... 1 "
bigmatrice = bigmatrice +    bigmat_centr() 
!! write (unit=0,fmt=*) "... defining bigmat_K... 2 "
bigmatrice = bigmatrice +    2.0d0* bigmat_corio()  
!! write (unit=0,fmt=*) "... defining bigmat_K... 3 "
bigmat_K=0.0d0
!! write (unit=0,fmt=*) "... defined bigmat_K..."

do i=1,NOB
  do j=1,dim
    do k=0,steps+1
      do ip=1,NOB
         do jp=1,dim
	    do kp=0,steps+1
		!! tmpvar=abs( bigmatrice(i,j,k,ip,jp,kp) - bigmatrice(ip,jp,kp,i,j,k) ) 
		!! if ( tmpvar  .GE. 10.0**(-10) ) then
		  !! write (unit=0,fmt=*) "WARNING: bigmat not symmetric!", tmpvar
		  !! write (unit=0,fmt=*) "WARNING bigmat not symm: k,kp= ", k, kp 
		!! end if
		bigmat_K (i,j,k,ip,jp,kp) = &
		& ( bigmatrice(i,j,k,ip,jp,kp) + bigmatrice(ip,jp,kp,i,j,k) )  / 2.0
            end do
          end do
        end do
      end do 
    end do
end do

!!  bigmat_K = bigmat_kinet() + bigmat_centr() + 2.0d0* bigmat_corio()  

!! write (unit=0,fmt=*) "DEBUG: ", sum( bigmat_centr()**2 )
!! write (unit=0,fmt=*) "DEBUG: ", sum( bigmat_corio()**2 )
!! write (unit=0,fmt=*) "DEBUG: omega= ", omega

do k=1,steps
	do h=1,NOP
		sin_kth(k,h) = sin( real(k * h,precis) * pi / real(NOP+1,precis) )
	end do
end do

return
end subroutine def_bigmat_K



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!
!!!! funzioni 
!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function bigmat_kinet()
  implicit none
  real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: bigmat_kinet
  integer :: i,j,k
!! write (unit=0,fmt=*) "... defined bigmat_kinet..."

  bigmat_kinet(:,:,:,:,:,:) = 0.0d0
   do i=1,NOB
      do j=1,dim
          bigmat_kinet(i,j,steps+1,i,j,steps+1) = 1.0d0/pi *m(i)
          bigmat_kinet(i,j,0,i,j,0) = 1.0d0/pi *m(i)
          bigmat_kinet(i,j,0,i,j,steps+1) = -1.0d0/pi *m(i)
          bigmat_kinet(i,j,steps+1,i,j,0) = -1.0d0/pi *m(i)
          do k=1,steps
                bigmat_kinet(i,j,k,i,j,k) = pi/2.0d0 * real(k,precis)**2.0d0 * m(i)
          end do
      end do
   end do
return
end function bigmat_kinet
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function bigmat_centr()
implicit none
real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: bigmat_centr
real(precis), dimension(NOB,dim,NOB,dim) :: BigOmegaSquare
integer :: i,k

!!!
BigOmegaSquare=0.0d0
bigmat_centr = 0.0d0
!! write (unit=0,fmt=*) "# bigmat_centr"

do i=1,NOB
	BigOmegaSquare(i,:,i,:) = m(i) * mat_omega_square(:,:)
end do

bigmat_centr(:,:,0,:,:,0 )             = - pi / 3.0 * BigOmegaSquare(:,:,:,:)
bigmat_centr(:,:,steps+1,:,:,steps+1 ) = - pi / 3.0 * BigOmegaSquare(:,:,:,:)
bigmat_centr(:,:,0,:,:,steps+1 )       = - pi / 6.0 * BigOmegaSquare(:,:,:,:)
bigmat_centr(:,:,steps+1,:,:,0 )       = - pi / 6.0 * BigOmegaSquare(:,:,:,:)


do k=1,steps
! bigmat_centr(:,:,k,:,:,0 ) = &
!   & - (int_sin(k) + int_tsin(k) / pi ) * BigOmegaSquare(:,:,:,:)
! bigmat_centr(:,:,0,:,:,k) = &
!   & - (int_sin(k) + int_tsin(k) / pi ) * BigOmegaSquare(:,:,:,:)
!!! TEMP !!!

bigmat_centr(:,:,k,:,:,0 ) = &
  & - (int_sin(k) - int_tsin(k) / pi ) * BigOmegaSquare(:,:,:,:)
bigmat_centr(:,:,0,:,:,k) = &
  & - (int_sin(k) - int_tsin(k) / pi ) * BigOmegaSquare(:,:,:,:)



bigmat_centr(:,:,k,:,:,steps+1) = &
  &  - ( int_tsin(k) / pi ) * BigOmegaSquare(:,:,:,:)
bigmat_centr(:,:,steps+1,:,:,k) = &
  &  -  ( int_tsin(k) / pi ) * BigOmegaSquare(:,:,:,:)

bigmat_centr(:,:,k,:,:,k) = & 
  & - pi / 2.0 * BigOmegaSquare(:,:,:,:)

end do

return
end function bigmat_centr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!! 
function bigmat_corio()
implicit none
real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: bigmat_corio
real(precis), dimension(NOB,dim,NOB,dim) :: BigOmega
integer :: i,k,kp
bigmat_corio = 0.0d0
BigOmega=0.0d0
!! write (unit=0,fmt=*) "# bigmat_corio"

do i=1,NOB
	BigOmega(i,:,i,:) = m(i) * mat_omega(:,:)
end do

bigmat_corio(:,:,0,:,:,0) = 0.0
bigmat_corio(:,:,steps+1,:,:,steps+1) = 0.0 
bigmat_corio(:,:,0,:,:,steps+1) = - BigOmega(:,:,:,:) / 2.0
bigmat_corio(:,:,steps+1,:,:,0) =  BigOmega(:,:,:,:) / 2.0

do k=1,steps
	bigmat_corio(:,:,0,:,:,k) =  1.0 / ( 2.0 * pi) * & 
	& ( - int_sin(k) + k * int_tcos(k) ) * BigOmega(:,:,:,:)
	bigmat_corio(:,:,k,:,:,0) = - 1.0 / ( 2.0 * pi) * & 
	& ( - int_sin(k) + k * int_tcos(k) ) * BigOmega(:,:,:,:)
	bigmat_corio(:,:,steps+1,:,:,k) = - 1.0 / ( 2.0 * pi) * & 
	& ( - int_sin(k) + k * int_tcos(k) ) * BigOmega(:,:,:,:)
	bigmat_corio(:,:,k,:,:,steps+1) = + 1.0 / ( 2.0 * pi) * & 
	& ( - int_sin(k) + k * int_tcos(k) ) * BigOmega(:,:,:,:)
end do


do k=1,steps-1
  do kp=k+1,steps
 	bigmat_corio(:,:,k,:,:,kp) =    k * int_sincos(kp,k) * BigOmega(:,:,:,:)
  	bigmat_corio(:,:,kp,:,:,k) =   -  k * int_sincos(kp,k) * BigOmega(:,:,:,:)
  end do
end do  

!!! END !!!
return
end function bigmat_corio

end module mod_bigmat_K
