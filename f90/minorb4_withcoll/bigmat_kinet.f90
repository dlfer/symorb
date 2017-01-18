module mod_bigmat_kinet

use initdata

implicit none

public
real(precis), dimension(:,:,:,:,:,:), allocatable :: bigmat_kinet

contains

subroutine alloc_bigmat_kinet
  implicit none
  allocate (bigmat_kinet(1:NOB,1:dim,0:steps+1,1:NOB,1:dim,0:steps+1))
  bigmat_kinet = 0.0d0
return
end subroutine alloc_bigmat_kinet


subroutine def_bigmat_kinet
  implicit none
  integer :: i,j,k
  bigmat_kinet = 0.0d0
   do i=1,NOB
      do j=1,dim
          bigmat_kinet(i,j,steps+1,i,j,steps+1) = 1.0d0/pi *m(i)
          bigmat_kinet(i,j,0,i,j,0) = 1.0d0/pi *m(i)
          bigmat_kinet(i,j,0,i,j,steps+1) = -1.0d0/pi *m(i)
          bigmat_kinet(i,j,steps+1,i,j,0) = -1.0d0/pi *m(i)
          do k=1,steps
                bigmat_kinet(i,j,k,i,j,k) = pi/2.0d0 * k**2 * m(i)
          end do
      end do
   end do
return
end subroutine def_bigmat_kinet

end module mod_bigmat_kinet
