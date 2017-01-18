module io

use initdata
!! use functions
use mod_action
use mod_project

implicit none

contains


subroutine srcprintsolution(mygamma,fdu)
 implicit none
 real(precis), dimension(NOB,dim,0:printsteps+1), intent(IN) :: mygamma
 integer, intent(IN) :: fdu
 real(precis), dimension(NOB,dim,0:(2*printsteps+1)) :: cyclic_gamma
 real(precis), dimension(NOB,dim) :: tmpconf
 integer :: i,j,k, maxindex
 logical :: only_cyclic

 if ( ( action_type .EQ. 2 ) .OR. (action_type .EQ. 1))  then 
	maxindex=2*printsteps+1
	only_cyclic=.false.
 else if (action_type .EQ. 0 ) then
	maxindex=printsteps ! +1
	only_cyclic=.true.
 else
	write (unit=0,fmt=*) "FATAL ERROR: cyclic_order = ", cyclic_order , "does not exist!\n"
 end if 
 do i=0,printsteps+1
        cyclic_gamma(:,:,i)=mygamma(:,:,i)
 end do


 if (.NOT. only_cyclic) then
	do k=0,printsteps
	call s_htildetwo(mygamma(:,:,printsteps+1-k), tmpconf )
	cyclic_gamma(:,:,k+printsteps+1) = tmpconf
	end do
 end if

 write (unit=fdu,fmt=*)"# PRINTSTEPS =", printsteps
 write (unit=fdu,fmt=*)"#"

 do i=1, NOB
     write (unit=fdu,fmt=*) "#BODY NUMBER ", i
	do j=1, cyclic_order
		do k = 0, maxindex
			call s_cyclic_rotation(cyclic_gamma(:,:,k),j, tmpconf(:,:))
			write (unit=fdu, fmt=*) tmpconf(i,:)
		end do
	end do
	
     call s_cyclic_rotation(cyclic_gamma(:,:,0),1, tmpconf(:,:))
     write (unit=fdu, fmt=*) tmpconf(i,:)
     write (unit=fdu, fmt='(a3)') ""
     write (unit=fdu, fmt='(a3)') ""
 end do

return 
end subroutine srcprintsolution


subroutine printsolution(mygamma)
  real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: mygamma
  real(precis), dimension(NOB,dim,0:steps+1):: grad_mygamma
  real(precis), dimension(NOB,dim,0:printsteps+1) :: tmpgamma
  integer :: k, kk
  real(precis) :: tempo
  
  tmpgamma=0.d0  
  tmpgamma(:,:,0) = mygamma(:,:,0)
  tmpgamma(:,:,printsteps+1) = mygamma(:,:,steps+1)
  
  do k=1,printsteps
    tempo = real(k,precis)/(real(printsteps+1,precis)) 
    tmpgamma(:,:,k) =  mygamma(:,:,0) + tempo *( mygamma(:,:,steps+1)-mygamma(:,:,0) ) 
    do kk =1,steps
      tmpgamma(:,:,k) = tmpgamma(:,:,k) + mygamma(:,:,kk)*sin(real(kk,precis)*tempo*pi)
    end do
  end do
  grad_mygamma=grad_action(mygamma)
  call project(grad_mygamma)
  write (unit=fdu1, fmt=*) "# action(x) = ", action(mygamma)
  write (unit=fdu1, fmt=*) "# |grad_action(x)| = ", SQRT(sum(grad_mygamma**2))
  
  call srcprintsolution(tmpgamma,fdu1) 
return 
end subroutine printsolution

subroutine stdout_path(mygamma)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: mygamma
integer :: i,k
integer, dimension(3) :: dims

dims(1)=NOB
dims(2)=dim
dims(3)=steps
!! write (unit=0,fmt=*) "# writing stout ", dims

write (unit=fdu1,fmt=*)  dims, " !! NOB,dim,steps"

write (unit=fdu1,fmt=*) omega, " !! omega"

write (unit=fdu1,fmt=*) ALPHA, " !! ALPHA"

do i=1,NOB
	write (unit=fdu1,fmt=*) m(i) , " !! m(",i,")"
end do

do i=1,NOB
do k=0,steps+1
	write (unit=fdu1,fmt=*) mygamma(i,:,k)
end do; end do;

return

!! format specification

end subroutine stdout_path


end module io
