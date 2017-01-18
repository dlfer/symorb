module qnewton_func
use initdata
use mod_action 
use mod_project
use functions
implicit none

contains

subroutine qnewton_fcn(N,X,F)
implicit none
integer, intent(IN) :: N
real(precis), intent(OUT) :: F
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(NOB,dim,0:steps+1) :: myg

	call s_emboss(X,myg)
	call project(myg)
	F=action(myg)

return
end subroutine qnewton_fcn


subroutine qnewton_grad(N,X,G)
implicit none
integer, intent(IN) :: N 
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N), intent(OUT) :: G
real(precis), dimension(NOB,dim,0:steps+1) :: myg, gradmyg
	call s_emboss(X,myg)
	call project(myg)	
	gradmyg=grad_action(myg)
	call project(gradmyg)
	call s_flatten(gradmyg,G)
return
end subroutine qnewton_grad

!!! LDH = N
subroutine qnewton_hess(N,X,H,LDH)
implicit none
integer, intent(IN) :: N , LDH
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N,N), intent(OUT) :: H
real(precis), dimension(NOB,dim,0:steps+1) :: myg
real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: myH
integer :: i,j,k,index
H=0.d0 !! TODO

call s_emboss(X,myg)
call project(myg)
myH=grad2_action(myg)

do i=1,NOB; do j=1,dim; do k=0,steps+1
	call project( myH(:,:,:,i,j,k) )
end do; end do; end do;
do i=1,NOB; do j=1,dim; do k=0,steps+1
	call project( myH(i,j,k,:,:,:) )
end do; end do; end do;



index=0
do i=1,NOB; do j=1,dim; do k=0,steps+1
	index=index+1
	myg=myH(:,:,:,i,j,k)	
	call s_flatten(myg,H(:,index))
end do; end do; end do;


return
end subroutine qnewton_hess







end module qnewton_func
