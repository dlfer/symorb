module tempfunc
use initdata
use mod_action 
use functions
implicit none

contains

subroutine optim_fcn(N,X,F)
implicit none
integer :: N
real(precis) :: F
real(precis), dimension(N) :: X
real(precis), dimension(NOB,dim,0:steps+1) :: myg
	call s_emboss(X,myg)
	F=action(myg)
return
end subroutine optim_fcn


subroutine optim_grad(N,X,G)
implicit none
integer :: N 
real(precis), dimension(N) :: X,G
real(precis), dimension(NOB,dim,0:steps+1) :: myg, gradmyg

call s_emboss(X,myg)
gradmyg=grad_action(myg)
call s_flatten(gradmyg,G)

return
end subroutine optim_grad



end module tempfunc
