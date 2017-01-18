module mod_minorb
 use initdata 
 use pars
 use io
 use mod_starting_path	
 use mod_bigmat_K
 use mod_stpflow
 use ccg
 use optim
 use qnewton
 use mod_nonlinear
implicit none 


contains

subroutine minorb_init
if ( VERBOSE ) then 
 write(unit=0,fmt=*) "# minorb_init"
end if 

 call inizializza
 call init_values
!!  call init_print_values
 call init_allocate
 call def_bigmat_K
 call qnewton_init
 call optim_init
 call nonlinear_init

end subroutine minorb_init
!! SPMETHOD = 2

subroutine minorb_exit
if ( VERBOSE ) then 
 write(unit=0,fmt=*) "# minorb_exit"
end if 


call fine
end subroutine minorb_exit


subroutine check 
use initdata
implicit none
real(precis), dimension(NOB,dim,0:steps+1) :: x, v
real(precis), dimension(NOB,dim,0:steps+1) :: gradiente, diff_gradiente
real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: grad2
real(precis) :: norma,t, result, other_result
integer :: i,j,k,  i1, j1, k1 ,i2 ,j2 ,k2 

call randompath(x)
call project(x)
call randompath(v)
call project(v)
norma = SQRT(sum( v**2 ))
t= 10.0D-12 /norma

write (unit=0,fmt=*) "****************************************************************************"
write (unit=0,fmt=*) "Checking on random x with action ", action(x)

write (unit=0,fmt=*) "Comparing derivatives with t:    ", t
write (unit=0,fmt=*) "num_result:    ", (action(x+t*v) - action(x-t*v) ) / (2*t)
gradiente=grad_action(x)
result=0.0d0
do i=1,NOB; do j=1,dim; do k=0,steps+1
	result = result + gradiente(i,j,k) * v(i,j,k)
end do; end do; end do;
write (unit=0,fmt=*) "comparing to:  ", result

!! t= 10.0D-12 /norma
t= 10.0D-12 /norma
write (unit=0,fmt=*) "Checking on random x with action ", action(x)
write (unit=0,fmt=*) "Comparing 2-nd derivatives with t:    ", t
write (unit=0,fmt=*) "delta**2(action):    ", (action(x+t*v)  + action(x -t*v)-  2.d0 * action(x) ) * t**(-2.0d0)

grad2=grad2_action(x)
result=0.0d0
do i1=1,NOB; do j1=1,dim; do k1=0,steps+1
	do i2=1,NOB; do j2=1,dim; do k2=0,steps+1
		result = result + grad2(i1,j1,k1,i2,j2,k2) * v(i1,j1,k1) * v(i2,j2,k2)
	end do; end do; end do;
end do; end do; end do;
write (unit=0,fmt=*) "grad2_action:        ", result


diff_gradiente= (grad_action(x+t*v) - grad_action(x-t*v) ) * (2.0d0*t)**(-1.0d0)
other_result=0.0d0
do i1=1,NOB; do j1=1,dim; do k1=0,steps+1
		other_result = other_result + diff_gradiente(i1,j1,k1) * v(i1,j1,k1) 
end do; end do; end do;
write (unit=0,fmt=*) "delta(grad_action):  ", other_result




write (unit=0,fmt=*) "****************************************************************************"
write (unit=0,fmt=*) "****************************************************************************"

end subroutine check

!! subroutine check2
!! implicit none
!! real(precis), dimension(NOB,dim,0:steps+1) :: xuv,grad_xuv
!! !! real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: grad2
!! !! real(precis) :: normau,normav,t, result
!! real(precis) :: tempo1, tempo2, dtime, totaltime
!! integer :: i1, j1, k1 ,i2 ,j2 ,k2 ,  passo
!! 
!! call randompath(xuv)
!! call project(xuv)
!! 
!! 
!! 
!! do passo=1,2
!! call randompath(xuv)
!! call project(xuv)
!! 
!! 	call mytimer(tempo1)
!! 	write (unit=0,fmt=*) "****************************************************************************"
!! 	write (unit=0,fmt=*) "Trying IMSL minimization QNEWTON subroutine... (Random path):    "
!! 	call qnewton_relax(xuv)
!! 	write (unit=0,fmt=*)  "action        = ", action(xuv)
!! 	grad_xuv=grad_action(xuv); call project(grad_xuv)
!! 	write (unit=0,fmt=*) "|grad_action|=   ",  norma(grad_xuv)
!! 	call mytimer(tempo2)
!! 	dtime = tempo2 - tempo1
!! 	write (unit=0, fmt=*) "tempo = ", dtime
!! 
!! end do
!! 
!! do passo=1,2
!! call randompath(xuv)
!! call project(xuv)
!! 
!! 	call mytimer(tempo1)
!! 	write (unit=0,fmt=*) "****************************************************************************"
!! 	write (unit=0,fmt=*) "Trying IMSL minimization QNEWTON_HESS subroutine... (Random path):    "
!! 	call qnewton_hess_relax(xuv)
!! 	write (unit=0,fmt=*)  "action        = ", action(xuv)
!! 	grad_xuv=grad_action(xuv); call project(grad_xuv)
!! 	write (unit=0,fmt=*) "|grad_action|=   ",  norma(grad_xuv)
!! 	call mytimer(tempo2)
!! 	dtime = tempo2 - tempo1
!! 	write (unit=0, fmt=*) "tempo = ", dtime
!! 
!! end do
!! 
!! do passo=1,2
!! call randompath(xuv)
!! call project(xuv)
!! 
!! 	call mytimer(tempo1)
!! 	write (unit=0,fmt=*) "****************************************************************************"
!! 	write (unit=0,fmt=*) "Trying IMSL minimization QNEWTON_DUMIDH subroutine... (Random path):    "
!! 	call qnewton_dumidh(xuv)
!! 	write (unit=0,fmt=*)  "action        = ", action(xuv)
!! 	grad_xuv=grad_action(xuv); call project(grad_xuv)
!! 	write (unit=0,fmt=*) "|grad_action|=   ", norma(grad_xuv)
!! 	call mytimer(tempo2)
!! 	dtime = tempo2 - tempo1
!! 	write (unit=0, fmt=*) "tempo = ", dtime
!! 
!! end do
!! 
!! do passo=1,2
!! call randompath(xuv)
!! call project(xuv)
!! 
!! 	call mytimer(tempo1)
!! 	write (unit=0,fmt=*) "****************************************************************************"
!! 	write (unit=0,fmt=*) "Trying IMSL minimization QNEWTON_DUMCGG subroutine... (Random path):    "
!! 	call qnewton_dumcgg(xuv)
!! 	write (unit=0,fmt=*)  "action        = ", action(xuv)
!! 	grad_xuv=grad_action(xuv); call project(grad_xuv)
!! 	write (unit=0,fmt=*) "|grad_action|=   ", norma(grad_xuv)
!! 	call mytimer(tempo2)
!! 	dtime = tempo2 - tempo1
!! 	write (unit=0, fmt=*) "tempo = ", dtime
!! 
!! end do
!! 
!! do passo=1,2
!! call randompath(xuv)
!! call project(xuv)
!! 
!! 	call mytimer(tempo1)
!! 	write (unit=0,fmt=*) "****************************************************************************"
!! 	write (unit=0,fmt=*) "Trying IMSL minimization QNEWTON_DUMPOL subroutine... (Random path):    "
!! 	call qnewton_dumpol(xuv)
!! 	write (unit=0,fmt=*)  "action        = ", action(xuv)
!! 	grad_xuv=grad_action(xuv); call project(grad_xuv)
!! 	write (unit=0,fmt=*) "|grad_action|=   ", norma(grad_xuv)
!! 	call mytimer(tempo2)
!! 	dtime = tempo2 - tempo1
!! 	write (unit=0, fmt=*) "tempo = ", dtime
!! 
!! end do
!! 
!! 
!! do passo=1,2
!! call randompath(xuv)
!! call project(xuv)
!! 
!! 	call mytimer(tempo1)
!! 	write (unit=0,fmt=*) "****************************************************************************"
!! 	write (unit=0,fmt=*) "Trying IMSL minimization OPTIM_RELAX subroutine... (Random path):    "
!! 	call optim_relax(xuv)
!! 	write (unit=0,fmt=*)  "action        = ", action(xuv)
!! 	grad_xuv=grad_action(xuv); call project(grad_xuv)
!! 	write (unit=0,fmt=*) "|grad_action|=   ", norma(grad_xuv)
!! 	call mytimer(tempo2)
!! 	dtime = tempo2 - tempo1
!! 	write (unit=0, fmt=*) "tempo = ", dtime
!! 
!! end do
!! 
!! 
!! 
!! 
!! end subroutine check2




subroutine relax(mygamma)
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: mygamma

if (ALG == 0) then
	write (unit=0,fmt=*) "# using IMSL DUMING"
	write (unit=0,fmt=*) "# Unconstrained Minimization with analytic Gradient"
	call qnewton_relax(mygamma)
else if (ALG == 1) then
	write (unit=0,fmt=*) "# using IMSL DUMIAH"
	write (unit=0,fmt=*) "# Unconstrained Minimization with Analytic Hessian"
	call qnewton_hess_relax(mygamma)
else if (ALG == 2) then
	write (unit=0,fmt=*) "# using IMSL DUMIDH"
	write (unit=0,fmt=*) "# Unconstrained Minimization with finite-Difference Hessian"
	call qnewton_dumidh(mygamma)
else if (ALG == 3) then
	write (unit=0,fmt=*) "# using IMSL DUMCGG"
	write (unit=0,fmt=*) "# Unconstrained Minimization with Conjugate Gradient and analytic Gradient"
	call qnewton_dumcgg(mygamma)
else if (ALG == 4) then
	write (unit=0,fmt=*) "# using IMSL DUMPOL"
	write (unit=0,fmt=*) "# Unconstrained Minimization without gradient (nonsmooth)"
	call qnewton_dumpol(mygamma)
else if (ALG == 5) then
	write (unit=0,fmt=*) "# using IMSL DLCONG"
	write (unit=0,fmt=*) "# Linearly Constrained Minimization with Analytic Gradient"
	call optim_relax(mygamma)
else if (ALG == 6) then
	write (unit=0,fmt=*) "# using S.V. STEPFLOW"
	write (unit=0,fmt=*) "# Step-Flow Descent"
	call stpflow(mygamma)
else if (ALG == 7) then
	write (unit=0,fmt=*) "# using our GC"
	write (unit=0,fmt=*) "# Simple Conjugate Gradient"
	call ccg_relax(mygamma)

else if (ALG == 100) then
	write (unit=0,fmt=*) "# using NONLINEAR DNEQNF"
	write (unit=0,fmt=*) "# Newton-Powell Finite-difference Jacobian"
	call nonlinear_dneqnf(mygamma)
else if (ALG == 200) then
	write (unit=0,fmt=*) "# using NONLINEAR DNEQNJ"
	write (unit=0,fmt=*) "# Newton-Powell Analytic Jacobian"
	call nonlinear_dneqnj(mygamma)
else if (ALG == 300) then
	write (unit=0,fmt=*) "# using NONLINEAR DNEQBF"
	write (unit=0,fmt=*) "# Secant Broyden's Update and Finite-difference Jacobian"
	call nonlinear_dneqbf(mygamma)

else if (ALG == 400) then
	write (unit=0,fmt=*) "# using NONLINEAR DNEQBJ"
	write (unit=0,fmt=*) "# Secant Broyden's Update and Analytic Jacobian"
	call nonlinear_dneqbj(mygamma)
else 
	write (unit=0,fmt=*) "FATAL ERROR: algorithm ", ALG, " NOT FOUND!"
	stop
end if


end subroutine relax


function howsol(mygamma)
implicit none
real(precis), dimension (NOB,dim,0:steps+1) :: mygamma, grad_mygamma
real(precis) :: howsol
!! 
grad_mygamma=grad_action(mygamma)
call project(grad_mygamma)
howsol=norma(grad_mygamma)
end function howsol



end module mod_minorb
