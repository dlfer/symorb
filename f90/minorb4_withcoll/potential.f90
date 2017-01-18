module mod_potential
  use initdata
  use functions
  implicit none
  
contains
  
function potential(conf)
implicit none
real(precis), dimension (NOB,dim), intent(IN) :: conf
real(precis) :: potential, distanza
integer :: i,j
potential = 0.0d0
do i=1, NOB-1
	do j=i+1,NOB
		distanza = euc_norm(conf(i,:)-conf(j,:)) + epsilon
		potential = potential+m(i)*m(j)*distanza**(-ALPHA)
	end do
end do
return
end function potential


function grad_potential(conf)
implicit none
real(precis), dimension (NOB,dim) :: conf, grad_potential
real(precis) :: distanza
integer :: i,j

grad_potential = 0.0d0
do i=1,NOB-1
	do j=i+1,NOB
		distanza = euc_norm(conf(i,:) - conf(j,:)) + epsilon
		grad_potential(i,:) = grad_potential(i,:)- ALPHA*m(i)*m(j)*distanza**(-ALPHA-2)*(conf(i,:)-conf(j,:))
		grad_potential(j,:) = grad_potential(j,:)- ALPHA*m(i)*m(j)*distanza**(-ALPHA-2)*(conf(j,:)-conf(i,:))
	end do
end do

return
end function grad_potential



function grad2_potential(conf)
implicit none
real(precis), dimension(NOB,dim), intent(IN) :: conf
real(precis), dimension(NOB,dim,NOB,dim)  :: grad2_potential
real(precis) :: distanza, delta, termine
integer :: i1,i2,j1,j2

grad2_potential = 0.0d0
do i1=1,NOB-1
	do i2=i1+1,NOB
		distanza = euc_norm(conf(i1,:) - conf(i2,:)) + epsilon
		do j1=1,dim; do j2=1,dim;
		if (j1/=j2) then
			delta = 0.0d0
		else
			delta = 1.0d0
		end if	
		termine= &
		-ALPHA * m(i1) * m(i2)  *  &
		( &
		(-ALPHA - 2.0d0) * distanza **(-ALPHA - 4.0d0) * &
		(conf(i1,j1) - conf(i2,j1)) * ( conf(i1,j2) - conf(i2,j2)) + &
		distanza **(-alpha - 2) * delta &
		)
		grad2_potential(i1,j1,i1,j2) = grad2_potential(i1,j1,i1,j2) +&
		termine
		
		grad2_potential(i2,j1,i1,j2) = grad2_potential(i2,j1,i1,j2) -&
		termine
		
		grad2_potential(i1,j1,i2,j2) = grad2_potential(i1,j1,i2,j2) -&
		termine
		
		grad2_potential(i2,j1,i2,j2) = grad2_potential(i2,j1,i2,j2) +&
		termine

		end do; end do;	
	end do
end do

return
end function grad2_potential







function UU_var(mygamma)
    implicit none
    real(precis), dimension (NOB,dim,0:steps+1), intent(IN) :: mygamma
    real(precis), dimension (NOB,dim,0:steps+1)  ::  gradU
    real(precis)  ::  UU_var
    gradU=grad_UU(mygamma)	
      UU_var=-ALPHA * prodotto_scalare(gradU,mygamma) 
    return
end function UU_var

function UU(mygamma)
implicit none
real(precis), dimension (NOB,dim,0:steps+1), intent(IN) :: mygamma
real(precis)  ::  UU, tempo
real(precis), dimension(NOB,dim) :: thisconf
integer :: h,k
UU=0.0d0

do h=1,NOP 
tempo=real(h,precis) * pi  / real(NOP+1,precis) 
	thisconf=mygamma(:,:,0) + (mygamma(:,:,steps+1) - mygamma(:,:,0))* tempo / pi 
	do k=1,steps
		thisconf(:,:)=thisconf(:,:)+mygamma(:,:,k)*sin_kth(k,h)
	end do
	UU=UU+potential(thisconf)
end do

UU = UU + 0.5d0 * ( potential(mygamma(:,:,0)) + potential(mygamma(:,:,steps+1)))

UU=UU * pi / real(NOP+1,precis) 
return
end function UU


function grad_UU(mygamma)
implicit none
real(precis), dimension (NOB,dim,0:steps+1), intent(IN) :: mygamma
real(precis), dimension(NOB,dim,0:steps+1) :: grad_UU 
real(precis), dimension(NOB,dim) :: thisconf, gradconf
real(precis) :: tempo
integer :: h,k
grad_UU=0.0d0
thisconf=0.0d0
gradconf=0.0d0

do h=1,NOP
	tempo=real(h,precis) * pi  / real(NOP+1,precis) 
	thisconf=mygamma(:,:,0) + (mygamma(:,:,steps+1) - mygamma(:,:,0))* tempo / pi 
	do k=1,steps
		thisconf(:,:)=thisconf(:,:)+mygamma(:,:,k)*sin_kth(k,h)
	end do
	gradconf = grad_potential(thisconf)
	
	do k=1,steps
		grad_UU(:,:,k) = grad_UU(:,:,k) + gradconf(:,:) * sin_kth(k,h)
	end do

	grad_UU(:,:,0) = grad_UU(:,:,0) + gradconf * (1-tempo/pi) 
	grad_UU(:,:,steps+1) = grad_UU(:,:,steps+1) + gradconf(:,:) * (tempo/pi)
end do

grad_UU(:,:,0) = grad_UU(:,:,0) + 0.5d0 * grad_potential(mygamma(:,:,0))
grad_UU(:,:,steps+1) = grad_UU(:,:,steps+1) + 0.5d0 * grad_potential(mygamma(:,:,steps+1))


grad_UU= grad_UU *pi  / real(NOP+1,precis) 

return
end function grad_UU


function grad2_UU(mygamma)
real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: mygamma
real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: grad2_UU
real(precis), dimension(NOB,dim) :: thisconf
real(precis), dimension(NOB,dim,NOB,dim) :: grad2conf
real(precis) :: tempo
integer :: h, k1, k2, k
grad2_UU=0.0d0
thisconf=0.0d0
grad2conf=0.0d0
grad2_UU=0.0d0


do h=1,NOP
	tempo=real(h,precis) * pi  / real(NOP+1,precis) 
	thisconf=mygamma(:,:,0) + (mygamma(:,:,steps+1) - mygamma(:,:,0))* tempo / pi 
	do k=1,steps
		thisconf(:,:)=thisconf(:,:)+mygamma(:,:,k)*sin_kth(k,h)
	end do
	grad2conf = grad2_potential(thisconf)

	do k1=1,steps-1; do k2=k1+1,steps
		grad2_UU(:,:,k1,:,:,k2) = grad2_UU(:,:,k1,:,:,k2) + &
		  grad2conf * sin_kth(k1,h) * sin_kth(k2,h)
		grad2_UU(:,:,k2,:,:,k1) = grad2_UU(:,:,k2,:,:,k1) + &
		  grad2conf * sin_kth(k1,h) * sin_kth(k2,h)
	end do; end do;

	do k=1,steps;
		grad2_UU(:,:,k,:,:,k) = grad2_UU(:,:,k,:,:,k) + &
		  grad2conf * sin_kth(k,h) ** 2 

		grad2_UU(:,:,k,:,:,0) = grad2_UU(:,:,k,:,:,0) + &
		  grad2conf * sin_kth(k,h) * (1 - tempo/pi)
		grad2_UU(:,:,0,:,:,k) = grad2_UU(:,:,0,:,:,k) + &
		  grad2conf * sin_kth(k,h) * (1 - tempo/pi)
		grad2_UU(:,:,steps+1,:,:,k) = grad2_UU(:,:,steps+1,:,:,k) + &
		  grad2conf * sin_kth(k,h) * tempo / pi
		grad2_UU(:,:,k,:,:,steps+1) = grad2_UU(:,:,k,:,:,steps+1) + &
		  grad2conf * sin_kth(k,h) * tempo / pi
	end do;

	grad2_UU(:,:,0,:,:,0) = grad2_UU(:,:,0,:,:,0) +  &
		  grad2conf * (1-tempo/pi)**2 
	grad2_UU(:,:,steps+1,:,:,steps+1) = grad2_UU(:,:,steps+1,:,:,steps+1) +  &
		  grad2conf * (tempo/pi)**2

	grad2_UU(:,:,0,:,:,steps+1) = grad2_UU(:,:,0,:,:,steps+1) + &
		grad2conf * (1-tempo/pi) * tempo /pi	
	grad2_UU(:,:,steps+1,:,:,0) = grad2_UU(:,:,steps+1,:,:,0) + &
		grad2conf * (1-tempo/pi) * tempo /pi	

end do;

grad2_UU(:,:,0,:,:,0) = grad2_UU(:,:,0,:,:,0) + &
	grad2_potential(mygamma(:,:,0)) * 0.5d0
grad2_UU(:,:,steps+1,:,:,steps+1) = grad2_UU(:,:,steps+1,:,:,steps+1) + &
	grad2_potential(mygamma(:,:,steps+1)) * 0.5d0

grad2_UU= grad2_UU *pi  / real(NOP+1,precis) 

return 
end function grad2_UU



end module mod_potential
