module functions

use initdata
implicit none

contains


function euc_norm(point)
 real(precis), dimension(dim) :: point
 real(precis) :: euc_norm
 euc_norm=SQRT(sum(point(:)**2))
end function euc_norm

function sq_euc_norm(point)
 real(precis), dimension(dim) :: point
 real(precis) :: sq_euc_norm
 sq_euc_norm=sum(point(:)**2)
end function sq_euc_norm

function maximum(mm)
 real(precis),dimension(NOB,0:steps+1), intent(IN) :: mm
 real(precis) :: maximum, tmpvar
 integer :: i,j
 maximum=0
 do i=1,NOB
        do j=0,steps+1
        tmpvar=mm(i,j)
        if (tmpvar > maximum) then
                maximum=tmpvar
        end if
        end do;
 end do;
end function maximum


function min_dist(conf)
 implicit none
 real(precis), dimension(NOB,dim) :: conf
 real(precis) :: tmpnorm
 real(precis) :: min_dist
 integer :: i,j

 min_dist=euc_norm((conf(1,:) - conf(2,:)))
 do i=1,NOB-1
  do j=i+1,NOB
   tmpnorm= euc_norm( (conf(i,:) - conf(j,:) ))
   if (tmpnorm < min_dist) then
        min_dist=tmpnorm
   end if
  end do
 end do

end function min_dist

subroutine s_path_min_dist(mygamma,path_min_dist)
 real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: mygamma
 real(precis) :: tmpnorm
 real(precis), intent(OUT) :: path_min_dist
 integer :: k
 path_min_dist=min_dist(mygamma(:,:,0))
 do k=1,(steps+1)
  tmpnorm = min_dist(mygamma(:,:,k))
  if (tmpnorm < path_min_dist) path_min_dist=tmpnorm
 end do
end subroutine s_path_min_dist

function path_min_dist(mygamma)
 real(precis), dimension(NOB,dim,0:steps+1) :: mygamma
 real(precis) :: path_min_dist
 call s_path_min_dist(mygamma,path_min_dist) 
return
end function path_min_dist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! APPLICAZIONI LINEARI E FORME QUADRATICHE!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine s_lin_appl_st(bigmat,vect_in,lin_appl_st)
  real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1), intent(IN) :: bigmat
  real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: vect_in
  real(precis), dimension(NOB,dim,0:steps+1), intent(OUT) :: lin_appl_st
  integer :: i,j,k,ip,jp,kp
  lin_appl_st = 0.0d0 
  
  do i=1,NOB
    do j=1,dim
      do k=0,steps+1
        do ip=1,NOB
          do jp=1,dim
            do kp=0,steps+1
              lin_appl_st(i,j,k)=lin_appl_st(i,j,k)+bigmat(i,j,k,ip,jp,kp)*vect_in(ip,jp,kp)	  
	    end do
	  end do
        end do
      end do
    end do
  end do
end subroutine s_lin_appl_st

function lin_appl_st(bigmat,vect_in)
  real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: bigmat
  real(precis), dimension(NOB,dim,0:steps+1) :: vect_in
  real(precis), dimension(NOB,dim,0:steps+1) :: lin_appl_st
  call s_lin_appl_st(bigmat,vect_in,lin_appl_st)
return
end function lin_appl_st

subroutine s_prodotto_scalare(vect1,vect2,prodotto_scalare)
  real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: vect1, vect2
  real(precis), intent(OUT) :: prodotto_scalare
  integer :: i,j,k
  
  prodotto_scalare = 0.0d0   
  do i=1,NOB
    do j=1,dim
      do k=0,steps+1
        prodotto_scalare = prodotto_scalare + vect1(i,j,k)*vect2(i,j,k)	  
      end do
    end do
  end do
return
end subroutine s_prodotto_scalare

function prodotto_scalare(vect1,vect2)
 real(precis), dimension(NOB,dim,0:steps+1) :: vect1, vect2
  real(precis) :: prodotto_scalare
  call s_prodotto_scalare(vect1,vect2,prodotto_scalare)
return
end function prodotto_scalare


subroutine s_quad_form_st(bigmat,vect_in,result)
  real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1), intent(IN) :: bigmat
  real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: vect_in
  real(precis), intent(OUT) :: result
  integer :: i,j,k,ip,jp,kp
  
  result = 0.
  do i=1,NOB
    do j=1,dim
      do k=0,steps+1
        do ip=1,NOB
          do jp=1,dim
            do kp=0,steps+1 
              result = result + bigmat(i,j,k,ip,jp,kp)*vect_in(i,j,k)*vect_in(ip,jp,kp)	  
	    end do
	  end do
        end do
      end do
    end do
  end do
 return
end subroutine s_quad_form_st

function quad_form_st(bigmat,vect_in)
  real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: bigmat
  real(precis), dimension(NOB,dim,0:steps+1) :: vect_in
  real(precis) :: quad_form_st
  call s_quad_form_st(bigmat,vect_in,quad_form_st)
return
end function quad_form_st
!!!!!



 function mycoeff(mypoints)
    implicit none
  
    real(precis), dimension(1:NOB,1:dim,0:NOP+1) :: mypoints, mycoefflunga
    real(precis), dimension(1:NOB,1:dim,0:steps+1) :: mycoeff
    real(precis) :: tempo_h
    integer :: k,h,l
       
    mycoeff=0.0d0 
    mycoefflunga=0.0d0
    mycoefflunga(:,:,0)=mypoints(:,:,0)
    mycoeff(:,:,0)=mypoints(:,:,0)
    
    mycoefflunga(:,:,steps+1)=mypoints(:,:,NOP+1)
    mycoeff(:,:,steps+1)=mypoints(:,:,NOP+1)
    
    do l=1,NOP
      do h=1,NOP
        tempo_h = pi*real(h,precis)/(real(NOP,precis)+1.0d0)

mycoefflunga(:,:,l) = & 
& mycoefflunga(:,:,l) + 2.0d0 / real(NOP+1,precis) * (mypoints(:,:,h)-mypoints(:,:,0) - real(h,precis) / real(NOP+1,precis) *&
& ( mypoints(:,:,NOP+1) - mypoints(:,:,0) ) ) * sin(l*tempo_h)

      end do
    end do
    do k=1,steps
      mycoeff(:,:,k) = mycoefflunga(:,:,k)
    end do
    return 
  end function mycoeff




subroutine s_flatten(myg,gg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(IN)  ::  myg
real(precis), dimension(NTOTALVAR), intent(OUT) :: gg
integer :: i,j,k, index
index=0
do i=1,NOB; do j=1,dim; do k=0,steps+1 
	index=index+1
	gg(index) = myg(i,j,k)
end do; end do; end do;

return 
end subroutine s_flatten

subroutine s_emboss(gg,myg)
implicit none
real(precis), dimension(NTOTALVAR), intent(IN) :: gg
real(precis), dimension(NOB,dim,0:steps+1), intent(OUT)  ::  myg
integer :: i,j,k, index

index=0
do i=1,NOB; do j=1,dim; do k=0,steps+1 
	index=index+1
	myg(i,j,k) = gg(index)
end do; end do; end do;

return 
end subroutine s_emboss

function norma(myg)
real(precis), dimension(NOB,dim,0:steps+1) , intent(IN) :: myg
real(precis) :: norma
integer :: i,j,k
norma=0.0d0
do i=1,NOB; do j=1,dim; do k=0,steps+1
norma = norma + myg(i,j,k) ** 2.0d0
end do; end do; end do;
norma = SQRT(norma)
end function norma

end module functions
