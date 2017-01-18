module ccg
use initdata
use functions
use mod_action
use mod_project

implicit none


contains

subroutine brent_linmin(brentgamma,XI,minimo)
implicit none
real(precis),  dimension(NOB,dim,0:steps+1) , intent(INOUT):: brentgamma
real(precis),  dimension(NOB,dim,0:steps+1) , intent(IN) :: XI
real(precis), intent(OUT)  :: minimo
real(precis) :: AX, CX, BX, FA, FB, FC,  DUM, R, Q, U, ULIM, FU, &
aa,bb,uu,vv,ww,xx,dd,ee,fx,fv,fw,xm,tol1,tol2,rr,qq,pp,eetemp
integer iter
AX=0.
BX=1.
CX=2.

FA=action(brentgamma+AX*XI)
FB=action(brentgamma+BX*XI)
if (FB > FA) then
  DUM=AX
  AX=BX
  BX=DUM
  DUM=FB
  FB=FA
  FA=DUM	
endif
CX=BX+GOLD*(BX-AX)
FC=action(brentgamma+CX*XI)
! 1
1 if ( FB >= FC ) then
  R= (BX-AX)*(FB-FC)
  Q=(BX-CX)*(FB-FA)
  U=BX-( (BX-CX)*Q - (BX-AX)*R)/ (2. * sign( max(abs(Q-R),TINY),Q-R))
  ULIM=BX+GLIMIT*(CX-BX)	
  if ( (BX-U)*(U-CX) > 0. ) then
    FU=action(brentgamma+U*XI)
    if (FU < FC) then
    AX=BX
    FA=FB
    BX=U
    FB=FU
    ! exit
    goto 1
  else if ( FU > FB ) then
    CX=U
    FC=FU
    ! exit !
    goto 1
  endif
  U=CX+GOLD*(CX-BX)
  FU=action(brentgamma+U*XI)
else if ( (CX-U) * ( U - ULIM) > 0. ) then
  FU=action(brentgamma + U*XI)
  if ( FU < FC ) then
    BX=CX
    CX=U
    U=CX+GOLD*(CX-BX)
    FB=FC
    FC=FU
    FU=action(brentgamma+U*XI)
  endif
else if ( (U-ULIM)*(ULIM - CX) >= 0. ) then
  U=ULIM
  FU=action(brentgamma+U*XI)
else
  U=CX+GOLD*(CX-BX)
  FU=action(brentgamma+U*XI)
endif
AX=BX; BX=CX; CX=U; FA=FB; FB=FC; FC=FU; 
goto 1
endif


! now AX, BX, CX, FA,FB,FC  are the brackets
! we procced with the brent line search (no gradient)
aa=min(AX,CX)
bb=max(AX,CX)
vv=BX
ww=vv; xx=vv; ee=0.0
fx=action(brentgamma+xx*XI)
fv=fx; fw=fx
do iter=1,brent_itmax
    xm=0.5*(aa+bb)
    tol1=ccg_linmin_TOL*abs(xx)+ZEPS
    tol2=2. * tol1
    if ( abs(xx-xm) <= (tol2 - .5*(bb-aa))) goto 103
    if (abs(ee) > tol1) then
       rr=(xx-ww)*(fx-fv)
       qq=(xx-vv)*(fx-fw)
       pp=(xx-vv)*qq-(xx-ww)*rr
       qq=2.*(qq-rr)
       if(qq>0.) pp=-pp
       qq=abs(qq)
       eetemp=ee
       ee=dd
       if (abs(pp) >= abs( .5*qq*eetemp) .OR. pp <= qq*(aa-xx) .OR. pp >= qq*(bb-xx)) goto 101
       dd=pp/qq
       uu=xx+dd
       if (uu-aa < tol2 .OR. bb-uu < tol2) dd=sign(tol1,xm-xx)
       goto 102
    endif
101 if (xx >= xm) then
       ee=aa-xx
    else
       ee=bb-xx
    endif
    dd=CGOLD*ee
102 if (ABS(dd) >= tol1) then
       uu=xx+dd
    else
       uu=xx+sign(tol1,dd)
    endif
    fu=action(brentgamma+uu*XI)
    if (fu <= fx ) then
       if (uu >= xx) then
           aa=xx
       else
           bb=xx
       endif
       vv=ww; fv=fw;ww=xx;fw=fx;xx=uu;fx=fu;
    else
       if(uu<xx) then
          aa=uu
       else
          bb=uu
       endif
       if(fu<=fw .OR. ww.EQ.xx) then
          vv=ww; fv=fw; ww=uu; fw=fu;
       else if (fu<=fv .OR. vv.EQ.xx .OR. vv.EQ.ww) then
          vv=uu
          fv=fu
       endif
    endif
end do 


103 brentgamma=brentgamma+xx*XI
minimo=fx
!!! write(unit=0,fmt=*) "brent_linmin returning action =", minimo

return 
end subroutine brent_linmin





subroutine ccg_relax(ccggamma)
implicit none

! path to relax (the boundary is supposed to have constraints)
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: ccggamma
! real(precis), dimension(NEQ) :: path
!!! integer, intent(INOUT) :: iterations
integer :: i,j,k,its
! rtol, atol
real(precis), dimension(NOB,dim,0:steps+1) :: G,H,XI
real(precis) :: GG, DGG, FP,minimo,GAM

!! call s_flatten(ccggamma,path)

FP=action(ccggamma)
!! write (unit=0,fmt=*)  "ccg_relax called with action...", FP

!! call DFUNC(ccggamma,XI)


XI = grad_action(ccggamma)
call project(XI)
G(:,:,:) = -XI(:,:,:)
H(:,:,:) = G(:,:,:)
XI(:,:,:) = H(:,:,:)

do its=1,ccgiterations
    call brent_linmin ( ccggamma , XI , minimo )
!!! write (unit=0,fmt=*) "minimo= ", minimo , "; FP= ", FP, abs(minimo-FP)    

    if (( 2.*abs(minimo - FP)) <  & 
& ccg_relax_RTOL*(ABS(minimo)+abs(FP)+ccg_relax_ATOL))  then
    !! write (unit=0,fmt=*) "minimo= ", minimo , "; FP= ", FP, ": they are very close"   

	RETURN  !!! TODO CHECK
    endif
    FP=action(ccggamma)
    XI = grad_action(ccggamma)
    call project(XI)
    GG=0
    DGG=0
    do i=1,NOB
      do j=1,dim
       do k=0,steps+1
	GG=GG+G(i,j,k)**2
	 DGG=DGG+ XI(i,j,k)**2. !!! Fletcher--Reeves
	!! DGG=DGG + ( XI(i,j,k)**2. + G(i,j,k))*XI(i,j,k) !!! Polak-Ribiere
    end do; end do; end do

    if (GG == 0 ) RETURN
    GAM=DGG/GG
    do i=1,NOB
      do j=1,dim
       do k=0,steps+1
    G(i,j,k)=-XI(i,j,k)
    H(i,j,k)=G(i,j,k)+GAM*H(i,j,k)
    XI(i,j,k)=H(i,j,k)
    end do; end do; end do
end do
RETURN
end subroutine ccg_relax

end module ccg
