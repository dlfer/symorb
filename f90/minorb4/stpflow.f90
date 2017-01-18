module mod_stpflow
  use initdata
  use mod_action
  use mod_project
  implicit none

contains
  
subroutine stpflow(mygamma)
  implicit none
  real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: mygamma
  real(precis), dimension(NOB,dim,0:steps+1) :: vunew, yp, vu, ypnew
  real(precis) :: p=0.1d0, passo, J, D, old, t, MAXT, tol, new
  integer ::  MaxNum, num
  
  t=0
  MAXT = 100
  MaxNum = 100
  vu = mygamma
  passo = p
  tol = epsilon
  num = 2
  old = action(mygamma) 
  J = old
  yp = - grad_action(mygamma)
  call project(yp)
  D = sqrt(sum(abs(yp)**2))
  
  write(unit=0,fmt=*) 'Valore azione iniziale=', J
  write(unit=0,fmt=*) 'Norma gradiente iniziale=', D
  
  do 
     if (t>MAXT) then
       write (unit=0,fmt=*) "Superato tempo massimo "
       exit
     end if
     vunew = vu + passo*yp
     new = action(vunew) 
     ypnew = - grad_action(vunew)
     call project(ypnew)
     if (new > old) then
        passo = passo/2.0d0;
     else
     old = new
     yp = ypnew
     vu = vunew
     D = sqrt(sum(abs(yp)**2))
     t = t+passo
     passo = min(2.0d0*passo,p)
     num = num+1
       if (D < tol) then
         write (unit=0,fmt=*) "Norma gradiente <", epsilon
	 exit
       end if
       if (num > MaxNum) then
         write(unit=0,fmt=*) "Numero iterazioni>", MaxNum
	 exit
       end if
     end if
  if (passo < 1.0D-10) then
    exit
  end if
    end do
  mygamma = vu
  
!  write(unit=0,fmt=*) 'Valore azione finale', old
!  write(unit=0,fmt=*) 'Norma gradiente finale',  D     
     
 return
end subroutine stpflow

end module mod_stpflow
