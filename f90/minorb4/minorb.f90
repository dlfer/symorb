program minorb
use mod_minorb
implicit none

!! write (unit=0,fmt=*) "# init..."
call minorb_init
!! write (unit=0,fmt='(a2)',advance='NO') " #"
!! write (unit=0,fmt=*) "# parsing the command..."
call parse_command
!! write (unit=0,fmt=*) "# exit..."
call minorb_exit

contains 


subroutine parse_command()
real(precis), dimension(NOB,dim,0:steps+1) :: minorbpath
integer :: relax_ALG, newton_ALG, original_ALG

original_ALG = ALG
relax_ALG = mod (ALG, 100)
newton_ALG = ALG / 100

if (COMMAND == "relax") then
	write (unit=0,fmt=*) "# relaxing..."
	ALG=relax_ALG
	call stdin_starting_path(minorbpath)
	call relax(minorbpath)
	call project(minorbpath)
	if  ((newton_ALG) /= 0) then
		ALG=newton_ALG * 100
		call relax(minorbpath)
		call project(minorbpath)
	end if
	write (unit=0,fmt=*) "# writing out..."
	call stdout_path(minorbpath)
	write (unit=0,fmt=*) "# done..."
	ALG=original_ALG
else if (COMMAND == "printsol") then
	call stdin_starting_path(minorbpath)
	call printsolution(minorbpath)

else if (COMMAND == "all") then
	write (unit=0,fmt=*) "# starting and doing all..."
	call starting_path(minorbpath)
	call relax(minorbpath)
	call project(minorbpath)
	call printsolution(minorbpath)

else if (COMMAND == "new") then
	write (unit=0,fmt=*) "# starting new path..."
	call starting_path(minorbpath)
	write (unit=0,fmt=*) "# writing out new path..."
	call stdout_path(minorbpath)
	write (unit=0,fmt=*) "# done!"

else if (COMMAND == "newton") then
	write (unit=0,fmt=*) "# computing newton path..."
	call stdin_starting_path(minorbpath)
	ALG=newton_ALG * 100
	call relax(minorbpath)
	call project(minorbpath)
	write (unit=0,fmt=*) "# writing out newton path..."
	call stdout_path(minorbpath)
	write (unit=0,fmt=*) "# done!"
	ALG=original_ALG

else if (COMMAND == "action") then
	call stdin_starting_path(minorbpath)
	write (unit=fdu1,fmt=*) action(minorbpath)

else if (COMMAND == "howsol") then
	call stdin_starting_path(minorbpath)
	write (unit=fdu1,fmt=*) howsol(minorbpath)
else
	write (unit=0,fmt=*) " ***FATAL ERROR***: UNKNOWN COMMAND: ", COMMAND
stop
end if

return
end subroutine parse_command

end program minorb
