module cFuncs
	implicit none

	interface
		function cf_puts(str) bind(C, name="puts")
			use :: iso_c_binding
			type(c_ptr), value :: str
			integer(c_int) :: c_puts
		end function cf_puts
		! strlen suddenly decided it only ever wanted to return zero. i don't know why, but my
		! best guess is it's linker-related, as gdb showed it was being passed the right value
		! and was returning the right one, but it was somehow getting clobbered going back to
		! Fortran. this is a super sketchy workaround, but i can't for the life of me figure
		! out how to fix strlen.
		subroutine cf_strlen(str, out) bind(C, name="cf_strlen")
			use :: iso_c_binding
			type(c_ptr), value, intent(in) :: str
			integer(c_int), intent(out) :: out
		end subroutine cf_strlen
	end interface
end module cFuncs
