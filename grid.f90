module grid
	implicit none

	integer :: gridSize
	integer, dimension(:,:), allocatable :: gridData
	logical :: gridAllocated = .false.
	real :: robotX, robotY, robotTheta

	contains
		subroutine resizeGrid(sz)
			integer, intent(in) :: sz
			integer :: i, j
			if (gridSize == sz) then
				write(*,"(A)") "Grid resize requested, but size is not changed. Ignoring request."
				return
			end if
			write(*,"(A,i0,A,i0,A)") "Resizing grid to ", sz, " by ", sz, "."
			if (gridAllocated) then
				! deallocate the old grid
				deallocate(gridData)
			end if
			gridAllocated = .true.
			gridSize = sz
			allocate(gridData(gridSize, gridSize))
			do i = 1, sz
				do j = 1, sz
					gridData(i, j) = 0
				end do
			end do
		end subroutine resizeGrid

		subroutine updateCell(x, y, v)
			integer, intent(in) :: x, y, v
			write(*,"(A,2i3,A,i0)") "Updating cell (", x, y, ") with value ", v
			if (gridAllocated .and. x >= 0 .and. x < gridSize .and. y >= 0 .and. y < gridSize) then
				! the grid is zero-indexed, but arrays in Fortran are 1-indexed
				gridData(x + 1, y + 1) = v
			end if
		end subroutine updateCell
end module grid
