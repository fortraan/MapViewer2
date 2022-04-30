module gfx
	use, intrinsic :: iso_c_binding
	use :: sdl2
	use :: sdl2_ttf
	use :: grid
	implicit none

	integer, parameter :: SCREEN_WIDTH = 500
	integer, parameter :: SCREEN_HEIGHT = 500
	integer, parameter :: CX = nint(SCREEN_WIDTH / 2.0)
	integer, parameter :: CY = nint(SCREEN_HEIGHT / 2.0)

	type(c_ptr) :: window
	type(c_ptr) :: renderer
	type(c_ptr) :: texture
	type(c_ptr) :: font
	type(sdl_surface), pointer :: surface
	type(sdl_event) :: event

	logical :: exitClicked

	public :: gfxInit, gfxDraw, gfxQuit, exitClicked
	private :: SCREEN_WIDTH, SCREEN_HEIGHT, window, renderer, event
	! drawing subroutines
	private :: setDrawColor

	contains
		logical function gfxInit()
			gfxInit = .true.
			if (sdl_init(SDL_INIT_VIDEO) < 0) then
				write(*,*) "SDL initialization failed with error code ", sdl_get_error()
				gfxInit = .false.
				return
			end if
			window = sdl_create_window("MapViewer2" // c_null_char, &
				SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, &
				SCREEN_WIDTH, SCREEN_HEIGHT, &
				SDL_WINDOW_SHOWN)
			if (.not. c_associated(window)) then
				write(*,*) "SDL window creation failed with error code ", sdl_get_error()
				gfxInit = .false.
				return
			end if
			if (ttf_init() < 0) then
				write (*,*) "TTF initialization failed with error code ", sdl_get_error()
				gfxInit = .false.
				return
			end if

			exitClicked = .false.
			renderer = sdl_create_renderer(window, -1, 0)
			font = ttf_open_font("font.ttf" // c_null_char, 25)
		end function gfxInit

		subroutine gfxDraw()
			integer :: rc, x, y
			integer, parameter :: MARGIN = 20
			! clear the screen
			call setDrawColor(0, 0, 0, SDL_ALPHA_OPAQUE)
			rc = sdl_render_clear(renderer)

			if (gridAllocated) then
				block
					integer, parameter :: GRID_SIZE_PX = 2 * (CX - MARGIN)
					type(sdl_rect) :: gridBounds = sdl_rect(MARGIN, MARGIN, GRID_SIZE_PX, GRID_SIZE_PX)
					real :: cellSize

					! draw the grid
					call setDrawColor(0, 255, 0, SDL_ALPHA_OPAQUE)

					rc = sdl_render_draw_rect(renderer, gridBounds)
					cellSize = real(GRID_SIZE_PX) / real(gridSize)
					do x = 1, gridSize - 1
						block
							integer :: linePos
							linePos = MARGIN + nint(real(x) * cellSize)
							rc = sdl_render_draw_line(renderer, linePos,  MARGIN, linePos, SCREEN_HEIGHT - MARGIN)
							rc = sdl_render_draw_line(renderer, MARGIN, linePos, SCREEN_WIDTH - MARGIN, linePos)
						end block
					end do
					
					block
						type(sdl_rect) :: rect
						integer :: screenX, screenY
						!integer, dimension(2,1) :: screenCoords
						!integer, dimension(2,2) :: gridTransform
						!integer, dimension(2,1) :: gridTranslation
						! matrices in Fortran are in column-major order
						! i have no idea why Fortran insists on these being reals. if you try to give it
						! integers it complains, but only for gridTransform.
						!gridTransform = reshape((/ real(cellSize), real(0), real(0), real(-cellSize) /), shape(gridTransform))
						!gridTranslation = reshape((/ MARGIN, SCREEN_HEIGHT - MARGIN /), shape(gridTranslation))
						do x = 0, gridSize - 1
							do y = 0, gridSize - 1
								associate (d => gridData(x + 1, y + 1))
									if (d > 0) then
										! translate grid coords to screen coords
										!screenCoords = reshape((/ x, y /), shape(screenCoords))
										!screenCoords = matmul(GRIDTRANSFORM, screenCoords) + GRIDTRANSLATION
										!write(*,"(i0,1x,i0,1x,i0,1x,i0)") x, y, screenCoords(1,1), screenCoords(2,1)
										screenX = MARGIN + nint(real(x) * cellSize)
										screenY = SCREEN_HEIGHT - MARGIN - nint(real(y) * cellSize)
										rect = sdl_rect(screenX, screenY, cellSize, -cellSize)
										rc = sdl_render_fill_rect(renderer, rect)
									end if
								end associate
							end do
						end do
					end block
				end block
			else
				block
					type(sdl_rect) :: textureRect, renderRect
					integer, parameter :: OFFSET = 4 * MARGIN
					! no data to display
					call setDrawColor(255, 0, 0, SDL_ALPHA_OPAQUE)
					surface => ttf_render_text_solid(font, "NO DATA" // c_null_char, &
						sdl_color(uint8(255), uint8(0), uint8(0), uint8(SDL_ALPHA_OPAQUE)))
					texture = sdl_create_texture_from_surface(renderer, surface)
					renderRect = sdl_rect(CX - surface%w / 2, MARGIN, surface%w, surface%h)
					textureRect = sdl_rect(0, 0, surface%w, surface%h)
					rc = sdl_render_copy(renderer, texture, textureRect, renderRect)
					rc = sdl_render_draw_line(renderer, CX - surface%w / 2, MARGIN + surface%h + 1, &
						CX + surface%w / 2, MARGIN + surface%h + 1)
					if (mod(time(), 2) == 0) then
						rc = sdl_render_draw_line(renderer, OFFSET, OFFSET, &
							SCREEN_WIDTH - OFFSET, SCREEN_HEIGHT - OFFSET)
						rc = sdl_render_draw_line(renderer, SCREEN_WIDTH - OFFSET, OFFSET, &
							OFFSET, SCREEN_HEIGHT - OFFSET)
					end if
				end block
			end if
			call sdl_render_present(renderer)
			if (sdl_poll_event(event) > 0) then
				select case (event%type)
					case (SDL_QUITEVENT)
						exitClicked = .true.
				end select
			end if
			call sdl_delay(10)
		end subroutine gfxDraw

		subroutine setDrawColor(r, g, b, mode)
			integer, intent(in) :: r, g, b, mode
			integer :: rc
			rc = sdl_set_render_draw_color(renderer, uint8(r), uint8(g), uint8(b), uint8(mode))
		end subroutine setDrawColor

		subroutine gfxQuit()
			call sdl_destroy_renderer(window)
			call sdl_destroy_window(window)
			call sdl_quit()
		end subroutine gfxQuit
end module gfx
