module day18_mod
    use astring_mod
    use vec_bool_mod
    use string_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function load_input(filename) result(board)
        implicit none
        !
        character(len=*), intent(in) :: filename
        logical, allocatable :: board(:, :)
        type(astring) :: line
        type(vec_bool) :: buf
        integer :: i, w, h
        logical :: b
        !
        call buf%new()
        call line%new()
        !
        open (1, file=filename)
        !
        h = 0
        do while (readline(1, line))
            w = size(line)
            h = h + 1
            do i = 1, w
                b = line%at(i) == '#'
                call buf%push(b)
            end do
            !
            call line%clear()
        end do
        !
        close (1)
        !
        allocate (board(w, h))
        board = reshape(buf%as_slice(), [w, h])
    end function
    !
    subroutine render_board(board)
        implicit none
        !
        logical :: board(:, :)
        integer :: i, j
        character :: c
        !
        do i = 1, size(board, 2)
            do j = 1, size(board, 1)
                if (board(j, i)) then
                    c = '#'
                else
                    c = '.'
                end if
                call show(c)
            end do
            print *
        end do
    end subroutine
    !
    subroutine next_lights(from, to)
        implicit none
        !
        logical, intent(in) :: from(:, :)
        logical, intent(out) :: to(:, :)
        integer :: i, j, amt
        !
        do i = 1, size(from, 1)
            do j = 1, size(from, 2)
                amt = count_neighbours((/i, j/))
                !
                if (from(i, j)) then
                    to(i, j) = amt == 2 .or. amt == 3
                else
                    to(i, j) = amt == 3
                end if
            end do
        end do
    contains
        integer function count_neighbours(pos) result(amt)
            implicit none
            !
            integer, intent(in) :: pos(2)
            integer, parameter :: dirs(2, 8) = reshape((/ &
                                                      1, 0, 1, 1, 0, 1, -1, 1, &
                                                   -1, 0, -1, -1, 0, -1, 1, -1 &
                                                       /), [2, 8])
            integer :: i
            !
            amt = 0
            do i = 1, 8
                if (alive(pos + dirs(:, i))) amt = amt + 1
            end do
        end function
        !
        logical function alive(pos)
            implicit none
            !
            integer, intent(in) :: pos(2)
            !
            if (pos(1) >= 1 .and. pos(1) <= size(from, 1) .and. &
                pos(2) >= 1 .and. pos(2) <= size(from, 2)) then
                alive = from(pos(1), pos(2))
            else
                alive = .false.
            end if
        end function
    end subroutine
    !
    subroutine part1()
        implicit none
        !
        logical, allocatable, target :: board(:, :), buffer(:, :)
        logical, pointer :: p(:, :), q(:, :), t(:, :)
        integer :: i, j, ans
        !
        board = load_input("inputfiles/day18/input.txt")
        allocate (buffer(size(board, 1), size(board, 2)))
        p => board
        q => buffer
        !
        do i = 1, 100
            call next_lights(p, q)
            t => p
            p => q
            q => t
        end do
        !
        ans = 0
        do i = 1, size(p, 1)
            do j = 1, size(p, 2)
                if (p(i, j)) ans = ans + 1
            end do
        end do
        !
        print *, ans
        !
        deallocate (board, buffer)
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        logical, allocatable, target :: board(:, :), buffer(:, :)
        logical, pointer :: p(:, :), q(:, :), t(:, :)
        integer :: i, j, ans
        !
        board = load_input("inputfiles/day18/input.txt")
        allocate (buffer(size(board, 1), size(board, 2)))
        p => board
        q => buffer
        !
        do i = 1, 100
            call next_lights(p, q)
            q(1, 1) = .true.
            q(1, size(q, 2)) = .true.
            q(size(q, 1), 1) = .true.
            q(size(q, 1), size(q, 2)) = .true.
            t => p
            p => q
            q => t
        end do
        !
        ans = 0
        do i = 1, size(p, 1)
            do j = 1, size(p, 2)
                if (p(i, j)) ans = ans + 1
            end do
        end do
        !
        print *, ans
        !
        deallocate (board, buffer)
    end subroutine
end module
