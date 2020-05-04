module day6_mod
    use astring_mod
    use string_util_mod
    use math_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function parse_input(s) result(r)
        implicit none
        !
        character, intent(in) :: s(:)
        integer :: r(5), i
        !
        i = 1
        do while (.not. is_numeric(s(i)))
            i = i + 1
        end do
        !
        select case (i)
        case (9)
            r(1) = 1
        case (10)
            r(1) = -1
        case (8)
            r(1) = 2
        end select
        !
        r(2) = get_int() + 1
        i = i + 1
        r(3) = get_int() + 1
        !
        do while (.not. is_numeric(s(i)))
            i = i + 1
        end do
        !
        r(4) = get_int() + 1
        i = i + 1
        r(5) = get_int() + 1
    contains
        integer function get_int()
            implicit none
            !
            integer :: j
            j = i
            do while (j <= size(s) .and. is_numeric(s(j)))
                j = j + 1
            end do
            get_int = parse_int(s(i:j - 1))
            i = j
        end function
    end function
    !
    subroutine part1()
        implicit none
        !
        type(astring) :: line
        integer :: buf(5), i, j, ans
        logical :: map(1000, 1000)
        !
        call line%new()
        !
        open (1, file="inputfiles/day6/input.txt")
        !
        map = .false.
        do while (readline(1, line))
            buf = parse_input(line%as_slice())
            select case (buf(1))
            case (1)
                map(buf(2):buf(4), buf(3):buf(5)) = .true.
            case (-1)
                map(buf(2):buf(4), buf(3):buf(5)) = .false.
            case (2)
                map(buf(2):buf(4), buf(3):buf(5)) = .not. &
                                               map(buf(2):buf(4), buf(3):buf(5))
            end select
            call line%clear()
        end do
        !
        ans = 0
        do i = 1, 1000
            do j = 1, 1000
                if (map(i, j)) ans = ans + 1
            end do
        end do
        !
        print *, ans
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(astring) :: line
        integer :: buf(5), i, j, ans, map(1000, 1000)
        !
        call line%new()
        !
        open (1, file="inputfiles/day6/input.txt")
        !
        map = 0
        do while (readline(1, line))
            buf = parse_input(line%as_slice())
            map(buf(2):buf(4), buf(3):buf(5)) = buf(1) &
                                             + map(buf(2):buf(4), buf(3):buf(5))
            if (buf(1) == -1) then
                do i = buf(2), buf(4)
                    do j = buf(3), buf(5)
                        map(i, j) = max(map(i, j), 0)
                    end do
                end do
            end if
            call line%clear()
        end do
        !
        ans = 0
        do i = 1, 1000
            do j = 1, 1000
                ans = ans + map(i, j)
            end do
        end do
        !
        print *, ans
    end subroutine
end module
