module day2_mod
    use astring_mod
    use vec_str_mod
    use string_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function get_dims(s, buf)
        implicit none
        !
        character, intent(in) :: s(:)
        type(vec_str), intent(inout) :: buf
        integer :: i, get_dims(3)
        !
        call buf%clear()
        call split_with_delim(s, 'x', buf)
        !
        do i = 1, 3
            get_dims(i) = parse_int(buf%data(i)%data)
        end do
    end function
    !
    integer function area(s, buf)
        implicit none
        !
        character, intent(in) :: s(:)
        type(vec_str), intent(inout) :: buf
        integer :: dims(3), areas(3), slack
        !
        dims = get_dims(s, buf)
        !
        areas = (/dims(1) * dims(2), dims(1) * dims(3), dims(2) * dims(3)/)
        slack = minval(areas)
        !
        area = 2 * sum(areas) + slack
    end function
    !
    subroutine sort(x)
        implicit none
        !
        integer, intent(inout) :: x(3)
        integer :: order(6) = (/1, 2, 2, 3, 1, 2/)
        integer :: tmp, i
        !
        do i = 1, 5, 2
            if (x(order(i + 1)) < x(order(i))) then
                tmp = x(order(i))
                x(order(i)) = x(order(i + 1))
                x(order(i + 1)) = tmp
            end if
        end do
    end subroutine
    !
    integer function ribbon(s, buf)
        implicit none
        !
        character, intent(in) :: s(:)
        type(vec_str), intent(inout) :: buf
        integer :: dims(3)
        !
        dims = get_dims(s, buf)
        !
        call sort(dims)
        !
        ribbon = 2 * (dims(1) + dims(2)) + product(dims)
    end function
    !
    subroutine part1()
        implicit none
        !
        type(astring) :: buf
        type(vec_str) :: splitbuf
        integer :: ans
        !
        call buf%new()
        call splitbuf%with_capacity(3)
        !
        open (1, file="inputfiles/day2/input.txt")
        !
        ans = 0
        do while (readline(1, buf))
            ans = ans + area(buf%as_slice(), splitbuf)
            call buf%clear()
        end do
        !
        call show(ans)
        print *
        !
        close (1)
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(astring) :: buf
        type(vec_str) :: splitbuf
        integer :: ans
        !
        call buf%new()
        call splitbuf%with_capacity(3)
        !
        open (1, file="inputfiles/day2/input.txt")
        !
        ans = 0
        do while (readline(1, buf))
            ans = ans + ribbon(buf%as_slice(), splitbuf)
            call buf%clear()
        end do
        !
        call show(ans)
        print *
        !
        close (1)
    end subroutine
end module
