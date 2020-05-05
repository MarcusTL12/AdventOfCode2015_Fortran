module day8_mod
    use astring_mod
    use astring_show_mod
    use string_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    pure integer function actual_len(s) result(n)
        implicit none
        !
        character, intent(in) :: s(:)
        integer :: i, m
        !
        m = 0
        n = 0
        do i = 1, size(s)
            if (m == 0) then
                if (s(i) == '\') then
                    m = -1
                end if
                n = n + 1
            else if (m == -1) then
                if (s(i) == 'x') then
                    m = 2
                else
                    m = 0
                end if
            else
                m = m - 1
            end if
        end do
    end function
    !
    pure integer function encoded_len(s) result(n)
        implicit none
        !
        character, intent(in) :: s(:)
        integer :: i
        !
        n = 0
        do i = 1, size(s)
            if (s(i) == '\' .or. s(i) == '"') then
                n = n + 2
            else
                n = n + 1
            end if
        end do
    end function
    !
    subroutine part1()
        implicit none
        !
        type(astring) :: line
        integer :: ans
        !
        call line%new()
        !
        open (1, file="inputfiles/day8/input.txt")
        !
        ans = 0
        do while (readline(1, line))
            ans = ans + (size(line) + 2 - actual_len(line%as_slice()))
            call line%clear()
        end do
        !
        print *, ans
        !
        close (1)
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(astring) :: line
        integer :: ans
        !
        call line%new()
        !
        open (1, file="inputfiles/day8/input.txt")
        !
        ans = 0
        do while (readline(1, line))
            ans = ans + (encoded_len(line%as_slice()) + 2 - size(line))
            call line%clear()
        end do
        !
        print *, ans
        !
        close (1)
    end subroutine
end module
