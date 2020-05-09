module day14_mod
    use math_util_mod
    use vec_int_mod
    use vec_vec_int_mod
    use astring_mod
    use string_util_mod
    use vec_str_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function loadinput(filename) result(deer)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(vec_vec_int) :: deer
        type(astring) :: line
        type(vec_str) :: split_buf
        type(vec_int) :: rain_buf
        !
        call line%new()
        call split_buf%new()
        call deer%new()
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call split_buf%clear()
            call split_with_delim(line%as_slice(), ' ', split_buf)
            !
            rain_buf = (/parse_int(split_buf%at(4)), &
                         parse_int(split_buf%at(7)), &
                         parse_int(split_buf%at(14))/)
            !
            call deer%push(rain_buf)
            !
            call line%clear()
        end do
        !
        close (1)
    end function
    !
    integer function raindist(t, r)
        implicit none
        !
        integer, intent(in) :: t, r(3)
        !
        raindist = (t / (r(2) + r(3))) * r(1) * r(2) + &
                   clamp(modulo(t, r(2) + r(3)), 0, r(2)) * r(1)
    end function
    !
    subroutine part1()
        implicit none
        !
        type(vec_vec_int) :: deer
        integer :: i, ans
        !
        deer = loadinput("inputfiles/day14/input.txt")
        !
        ans = 0
        do i = 1, size(deer)
            associate (r => deer%at(i))
                ans = max(ans, raindist(2503, r%as_slice()))
            end associate
        end do
        !
        print *, ans
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(vec_vec_int) :: deer
        integer :: i, j, m, l
        integer, allocatable :: scores(:)
        !
        deer = loadinput("inputfiles/day14/input.txt")
        !
        allocate (scores(size(deer)))
        scores = 0
        !
        do i = 1, 2503
            m = 0
            do j = 1, size(deer)
                associate (r => deer%at(j))
                    l = raindist(i, r%as_slice())
                end associate
                !
                if (l > m) then
                    m = l
                end if
            end do
            !
            do j = 1, size(deer)
                associate (r => deer%at(j))
                    l = raindist(i, r%as_slice())
                end associate
                !
                if (l == m) then
                    scores(j) = scores(j) + 1
                end if
            end do
        end do
        !
        print *, maxval(scores)
        !
        deallocate (scores)
    end subroutine
end module
