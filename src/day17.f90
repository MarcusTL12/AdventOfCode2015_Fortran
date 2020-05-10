module day17_mod
    use vec_int_mod
    use map_int_int_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function load_input(filename) result(caps)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(vec_int) :: caps
        integer :: i, ios
        !
        call caps%new()
        !
        open (1, file=filename)
        !
        do
            read (1, *, iostat=ios) i
            !
            if (ios == 0) then
                call caps%push(i)
            else
                exit
            end if
        end do
        !
        close (1)
    end function
    !
    subroutine part1()
        implicit none
        !
        type(vec_int) caps
        integer :: ans
        !
        caps = load_input("inputfiles/day17/input.txt")
        !
        ans = 0
        call solve(1, 150)
        print *, ans
    contains
        recursive subroutine solve(i, s)
            implicit none
            !
            integer, intent(in) :: i, s
            !
            if (i > size(caps)) return
            !
            if (caps%at(i) < s) then
                call solve(i + 1, s - caps%at(i))
            else if (caps%at(i) == s) then
                ans = ans + 1
            end if
            !
            call solve(i + 1, s)
        end subroutine
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(vec_int) caps
        type(map_int_int) amts
        integer :: i, ans
        integer, pointer :: k, v
        logical stat
        !
        caps = load_input("inputfiles/day17/input.txt")
        !
        call amts%new()
        ans = 0
        call solve(1, 150, 0)
        !
        stat = .false.
        i = 0
        do while (amts%next_kvp(k, v, stat))
            if (i == 0) i = k
            if (k <= i) then
                i = k
                ans = v
            end if
        end do
        !
        print *, ans
    contains
        recursive subroutine solve(i, s, a)
            implicit none
            !
            integer, intent(in) :: i, s, a
            integer :: j, k
            integer, pointer :: p
            !
            if (i > size(caps)) return
            !
            if (caps%at(i) < s) then
                call solve(i + 1, s - caps%at(i), a + 1)
            else if (caps%at(i) == s) then
                j = a + 1
                if (amts%contains_key(j)) then
                    p=>amts%get(j)
                    p = p + 1
                else
                    k = 1
                    call amts%insert(j, k)
                end if
            end if
            !
            call solve(i + 1, s, a)
        end subroutine
    end subroutine
end module
