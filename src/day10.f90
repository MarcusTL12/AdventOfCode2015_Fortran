module day10_mod
    use vec_int8_mod
    use string_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    subroutine lookandsay(a, b)
        implicit none
        !
        integer(1), intent(in) :: a(:)
        type(vec_int8), intent(inout) :: b
        integer(1) :: cur, amt
        integer :: i, j
        !
        call b%clear()
        !
        cur = 0
        amt = 0
        do i = 1, size(a)
            if (a(i) /= cur) then
                if (amt > 0) then
                    call b%push(amt)
                    call b%push(cur)
                end if
                cur = a(i)
                amt = 1
            else
                amt = amt + int(1, 1)
            end if
        end do
        if (amt > 0) then
            call b%push(amt)
            call b%push(cur)
        end if
    end subroutine
    !
    function convert_to_int8(a) result(b)
        implicit none
        !
        character, intent(in) :: a(:)
        type(vec_int8) :: b
        integer(1) :: tmp
        integer :: i, tmp2
        !
        call b%with_capacity(size(a))
        do i = 1, size(a)
            tmp2 = parse_int(a(i))
            tmp = int(tmp2, 1)
            call b%push(tmp)
        end do
    end function
    !
    subroutine part1()
        implicit none
        !
        type(vec_int8), target :: a, b
        type(vec_int8), pointer :: p, q, r
        integer :: i
        !
        a = convert_to_int8(str_p("1321131112"))
        call b%new()
        !
        p => a
        q => b
        !
        do i = 1, 40
            call lookandsay(p%as_slice(), q)
            r => p
            p => q
            q => r
        end do
        !
        print *, size(p)
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(vec_int8), target :: a, b
        type(vec_int8), pointer :: p, q, r
        integer :: i
        !
        a = convert_to_int8(str_p("1321131112"))
        call b%new()
        !
        p => a
        q => b
        !
        do i = 1, 50
            call lookandsay(p%as_slice(), q)
            r => p
            p => q
            q => r
        end do
        !
        print *, size(p)
    end subroutine
end module
