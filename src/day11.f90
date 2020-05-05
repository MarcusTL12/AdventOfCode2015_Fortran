module day11_mod
    use vec_int_mod
    use string_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    subroutine increment(pass)
        implicit none
        !
        integer, intent(inout) :: pass(:)
        integer :: i
        !
        i = 1
        do
            pass(i) = pass(i) + 1
            pass(i) = modulo(pass(i), 26)
            if (pass(i) /= 0) exit
            i = i + 1
        end do
    end subroutine
    !
    logical function is_valid(pass)
        implicit none
        !
        integer, intent(inout) :: pass(:)
        logical :: a, b, c
        !
        a = r1()
        b = r2()
        c = r3()
        !
        print *, a, b, c
        !
        is_valid = a .and. b .and. c
    contains
        logical function r1()
            implicit none
            !
            integer :: i
            !
            do i = 1, size(pass) - 2
                if (pass(i) == pass(i + 1) + 1 .and. &
                    pass(i + 1) == pass(i + 2) + 1) then
                        r1 = .true.
                        return
                end if
            end do
            !
            r1 = .false.
        end function
        !
        logical function r2()
            implicit none
            !
            integer :: i
            !
            do i = 1, size(pass)
                if (pass(i) == 8 .or. pass(i) == 14 .or. pass(i) == 11) then
                    r2 = .false.
                    return
                end if
            end do
            r2 = .true.
        end function
        !
        logical function r3()
            implicit none
            !
            integer :: i
            logical :: a, b
            !
            a = .false.
            b = .false.
            do i = 1, size(pass) - 1
                if (a) cycle
                if (pass(i) == pass(i + 1)) then
                    if (b) then
                        r3 = .true.
                        return
                    else
                        b = .true.
                    end if
                end if
            end do
            r3 = .false.
        end function
    end function
    !
    subroutine part1()
        implicit none
        !
        character, pointer :: a(:)
        integer, target :: b(8)
        type(vec_int) :: p
        integer :: i, j
        !
        a => str_p("hijklmmn")
        call p%from_buffer(b)
        !
        do i = size(a), 1, -1
            j = ichar(a(i)) - 97
            call p%push(j)
        end do
        !
        call show(p)
        print *
    end subroutine
    !
    subroutine part2()
        implicit none
        !
    end subroutine
end module
