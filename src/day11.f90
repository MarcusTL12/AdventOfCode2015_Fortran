module day11_mod
    use vec_int_mod
    use astring_mod
    use astring_show_mod
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
        !
        is_valid = r1() .and. r2() .and. r3()
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
                if (a) then
                    a = .false.
                    cycle
                end if
                if (pass(i) == pass(i + 1)) then
                    if (b) then
                        r3 = .true.
                        return
                    else
                        b = .true.
                    end if
                    a = .true.
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
        character, target :: c(8)
        character :: d
        type(astring) :: q
        integer, target :: b(8)
        type(vec_int) :: p
        integer :: i, j
        !
        a => str_p("hepxcrrq")
        call p%from_buffer(b)
        call q%from_buffer(c)
        !
        do i = size(a), 1, -1
            j = ichar(a(i)) - 97
            call p%push(j)
        end do
        !
        do while (.not. is_valid(b))
            call increment(b)
        end do
        !
        do i = size(p), 1, -1
            d = char(p%at(i) + 97)
            call q%push(d)
        end do
        !
        call show(q)
        print *
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        character, pointer :: a(:)
        character, target :: c(8)
        character :: d
        type(astring) :: q
        integer, target :: b(8)
        type(vec_int) :: p
        integer :: i, j
        !
        a => str_p("hepxcrrq")
        call p%from_buffer(b)
        call q%from_buffer(c)
        !
        do i = size(a), 1, -1
            j = ichar(a(i)) - 97
            call p%push(j)
        end do
        !
        do while (.not. is_valid(b))
            call increment(b)
        end do
        call increment(b)
        do while (.not. is_valid(b))
            call increment(b)
        end do
        !
        do i = size(p), 1, -1
            d = char(p%at(i) + 97)
            call q%push(d)
        end do
        !
        call show(q)
        print *
    end subroutine
end module
