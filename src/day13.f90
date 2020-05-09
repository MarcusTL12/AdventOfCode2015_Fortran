module day13_mod
    use map_vec_int_int_mod
    use astring_mod
    use string_util_mod
    use vec_str_mod
    use map_str_int_mod
    use vec_int_mod
    use math_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function load_input(filename, pref) result(amt_people)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(map_vec_int_int), intent(inout) :: pref
        integer :: amt_people, a, b, n
        type(astring) :: line, namebuf, namebuf2
        type(vec_str) :: splitbuf
        type(map_str_int) :: people
        type(vec_int) :: vbuf
        !
        call pref%new()
        call line%new()
        call splitbuf%with_capacity(11)
        call people%new()
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call splitbuf%clear()
            call split_with_delim(line%as_slice(), ' ', splitbuf)
            !
            associate (namep=>splitbuf%at(1))
                if (.not. people%contains_key(namep)) then
                    namebuf = namep
                    a = size(people) + 1
                    call people%insert(namebuf, a)
                else
                    associate (x=>people%get(namep))
                        a = x
                    end associate
                end if
            end associate
            !
            associate (namep=>splitbuf%at(size(splitbuf)))
                call namebuf2%from_borrow(namep%as_slice())
                call namebuf2%truncate(size(namebuf2) - 1)
                !
                if (.not. people%contains_key(namebuf2)) then
                    namebuf = namebuf2
                    b = size(people) + 1
                    call people%insert(namebuf, b)
                else
                    associate (x=>people%get(namebuf2))
                        b = x
                    end associate
                end if
            end associate
            !
            n = parse_int(splitbuf%at(4))
            !
            associate (x => splitbuf%at(3))
                if (str_eq(x%as_slice(), str_p("lose"))) n = -n
            end associate
            !
            vbuf = (/a, b/)
            call pref%insert(vbuf, n)
            !
            call line%clear()
        end do
        !
        close (1)
        !
        amt_people = size(people)
    end function
    !
    integer function happiness(pref, order)
        implicit none
        !
        type(map_vec_int_int), intent(in) :: pref
        integer, intent(in) :: order(:)
        integer :: i
        integer, target :: buf(2)
        type(vec_int) :: vbuf
        !
        call vbuf%from_buffer(buf, 2)
        !
        happiness = 0
        !
        do i = 1, size(order) - 1
            buf = (/order(i), order(i + 1)/)
            happiness = happiness + pref%get(vbuf)
            !
            buf = (/order(i + 1), order(i)/)
            happiness = happiness + pref%get(vbuf)
        end do
        !
        buf = (/order(1), order(size(order))/)
        happiness = happiness + pref%get(vbuf)
        !
        buf = (/order(size(order)), order(1)/)
        happiness = happiness + pref%get(vbuf)
    end function
    !
    subroutine part1()
        implicit none
        !
        type(map_vec_int_int) :: pref
        integer :: amt_people, ans
        logical :: b
        integer, allocatable :: order(:), s(:)
        logical, allocatable :: u(:)
        !
        amt_people = load_input("inputfiles/day13/input.txt", pref)
        !
        allocate (order(amt_people), s(amt_people - 2), u(amt_people - 1))
        !
        order(size(order)) = size(order)
        s = 0
        u = .false.
        b = .true.
        ans = 0
        !
        do while (b)
            b = next_permutation(order(1:size(order) - 1), s, u)
            !
            ans = max(ans, happiness(pref, order))
        end do
        !
        print *, ans
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(map_vec_int_int) :: pref
        integer :: i, amt_people, ans
        logical :: b
        integer, allocatable :: order(:), s(:)
        logical, allocatable :: u(:)
        type(vec_int) :: vbuf
        !
        amt_people = load_input("inputfiles/day13/input.txt", pref)
        !
        ans = 0
        !
        do i = 1, amt_people
            vbuf = (/amt_people + 1, i/)
            call pref%insert(vbuf, ans)
            vbuf = (/i, amt_people + 1/)
            call pref%insert(vbuf, ans)
        end do
        !
        allocate (order(amt_people + 1), s(amt_people - 1), u(amt_people))
        !
        order(size(order)) = size(order)
        s = 0
        u = .false.
        b = .true.
        !
        do while (b)
            b = next_permutation(order(1:size(order) - 1), s, u)
            !
            ans = max(ans, happiness(pref, order))
        end do
        !
        print *, ans
    end subroutine
end module
