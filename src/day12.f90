module day12_mod
    use astring_mod
    use astring_show_mod
    use string_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function load_input(filename) result(text)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(astring) :: text
        !
        call text%new()
        open (1, file=filename)
        do while (readline(1, text))
        end do
        close (1)
    end function
    !
    recursive subroutine count_numbers(text, i, s)
        implicit none
        !
        character, intent(in) :: text(:)
        integer, intent(inout) :: i, s
        integer :: j
        !
        do while (text(i) == ' ')
            i = i + 1
        end do
        !
        if (str_eq(text(i:i + 3), str_p("null"))) then
            i = i + 4
        else if (str_eq(text(i:i + 3), str_p("true"))) then
            i = i + 4
        else if (str_eq(text(i:i + 4), str_p("false"))) then
            i = i + 5
        else if (is_numeric(text(i)) .or. text(i) == '-') then
            j = i
            do while (text(j + 1) /= ' ' .and. text(j + 1) /= ',' .and. &
                      text(j + 1) /= ']' .and. text(j + 1) /= '}')
                j = j + 1
            end do
            !
            if (is_numeric(text(i:j))) then
                s = s + parse_int(text(i:j))
            end if
            i = j + 1
        else if (text(i) == '"') then
            j = i
            do while (text(j + 1) /= '"')
                j = j + 1
            end do
            i = j + 2
        else if (text(i) == '[') then
            i = i + 1
            call count_array()
        else if (text(i) == '{') then
            i = i + 1
            call count_map()
        end if
    contains
        subroutine count_array()
            implicit none
            !
            logical :: done
            !
            done = .false.
            !
            do while (.not. done)
                !
                do while (text(i) == ' ' .or. text(i) == ',')
                    i = i + 1
                end do
                !
                if (text(i) == ']') then
                    i = i + 1
                    done = .true.
                else
                    call count_numbers(text, i, s)
                end if
            end do
        end subroutine
        !
        subroutine count_map()
            implicit none
            !
            integer :: j
            logical :: done
            !
            done = .false.
            !
            do while (.not. done)
                !
                do while (text(i) == ' ' .or. text(i) == ',')
                    i = i + 1
                end do
                !
                if (text(i) == '}') then
                    i = i + 1
                    done = .true.
                else
                    j = i + 1
                    do while (text(j + 1) /= '"')
                        j = j + 1
                    end do
                    !
                    ! k_buff = text(i + 1:j)
                    !
                    i = j + 2
                    !
                    do while (text(i) /= ':')
                        i = i + 1
                    end do
                    !
                    i = i + 1
                    !
                    call count_numbers(text, i, s)
                end if
            end do
        end subroutine
    end subroutine
    !
    recursive subroutine count_numbers_red(text, i, s, red)
        implicit none
        !
        character, intent(in) :: text(:)
        integer, intent(inout) :: i, s
        logical, intent(out) :: red
        logical :: b
        integer :: j, s2
        !
        red = .false.
        !
        do while (text(i) == ' ')
            i = i + 1
        end do
        !
        if (str_eq(text(i:i + 3), str_p("null"))) then
            i = i + 4
        else if (str_eq(text(i:i + 3), str_p("true"))) then
            i = i + 4
        else if (str_eq(text(i:i + 4), str_p("false"))) then
            i = i + 5
        else if (is_numeric(text(i)) .or. text(i) == '-') then
            j = i
            do while (text(j + 1) /= ' ' .and. text(j + 1) /= ',' .and. &
                      text(j + 1) /= ']' .and. text(j + 1) /= '}')
                j = j + 1
            end do
            !
            if (is_numeric(text(i:j))) then
                s = s + parse_int(text(i:j))
            end if
            i = j + 1
        else if (text(i) == '"') then
            j = i
            do while (text(j + 1) /= '"')
                j = j + 1
            end do
            red = str_eq(text(i + 1:j), str_p("red"))
            i = j + 2
        else if (text(i) == '[') then
            i = i + 1
            call count_array()
        else if (text(i) == '{') then
            i = i + 1
            s2 = count_map(b)
            if (.not. b) s = s + s2
        end if
    contains
        subroutine count_array()
            implicit none
            !
            logical :: done, b
            !
            done = .false.
            !
            do while (.not. done)
                !
                do while (text(i) == ' ' .or. text(i) == ',')
                    i = i + 1
                end do
                !
                if (text(i) == ']') then
                    i = i + 1
                    done = .true.
                else
                    call count_numbers_red(text, i, s, b)
                end if
            end do
        end subroutine
        !
        function count_map(red) result(s)
            implicit none
            !
            logical, intent(out) :: red
            integer :: j, s
            logical :: done, rb
            !
            s = 0
            !
            done = .false.
            red = .false.
            !
            do while (.not. done)
                !
                do while (text(i) == ' ' .or. text(i) == ',')
                    i = i + 1
                end do
                !
                if (text(i) == '}') then
                    i = i + 1
                    done = .true.
                else
                    j = i + 1
                    do while (text(j + 1) /= '"')
                        j = j + 1
                    end do
                    !
                    i = j + 2
                    !
                    do while (text(i) /= ':')
                        i = i + 1
                    end do
                    !
                    i = i + 1
                    !
                    rb = .false.
                    call count_numbers_red(text, i, s, rb)
                    if (rb) red = .true.
                end if
            end do
        end function
    end subroutine
    !
    subroutine part1()
        implicit none
        !
        type(astring) :: inp
        integer :: i, s
        !
        inp = load_input("inputfiles/day12/input.txt")
        i = 1
        s = 0
        call count_numbers(inp%as_slice(), i, s)
        !
        print *, s
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(astring) :: inp
        integer :: i, s
        logical :: b
        !
        inp = load_input("inputfiles/day12/input.txt")
        i = 1
        s = 0
        call count_numbers_red(inp%as_slice(), i, s, b)
        !
        print *, s
    end subroutine
end module
