module day7_mod
    use astring_mod
    use string_util_mod
    use vec_str_mod
    use map_str_vec_str_mod
    use map_str_int_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function make_map(lines) result(m)
        implicit none
        !
        type(astring), intent(in) :: lines(:)
        type(map_str_vec_str) :: m
        type(astring) :: k, buf
        type(vec_str) :: v, split_buf
        integer :: i, j
        !
        call m%with_capacity(size(lines))
        !
        call split_buf%new()
        !
        do i = 1, size(lines)
            call split_buf%clear()
            call split_with_delim(lines(i)%as_slice(), ' ', split_buf)
            !
            associate (p=>split_buf%at(size(split_buf)))
                call k%from_borrow(p%as_slice())
            end associate
            !
            associate (p=>split_buf%as_slice())
                call v%with_capacity(size(split_buf) - 2)
                do j = 1, size(split_buf) - 2
                    associate (q=>split_buf%at(j))
                        call buf%from_borrow(q%as_slice())
                        call v%push(buf)
                    end associate
                end do
            end associate
            !
            call m%insert(k, v)
        end do
    end function
    !
    recursive integer function eval_wire(prog, memo, wire) result(val)
        implicit none
        !
        type(map_str_vec_str), intent(in) :: prog
        type(map_str_int), intent(inout)  :: memo
        type(astring), intent(inout)      :: wire
        !
        if (memo%contains_key(wire)) then
            associate (x=>memo%get(wire))
                val = x
            end associate
        else
            associate (x=>prog%get(wire))
                select case (size(x))
                case (1)
                    val = eval_str(x%at(1))
                case (2)
                    val = int(not(int(eval_str(x%at(2)), 2)), 4)
                case (3)
                    associate (y=>x%at(2))
                        if (str_eq(y%as_slice(), str_p("AND"))) then
                            val = int(iand(int(eval_str(x%at(1)), 2), &
                                           int(eval_str(x%at(3)), 2)), 4)
                        else if (str_eq(y%as_slice(), str_p("OR"))) then
                            val = int(ior(int(eval_str(x%at(1)), 2), &
                                          int(eval_str(x%at(3)), 2)), 4)
                        else if (str_eq(y%as_slice(), str_p("LSHIFT"))) then
                            val = int(ishft(int(eval_str(x%at(1)), 2), &
                                            eval_str(x%at(3))), 4)
                        else if (str_eq(y%as_slice(), str_p("RSHIFT"))) then
                            val = int(ishft(int(eval_str(x%at(1)), 2), &
                                            -eval_str(x%at(3))), 4)
                        end if
                    end associate
                end select
            end associate
            !
            call memo%insert(wire, val)
        end if
    contains
        integer function eval_str(s)
            implicit none
            !
            type(astring), intent(inout) :: s
            !
            if (is_numeric(s%as_slice())) then
                eval_str = parse_int(s)
            else
                eval_str = eval_wire(prog, memo, s)
            end if
        end function
    end function
    !
    subroutine part1()
        implicit none
        !
        type(vec_str) :: lines
        type(map_str_vec_str) :: prog
        type(map_str_int) :: memo
        type(astring) :: key
        integer :: ans
        !
        call lines%new()
        !
        call file_to_lines("inputfiles/day7/input.txt", lines)
        !
        prog = make_map(lines%as_slice())
        !
        call memo%new()
        !
        call key%from_borrow(str_p('a'))
        ans = eval_wire(prog, memo, key)
        !
        print *, iand(ans, 65535)
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(vec_str) :: lines
        type(map_str_vec_str) :: prog
        type(map_str_int) :: memo
        type(astring) :: key
        integer :: ans
        !
        call lines%new()
        !
        call file_to_lines("inputfiles/day7/input.txt", lines)
        !
        prog = make_map(lines%as_slice())
        !
        call memo%new()
        !
        call key%from_borrow(str_p('a'))
        ans = eval_wire(prog, memo, key)
        !
        call memo%clear()
        call key%from_borrow(str_p('b'))
        call memo%insert(key, ans)
        !
        call key%from_borrow(str_p('a'))
        ans = eval_wire(prog, memo, key)
        !
        print *, iand(ans, 65535)
    end subroutine
end module
