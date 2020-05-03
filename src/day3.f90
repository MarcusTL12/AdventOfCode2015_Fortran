module day3_mod
    use astring_mod
    use string_util_mod
    use vec_int_mod
    use map_vec_int_int_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    pure function get_dir(c) result(d)
        implicit none
        !
        character, intent(in) :: c
        integer :: d(2)
        !
        select case (c)
        case ('^')
            d = (/0, 1/)
        case ('v')
            d = (/0, -1/)
        case ('>')
            d = (/1, 0/)
        case ('<')
            d = (/-1, 0/)
        end select
    end function
    !
    subroutine part1()
        implicit none
        !
        type(astring) :: inp
        logical :: status
        integer :: i, pos(2), a = 1
        type(map_vec_int_int) :: visited
        type(vec_int) :: buf
        !
        call inp%new()
        open (1, file="inputfiles/day3/input.txt")
        status = readline(1, inp)
        !
        pos = (/0, 0/)
        call visited%new()
        buf = pos
        call visited%insert(buf, a)
        !
        do i = 1, size(inp)
            pos = pos + get_dir(inp%data(i))
            !
            buf = pos
            call visited%insert(buf, a)
        end do
        !
        print *, size(visited)
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(astring) :: inp
        logical :: status
        integer :: i, a = 1
        integer, target :: santa(2), bot(2)
        integer, pointer :: p1(:), p2(:), tmp(:)
        type(map_vec_int_int) :: visited
        type(vec_int) :: buf
        !
        call inp%new()
        open (1, file="inputfiles/day3/input.txt")
        status = readline(1, inp)
        !
        santa = (/0, 0/)
        bot = (/0, 0/)
        !
        p1 => santa
        p2 => bot
        !
        call visited%new()
        buf = p1
        call visited%insert(buf, a)
        !
        do i = 1, size(inp)
            p1 = p1 + get_dir(inp%data(i))
            !
            buf = p1
            call visited%insert(buf, a)
            !
            tmp => p1
            p1 => p2
            p2 => tmp
        end do
        !
        print *, size(visited)
    end subroutine
end module
