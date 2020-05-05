module day9_mod
    use astring_mod
    use string_util_mod
    use vec_str_mod
    use map_str_int_mod
    use map_vec_int_int_mod
    use vec_int_mod
    use math_util_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    integer function load_input(filename, dists) result(amt_places)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(map_vec_int_int), intent(inout) :: dists
        type(vec_str) :: places
        type(map_str_int) :: places_set
        type(astring) :: line, buff, buff2
        type(astring), target :: sbuff(5)
        type(vec_str) :: split_buff
        integer :: i, j, d
        type(vec_int) :: vbuff
        !
        call line%new()
        call places%new()
        call dists%new()
        call places_set%new()
        call split_buff%from_buffer(sbuff)
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call split_buff%clear()
            call split_with_delim(line%as_slice(), ' ', split_buff)
            !
            if (places_set%contains_key(split_buff%at(1))) then
                associate (x=>places_set%get(split_buff%at(1)))
                    i = x
                end associate
            else
                buff = split_buff%at(1)
                call buff2%from_borrow(buff%as_slice())
                call places%push(buff)
                i = size(places)
                call places_set%insert(buff2, i)
            end if
            !
            if (places_set%contains_key(split_buff%at(3))) then
                associate (x=>places_set%get(split_buff%at(3)))
                    j = x
                end associate
            else
                buff = split_buff%at(3)
                call buff2%from_borrow(buff%as_slice())
                call places%push(buff)
                j = size(places)
                call places_set%insert(buff2, j)
            end if
            !
            d = parse_int(split_buff%at(5))
            !
            vbuff = (/i, j/)
            call dists%insert(vbuff, d)
            !
            vbuff = (/j, i/)
            call dists%insert(vbuff, d)
            !
            call line%clear()
        end do
        !
        amt_places = size(places)
        !
        close (1)
    end function
    !
    integer function pathdist(dists, path)
        implicit none
        !
        type(map_vec_int_int), intent(in) :: dists
        integer, target, intent(in) :: path(:)
        type(vec_int) :: buf
        integer :: i
        !
        pathdist = 0
        do i = 1, size(path) - 1
            associate (x=>path(i:i + 1))
                call buf%from_borrow(x)
            end associate
            pathdist = pathdist + dists%get(buf)
        end do
    end function
    !
    subroutine part1()
        implicit none
        !
        type(map_vec_int_int) :: dists
        integer :: l, ans, plen
        logical :: b
        integer, allocatable :: path(:), s(:)
        logical, allocatable :: u(:)
        !
        l = load_input("inputfiles/day9/input.txt", dists)
        !
        allocate (path(l), s(l - 1), u(l))
        !
        s = 0
        u = .false.
        b = .true.
        ans = 0
        do while (b)
            b = next_permutation(path, s, u)
            plen = pathdist(dists, path)
            if (ans == 0) then
                ans = plen
            else
                ans = min(ans, plen)
            end if
        end do
        !
        print *, ans
        !
        deallocate (path, s, u)
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(map_vec_int_int) :: dists
        integer :: l, ans, plen
        logical :: b
        integer, allocatable :: path(:), s(:)
        logical, allocatable :: u(:)
        !
        l = load_input("inputfiles/day9/input.txt", dists)
        !
        allocate (path(l), s(l - 1), u(l))
        !
        s = 0
        u = .false.
        b = .true.
        ans = 0
        do while (b)
            b = next_permutation(path, s, u)
            plen = pathdist(dists, path)
            ans = max(ans, plen)
        end do
        !
        print *, ans
        !
        deallocate (path, s, u)
    end subroutine
end module
