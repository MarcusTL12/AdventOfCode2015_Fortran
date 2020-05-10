module day16_mod
    use astring_mod
    use map_str_int_mod
    use string_util_mod
    use vec_str_mod
    use vec_vec_str_mod
    use vec_int_mod
    use vec_vec_int_mod
    implicit none
    !
    private
    public :: part1, part2
    !
    type pair
        type(astring) :: thing
        integer :: amt
    end type
contains
    function load_mfcsam(filename) result(mfcsam)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(map_str_int) :: mfcsam
        type(astring) :: line, k
        type(vec_str) :: split_buf
        integer :: v
        !
        call line%new()
        call mfcsam%new()
        call split_buf%new()
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call split_buf%clear()
            call split_with_delim(line%as_slice(), ' ', split_buf)
            !
            k = split_buf%at(1)
            call k%truncate(size(k) - 1)
            !
            v = parse_int(split_buf%at(2))
            !
            call mfcsam%insert(k, v)
            !
            call line%clear()
        end do
        !
        close (1)
    end function
    !
    subroutine load_input(filename, things, amts)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(vec_vec_str), intent(inout) :: things
        type(vec_vec_int), intent(inout) :: amts
        type(astring) :: line, s1, s2
        type(vec_str) :: split_buf, thingbuf
        type(vec_int) :: amtbuf
        integer :: i, n
        !
        call line%new()
        call split_buf%new()
        call things%with_capacity(500)
        call amts%with_capacity(500)
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call split_buf%clear()
            call split_with_delim(line%as_slice(), ' ', split_buf)
            !
            call thingbuf%with_capacity(3)
            call amtbuf%with_capacity(3)
            !
            do i = 3, 7, 2
                associate (p=>split_buf%at(i))
                    call s1%from_borrow(p%as_slice())
                    call s1%truncate(size(s1) - 1)
                    s2 = s1
                    call thingbuf%push(s2)
                end associate
                !
                associate (p=>split_buf%at(i + 1))
                    if (p%at(size(p)) == ',') then
                        associate (x=>p%as_slice())
                            n = parse_int(x(1:size(p) - 1))
                        end associate
                    else
                        n = parse_int(p)
                    end if
                    call amtbuf%push(n)
                end associate
            end do
            !
            call things%push(thingbuf)
            call amts%push(amtbuf)
            !
            call line%clear()
        end do
        !
        close (1)
    end subroutine
    !
    subroutine part1()
        implicit none
        !
        type(map_str_int) :: mfcsam
        type(vec_vec_str) :: things
        type(vec_vec_int) :: amts
        integer :: i, j, ans
        logical :: b
        !
        mfcsam = load_mfcsam("inputfiles/day16/mfcsam.txt")
        call load_input("inputfiles/day16/input.txt", things, amts)
        !
        ans = 0
        !
        do i = 1, size(things)
            b = .true.
            associate (p=>things%at(i))
                associate (q=>amts%at(i))
                    do j = 1, size(p)
                        if (q%at(j) /= mfcsam%get(p%at(j))) b = .false.
                    end do
                end associate
            end associate
            if (b) then
                ans = i
                exit
            end if
        end do
        !
        print *, ans
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(map_str_int) :: mfcsam
        type(vec_vec_str) :: things
        type(vec_vec_int) :: amts
        integer :: i, j, ans
        logical :: b
        !
        mfcsam = load_mfcsam("inputfiles/day16/mfcsam.txt")
        call load_input("inputfiles/day16/input.txt", things, amts)
        !
        ans = 0
        !
        do i = 1, size(things)
            b = .true.
            associate (p=>things%at(i))
                associate (q=>amts%at(i))
                    do j = 1, size(p)
                        associate (r=>p%at(j))
                            if (str_eq(r%as_slice(), str_p("cats")) .or. &
                                str_eq(r%as_slice(), str_p("trees"))) then
                                if (q%at(j) <= mfcsam%get(p%at(j))) b = .false.
                            else if (str_eq(r%as_slice(), &
                                            str_p("pomeranians")) .or. &
                                   str_eq(r%as_slice(), str_p("goldfish"))) then
                                if (q%at(j) >= mfcsam%get(p%at(j))) b = .false.
                            else
                                if (q%at(j) /= mfcsam%get(p%at(j))) b = .false.
                            end if
                        end associate
                    end do
                end associate
            end associate
            if (b) then
                ans = i
                exit
            end if
        end do
        !
        print *, ans
    end subroutine
end module
