module day21_mod
    use astring_mod
    use string_util_mod
    use vec_str_mod
    use vec_int_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    subroutine load_store(filename, wc, wd, ac, aa, rc, rd, ra)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(vec_int), intent(inout) :: wc, wd, ac, aa, rc, rd, ra
        type(astring) :: line
        type(vec_str) :: split_buf
        integer :: state, ibuf
        !
        call wc%with_capacity(5)
        call wd%with_capacity(5)
        call ac%with_capacity(5)
        call aa%with_capacity(5)
        call rc%with_capacity(6)
        call rd%with_capacity(6)
        call ra%with_capacity(6)
        !
        call line%new()
        call split_buf%with_capacity(5)
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call split_buf%clear()
            call split_whitespace(line%as_slice(), split_buf)
            !
            if (size(split_buf) > 0) then
                associate (p=>split_buf%at(1))
                    if (str_eq(p%as_slice(), str_p("Weapons:"))) then
                        state = 1
                    else if (str_eq(p%as_slice(), str_p("Armor:"))) then
                        state = 2
                    else if (str_eq(p%as_slice(), str_p("Rings:"))) then
                        state = 3
                    else
                        select case (state)
                        case (1)
                            ibuf = parse_int(split_buf%at(2))
                            call wc%push(ibuf)
                            ibuf = parse_int(split_buf%at(3))
                            call wd%push(ibuf)
                        case (2)
                            ibuf = parse_int(split_buf%at(2))
                            call ac%push(ibuf)
                            ibuf = parse_int(split_buf%at(4))
                            call aa%push(ibuf)
                        case (3)
                            ibuf = parse_int(split_buf%at(3))
                            call rc%push(ibuf)
                            ibuf = parse_int(split_buf%at(4))
                            call rd%push(ibuf)
                            ibuf = parse_int(split_buf%at(5))
                            call ra%push(ibuf)
                        end select
                    end if
                end associate
            end if
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
        type(vec_int) :: wc, wd, ac, aa, rc, rd, ra
        !
        call load_store("inputfiles/day21/shop.txt", wc, wd, ac, aa, rc, rd, ra)
        !
    end subroutine
    !
    subroutine part2()
        implicit none
        !
    end subroutine
end module
