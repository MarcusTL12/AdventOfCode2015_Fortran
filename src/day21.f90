module day21_mod
    use astring_mod
    use string_util_mod
    use vec_str_mod
    use vec_int_mod
    implicit none
    !
    private
    public :: part1, part2
    !
    integer, parameter :: boss(3) = (/103, 9, 2/)
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
    logical function playgame(player, boss) result(won)
        implicit none
        !
        integer, intent(in) :: player(3), boss(3)
        integer, target :: p(3), b(3)
        integer, pointer :: a(:), d(:), tmp(:)
        !
        p = player
        b = boss
        !
        a => p
        d => b
        !
        won = .true.
        !
        do
            if (doturn(a, d)) exit
            tmp => a
            a => d
            d => tmp
            won = .not. won
        end do
    contains
        logical function doturn(attacker, defender) result(died)
            implicit none
            !
            integer, intent(in) :: attacker(3)
            integer, intent(inout) :: defender(3)
            !
            defender(1) = defender(1) - max(1, attacker(2) - defender(3))
            died = defender(1) <= 0
        end function
    end function
    !
    subroutine part1()
        implicit none
        !
        type(vec_int) :: wc, wd, ac, aa, rc, rd, ra
        integer :: player(3), w, a, r1, r2, n, cost, ans
        !
        call load_store("inputfiles/day21/shop.txt", &
                        wc, wd, ac, aa, rc, rd, ra)
        !
        n = 0
        call ac%push(n)
        call aa%push(n)
        !
        ans = 0
        do w = 1, size(wc)
            do a = 1, size(ac)
                do r1 = 0, size(rc)
                    if (r1 == 0) then
                        n = 0
                    else
                        n = r1 + 1
                    end if
                    !
                    do r2 = n, size(rc)
                        player = (/100, wd%at(w), aa%at(a)/)
                        cost = wc%at(w) + ac%at(a)
                        if (r1 /= 0) then
                            player(2) = player(2) + rd%at(r1)
                            player(3) = player(3) + ra%at(r1)
                            cost = cost + rc%at(r1)
                        end if
                        if (r2 /= 0) then
                            player(2) = player(2) + rd%at(r2)
                            player(3) = player(3) + ra%at(r2)
                            cost = cost + rc%at(r2)
                        end if
                        !
                        if (playgame(player, boss)) then
                            if (ans == 0) then
                                ans = cost
                            else
                                ans = min(ans, cost)
                            end if
                        end if
                    end do
                end do
            end do
        end do
        !
        print *, ans
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(vec_int) :: wc, wd, ac, aa, rc, rd, ra
        integer :: player(3), w, a, r1, r2, n, cost, ans
        !
        call load_store("inputfiles/day21/shop.txt", &
                        wc, wd, ac, aa, rc, rd, ra)
        !
        n = 0
        call ac%push(n)
        call aa%push(n)
        !
        ans = 0
        do w = 1, size(wc)
            do a = 1, size(ac)
                do r1 = 0, size(rc)
                    if (r1 == 0) then
                        n = 0
                    else
                        n = r1 + 1
                    end if
                    !
                    do r2 = n, size(rc)
                        player = (/100, wd%at(w), aa%at(a)/)
                        cost = wc%at(w) + ac%at(a)
                        if (r1 /= 0) then
                            player(2) = player(2) + rd%at(r1)
                            player(3) = player(3) + ra%at(r1)
                            cost = cost + rc%at(r1)
                        end if
                        if (r2 /= 0) then
                            player(2) = player(2) + rd%at(r2)
                            player(3) = player(3) + ra%at(r2)
                            cost = cost + rc%at(r2)
                        end if
                        !
                        if (.not. playgame(player, boss)) then
                            ans = max(ans, cost)
                        end if
                    end do
                end do
            end do
        end do
        !
        print *, ans
    end subroutine
end module
