module day5_mod
    use astring_mod
    use string_util_mod
    use map_str_int_mod
    implicit none
    !
    private
    !
    character, parameter :: vowels(5) = (/'a', 'e', 'i', 'o', 'u'/)
    character, parameter :: blacklist(2, 4) = reshape((/ &
                                                      'a', 'b', &
                                                      'c', 'd', &
                                                      'p', 'q', &
                                                      'x', 'y' &
                                                      /), [2, 4] &
                                                      )
    !
    public :: part1, part2
contains
    pure logical function isvowel(c)
        implicit none
        !
        character, intent(in) :: c
        integer :: i
        !
        do i = 1, size(vowels)
            if (vowels(i) == c) then
                isvowel = .true.
                return
            end if
        end do
        !
        isvowel = .false.
    end function
    !
    pure logical function isnice(s)
        implicit none
        !
        character, intent(in) :: s(:)
        !
        isnice = three_vowels() .and. twiceinarow() .and. .not. inblacklist()
    contains
        pure logical function three_vowels()
            implicit none
            !
            integer :: i, amt_vowel
            !
            amt_vowel = 0
            do i = 1, size(s)
                if (isvowel(s(i))) amt_vowel = amt_vowel + 1
            end do
            !
            three_vowels = amt_vowel >= 3
        end function
        !
        pure logical function twiceinarow()
            implicit none
            !
            integer :: i
            character :: prev
            !
            twiceinarow = .false.
            prev = s(1)
            do i = 2, size(s)
                if (s(i) == prev) then
                    twiceinarow = .true.
                    return
                end if
                prev = s(i)
            end do
        end function
        !
        pure logical function inblacklist()
            implicit none
            !
            integer :: i, j, k
            !
            do i = 1, size(s) - 1
                do j = 1, size(blacklist, 2)
                    inblacklist = .true.
                    do k = 0, 1
                        if (s(i + k) /= blacklist(k + 1, j)) then
                            inblacklist = .false.
                        end if
                    end do
                    if (inblacklist) return
                end do
            end do
            inblacklist = .false.
        end function
    end function
    !
    logical function isreallynice(s, m)
        implicit none
        !
        character, target, intent(in) :: s(:)
        type(map_str_int) :: m
        !
        isreallynice = pair_repeat() .and. tri_repeat()
    contains
        logical function pair_repeat()
            implicit none
            !
            character, pointer :: p(:)
            type(astring) :: buf
            integer :: i
            !
            call m%new()
            !
            do i = 1, size(s) - 1
                p => s(i:i + 1)
                call buf%from_borrow(p)
                if (m%contains_key(buf)) then
                    if (i - m%get(buf) >= 2) then
                        pair_repeat = .true.
                        return
                    end if
                else
                    call m%insert(buf, i)
                end if
            end do
            pair_repeat = .false.
        end function
        !
        logical function tri_repeat()
            implicit none
            !
            integer :: i
            !
            do i = 1, size(s) - 2
                if (s(i) == s(i + 2)) then
                    tri_repeat = .true.
                    return
                end if
            end do
            tri_repeat = .false.
        end function
    end function
    !
    subroutine part1()
        implicit none
        !
        type(astring) :: line
        integer :: ans
        !
        call line%new()
        !
        open (1, file="inputfiles/day5/input.txt")
        !
        ans = 0
        do while (readline(1, line))
            if (isnice(line%as_slice())) ans = ans + 1
            call line%clear()
        end do
        !
        print *, ans
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(astring) :: line
        type(map_str_int) :: m
        integer :: ans
        
        call line%new()
        call m%new()
        !
        open (1, file="inputfiles/day5/input.txt")
        !
        ans = 0
        do while (readline(1, line))
            if (isreallynice(line%as_slice(), m)) ans = ans + 1
            call line%clear()
        end do
        !
        print *, ans
    end subroutine
end module
