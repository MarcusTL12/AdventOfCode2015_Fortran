module day4_mod
    use md5_mod
    use astring_mod
    use string_util_mod
    implicit none
    !
    private
    !
    character(len=*), parameter :: key = "ckczppom"
    !
    public :: part1, part2
contains
    subroutine part1()
        implicit none
        !
        type(astring) :: s
        integer :: i, j
        logical :: done
        character :: h(32)
        !
        s = tostring(key)
        !
        done = .false.
        i = 0
        !
        do while (.not. done)
            i = i + 1
            call s%truncate(len(key))
            call into_string(s, i)
            h = md5_to_hex(md5(s%as_slice()))
            do j = 1, 5
                done = .true.
                if (h(j) /= '0') then
                    done = .false.
                    exit
                end if
            end do
        end do
        !
        print *, i
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(astring) :: s
        integer :: i, j
        logical :: done
        character :: h(32)
        !
        s = tostring(key)
        !
        done = .false.
        i = 0
        !
        do while (.not. done)
            i = i + 1
            call s%truncate(len(key))
            call into_string(s, i)
            h = md5_to_hex(md5(s%as_slice()))
            do j = 1, 6
                done = .true.
                if (h(j) /= '0') then
                    done = .false.
                    exit
                end if
            end do
        end do
        !
        print *, i
    end subroutine
end module
