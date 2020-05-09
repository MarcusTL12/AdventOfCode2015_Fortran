module day15_mod
    use vec_int_mod
    use vec_vec_int_mod
    use astring_mod
    use string_util_mod
    use vec_str_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    function loadinput(filename) result(ingredients)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(vec_vec_int) :: ingredients
        type(astring) :: line, nbuf
        type(vec_str) :: split_buf
        type(vec_int) :: ing_buf
        integer :: i, j, poss(4)
        !
        poss = (/3, 5, 7, 9/)
        call line%new()
        call split_buf%new()
        call ingredients%new()
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call split_buf%clear()
            call split_with_delim(line%as_slice(), ' ', split_buf)
            !
            call ing_buf%with_capacity(5)
            !
            do j = 1, 4
                associate (p=>split_buf%at(poss(j)))
                    call nbuf%from_borrow(p%as_slice())
                    call nbuf%truncate(size(nbuf) - 1)
                    i = parse_int(nbuf)
                    call ing_buf%push(i)
                end associate
            end do
            !
            associate (p=>split_buf%at(11))
                call nbuf%from_borrow(p%as_slice())
                i = parse_int(nbuf)
                call ing_buf%push(i)
            end associate
            !
            call ingredients%push(ing_buf)
            !
            call line%clear()
        end do
        !
        close (1)
    end function
    !
    integer function score(ingredients, recipie)
        implicit none
        !
        type(vec_int), intent(in) :: ingredients(:)
        integer, intent(in) :: recipie(:)
        integer :: i, j, buf
        !
        score = 1
        !
        do i = 1, 4
            buf = 0
            do j = 1, size(ingredients)
                associate (ing=>ingredients(j))
                    buf = buf + ing%at(i) * recipie(j)
                end associate
            end do
            score = score * max(buf, 0)
        end do
    end function
    !
    integer function cals(ingredients, recipie)
        implicit none
        !
        type(vec_int), intent(in) :: ingredients(:)
        integer, intent(in) :: recipie(:)
        integer :: i
        !
        cals = 0
        !
        do i = 1, size(ingredients)
            associate (ing=>ingredients(i))
                cals = cals + ing%at(5) * recipie(i)
            end associate
        end do
    end function
    !
    subroutine part1()
        implicit none
        !
        type(vec_vec_int) :: ingredients
        integer, allocatable :: recipie(:)
        integer :: ans
        !
        ingredients = loadinput("inputfiles/day15/input.txt")
        !
        allocate (recipie(size(ingredients)))
        !
        recipie = 0
        ans = 0
        !
        call solve(1, 100)
        !
        print *, ans
        !
        deallocate (recipie)
    contains
        recursive subroutine solve(i, s)
            implicit none
            !
            integer, intent(in) :: i, s
            integer :: j
            !
            if (i >= size(recipie)) then
                recipie(size(recipie)) = s
                ans = max(ans, score(ingredients%as_slice(), recipie))
            else
                do j = 0, s
                    recipie(i) = j
                    call solve(i + 1, s - j)
                end do
            end if
        end subroutine
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(vec_vec_int) :: ingredients
        integer, allocatable :: recipie(:)
        integer :: ans
        !
        ingredients = loadinput("inputfiles/day15/input.txt")
        !
        allocate (recipie(size(ingredients)))
        !
        recipie = 0
        ans = 0
        !
        call solve(1, 100)
        !
        print *, ans
        !
        deallocate (recipie)
    contains
        recursive subroutine solve(i, s)
            implicit none
            !
            integer, intent(in) :: i, s
            integer :: j
            !
            if (i >= size(recipie)) then
                recipie(size(recipie)) = s
                !
                if (cals(ingredients%as_slice(), recipie) == 500) then
                    ans = max(ans, score(ingredients%as_slice(), recipie))
                end if
            else
                do j = 0, s
                    recipie(i) = j
                    call solve(i + 1, s - j)
                end do
            end if
        end subroutine
    end subroutine
end module
