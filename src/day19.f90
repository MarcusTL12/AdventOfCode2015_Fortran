module day19_mod
    use astring_mod
    use astring_show_mod
    use string_util_mod
    use vec_str_mod
    use map_str_int_mod
    use vec_vec_str_mod
    implicit none
    !
    private
    public :: part1, part2
contains
    subroutine load_input(filename, reps_ind, reps, mol)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(map_str_int), intent(inout) :: reps_ind
        type(vec_vec_str), intent(inout) :: reps
        type(astring), intent(inout) :: mol
        type(astring) :: line, k, v
        type(vec_str) :: split_buf, vbuf
        type(vec_str), pointer :: p
        integer :: i
        !
        call line%new()
        call split_buf%new()
        call reps_ind%new()
        call reps%new()
        !
        open (1, file=filename)
        !
        do while (readline(1, line))
            call split_buf%clear()
            call split_with_delim(line%as_slice(), ' ', split_buf)
            !
            if (size(split_buf) == 3) then
                v = split_buf%at(3)
                !
                if (reps_ind%contains_key(split_buf%at(1))) then
                    p => reps%at(reps_ind%get(split_buf%at(1)))
                    call p%push(v)
                else
                    call vbuf%with_capacity(2)
                    call vbuf%push(v)
                    call reps%push(vbuf)
                    !
                    k = split_buf%at(1)
                    i = size(reps)
                    call reps_ind%insert(k, i)
                end if
            else if (size(split_buf%at(1)) > 0) then
                mol = split_buf%at(1)
            end if
            !
            call line%clear()
        end do
        !
        close (1)
    end subroutine
    !
    subroutine split_atoms(mol, atoms)
        implicit none
        !
        character, target, intent(in) :: mol(:)
        type(vec_str), intent(inout) :: atoms
        character, pointer :: p(:)
        type(astring) :: buf
        integer :: i, j
        logical :: b
        !
        call atoms%clear()
        !
        b = .false.
        do i = 1, size(mol)
            if (is_uppercase(mol(i))) then
                if (b) then
                    p => mol(j:i - 1)
                    call buf%from_borrow(p)
                    call atoms%push(buf)
                end if
                b = .true.
                j = i
            end if
        end do
        !
        p => mol(j:size(mol))
        call buf%from_borrow(p)
        call atoms%push(buf)
    end subroutine
    !
    subroutine atoms_to_mol(atoms, mol)
        implicit none
        !
        type(astring), target, intent(in) :: atoms(:)
        type(astring), intent(inout) :: mol
        type(astring), pointer :: p
        integer :: i, j
        !
        do i = 1, size(atoms)
            p => atoms(i)
            do j = 1, size(p)
                call mol%push(p%at(j))
            end do
        end do
    end subroutine
    !
    subroutine part1()
        implicit none
        !
        type(map_str_int) :: reps_ind
        type(vec_vec_str) :: reps
        type(astring) :: mol, mol2, mol3
        type(astring), pointer :: k, r
        type(vec_str) :: atoms
        type(map_str_int) :: uniques
        integer :: i, j, n
        integer, pointer :: v
        logical :: stat
        !
        call load_input("inputfiles/day19/input.txt", reps_ind, reps, mol)
        !
        call atoms%new()
        call uniques%new()
        call mol2%new()
        !
        call split_atoms(mol%as_slice(), atoms)
        n = 1
        !
        stat = .false.
        do while (reps_ind%next_kvp(k, v, stat))
            associate (q=>reps%at(v))
                do i = 1, size(q)
                    do j = 1, size(atoms)
                        call atoms%clear()
                        call split_atoms(mol%as_slice(), atoms)
                        !
                        r => atoms%at(j)
                        if (str_eq(r%as_slice(), k%as_slice())) then
                            associate (s=>q%at(i))
                                call r%from_borrow(s%as_slice())
                            end associate
                            !
                            call mol2%clear()
                            call atoms_to_mol(atoms%as_slice(), mol2)
                            !
                            mol3 = mol2
                            call uniques%insert(mol3, n)
                        end if
                    end do
                end do
            end associate
        end do
        !
        print *, size(uniques)
        print *, uniques%meta
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        type(map_str_int) :: reps_ind
        type(vec_vec_str) :: reps
        type(astring) :: mol
        type(vec_str) :: atoms
        integer :: a, b, i
        !
        call load_input("inputfiles/day19/input.txt", reps_ind, reps, mol)
        !
        call atoms%new()
        call split_atoms(mol%as_slice(), atoms)
        !
        a = 0
        b = 0
        do i = 1, size(atoms)
            associate (p=>atoms%at(i))
                if (str_eq(p%as_slice(), str_p("Rn")) .or. &
                    str_eq(p%as_slice(), str_p("Ar"))) then
                    a = a + 1
                else if (str_eq(p%as_slice(), str_p("Y"))) then
                    b = b + 1
                end if
            end associate
        end do
        !
        print *, size(atoms) - a - 2 * b - 1
    end subroutine
end module
