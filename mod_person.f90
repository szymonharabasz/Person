! Created by Szymon Harabasz on 01.10.21.

module mod_person
    use iso_fortran_env, only: int32
    implicit none
    type :: Person
        private
        character(len=256) :: name
        integer(kind=int32) :: age
        character(len=256) :: occupation
        character(len=256) :: greeting
    contains
        procedure, pass(self) :: greet
        procedure, pass(self) :: getAge
        procedure, pass(self) :: setAge
        procedure, pass(self) :: getName
        procedure, pass(self) :: setName
        procedure, pass(self) :: getOccupation
        procedure, pass(self) :: setOccupation
    end type Person

    interface Person
        module procedure :: person_constructor
    end interface Person


    interface operator(+)
        module procedure :: plus_str_str
        module procedure :: plus_str_int
        module procedure :: plus_int_str
    end interface

contains
    impure elemental subroutine greet(self)
        class(Person), intent(in) :: self
        print *, trim(self % greeting) // ' My name is ' // trim(self % name) // '!'
    end subroutine greet

    pure integer function getAge(self) result(age)
        class(Person), intent(in) :: self
        age = self % age
    end function getAge

    subroutine setAge(self, age)
        class(Person), intent(inout) :: self
        integer, intent(in) :: age
        if (age <= 0) then
            stop 'Error: age must be > 0'
        end if
        self % age = age
    end subroutine setAge

    pure function getName(self) result(name)
        class(Person), intent(in) :: self
        character(:), allocatable :: name
        allocate(name, source = trim(self % name))
    end function getName

    subroutine setName(self, name)
        class(Person), intent(inout) :: self
        character(*), intent(in) :: name
        self % name = name
    end subroutine setName

    pure function getOccupation(self) result(occupation)
        class(Person), intent(in) :: self
        character(:), allocatable :: occupation
        allocate(occupation, source = trim(self % occupation))
    end function getOccupation

    subroutine setOccupation(self, occupation)
        class(Person), intent(inout) :: self
        character(*), intent(in) :: occupation
        self % occupation = occupation
    end subroutine setOccupation

    pure type(Person) function person_constructor(name, age, occupation) result(res)
        character(len=*), intent(in) :: name
        integer, intent(in) :: age
        character(len=*), intent(in) :: occupation
        res % name = name
        res % age = age
        res % occupation = occupation

        if (occupation == 'Pirate') then
            res % greeting = 'Ahoy, matey!'
        else
            res % greeting = 'Hi there!'
        end if

    end function person_constructor

    function plus_str_str(a, b) result(c)
        character (len=*), intent(in) :: a
        character (len=*), intent(in) :: b
        character (:), allocatable :: c
        c = a // b
    end function plus_str_str

    function plus_str_int(s, i) result(res)

        character (len=*), intent(in) :: s
        integer, intent(in) :: i
        integer :: alloc_size
        character (:), allocatable :: res

        if (i >= 100) then
            alloc_size = len(s) + 3
        else
            alloc_size = len(s) + 2
        end if

        allocate(character(alloc_size) :: res)

        if (i >= 100) then
            write(res,'(a,i3)') s, i
        else
            write(res,'(a,i2)') s, i
        end if

    end function plus_str_int

    function plus_int_str(i, s) result(res)

        integer, intent(in) :: i
        character (len=*), intent(in) :: s
        integer :: alloc_size
        character (:), allocatable :: res

        if (i >= 100) then
            alloc_size = len(s) + 3
        else
            alloc_size = len(s) + 2
        end if

        allocate(character(alloc_size) :: res)

        if (i >= 100) then
            write(res,'(i3,a)') i, s
        else
            write(res,'(i2,a)') i, s
        end if

    end function plus_int_str

end module mod_person