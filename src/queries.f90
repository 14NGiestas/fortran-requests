module queries
implicit none
private

type param_t
    character(:), allocatable :: key
    character(:), allocatable :: value
end type

type, public :: query_t
    type(param_t), allocatable :: params(:)
contains
    procedure :: string
end type

public :: make_query

interface operator(.kv.)
    module procedure make_param
end interface
public :: operator(.kv.)

contains

    elemental function make_param(lhs, rhs) result(param)
        character(*), intent(in) :: lhs
        character(*), intent(in) :: rhs
        type(param_t) :: param
        param % key = lhs
        param % value = rhs
    end function

    function make_query(params) result(query)
        type(param_t), intent(in) :: params(:)
        type(query_t) :: query
        query = query_t(params)
    end function

    function string(self) result(query_string)
        class(query_t) :: self
        character(:), allocatable :: query_string
        integer :: i
        query_string = ''
        do i=1,size(self % params)
            query_string = query_string     &
                // self % params(i) % key   &
                // '='                      &
                // self % params(i) % value
            if (i <= size(self % params)-1) &
                query_string = query_string // "&"
        end do
    end function

end module
