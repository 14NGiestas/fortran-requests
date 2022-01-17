module responses
use json_module
use fregex
use pcre_constants
use datetime_module, only: timedelta_t => timedelta
implicit none
private

type, public :: response_t
    logical :: ok
    integer :: status_curl
    integer :: status_code
    character(:), allocatable :: url
    character(:), allocatable :: body
    character(:), allocatable :: headers
    type(timedelta_t) :: elapsed
contains
    procedure :: json
    procedure :: header
    procedure :: raise_for_status
end type

contains

    function json(self)
        class(response_t) :: self
        type(json_file) :: json
        call json % initialize()
        call json % deserialize(self % body)
    end function

    function header(self, name)
        class(response_t) :: self
        character(*), intent(in)  :: name
        character(:), allocatable :: header
        integer :: i, info
        type(regex_t) :: re
        type(match_t) :: mt
        call re % compile(name // ": (.*)", flags=[PCRE_CASELESS], info=info)
        call re % match(self % headers)
        header = ''
        if (allocated(re % matches)) then
            do i=1,size(re % matches)
                mt = re % matches(i)
                ! https://datatracker.ietf.org/doc/html/rfc7230#section-3.2.2
                ! [RFC7230] A recipient MAY combine multiple header fields with the same field
                ! name into one "field-name: field-value" pair, without changing the
                ! semantics of the message, by appending each subsequent field value to
                ! the combined field value in order, separated by a comma.
                if (i == 1) then
                    header = mt % groups(1) % content
                else
                    header = header // ',' // mt % groups(i) % content
                end if
            end do
        end if
        call re % free()
    end function

    subroutine raise_for_status(self)
        class(response_t) :: self
        integer :: status_code
        status_code = self % status_code
        if (.not. (status_code >= 100 .and. status_code < 400)) &
            error stop status_code
    end subroutine

end module
