module responses
use json_module
use datetime_module, only: timedelta_t => timedelta
implicit none
private

type, public :: response_t
    logical :: ok
    integer :: status_curl
    integer :: status_code
    character(:), allocatable :: url
    character(:), allocatable :: body
    character(:), allocatable :: raw_headers
    type(timedelta_t) :: elapsed
contains
    procedure :: headers
    procedure :: json
    procedure :: raise_for_status
end type

contains

    type(json_file) &
    function headers(self)
        use fregex
        use pcre_constants
        !! Return the headers into a json_file format
        class(response_t), intent(inout) :: self
        character(:), allocatable :: header_name
        character(:), allocatable :: header_value
        integer :: i, info
        type(regex_t) :: re
        type(match_t) :: mt

        call re % compile("([\w-]+): (.*)", flags=[PCRE_MULTILINE, &
                                                   PCRE_NEWLINE_ANY], info=info)
        call re % match(self % raw_headers)
        if (allocated(re % matches)) then
            call headers % initialize(case_sensitive_keys=.false.)
            do i=1,size(re % matches)
                mt = re % matches(i)
                ! https://datatracker.ietf.org/doc/html/rfc7230#section-3.2.2
                header_name  = mt % groups(1) % content
                header_value = mt % groups(2) % content
                call headers % add(header_name, header_value)
            end do
        end if
        call re % free()
    end function

    function json(self)
        class(response_t) :: self
        type(json_file) :: json
        call json % initialize()
        call json % deserialize(self % body)
    end function

    subroutine raise_for_status(self)
        class(response_t) :: self
        integer :: status_code
        status_code = self % status_code
        if (.not. (status_code >= 100 .and. status_code < 400)) &
            error stop status_code
    end subroutine

end module
