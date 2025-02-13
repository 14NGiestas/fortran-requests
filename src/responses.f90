module responses
use headers
use json_module
use datetime_module, only: timedelta_t => timedelta
implicit none
private

type, public :: response_t
    logical :: ok
    integer :: status_curl
    integer :: status_code
    character(:), allocatable :: error_message 
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
        !! Return the headers into a json_file format
        class(response_t), intent(inout) :: self
        headers = parse_headers(self % raw_headers)
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
