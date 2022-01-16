module responses
use json_module
use fregex
use pcre_constants
use datetime_module, only: timedelta_t => timedelta
implicit none
private

type, public :: response_t
    logical :: ok
    integer :: status_code
    integer :: status_curl
    character(:), allocatable :: url
    character(:), allocatable :: body
    character(:), allocatable :: headers
    type(timedelta_t) :: elapsed
contains
    procedure :: header
    procedure :: json
end type

contains

    function header(self, name)
        class(response_t) :: self
        character(*), intent(in)  :: name
        character(:), allocatable :: header
        integer :: info
        type(regex_t) :: re
        type(match_t) :: mt
        call re % compile(name // ": (.*)", flags=[PCRE_CASELESS], info=info)
        call re % match(self % headers)
        header = ''
        if (size(re % matches) > 0) then
            mt = re % matches(1)
            header = mt % groups(1) % content
        end if
    end function

    function json(self)
        class(response_t) :: self
        type(json_file) :: json
        call json % initialize()
        call json % deserialize(self % body)
    end function

end module
