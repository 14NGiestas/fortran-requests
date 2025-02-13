module config
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
implicit none

type :: options_t
    integer(c_long) :: timeout         = 10000_c_long ! 10 seconds
    integer(c_long) :: allow_redirects =     1_c_long ! True, allow redirection
    integer(c_long) :: verify          =     1_c_long ! True, verify SSL certs
    character(:), allocatable :: ca_cert
    character(:), allocatable :: ca_path
    character(:), allocatable :: headers
contains

end type

contains

    function make_options(timeout, allow_redirects, verify, ca_cert, ca_path, headers) result(new)
        type(options_t) :: new
        real,         intent(in), optional :: timeout
        logical,      intent(in), optional :: allow_redirects
        logical,      intent(in), optional :: verify
        character(*), intent(in), optional :: ca_cert
        character(*), intent(in), optional :: ca_path
        character(*), intent(in), optional :: headers
        new % ca_cert = ''
        new % ca_path = ''
        new % headers = ''

        if (present(timeout)) &
            new % timeout = int(timeout*1000, kind=c_long)
        if (present(allow_redirects)) &
            new % allow_redirects = int(merge(1,0,allow_redirects), kind=c_long)
        if (present(verify)) &
            new % verify = int(merge(1,0,verify), kind=c_long)
        if (present(ca_cert)) new % ca_cert = ca_cert
        if (present(ca_path)) new % ca_path = ca_path
        if (present(headers)) new % headers = headers
    end function

end module
