module config
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
implicit none

type :: options_t
    integer(c_long) :: timeout
    integer(c_long) :: allow_redirects
    integer(c_long) :: verify
    character(:), allocatable :: ca_cert
    character(:), allocatable :: ca_path
contains

end type

contains

    function make_options(timeout, allow_redirects, verify, ca_cert, ca_path) result(new)
        type(options_t) :: new
        real,         intent(in), optional :: timeout
        logical,      intent(in), optional :: allow_redirects
        logical,      intent(in), optional :: verify
        character(*), intent(in), optional :: ca_cert
        character(*), intent(in), optional :: ca_path
        new % timeout         = 10000_c_long ! 10 seconds
        new % allow_redirects =     1_c_long ! True, allow redirection
        new % verify          =     1_c_long ! True, verify SSL certs
        new % ca_cert = ''
        new % ca_path = ''

        if (present(timeout)) &
            new % timeout = int(timeout*1000, kind=c_long)
        if (present(allow_redirects)) &
            new % allow_redirects = int(merge(1,0,allow_redirects), kind=c_long)
        if (present(verify)) &
            new % verify = int(merge(1,0,verify), kind=c_long)
        if (present(ca_cert)) new % ca_cert = ca_cert
        if (present(ca_path)) new % ca_path = ca_path
    end function

end module
