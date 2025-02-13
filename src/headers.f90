module headers
    use json_module
    use fregex
    use pcre_constants
    implicit none
    private

    public :: parse_headers!, dump_headers

contains

    type(json_file) &
    function parse_headers(raw_headers) result(headers)
        character(*), intent(in) :: raw_headers 
        character(:), allocatable :: header_name
        character(:), allocatable :: header_value
        integer :: i, info
        type(regex_t) :: re
        type(match_t) :: mt

        call re % compile("([\w-]+): (.*)", flags=[PCRE_MULTILINE, &
                                                   PCRE_NEWLINE_ANY], info=info)
        call re % match(raw_headers)
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

  end module
