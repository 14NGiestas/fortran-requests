module cookie_store

implicit none
private

type, public :: cookie_store_t
    character(:), allocatable :: cookies
contains
    procedure :: initialize
    procedure :: add
    procedure :: get_header_string
    procedure :: update
    procedure :: free 
    final :: finalize
end type

contains

    subroutine initialize(self)
        class(cookie_store_t) :: self
        if (allocated(self % cookies)) deallocate(self % cookies)
        self % cookies = ""
    end subroutine

    subroutine add(self, name, value)
        class(cookie_store_t) :: self
        character(*), intent(in) :: name
        character(*), intent(in) :: value
        character(len=:), allocatable :: new_cookie
        character(len=:), allocatable :: existing_cookies
        integer :: pos, len_name, len_cookies
        integer :: end_pos

        len_name = len_trim(name)
        len_cookies = len_trim(self % cookies)

        ! Check if cookie already exists
        pos = index(self % cookies, trim(name) // "=", .true.) ! Case-insensitive

        if (pos > 0) then
            ! Replace existing cookie (simplistic approach)
            ! Find the end of the existing cookie
            end_pos = index(self % cookies(pos:), ";", .false.)
            if (end_pos == 0) then
                end_pos = len_cookies - pos + 1
            else
                end_pos = end_pos -1 
            end if
            new_cookie = trim(name) // "=" // trim(value)
            self % cookies = self % cookies(:pos-1) // new_cookie // self % cookies(pos+end_pos:)
        else
            ! Add new cookie
            if (len_cookies > 0) then
                new_cookie = self % cookies // "; " // trim(name) // "=" // trim(value)
            else
                new_cookie = trim(name) // "=" // trim(value)
            end if
            self % cookies = new_cookie
        end if

    end subroutine

    function get_header_string(self) result(header_string)
        class(cookie_store_t) :: self
        character(len=:), allocatable :: header_string

        header_string = self % cookies
    end function

    ! Simplistic update from Set-Cookie header (single cookie per header)
    subroutine update(self, header_string)
        use fregex
        use pcre_constants
        class(cookie_store_t) :: self
        character(*), intent(in) :: header_string
        character(len=:), allocatable :: cookie_name, cookie_value
        type(regex_t) :: re
        type(match_t) :: mt
        integer :: i, info

        ! Regex to extract cookie name and value
        call re % compile("([^=]+)=([^;]*)", flags=[PCRE_CASELESS], info=info)
        call re % match(header_string)

        if (allocated(re % matches)) then
            do i=1, size(re % matches)
                mt = re % matches(i)
                cookie_name  = mt % groups(1) % content
                cookie_value = mt % groups(2) % content
                call self % add(cookie_name, cookie_value)
            end do
        end if

        call re % free()

    end subroutine

    subroutine free(self)
        class(cookie_store_t) :: self
        if (allocated(self % cookies)) deallocate(self % cookies)
    end subroutine

    subroutine finalize(self)
        type(cookie_store_t) :: self
        call self % free()
    end subroutine

end module
