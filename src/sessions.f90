module sessions

use requests_
use responses
use queries
use config
use cookie_store

implicit none

type, extends(request_t), public :: session_t
    type(cookie_store_t) :: cookies
    type(request_t) :: requester 
contains
    procedure :: create
    procedure :: request
    procedure :: free
    final :: finalize
end type

contains

    subroutine create(self, cookies)
        class(session_t) :: self
        type(cookie_store_t), optional, intent(in) :: cookies
        ! Initialize the session
        if (present(cookies)) then
            self % cookies = cookies
        else
            call self % cookies % initialize()
        end if
    end subroutine

    function request(self, method, url, params, data, options) result(response)
        class(session_t) :: self
        character(*),    intent(in) :: method
        character(*),    intent(in) :: url
        type(query_t),   intent(in), optional :: params
        type(query_t),   intent(in), optional :: data
        type(options_t), intent(in), optional :: options
        type(response_t) :: response
        character(len=:), allocatable :: cookie_header
        !Need to create new options_t to add the cookie_header
        type(options_t) :: session_options

        if (present(options)) then
            ! Process the user's custom headers (update the cookie store)
            if (len_trim(options % headers) > 0)  then
                call self % cookies % update(options % headers)
            end if
            session_options = options
        end if

        ! Modify the request based on the session (add cookies to headers)
        cookie_header = self % cookies % get_header_string()
        session_options % headers = cookie_header

        ! Call the request function of the base type (request_t)
        response = self % requester % request(method, url, params, data, session_options)

         ! Process the response (e.g., update the cookie store)
        call self % cookies % update(response % raw_headers)

    end function

    subroutine free(self)
        class(session_t) :: self
        ! Clean up the session
        call self % cookies % free()
    end subroutine

    subroutine finalize(self)
        type(session_t) :: self
        ! Clean up the session
        call self % free() 
    end subroutine

end module
