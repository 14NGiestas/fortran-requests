module requests
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
use curl
use json_module
use curl, only: set_option => curl_easy_setopt,  &
                curl_init  => curl_easy_init,    &
                curl_free  => curl_easy_cleanup, &
                curl_exec  => curl_easy_perform
use responses
implicit none
private

type, public :: request_t
    type(c_ptr) :: curl
contains
    !procedure :: get
    !procedure :: post
    !procedure :: delete
    procedure :: request
    procedure, private :: init
    procedure, private :: prepare_url
    procedure, private :: prepare_method
    procedure, private :: prepare_options
end type

contains

    subroutine init(self)
        class(request_t) :: self
        integer :: rc
        self % curl = curl_init()
        if (.not. c_associated(self % curl)) then
            error stop
        end if
        rc = set_option(self % curl, CURLOPT_TIMEOUT,        10_INT64)
        rc = set_option(self % curl, CURLOPT_CONNECTTIMEOUT, 10_INT64)
    end subroutine

    subroutine prepare_method(self, method)
        class(request_t) :: self
        character(*), intent(in) :: method
        integer :: rc
        select case(method)
        case ("get","GET")
            rc = set_option(self % curl, CURLOPT_HTTPGET, 1_INT64)
        case ("post","POST")
            rc = set_option(self % curl, CURLOPT_POST,    1_INT64)
        end select
    end subroutine

    subroutine prepare_url(self, url)
        class(request_t) :: self
        character(*), intent(in) :: url
        integer :: rc
        rc = set_option(self % curl, CURLOPT_URL, url // c_null_char)
    end subroutine

    subroutine prepare_options(self, &
            params,  &
            timeout, &
            allow_redirects &
    )
        class(request_t) :: self
        character(*), intent(in), optional :: params
        real,         intent(in), optional :: timeout
        logical,      intent(in), optional :: allow_redirects
        integer :: rc
        if (present(timeout)) &
            rc = set_option(self % curl, CURLOPT_TIMEOUT_MS, int(timeout*1000,INT64))
        if (present(allow_redirects)) &
            rc = set_option(self % curl, CURLOPT_FOLLOWLOCATION, merge(1_INT64,0_INT64,allow_redirects))
    end subroutine

    function request(self, method, url, &
            params,  &
            timeout, &
            allow_redirects &
        ) result(response)
        class(request_t) :: self
        character(*), intent(in) :: method
        character(*), intent(in) :: url
        character(*), intent(in), optional :: params
        real,         intent(in), optional :: timeout
        logical,      intent(in), optional :: allow_redirects
        type(response_t), target :: response
        integer :: rc

        call self % init
        call self % prepare_url(url)
        call self % prepare_method(method)
        call self % prepare_options(params, timeout, allow_redirects)
        rc = set_option(self % curl, CURLOPT_HEADERFUNCTION, c_funloc(header_callback))
        rc = set_option(self % curl, CURLOPT_HEADERDATA,        c_loc(response))
        rc = set_option(self % curl, CURLOPT_WRITEFUNCTION,  c_funloc(body_callback))
        rc = set_option(self % curl, CURLOPT_WRITEDATA,         c_loc(response))
        rc = curl_exec(self % curl)
    end function

    function get(self) result(response)
        class(request_t) :: self
        type(response_t), target :: response
        !response = request("GET", url, data)
    end function

    ! static size_t header_callback(char *buffer, size_t size,
    !                               size_t nitems, void *userdata)
    integer(c_size_t) &
    function header_callback(chunk, size_, chunk_size, client_data) bind(c)
        type(c_ptr),       intent(in), value :: chunk      ! Chunk of the response.
        integer(c_size_t), intent(in), value :: size_      ! Always 1.
        integer(c_size_t), intent(in), value :: chunk_size ! Size of the chunk.
        type(c_ptr),       intent(in), value :: client_data
        character(:), allocatable :: buffer
        type(response_t), pointer :: response

        header_callback = 0_c_size_t

        ! Are the passed C pointers associated?
        if (.not. c_associated(chunk)) return
        if (.not. c_associated(client_data)) return

        ! Convert C pointer to Fortran pointer.
        call c_f_pointer(client_data, response)
        if (.not. allocated(response % headers)) response % headers = ''

        ! Convert C pointer to Fortran allocatable character.
        call c_f_str_ptr(chunk, buffer, chunk_size)
        if (.not. allocated(buffer)) return
        response % headers = response % headers // buffer
        deallocate(buffer)

        ! Return number of bytes read.
        header_callback = chunk_size
    end function

    integer(c_size_t) &
    function body_callback(chunk, size_, chunk_size, client_data) bind(c)
        type(c_ptr),       intent(in), value :: chunk      ! Chunk of the response.
        integer(c_size_t), intent(in), value :: size_      ! Always 1.
        integer(c_size_t), intent(in), value :: chunk_size ! Size of the chunk.
        type(c_ptr),       intent(in), value :: client_data
        character(:), allocatable :: buffer
        type(response_t), pointer :: response

        body_callback = 0_c_size_t

        ! Are the passed C pointers associated?
        if (.not. c_associated(chunk)) return
        if (.not. c_associated(client_data)) return

        ! Convert C pointer to Fortran pointer.
        call c_f_pointer(client_data, response)
        if (.not. allocated(response % body)) response % body = ''

        ! Convert C pointer to Fortran allocatable character.
        call c_f_str_ptr(chunk, buffer, chunk_size)
        if (.not. allocated(buffer)) return
        response % body = response % body // buffer
        deallocate(buffer)

        ! Return number of bytes read.
        body_callback = chunk_size
    end function

    subroutine free(self)
        class(request_t) :: self
        if (c_associated(self % curl)) &
            call curl_free(self % curl)
    end subroutine
end module
