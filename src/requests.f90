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
use config
implicit none
private

type, public :: request_t
    type(c_ptr) :: curl
contains
    !procedure :: get
    !procedure :: post
    !procedure :: delete
    procedure :: request
    procedure, private :: prepare_url
    procedure, private :: prepare_method
    procedure, private :: prepare_options
end type

contains

    subroutine prepare_method(self, method)
        class(request_t) :: self
        character(*), intent(in) :: method
        integer :: rc
        select case(method)
        case ("get","GET")
            rc = set_option(self % curl, CURLOPT_HTTPGET, 1_c_long)
        case ("post","POST")
            rc = set_option(self % curl, CURLOPT_POST,    1_c_long)
        case ("head","HEAD")
            rc = set_option(self % curl, CURLOPT_NOBODY,  1_c_long)
        case ("put","PUT")
            rc = set_option(self % curl, CURLOPT_UPLOAD,  1_c_long)
        case ("delete","DELETE")
            rc = set_option(self % curl, CURLOPT_CUSTOMREQUEST, "DELETE" // c_null_char)
        end select
    end subroutine

    subroutine prepare_url(self, url)
        class(request_t) :: self
        character(*), intent(in) :: url
        integer :: rc
        rc = set_option(self % curl, CURLOPT_URL, url // c_null_char)
    end subroutine

    subroutine prepare_options(self, options)
        class(request_t) :: self
        type(options_t), intent(in), optional :: options
        integer :: rc
        if (present(options)) then
            rc = set_option(self % curl, CURLOPT_TIMEOUT_MS,     options % timeout)
            rc = set_option(self % curl, CURLOPT_FOLLOWLOCATION, options % allow_redirects)
            rc = set_option(self % curl, CURLOPT_SSL_VERIFYPEER, options % verify)
            if (len_trim(options % ca_cert) > 0) &
                rc = set_option(self % curl, CURLOPT_CAINFO, options % ca_cert // c_null_char)
            if (len_trim(options % ca_path) > 0) &
                rc = set_option(self % curl, CURLOPT_CAPATH, options % ca_path // c_null_char)
        end if
    end subroutine


    function request(self, method, url, params, options) result(response)
        class(request_t) :: self
        character(*),    intent(in) :: method
        character(*),    intent(in) :: url
        character(*),    intent(in), optional :: params
        type(options_t), intent(in), optional :: options
        type(response_t), target :: response
        integer :: rc

        self % curl = curl_init()
        if (.not. c_associated(self % curl)) return

        call self % prepare_url(url)
        call self % prepare_method(method)
        call self % prepare_options(options)

        rc = set_option(self % curl, CURLOPT_HEADERFUNCTION, c_funloc(header_callback))
        rc = set_option(self % curl, CURLOPT_HEADERDATA,        c_loc(response))
        rc = set_option(self % curl, CURLOPT_WRITEFUNCTION,  c_funloc(writer_callback))
        rc = set_option(self % curl, CURLOPT_WRITEDATA,         c_loc(response))
        rc = curl_exec(self % curl)
        response % ok = (rc == CURLE_OK)
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
    function writer_callback(chunk, size_, chunk_size, client_data) bind(c)
        type(c_ptr),       intent(in), value :: chunk      ! Chunk of the response.
        integer(c_size_t), intent(in), value :: size_      ! Always 1.
        integer(c_size_t), intent(in), value :: chunk_size ! Size of the chunk.
        type(c_ptr),       intent(in), value :: client_data
        character(:), allocatable :: buffer
        type(response_t), pointer :: response

        writer_callback = 0_c_size_t

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
        writer_callback = chunk_size
    end function

    subroutine free(self)
        class(request_t) :: self
        if (c_associated(self % curl)) &
            call curl_free(self % curl)
    end subroutine
end module
