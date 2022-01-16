program main
  use requests
  use responses
  use config, only: c_ => make_options
  use json_module
  implicit none
  type(response_t) :: res
  type(request_t) :: req
  type(json_file) :: json
  res = req % get("https://httpbin.org/get", options=c_(timeout=1.0))
  print*, '----  Body   ----'
  json = res % json() ! Parse response body into json object
  call json % print()
  print*, '-----------------'

  res = req % head("https://httpbin.org/get")
  print '(A)', res % headers
  print '(A)', res % header('date')
  print '(A)', res % header('content-type')
  print '(A)', res % header('Content-Type') ! case insensitive

  res = req % head("https://httpbin.org/get", options=c_(verify=.true.,  ca_cert="/in/valid/cert"))
  if (.not. res % ok) print '("Invalid SSL cert: ",i0)', res % status_curl

  res = req % head("https://httpbin.org/get", options=c_(verify=.false., ca_path="/in/valid/path"))
  if (res % ok) print '("Invalid SSL cert, but we are not verifying")'

end program main
