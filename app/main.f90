program main
  use requests
  use responses
  use config, only: c_ => make_options
  use json_module
  implicit none
  type(response_t) :: res
  type(request_t) :: req
  type(json_file) :: json
  res = req % request("get", "https://httpbin.org/get", options=c_(timeout=1.0))
  print*, '---- Headers ----'
  print*, res % headers
  print*, '----  Body   ----'
  json = res % json() ! Parse response body into json object
  call json % print()
  print*, '-----------------'
  print*, res % header('date')
  print*, res % header('content-type')
  print*, res % header('Content-Type') ! case insensitive

  res = req % request("head", "https://httpbin.org/get")
  print*, res % headers

  res = req % request("get", "https://httpbin.org/get", options=c_(verify=.true.,  ca_cert="/in/valid/cert"))
  if (.not. res % ok) print '("Invalid SSL cert")'

  res = req % request("get", "https://httpbin.org/get", options=c_(verify=.false., ca_path="/in/valid/path"))
  if (res % ok) print '("Invalid SSL cert, but we are not verifying")'

end program main
