program main
  use requests
  use responses
  use json_module
  implicit none
  type(response_t) :: res
  type(request_t) :: req
  type(json_file) :: json
  res = req % request("get", "https://httpbin.org/get", params='{"thing": 1}')
  print*, '---- Headers ----'
  print*, res % headers
  print*, '----  Body   ----'
  json = res % json() ! Parse response body into json object
  call json % print()
  print*, '-----------------'
  print*, res % header('date')
  print*, res % header('content-type')
  print*, res % header('Content-Type') ! case insensitive

end program main
