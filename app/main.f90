program main
  use requests
  use requests, only: q => make_query, set => make_options
  use json_module
  implicit none
  type(query_t) :: query
  type(response_t) :: res
  type(request_t) :: req
  type(json_file) :: json

  query = q(["string" .kv. "1", "second" .kv. "2"])

  res = req % get("https://httpbin.org/get", params=query, options=set(timeout=1.0))
  print*, '---- Headers ----'
  print*, res % headers
  print*, '----  Body   ----'
  json = res % json() ! Parse response body into json object
  call json % print()
  print*, '-----------------'

  res = req % post("https://httpbin.org/post", &
                    data=q(["symbol" .kv. "thing1", &
                            "symbol" .kv. "thing2"]))
  print '(a)', '---- Headers ----'
  print '(a)', res % headers
  print '(a)', '----  Body   ----'
  json = res % json() ! Parse response body into json object
  call json % print()
  print '(a)', '-----------------'

  res = req % head("https://httpbin.org/get")
  print '(a)', res % headers
  print '(a)', res % header('date')
  print '(a)', res % header('content-type')
  print '(a)', res % header('Content-Type') ! case insensitive

  res = req % head("https://httpbin.org/get", options=set(verify=.true.,  ca_cert="/in/valid/cert"))
  if (.not. res % ok) print '("Invalid SSL cert: ",i0)', res % status_curl

  res = req % head("https://httpbin.org/get", options=set(verify=.false., ca_path="/in/valid/path"))
  if (res % ok) print '("Invalid SSL cert, but we are not verifying")'

  res = req % options("https://httpbin.org/get")
  print '("Available verbs: ",a)', res % header('allow')

  res = req % head('https://httpbin.org/status/404')
  print '("Returned ",i0)', res % status_code
  call res % raise_for_status()

end program main
