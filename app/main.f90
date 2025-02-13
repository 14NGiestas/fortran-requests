program main
    use requests
    use requests, only: q => make_query, set => make_options
    use json_module
    implicit none
    type(response_t) :: res
    type(request_t) :: req

    block
        type(json_file) :: json
        res = req % get("https://httpbin.org/get", &
                        params=q(["fruit" .kv. "apple", &
                                  "price" .kv. "20000"]), &
                                    options=set(timeout=1.0))
        print '(a)', '---- Headers ----'
        print '(a)', res % raw_headers ! Headers, but in a string
        print '(a)', '----  Body   ----'
        json = res % json() ! Parse response body into json object
        call json % print()
        print '(a)', '-----------------'
    end block

    block
        type(json_file) :: json
        res = req % post("https://httpbin.org/post", &
                          data=q(["symbol" .kv. "thing1", &
                                  "symbol" .kv. "thing2"]))
        print '(a)', '----  Body   ----'
        json = res % json() ! Parse response body into json object
        call json % print()
        print '(a)', '-----------------'
    end block

    block
        type(json_file) :: headers
        character(:), allocatable :: allowed
        res = req % options("https://httpbin.org/get")
        headers = res % headers() ! Case insensitive json (parses the raw_header)
        call headers % get('allow', allowed) ! Get a header
        print '("Allowed request verbs: ",a)', allowed
        call headers % add('x-burger', 'nham nham!') ! Add new custom header
        print '(a)', '---- Headers ----'
        call headers % print() ! Show all headers
        print '(a)', '-----------------'
    end block

    block
        res = req % head("https://httpbin.org/get", &
                            options=set(verify=.true.,&
                                        ca_cert="/in/valid/cert"))
        if (.not. res % ok) &
            print '("Invalid SSL cert: ",i0)', res % status_curl

        res = req % head("https://httpbin.org/get", &
                            options=set(verify=.false.,&
                                        ca_path="/in/valid/path"))
        if (res % ok) &
            print '("Invalid SSL cert, but we are not verifying")'
    end block

    block
        res = req % head('https://httpbin.org/status/404')
        print '("Returned ",i0)', res % status_code
        call res % raise_for_status() ! This should call error stop, it's alright!
    end block

end program main
