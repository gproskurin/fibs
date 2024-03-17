# Fibonacci REST JSON API server

## Requirements
- Erlang >= 25.0
- rebar3
- dependencies
  - cowboy
  - jiffy

## Testing
```
rebar3 eunit
```

## Running
```
rebar3 shell
curl "http://localhost:8000/fibs/generate?count=20&pagesize=15"
curl "http://localhost:8000/fibs/generate?pagesize=15&continuation=%7B%22rem_count%22%3A5%2C%22prev%22%3A%5B377%2C233%5D%7D"
(copy continuation from response of the first curl command to query parameters of the second curl command)
```

### Using blacklisted numbers
```
rebar3 shell
> fibs_api:blacklist_add(5).
curl ...
```

## REST API
- GET /fibs/generate?pagesize={**pagesize**}&count={**count**}
- GET /fibs/generate?pagesize={**pagesize**}&continuation={**continuation**}

**pagesize** can be specified in every request and affects only current response. Default value is 100.

There are two types of "generate" requests:
- initial request, when **count** is specified, but not **continuation**
- continuation request, when **continuation** is specified, but not **count**

## TODO
- An endpoint that returns the value from the Fibonacci sequence for a given number.
- Add blacklist REST API endpoints
- Better parameters validation

# Authors
- [Gennady Proskurin] (https://github.com/gproskurin)

