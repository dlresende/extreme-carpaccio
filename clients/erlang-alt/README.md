# Get Started

Compile the application using rebar:

    ./rebar get-deps compile

Start the application:

    erl -pa ebin deps/*/ebin -s xcarpaccio

Execute tests:

    ./rebar compile eunit skip_deps=true

Execute tests for dependencies too:

    ./rebar compile eunit


# Developer Notes

```
$ wget https://github.com/rebar/rebar/wiki/rebar
$ chmod +x rebar
```

# Resources

* [cowboy](https://github.com/extend/cowboy): Small, fast, modular HTTP server written in Erlang
* [jsx](https://github.com/talentdeficit/jsx): JSON parser (in erlang no NIF)
* [rebar](https://github.com/rebar/rebar)
