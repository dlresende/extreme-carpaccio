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

## Live code reload

**First shell: start the application**

```
$ erl -pa ebin deps/*/ebin -s xcarpaccio -sname bob -noshell
```

**Second Shell: remote shell on the first shell**

```
$ erl -sname razowski -remsh bob@Mentem
→ c("src/xcarpaccio_webhandler").
```

or

```
$ erl -sname razowski -remsh bob@Mentem
→ cd("src/").
→ c(xcarpaccio_webhandler).
```


**Warning**
By default, beam files - created from shell compilation - are not created within the `ebin/` folder but in the root folder (or in `src` if you changed the current directory). You should take care to recompile the application if it is restarted from scratch afterwards. Otherwise you may still run old behaviors from previous beam files.

A solution is to define the output dir of the compilation.

```
$ erl -sname razowski -remsh bob@Mentem
→ cd("src/").
→ OPTs =  [{outdir,"../ebin"}].
→ c(xcarpaccio_webhandler, OPTs).
```

# Resources

* [cowboy](https://github.com/extend/cowboy): Small, fast, modular HTTP server written in Erlang
* [jsx](https://github.com/talentdeficit/jsx): JSON parser (in erlang no NIF)
* [rebar](https://github.com/rebar/rebar)
