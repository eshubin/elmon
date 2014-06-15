elmon
=====
**elmon** is a demo project for monitoring using Erlang tracing.
Monitoring is enabled for all processes spawned after application's start. Functions to monitor are specified in `trace_targets` environment variable as a list of `MFA`s (tuples of format `{Module, Function, Arity}`).

In order to receive monitoring info client should implement [gen_event](http://www.erlang.org/doc/man/gen_event.html) behaviour and specify it in `trace_handlers` environment variable, which is a list of handlers of format `{Module, Args}` (values are second and third arguments of [gen_event:add_handler](http://www.erlang.org/doc/man/gen_event.html#add_handler-3) correspondingly). Example of handler could be found in [msg_accumulator](test/msg_accumulator.erl) module. Usage is illustrated by `correctness_test_` case in [elmon_app.erl](src/elmon_app.erl). 

Basically gen_event, "subscribed" for monitoring data, shall expect tuple `{MFA, Time, ExecutionInfo}` in handle_event callback, where `Time` - is an execution time of function and `ExecutionInfo` could be two Erlang record types :
* `#return_value{}` in case of successful function return.
* `#exception{}` if any exception occurs.

Both records contain an additional information of call and should be included from [elmon.hrl](include/elmon.hrl).

## IMPORTANT NOTICE
Tracing and any other code which uses tracing does not work when elmon application is started.
