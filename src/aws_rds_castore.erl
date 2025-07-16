%% Functionality to retrieve the AWS RDS CA certificate store and use it in the
%% DB connection configuration.

-module(aws_rds_castore).

-export([file_path/0, ssl_opts/1]).

%% Returns the path to the AWS RDS CA certificate store PEM file.
file_path() ->
    LibDir = code:lib_dir(aws_rds_castore),
    list_to_binary(filename:join([LibDir, "priv", "global-bundle.pem"])).

%% Returns a set of `:ssl` transport options for certificate verification.
ssl_opts(Hostname) when is_binary(Hostname) ->
    ssl_opts(binary_to_list(Hostname));

ssl_opts(Hostname) ->
    [
      {verify, verify_peer},
      {cacertfile, file_path()},
      {depth, 10},
      {server_name_indication, Hostname}
    ].
