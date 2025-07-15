%% Functionality to retrieve the AWS RDS CA certificate store and use it in the
%% DB connection configuration.

-module(aws_rds_castore).

-export([file_path/0, file_path/1, ssl_opts/1, ssl_opts/2]).

%% Returns the path to the AWS RDS CA certificate store PEM file.
file_path() ->
    file_path(aws).

file_path(aws) ->
    PrivDir = code:lib_dir(aws_rds_castore, priv),
    list_to_binary(filename:join(PrivDir, "global-bundle.pem"));
file_path(govcloud) ->
    PrivDir = code:lib_dir(aws_rds_castore, priv),
    list_to_binary(filename:join(PrivDir, "govcloud-bundle.pem")).

%% Returns a set of `:ssl` transport options for certificate verification.
ssl_opts(Hostname) when is_binary(Hostname) ->
    ssl_opts(binary_to_list(Hostname));

ssl_opts(Hostname) ->
    ssl_opts(Hostname, []).

%% Returns a set of `:ssl` transport options for certificate verification with options.
ssl_opts(Hostname, Options) when is_binary(Hostname) ->
    ssl_opts(binary_to_list(Hostname), Options);

ssl_opts(Hostname, Options) ->
    Partition = proplists:get_value(aws_partition, Options, aws),
    [
      {verify, verify_peer},
      {cacertfile, file_path(Partition)},
      {depth, 10},
      {server_name_indication, Hostname}
    ].
