%% Functionality to retrieve the AWS RDS CA certificate store and use it in the
%% DB connection configuration.

-module(aws_rds_castore).

-export([file_path/0, ssl_opts/1]).

%% Returns the path to the AWS RDS CA certificate store PEM file.
file_path() ->
    PrivDir = code:lib_dir(aws_rds_castore, priv),
    list_to_binary(filename:join(PrivDir, "global-bundle.pem")).

%% Returns a set of `:ssl` transport options for certificate verification.
ssl_opts(UrlOrHostname) when is_binary(UrlOrHostname) ->
    ssl_opts(binary_to_list(UrlOrHostname));

ssl_opts(UrlOrHostname) ->
    % Accepts a database URI if uri_string module is available, so from OTP 21
    ServerName =
        case code:ensure_loaded(uri_string) of
            {module,uri_string} ->
                case uri_string:parse(UrlOrHostname) of
                    #{host := Hostname} ->
                        Hostname;
                    _ ->
                        UrlOrHostname
                end;

            _ ->
                UrlOrHostname
        end,

    [
      {verify, verify_peer},
      {cacertfile, file_path()},
      {depth, 10},
      {server_name_indication, ServerName},
      {customize_hostname_check, [
        {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
      ]}
    ].
