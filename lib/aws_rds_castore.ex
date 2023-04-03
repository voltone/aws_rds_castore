defmodule AwsRdsCAStore do
  @moduledoc """
  Functionality to retrieve the AWS RDS CA certificate store and use it in the
  DB connection configuration.
  """

  @doc """
  Returns the path to the AWS RDS CA certificate store PEM file.

  ## Examples

      AwsRdsCAStore.file_path()
      #=> /Users/me/aws_rds_castore/_build/dev/lib/aws_rds_castore/priv/global-bundle.pem"

  """
  @spec file_path() :: Path.t()
  def file_path() do
    Application.app_dir(:aws_rds_castore, "priv/global-bundle.pem")
  end

  @doc """
  Returns a set of `:ssl` transport options for certificate verification.

  ## Examples

      # In runtime.exs:
      config :my_app, MyApp.Repo,
        url: database_url,
        ssl: true,
        ssl_opts: AwsRdsCAStore.ssl_opts(database_url),
        pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
        socket_options: maybe_ipv6

  """
  def ssl_opts(url_or_hostname) do
    hostname =
      case URI.parse(url_or_hostname) do
        %URI{scheme: nil} ->
          url_or_hostname

        %URI{host: host} ->
          host
      end

    [
      verify: :verify_peer,
      cacertfile: file_path(),
      depth: 10,
      server_name_indication: to_charlist(hostname)
    ]
  end
end
