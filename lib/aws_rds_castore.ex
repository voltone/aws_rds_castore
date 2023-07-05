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
    :aws_rds_castore.file_path()
  end

  @doc """
  Returns a set of `:ssl` transport options for certificate verification.

  Accepts a database URI instead of hostname on OTP 21 or later.

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
    :aws_rds_castore.ssl_opts(url_or_hostname)
  end
end
