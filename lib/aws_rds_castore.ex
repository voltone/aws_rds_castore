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

      AwsRdsCAStore.file_path(:govcloud)
      #=> /Users/me/aws_rds_castore/_build/dev/lib/aws_rds_castore/priv/govcloud-bundle.pem"
  """
  @spec file_path(:aws | :govcloud) :: Path.t()
  def file_path(partition \\ :aws) do
    :aws_rds_castore.file_path(partition)
  end

  @doc """
  Returns a set of `:ssl` transport options for certificate verification.

  Accepts an Ecto database URI or a hostname.

  ## Examples

      # In runtime.exs:
      config :my_app, MyApp.Repo,
        url: database_url,
        ssl: AwsRdsCAStore.ssl_opts(database_url, aws_partition: :aws),
        ### With older Postgrex versions:
        # ssl: true,
        # ssl_opts: AwsRdsCAStore.ssl_opts(database_url),
        pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
        socket_options: maybe_ipv6

  """
  def ssl_opts(url_or_hostname, opts \\ []) when is_list(url_or_hostname) and is_list(opts) do
    hostname =
      case URI.parse(List.to_string(url_or_hostname)) do
        %URI{scheme: nil} ->
          url_or_hostname

        %URI{host: host} ->
          host
      end

    :aws_rds_castore.ssl_opts(hostname, opts)
  end
end
