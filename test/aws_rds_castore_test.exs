defmodule AwsRdsCAStoreTest do
  use ExUnit.Case
  doctest AwsRdsCAStore

  test "file_path/0" do
    assert String.ends_with?(AwsRdsCAStore.file_path(), "/priv/global-bundle.pem")
  end

  describe "ssl_opts/1" do
    test "with hostname" do
      ssl_opts = AwsRdsCAStore.ssl_opts("some.host.name")
      assert :verify_peer = ssl_opts[:verify]
      assert String.ends_with?(ssl_opts[:cacertfile], "/priv/global-bundle.pem")
      assert ~c"some.host.name" = ssl_opts[:server_name_indication]
    end

    test "with hostname (charlist)" do
      ssl_opts = AwsRdsCAStore.ssl_opts(~c"some.host.name")
      assert :verify_peer = ssl_opts[:verify]
      assert String.ends_with?(ssl_opts[:cacertfile], "/priv/global-bundle.pem")
      assert ~c"some.host.name" = ssl_opts[:server_name_indication]
    end

    test "with url" do
      ssl_opts = AwsRdsCAStore.ssl_opts("postgres://postgres:postgres@some.host.name/my_app_db")
      assert :verify_peer = ssl_opts[:verify]
      assert String.ends_with?(ssl_opts[:cacertfile], "/priv/global-bundle.pem")
      assert ~c"some.host.name" = ssl_opts[:server_name_indication]
    end

    test "with url (charlist)" do
      ssl_opts = AwsRdsCAStore.ssl_opts(~c"postgres://postgres:postgres@some.host.name/my_app_db")
      assert :verify_peer = ssl_opts[:verify]
      assert String.ends_with?(ssl_opts[:cacertfile], "/priv/global-bundle.pem")
      assert ~c"some.host.name" = ssl_opts[:server_name_indication]
    end

    test "with url with special characters" do
      ssl_opts =
        AwsRdsCAStore.ssl_opts("postgres://postgres:secret[]123!{}@some.host.name/my_app_db")

      assert :verify_peer = ssl_opts[:verify]
      assert String.ends_with?(ssl_opts[:cacertfile], "/priv/global-bundle.pem")
      assert ~c"some.host.name" = ssl_opts[:server_name_indication]
    end
  end

  describe "Erlang API" do
    test "file_path/0" do
      assert String.ends_with?(:aws_rds_castore.file_path(), "/priv/global-bundle.pem")
    end

    test "ssl_opts/1 (charlist)" do
      ssl_opts = :aws_rds_castore.ssl_opts(~c"some.host.name")
      assert :verify_peer = ssl_opts[:verify]
      assert String.ends_with?(ssl_opts[:cacertfile], "/priv/global-bundle.pem")
      assert ~c"some.host.name" = ssl_opts[:server_name_indication]
    end

    test "ssl_opts/1 (binary)" do
      ssl_opts = :aws_rds_castore.ssl_opts("some.host.name")
      assert :verify_peer = ssl_opts[:verify]
      assert String.ends_with?(ssl_opts[:cacertfile], "/priv/global-bundle.pem")
      assert ~c"some.host.name" = ssl_opts[:server_name_indication]
    end
  end
end
