defmodule Mix.Tasks.AwsRdsCaStore.Certdata do
  @moduledoc """
  Fetches an up-to-date version of the AWS RDS CA certificate store.

  The certificate store is then stored in the private storage of this library. The path
  to the CA certificate store can be retrieved through `AwsRdsCAStore.file_path/0`.

  ## Usage

  You can use this task to fetch an up-to-date version of the AWS RDS CA certificate
  store.

      mix aws_rds_ca_store.certdata

  You can also use this task to verify if the currently stored certificate store is
  up to date. To do that, use the `--check-outdated` option.

      mix aws_rds_ca_store.certdata --check-outdated

  """

  use Mix.Task

  require Record

  Record.defrecordp(
    :certificate,
    :Certificate,
    Record.extract(:Certificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")
  )

  Record.defrecordp(
    :tbs_certificate,
    :TBSCertificate,
    Record.extract(:TBSCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")
  )

  Record.defrecordp(
    :validity,
    :Validity,
    Record.extract(:Validity, from_lib: "public_key/include/OTP-PUB-KEY.hrl")
  )

  @shortdoc "Fetches an up-to-date version of the AWS RDS CA certificate store"

  @aws_rds_global_bundle "https://truststore.pki.rds.amazonaws.com/global/global-bundle.pem"
  @ca_bundle "global-bundle.pem"
  @ca_bundle_target "priv/global-bundle.pem"

  @impl true
  def run(args)

  def run([]) do
    bundle =
      fetch_ca_bundle()
      |> parse_bundle()
      |> filter_expired()
      |> filter_intermediates()
      |> rebuild_bundle()

    File.write!(@ca_bundle_target, bundle)
  end

  def run(["--check-outdated"]) do
    new_bundle =
      fetch_ca_bundle()
      |> parse_bundle()
      |> filter_expired()
      |> filter_intermediates()

    old_bundle = read_certificates_set(parse_bundle(File.read!(AwsRdsCAStore.file_path())))
    new_bundle = read_certificates_set(new_bundle)

    if not MapSet.equal?(old_bundle, new_bundle) do
      Mix.raise(
        "#{AwsRdsCAStore.file_path()} is outdated. Run \"mix aws_rds_ca_store.certdata\" to update it."
      )
    end
  end

  def run(_args) do
    Mix.raise("Invalid arguments. See `mix help aws_rds_ca_store.certdata`.")
  end

  defp fetch_ca_bundle do
    fetch!(@aws_rds_global_bundle)
    bundle = File.read!(@ca_bundle)
    File.rm!(@ca_bundle)
    bundle
  end

  defp fetch!(url) do
    if System.find_executable("curl") do
      cmd!("curl -LO #{url}")
    else
      cmd!("wget #{url}")
    end
  end

  defp cmd!(cmd) do
    Mix.shell().info([:magenta, "Running: #{cmd}"])

    exit_status = Mix.shell().cmd(cmd)

    if exit_status != 0 do
      Mix.raise("Non-zero result (#{exit_status}) from command: #{cmd}")
    end
  end

  defp read_certificates_set(bundle) do
    MapSet.new(bundle)
  end

  defp parse_bundle(bundle) do
    bundle
    |> :public_key.pem_decode()
    |> Enum.map(&:public_key.pem_entry_decode/1)
  end

  defp rebuild_bundle(certs) do
    certs
    |> Enum.map(&:public_key.pem_entry_encode(:Certificate, &1))
    |> :public_key.pem_encode()
  end

  defp filter_expired(certs) do
    Enum.reject(certs, &expired?/1)
  end

  defp expired?(cert) do
    not_after =
      cert
      |> certificate(:tbsCertificate)
      |> tbs_certificate(:validity)
      |> validity(:notAfter)
      |> parse_asn1_date_time()

    DateTime.to_unix(not_after) < DateTime.to_unix(DateTime.utc_now())
  end

  # Technically we would have to account for utcTime values between 1950 and
  # 2000, but I think it is safe to ignore that here
  defp parse_asn1_date_time({:utcTime, date_time}) when length(date_time) == 13 do
    parse_asn1_date_time({:generalTime, [?2, ?0 | date_time]})
  end

  defp parse_asn1_date_time({:generalTime, date_time}) when length(date_time) == 15 do
    [y1, y2, y3, y4, m1, m2, d1, d2 | _] = date_time

    %DateTime{
      year: List.to_integer([y1, y2, y3, y4]),
      month: List.to_integer([m1, m2]),
      day: List.to_integer([d1, d2]),
      hour: 0,
      minute: 0,
      second: 0,
      time_zone: "Etc/UTC",
      zone_abbr: "UTC",
      utc_offset: 0,
      std_offset: 0
    }
  end

  defp filter_intermediates(certs) do
    Enum.reject(certs, &intermediate?/1)
  end

  defp intermediate?(cert) do
    issuer =
      cert
      |> certificate(:tbsCertificate)
      |> tbs_certificate(:issuer)

    subject =
      cert
      |> certificate(:tbsCertificate)
      |> tbs_certificate(:subject)

    issuer != subject
  end
end
