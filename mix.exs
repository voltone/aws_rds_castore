defmodule AwsRdsCAStore.MixProject do
  use Mix.Project

  @version "1.0.0"
  @repo_url "https://github.com/voltone/aws_rds_castore"

  def project do
    [
      app: :aws_rds_castore,
      version: @version,
      elixir: "~> 1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      xref: [exclude: [:public_key]],

      # Hex
      package: package(),
      description: "AWS RDS CA certificate store.",

      # Docs
      name: "AwsRdsCAStore",
      docs: [
        source_ref: "v#{@version}",
        source_url: @repo_url
      ]
    ]
  end

  def application do
    [
      extra_applications: extra_applications(Mix.env())
    ]
  end

  defp extra_applications(:prod), do: [:logger]
  defp extra_applications(_env), do: [:public_key] ++ extra_applications(:prod)

  defp deps do
    [
      {:ex_doc, "~> 0.29", only: :dev}
    ]
  end

  defp package do
    [
      files: ["lib/aws_rds_castore.ex", "priv", "mix.exs", "README.md"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @repo_url}
    ]
  end
end
