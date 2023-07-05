# AwsRdsCAStore

[![Github.com](https://github.com/voltone/aws_rds_castore/workflows/CI/badge.svg)](https://github.com/voltone/aws_rds_castore/actions)
[![Hex.pm](https://img.shields.io/hexpm/v/aws_rds_castore.svg)](https://hex.pm/packages/aws_rds_castore)
[![Hexdocs.pm](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/aws_rds_castore/)
[![Hex.pm](https://img.shields.io/hexpm/dt/aws_rds_castore.svg)](https://hex.pm/packages/aws_rds_castore)
[![Hex.pm](https://img.shields.io/hexpm/l/aws_rds_castore.svg)](https://hex.pm/packages/aws_rds_castore)
[![Github.com](https://img.shields.io/github/last-commit/voltone/aws_rds_castore.svg)](https://github.com/voltone/aws_rds_castore/commits/master)

AWS RDS CA certificate store for Elixir and Erlang. See https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html

Based on and derived from https://hex.pm/packages/castore

## Installation

### Elixir (Mix)

In your `mix.exs`:

```elixir
def deps do
  [
    {:aws_rds_castore, "~> 1.1"}
  ]
end
```

Then, run `$ mix deps.get`.

### Erlang (Rebar3)

In your `rebar.config`:

```erlang
{deps, [
  {aws_rds_castore, "~> 1.1"}
]}.
```

Then, run `$ rebar3 get-deps`.

## Usage

### Elixir (with Ecto)

```elixir
# In runtime.exs:
config :my_app, MyApp.Repo,
  url: database_url,
  ssl: true,
  ssl_opts: AwsRdsCAStore.ssl_opts(database_url),
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
  socket_options: maybe_ipv6
```

See [the documentation](https://hexdocs.pm/aws_rds_castore).

### Erlang (with PGo)

```erlang
PoolConfig = #{pool_size => 10,
               host => Host,
               database => "test",
               user => "test",
               ssl => true,
               ssl_options => aws_rds_castore:ssl_opts(Host)},
pgo:start_pool(default, PoolConfig).
```

## Updates

Every time there is an update to the AWS RDS CA certificate store, we'll release a new **patch version** of the library. For example, `1.1.12` → `1.1.13`.

## Contributing

If you want to locally update the CA certificate store file bundled with this library, run the `mix aws_rds_ca_store.certdata` from the root of this library.

## License

Copyright 2022 Bram Verburg

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
