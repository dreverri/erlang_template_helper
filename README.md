# ErlangTemplateHelper

This library allows one to specify Erlang config and args files in JSON. This is
useful when deploying an Erlang application with Chef. For example, here is a
snippet of a Riak config file specified in JSON:

```json
{
  "riak_kv": {
    "storage_backend": "riak_kv_multi_backend",
    "multi_backend_default": "first_backend",
    "multi_backend": [
      ["__tuple", "first_backend", "riak_kv_bitcask_backend", {
        "data_root": "__string_/var/lib/riak/bitcask"}],
      ["__tuple", "second_backend", "riak_kv_leveldb_backend", {
        "data_root": "__string_/var/lib/riak/leveldb"}]
    ]
  }
}
```

## Usage

### Config

* JSON strings are Erlang atoms unless prefixed with `__binary_`, or 
  `__string_`. The prefix `__atom_` is also recognized.
  * `"ok"` becomes `ok`
  * `"__binary_0b:"` becomes `<<"0b:">>`
  * `"__string_127.0.0.1"` becomes `"127.0.0.1"`
  * `"__atom_riak@127.0.0.1"` becomes `'riak@127.0.0.1'`
* JSON arrays are Erlang lists unless prefixed with `__tuple`. The prefix 
  `__list` is also recognized.
  * `[1, 2, 3]` becomes `[1, 2, 3]`
  * `["__tuple", 1, 2, 3]` becomes `{1, 2, 3}`
  * `["__list", 1, 2, 3]` becomes `[1, 2, 3]`
* JSON objects are Erlang proplists.
  * `{"storage_backend":"bitcask"}` becomes `[{storage_backend, bitcask}]`

```ruby
> require "erlang_template_helper"
=> true
> config = Eth::Config.new({"riak_kv" => {"storage_backend" => "bitcask"}})
=> [{riak_kv, [{storage_backend, bitcask}]}]
> puts config.pp
[
	{riak_kv, [
		{storage_backend, bitcask}
	]}
]
=> nil
```

### Args

Arguments can be specified as a simple JSON object. Nested objects are flattened
but joining the keys and final value by a single space. For example:

```ruby
> require "erlang_template_helper"
=> true
> args = Eth::Args.new({"-name" => "riak@127.0.0.1", "-env" => {"ERL_MAX_PORTS" => 4096}})
=> -name riak@127.0.0.1 -env ERL_MAX_PORTS 4096
> puts args.pp
-name riak@127.0.0.1
-env ERL_MAX_PORTS 4096
=> nil
```

## Command line applications

Two command line applications are provided in `bin`. These are:

* `config_to_json`
* `json_to_config`

These applications allow one to convert between Erlang config format and the 
JSON specification used by ErlangTemplateHelper. They work as follows:

```bash
$ ./bin/config_to_json test/examples/multi_backend.config -p
{
  "riak_kv": {
    "multi_backend_prefix_list": {
      "__binary_0b:": "be_blocks"
    },
    "multi_backend": [
      [
        "__tuple",
        "be_default",
        "riak_kv_eleveldb_backend",
        {
          "cache_size": 47721858,
          "data_root": "__string_/var/lib/riak/leveldb",
          "max_open_files": 50
        }
      ],
      [
        "__tuple",
        "be_blocks",
        "riak_kv_bitcask_backend",
        {
          "data_root": "__string_/var/lib/riak/bitcask"
        }
      ]
    ]
  }
}
```

```bash
$ ./bin/json_to_config test/examples/multi_backend.json -p  
[
	{riak_kv, [
		{multi_backend_prefix_list, [
			{<<"0b:">>, be_blocks}
		]}, 
		{multi_backend, [
			{be_default, riak_kv_eleveldb_backend, [
					{cache_size, 47721858}, 
					{data_root, "/var/lib/riak/leveldb"}, 
					{max_open_files, 50}
				]}, 
			{be_blocks, riak_kv_bitcask_backend, [
					{data_root, "/var/lib/riak/bitcask"}
				]}
		]}
	]}
].
```

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
