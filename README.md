# ErlangTemplateHelper

This library allows one to specify an Erlang config file in JSON. This is useful 
when deploying an Erlang application with Chef.

## Usage

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
