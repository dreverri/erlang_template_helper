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

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
