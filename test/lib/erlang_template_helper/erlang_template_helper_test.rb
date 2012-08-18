require_relative '../../test_helper'
require 'json'

describe "string conversions" do
  it "should default to an atom" do
    Eth::Config.new("key").to_s.must_equal "key."
  end

  it "should produce an atom" do
    Eth::Config.new("__atom_key").to_s.must_equal "'key'."
  end

  it "should produce a binary" do
    Eth::Config.new("__binary_key").to_s.must_equal "<<\"key\">>."
  end

  it "should produce a string" do
    Eth::Config.new("__string_key").to_s.must_equal "\"key\"."
  end
end

describe "list conversions" do
  it "should default to a list" do
    Eth::Config.new([1, 2, 3]).to_s.must_equal "[1, 2, 3]."
  end

  it "should produce a list" do
    Eth::Config.new(["__list", 1, 2, 3]).to_s.must_equal "[1, 2, 3]."
  end

  it "should produce a tuple" do
    Eth::Config.new(["__tuple", 1, 2, 3]).to_s.must_equal "{1, 2, 3}."
  end
end

describe "object conversions" do
  it "should produce a proplist" do
    obj = Eth::Config.new({"storage_backend" => "bitcask"})
    obj.to_s.must_equal "[{storage_backend, bitcask}]."
  end

  it "should handle nested objects" do
    obj = Eth::Config.new({"riak_kv" => {"storage_backend" => "bitcask"}})
    obj.to_s.must_equal "[{riak_kv, [{storage_backend, bitcask}]}]."
  end

  it "should handle nested objects with strings" do
    v = {"http" => {"__string_127.0.0.1" => 8098}}
    obj = Eth::Config.new(v)
    obj.to_s.must_equal "[{http, [{\"127.0.0.1\", 8098}]}]."
  end

  it "should handle nested objects with binaries" do
    v = {"multi_backend_prefix_list" => {"__binary_0b:" => "be_blocks"}}
    obj = Eth::Config.new(v)
    obj.to_s.must_equal "[{multi_backend_prefix_list, [{<<\"0b:\">>, be_blocks}]}]."
  end
end

describe "pretty print" do
  it "should pretty print" do
    jpath = File.expand_path('../../../examples/multi_backend.json', __FILE__)
    cpath = File.expand_path('../../../examples/multi_backend.config', __FILE__)
    j = IO.read(jpath)
    c = IO.read(cpath)
    v = JSON.parse(j)
    obj = Eth::Config.new(v)
    obj.pp.must_equal c.strip
  end
end

describe "args file" do
  it "should convert args file" do
    json_path = File.expand_path('../../../examples/vm.json', __FILE__)
    args_path = File.expand_path('../../../examples/vm.args', __FILE__)
    j = IO.read(json_path)
    a = IO.read(args_path)
    v = JSON.parse(j)
    obj = Eth::Args.new(v)
    obj.pp.must_equal a.strip
  end
end

describe "helper methods" do
  before do
    class String
      include Eth::Erlang::String
    end

    class Array
      include Eth::Erlang::Array
    end
  end

  it "should prefix strings" do
    "/var/lib/riak".to_erl_string.must_equal "__string_/var/lib/riak"
  end

  it "should prefix binaries" do
    "0b:".to_erl_binary.must_equal "__binary_0b:"
  end

  it "should prefix tuples" do
    actual = ["first_backend", "riak_kv_bitcask_backend"].to_erl_tuple
    expected = ["__tuple", "first_backend", "riak_kv_bitcask_backend"]
    actual.must_equal expected
  end

  it "should prefix tuples" do
    actual = [1, 2].to_erl_list
    expected = ["__list", 1, 2]
    actual.must_equal expected
  end
end
