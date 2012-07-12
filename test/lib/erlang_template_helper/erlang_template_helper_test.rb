require_relative '../../test_helper'
require 'json'

describe "string conversions" do
  it "should default to an atom" do
    Eth::Config.new("key").to_s.must_equal "key"
  end

  it "should produce an atom" do
    Eth::Config.new("__atom_key").to_s.must_equal "'key'"
  end

  it "should produce a binary" do
    Eth::Config.new("__binary_key").to_s.must_equal "<<\"key\">>"
  end

  it "should produce a string" do
    Eth::Config.new("__string_key").to_s.must_equal "\"key\""
  end
end

describe "list conversions" do
  it "should default to a list" do
    Eth::Config.new([1, 2, 3]).to_s.must_equal "[1, 2, 3]"
  end

  it "should produce a list" do
    Eth::Config.new(["__list", 1, 2, 3]).to_s.must_equal "[1, 2, 3]"
  end

  it "should produce a tuple" do
    Eth::Config.new(["__tuple", 1, 2, 3]).to_s.must_equal "{1, 2, 3}"
  end
end

describe "object conversions" do
  it "should produce a proplist" do
    obj = Eth::Config.new({"storage_backend" => "bitcask"})
    obj.to_s.must_equal "[{storage_backend, bitcask}]"
  end

  it "should handle nested objects" do
    obj = Eth::Config.new({"riak_kv" => {"storage_backend" => "bitcask"}})
    obj.to_s.must_equal "[{riak_kv, [{storage_backend, bitcask}]}]"
  end

  it "should handle nested objects with strings" do
    v = {"http" => {"__string_127.0.0.1" => 8098}}
    obj = Eth::Config.new(v)
    obj.to_s.must_equal "[{http, [{\"127.0.0.1\", 8098}]}]"
  end

  it "should handle nested objects with binaries" do
    v = {"multi_backend_prefix_list" => {"__binary_0b:" => "be_blocks"}}
    obj = Eth::Config.new(v)
    obj.to_s.must_equal "[{multi_backend_prefix_list, [{<<\"0b:\">>, be_blocks}]}]"
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
