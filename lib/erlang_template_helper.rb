require "erlang_template_helper/version"

module Eth
  module InstanceMethods
    def convert(value)
      case value
      when ::String
        Eth::String.new(value)
      when ::Array
        Eth::Array.new(value)
      when ::Hash
        Eth::Hash.new(value)
      when ::TrueClass
        Eth::Value.new("true")
      when ::FalseClass
        Eth::Value.new("false")
      when ::NilClass
        Eth::Value.new("null")
      else
        Eth::Value.new(value)
      end
    end

    def indent(level)
      "\t" * level
    end
  end

  class Value
    def initialize(val)
      @val = val
    end

    def to_s
      @val
    end

    def pp(level=0)
      to_s
    end
  end

  class String
    def initialize(str)
      @str = str
    end

    def to_s
      case @str
      when /^__atom_(.*)/
        "'#{$1}'"
      when /^__binary_(.*)/
        "<<\"#{$1}\">>"
      when /^__string_(.*)/
        "\"#{$1}\""
      else
        @str
      end
    end

    def pp(level=0)
      to_s
    end
  end

  class Array
    include InstanceMethods

    def initialize(arr)
      case arr[0]
      when "__list"
        @type = :list
        @values = arr[1..-1].map { |e| convert(e) }
      when "__tuple"
        @type = :tuple
        @values = arr[1..-1].map { |e| convert(e) }
      else
        @type = :list
        @values = arr.map { |e| convert(e) }
      end
    end

    def to_s
      values1 = @values.map { |v| v.to_s }
      case @type
      when :tuple
        "{#{values1.join(", ")}}"
      else
        "[#{values1.join(", ")}]"
      end
    end

    def pp(level=0)
      case @type
      when :tuple
        values1 = @values.map { |v| v.pp(level+1) }
        "{#{values1.join(", ")}}"
      else
        values1 = @values.map { |v| "\n#{indent(level+1)}#{v.pp(level+1)}" }
        "[#{values1.join(", ")}\n#{indent(level)}]"
      end
    end
  end

  class Hash
    include InstanceMethods

    def initialize(hsh)
      @hsh = {}
      hsh.map do |k,v|
        k1 = Eth::String.new(k.to_s)
        v1 = convert(v)
        @hsh[k1] = v1
      end
    end

    def to_s
      values = @hsh.map do |k,v|
        "{#{k.to_s}, #{v.to_s}}"
      end
      "[#{values.join(", ")}]"
    end

    def pp(level=0)
      values = @hsh.map do |k,v|
        "\n#{indent(level+1)}{#{k.to_s}, #{v.pp(level+1)}}"
      end
      "[#{values.join(", ")}\n#{indent(level)}]"
    end
  end

  class Config
    include InstanceMethods

    def initialize(value)
      @config = convert(value)
    end

    def to_s
      @config.to_s + "."
    end

    def pp(level=0)
      @config.pp(level) + "."
    end
  end
end
