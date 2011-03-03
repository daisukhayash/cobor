# -*- coding: utf-8 -*-
require "cobor/type"
require "pp"

module Cobor
  class Record
    def initialize()
    end

    class << self
      def define(contents)
        obj = self.new
        obj.parse_contents(contents)
        obj
      end
    end

    def add(content)
      parse_a_content(content)
      self
    end

    def inspect
      "\#<#{self.class} #{@fields.map {|name, type, size| "#{name.inspect}, #{type.inspect}, #{size.inspect}" }.join(' ')}>"
    end

    def pretty_print q
      q.group(2, "#<#{self.class}", '>') do
        self.fields.each do |field|
          q.group(2) do
            q.breakable
            q.pp field
          end
        end
      end
    end

    def size
      @fields.map{|field| field.bytesize}.inject(0) {|sum, s| sum + s}
    end

    def names
      @fields.map{|field| field.name}
    end
    alias members names

    def values
      @fields.map{|field| field.value}
    end

    def fields
      @fields
    end

    def clear
      @fields.each{|field| field.clear}
      self
    end

    def [](name)
      (field = @fields.find{|field| field.name == name}) and return field.value
      (field = @fields.find{|field| "serial_#{field.name}" == name}) and return field.serial_value
      raise ArgumentError, "no such field: #{name}"
    end

    def []=(name, val)
      (field = @fields.find{|field| field.name == name}) and field.value = val and return val
      (field = @fields.find{|field| "serial_#{field.name}" == name}) and field.serial_value = val and return val
      raise ArgumentError, "no such field: #{name}"
    end

    def read(bytes)
      bytes = StringIO.new(bytes)
      @fields.each{|field| field.read(bytes)}
      self
    end

    def parse_contents(contents)
      contents.map do |content|
        if content.is_a? Array
          content.map do |a_content|
            parse_a_content(a_content)
          end
        else
          [parse_a_content(content)]
        end
      end
    end

    def parse_a_content(a_content)
      return if /\A\s*\*/ =~ a_content
      if /(\S+)\s+PIC\s+(\S+)\s*\(([0-9]+)\)/i =~ a_content
        name = $1.dup
        type = $2.dup.upcase
        size = $3.dup.to_i
        usage = ($&.dup).upcase if /COMP-[35]/i =~ $'
        type << usage if usage
        #p "name=#{name}, type=#{type}, size=#{size}, usage=#{usage}"
        (@cobstruct ||= []).push a_content
        define_field(name, type, size)
      end
    end

    def c_decl
      @fields.map{|field| field.c_decl}
    end

    def serialize
      ret = []
      @fields.each do |field|
        ret << field.serialize
      end
      ret
    end

    private

    def define_field(name, type, size)
      (@fields ||= []).push Field.new(name, type, size)
      define_accessor @fields.last
    end

    def define_accessor(field)
      name = field.name
      instance_eval(<<-End, __FILE__, __LINE__ + 1)
        def #{name}
          self['#{name}']
        end

        def #{name}=(val)
          self['#{name}'] = val
        end

        def serial_#{name}
          self['serial_#{name}']
        end

        def serial_#{name}=(val)
          self['serial_#{name}'] = val
        end
      End
    end
  end

  class Field
    def initialize(name, type, size)
      @@TYPES ||= Types.new
      @name = (name.downcase.gsub(/-/,'_') or name)
      @cname =  /\A[_A-Za-z]/ =~ @name ? @name : 'var'+self.object_id.to_s
      @type = type
      @size = size
      @bytesize = @@TYPES.bytesize(@type, @size)
      @encoder = @@TYPES.encoder(@type)
      @decoder = @@TYPES.decoder(@type)
      clear
    end

    attr_reader :name
    attr_reader :cname
    attr_reader :size
    attr_reader :bytesize

    def clear
      self.value = @@TYPES.default(@type)
    end

    def encode(val)
      if _proc = @encoder
        _proc.call val, @size
      else
        val
      end
    end

    def serialize
      @value or encode self.value
    end

    def decode(val)
      if _proc = @decoder
        _proc.call val
      else
        val
      end
    end
    alias deserialize decode

    def value=(val)
      @value = encode val
    end

    def value
      decode @value
    end

    def serial_value
      @value
    end

    def serial_value=(val)
      @value = val
    end

    def read(bytes)
      @value = []
      @bytesize.times do
        @value << bytes.getc
      end
    end

    def c_decl
      ctype = @@TYPES.ctype(@type, @size).to_s
      charlength = ''
      charlength = "[#{@bytesize.to_s}]" if ctype == 'char' and @bytesize > 1
      "#{ctype} #{@cname}#{charlength}"
    end
  end
end

if __FILE__ == $0
  rec = Cobor::Record.define [
       "    03 X1  PIC  X(05)",
       "    03 D1  PIC  9(05)",
       "    03 D2  PIC S9(05)",
       "    03 D3  PIC  9(05) COMP-3",
       "    03 D4  PIC S9(05) COMP-3",
       "    03 D5  PIC  9(05) COMP-5",
       "    03 D6  PIC S9(05) COMP-5",
       "    03 N1  PIC  N(05)"
  ]
  rec.x1 = "ABCDE"
  rec.d1 = 13579
  rec.d2 = 24680
  rec.d3 = 11235
  rec.d4 = 97531
  rec.d5 = 8642
  rec.d6 = 53211
  rec.n1 = "あかさたな"
  puts rec.c_decl
  pp rec.fields
  p rec.serialize
end
