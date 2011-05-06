# -*- coding: utf-8 -*-
require 'cobor/type'
require 'stringio'
require 'pp'

module Cobor
  class Record
    def initialize()
      @data = nil
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
        fields.each do |field|
          q.group(2) do
            q.breakable
            q.pp field
          end
        end
      end
    end

    def size
      fields.inject(0){|sum, field| sum + field.bytesize}
    end

    def names
      fields.map{|field| field.name}
    end

    def values
      fields.map{|field| field.value}
    end

    def field(name)
      fields.find{|field| field.name == name} or fields.find{|field| "serial_#{field.name}" == name}
    end

    def fields
      @fields ||= []
    end

    def clear
      fields.each{|field| field.clear}
      self
    end

    def [](name)
      (field = field(name)) and return (/serial_/ =~ name ? field.serial_value : field.value)
      raise ArgumentError, "no such field: #{name}"
    end

    def []=(name, val)
      (field = field(name)) and
        (/serial_/ =~ name ? field.serial_value = val : field.value = val) and
        return val
      raise ArgumentError, "no such field: #{name}"
    end

    def read(bytes)
      if bytes
        bytes = StringIO.new(bytes)
        fields.each{|field| field.read(bytes)}
      end
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
      fields.map{|field| field.c_decl}
    end

    def serialize
      ret = []
      fields.each do |field|
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
    def initialize name, type, size
      @@TYPE ||= Type
      @name = (name.gsub(/-/,'_') or name)
      @cname =  /\A[_A-Za-z]/ =~ @name ? @name : 'var'+self.object_id.to_s
      @type = type
      @size = size

      raise ArgumentError unless @@TYPE.include? type

      @bytesize = @@TYPE.bytesize @type, @size
      @encoder = @@TYPE.encoder @type
      @decoder = @@TYPE.decoder @type
      clear
    end

    attr_reader :name
    attr_reader :cname
    attr_reader :size
    attr_reader :bytesize

    def fieldtype
      @type
    end

    def clear
      self.value = @@TYPE.default @type
    end

    def serialize
      @value or decode self.value
    end

    def value= val
      @value = decode val
    end

    def value
      encode @value
    end

    def serial_value
      @value
    end

    def serial_value= val
      @value = val
    end

    def read bytes
      @value = []
      @bytesize.times do
        @value << bytes.getc
      end
    end

    def c_decl
      ctype = @@TYPE.ctype(@type, @size).to_s
      charlength = ''
      charlength = "[#{@bytesize.to_s}]" if ctype == 'char' and @bytesize > 1
      "#{ctype} #{@cname}#{charlength}"
    end

    private

    def decode(val)
      if _proc = @decoder
        _proc.call val, @size
      else
        val
      end
    end

    def encode(val)
      if _proc = @encoder
        _proc.call val
      else
        val
      end
    end
  end
end
