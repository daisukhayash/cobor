# -*- coding: utf-8 -*-
require 'cobor/type'
require 'stringio'
require 'pp'

module Cobor
  class Record
    def initialize()
      @fields = []
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

    def field(name)
      @fields.find do |field|
        if field.is_a? FieldArray
          field.name == name or "serial_#{field.name}" == name
        else
          field.name == name or "serial_#{field.name}" == name
        end
      end
    end

    def fields
      @fields.flatten.sort { |a, b| a.offset <=> b.offset }
    end

    def size
      fields.inject(0) { |sum, field| sum + field.bytesize }
    end

    def names
      fields.map { |field| field.uname }
    end

    def values
      fields.map { |field| field.value }
    end

    def clear
      fields.each{ |field| field.clear }
      self
    end

    def [](name)
      if (f = field(name))
        if f.is_a? FieldArray
          return (/serial_/ =~ name ? f.serial : f.normal)
        else
          return (/serial_/ =~ name ? f.serial_value : f.value)
        end
      end
      raise ArgumentError, "no such field: #{name}"
    end

    def []=(name, val)
      if (f = field(name))
        if f.is_a? FieldArray
          return f
        else
          (/serial_/ =~ name ? f.serial_value = val : f.value = val) and
            return val
        end
      end
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
      @offset = 0
      @frame = Frame.parse(contents)
      define_ @frame
    end

    def frame
      @frame
    end

    def define_ frame, suffix=''
      frame.repeat.times do |repeat|
        suf = frame.repeat > 1 ? "#{suffix}_#{repeat}" : suffix
        define_accessor "#{frame.name}#{suf}"
        define_accessor frame.name unless self.respond_to? frame.name
        name = "#{frame.name}#{suf}"
        frame.stepup do |upper|
          define_accessor (name = "#{name}_OF_#{upper.name}")
        end
        unless frame.type.nil?
          field = define_field frame.name, suf, frame.type, frame.size
        end
        frame.members.each do |member|
          define_ member, suf
        end
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

    def define_field name, suffix, type, size
      field = Field.new(name, suffix, type, size, @offset)
      @offset += field.bytesize
      @fields.each_with_index do |obj, i|
        if obj.is_a? FieldArray
          next unless obj.name == name
          obj << field
          return field
        else
          next unless obj.name == name
          @fields[i] = FieldArray.new([obj, field])
          return field
        end
      end
      @fields << field
      return field
    end

    def define_accessor name, repeat=0
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

  class Frame
    attr_accessor :level, :name, :repeat, :members, :upper, :type, :size

    def initialize definition
      if /\s*([0-9]{2})\s+([^\s.]+)(?:\s.*OCCURS\s+([0-9]+))?/i =~ definition
        @level = $1.to_i
        @name = $2.dup
        @repeat = $3.nil? ? 1 : $3.to_i
        @upper = nil
        if /PIC\s+(\S+)\s*\(([0-9]+)\)/i =~ definition
          @type = $1.upcase
          @size = $2.to_i
          usage = $&.upcase if /COMP-[35]/i =~ $'
          @type << usage if usage
        end
      else
        raise ArgumentError
      end
      @members = []
    end

    def self.parse definition
      top = prev = Frame.new("00 TOP.")
      definition.each do |d|
        next if /\A\s*\*/ =~ d
        current = Frame.new d
        current.upper = current.find_upper prev
        current.upper.members << current
        prev = current
      end
      return top
    end

    def find_upper obj
      if obj.level == level
        return obj.upper
      elsif obj.level < level
        return obj
      elsif obj.level > level
        find_upper obj.upper
      end
    end

    def each
      yield self
      members.each do |mem|
        mem.each
      end
    end

    def stepup
      f = self
      until (f = f.upper).nil?
        yield f
      end
    end
  end

  class FieldArray < Array
    def normal
      @value_type = :normal
      self
    end

    def serial
      @value_type = :serial
      self
    end

    def []=(index, val)
      field = self.at(index)
      @value_type == :serial ? field.serial_value  = val : field.value = val
      val
    end

    def [](index)
      field = self.at(index)
      @value_type == :serial ? field.serial_value : field.value
    end

    def name
      self.first.name
    end

    def bytesize
      self.inject(0){ |sum, field| sum + field.bytesize }
    end

    def value
      self.map { |field| field.value }
    end

    def serialize
      self.map { |field| field.serialize }
    end
  end

  class Field
    def initialize name, suffix, type, size, offset
      @@TYPE ||= Type
      @name = (name.gsub(/-/,'_') or name)
      @uname = @name + (suffix.size>0 ? suffix : '')
      @cname = Field.create_cname(@uname)
      @type = type
      @size = size
      @offset = offset

      raise ArgumentError unless @@TYPE.include? type

      @bytesize = @@TYPE.bytesize @type, @size
      @encoder = @@TYPE.encoder @type
      @decoder = @@TYPE.decoder @type
      clear
    end

    attr_reader :name
    attr_reader :uname
    attr_reader :cname
    attr_reader :size
    attr_reader :bytesize
    attr_reader :offset

    def self.create_cname str
      /\A[_A-Za-z]/ =~ str ? str : 'var' + str.unpack('H*').first
    end
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
