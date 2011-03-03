# -*- coding: utf-8 -*-
require 'cobor/record'
require 'pp'

module Cobor
  class RecordSet

    MAX = 100

    def RecordSet.open path, record, mode = 'r'
      recset = new(path, record, mode)
      if block_given?
        begin
          return yield(recset)
        ensure
          recset.close
        end
      else
        recset
      end
    end

    def initialize path, record, mode = 'r'
      @record = record
      @idx = 0
      case mode
      when 'r'
        @f = File.open(path, 'rb+')
        set_current 0
      when 'c'
        @f = File.open(path, 'wb+')
      when 'a'
        @f = File.open(path, 'ab+')
      else
        raise ArgumentError, "invalid open mode: #{mode.inspect}"
      end
    end

    def close
      buffer.each do |rec|
        write rec
      end
      @f.close
    end

    def size
      File.size @f
    end

    def record_count
      size / @record.size
    end

    def read
      @current = @record.read(@f.read(@record.size))
    end

    def eof?
      @idx * @record.size >= size
    end

    def empty?
      size == 0
    end

    def current
      return nil if empty?
      return nil if eof?
      @current
    end

    def first
      return nil if empty?
      set_current 0
      self
    end

    def next
      return nil if eof?
      set_current @idx + 1
      self
    end

    def prev
      return nil if @idx == 0
      set_current @idx - 1
      self
    end

    def each_record
      until eof?
        yield current
        self.next
      end
    end
    alias each each_record

    def write record = @record
      if record.is_a? Cobor::Record
        #record.serialize.flatten.each do |ch|
        #  @f.putc ch
        #end
        @f.write record.serialize.flatten.pack('C*')
      end
    end

    def buffer
      @buffer ||= []
    end

    def clear_buffer
      @buffer = []
    end

    def write_buffering record = @record, n = MAX
      if record.is_a? Cobor::Record
        if buffer.empty?
          @buffer_count = n
          p @buffer_count
        end
        buffer.push record
        @buffer_count -= 1
        if @buffer_count <= 0
          buffer.each do |rec|
            write rec
          end
          clear_buffer
        end
      end
    end

    def nth n
      if @idx == n - 1
        @record
      else
        set_current n - 1
        current
      end
    end

    def bsearch key, value
      top = -1
      bottom = record_count + 1
      mid = (top + bottom) / 2

      while bottom - top > 1
        raise Error unless nth mid

        #p "top=#{top}, bot=#{bottom}, mid=#{mid}, key=#{nth(mid)[key]}"
        if nth(mid)[key] >= value
          bottom = mid
        else
          top = mid
        end
        mid = (top + bottom) / 2
      end

      until eof? or current[key] != value
        yield current
        self.next
      end
    end

    private

    def set_current idx
      @idx = idx
      seek idx
      read
    end

    def seek idx
      @f.seek(pos(@idx), File::SEEK_SET)
    end

    def pos idx
      @record.size * idx
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

  Cobor::RecordSet.open('./out.dat', rec, 'a') do |rs|
    rec.x1 = 'test'
    rec.d1 = 1
    rec.d2 = -2
    rec.d3 = 3
    rec.d4 = -4
    rec.d5 = 5
    rec.d6 = -6
    rec.n1 = 'テスト'
    rs.write
  end

  Cobor::RecordSet.open('./out.dat', rec).each do |record|
    p "x1=" + record.x1.to_s
    p "d1=" + record.d1.to_s
    p "d2=" + record.d2.to_s
    p "d3=" + record.d3.to_s
    p "d4=" + record.d4.to_s
    p "d5=" + record.d5.to_s
    p "d6=" + record.d6.to_s
    p "n1=" + record.n1.to_s
  end

end
