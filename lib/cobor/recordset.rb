# -*- coding: utf-8 -*-
require 'cobor/record'
require 'pp'

module Cobor
  class RecordSet
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
      (@idx + 1) >= record_count
    end

    def empty?
      size == 0
    end

    def current
      return nil if empty?
      @current
    end

    def first
      return nil if empty?
      set_current 0
      @current
    end

    def next
      return nil if eof?
      set_current @idx + 1
      @current
    end

    def prev
      return nil if @idx == 0
      set_current @idx - 1
      @current
    end

    def each_record
      yield self.first unless empty?
      until eof?
        yield self.next
      end
    end
    alias each each_record

    def write record = @record
      if record.is_a? Cobor::Record
        @f.write record.serialize.flatten.pack('C*')
      end
    end

    def nth n
      set_current n - 1 unless @idx == n - 1
      current
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
      seek(@idx) and read
    end

    def seek idx
      return false if (pos = pos(idx)) > size
      return @f.seek(pos, File::SEEK_SET)
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
       "    03 N1  PIC  N(10)"
  ]

  Cobor::RecordSet.open('./out.dat', rec, 'c') do |rs|
    rec.X1 = 'test1'
    rec.D1 = 1
    rec.D2 = -2
    rec.D3 = 3
    rec.D4 = -4
    rec.D5 = 5
    rec.D6 = -6
    rec.N1 = 'テスト１'
    rs.write

    rec.X1 = 'test2'
    rec.D1 = 10
    rec.D2 = -20
    rec.D3 = 30
    rec.D4 = -4
    rec.D5 = 50
    rec.D6 = -60
    rec.N1 = 'テスト２'
    rs.write
  end

  Cobor::RecordSet.open('./out.dat', rec).each do |record|
    puts "X1=" + record.X1.to_s
    puts "D1=" + record.D1.to_s
    puts "D2=" + record.D2.to_s
    puts "D3=" + record.D3.to_s
    puts "D4=" + record.D4.to_s
    puts "D5=" + record.D5.to_s
    puts "D6=" + record.D6.to_s
    puts "N1=" + record.N1.to_s
    puts ""
  end

  Cobor::RecordSet.open('./zero.dat', rec).each do |record|
    puts "X1=" + record.X1.to_s
    puts "D1=" + record.D1.to_s
    puts "D2=" + record.D2.to_s
    puts "D3=" + record.D3.to_s
    puts "D4=" + record.D4.to_s
    puts "D5=" + record.D5.to_s
    puts "D6=" + record.D6.to_s
    puts "N1=" + record.N1.to_s
    puts ""
  end

end
