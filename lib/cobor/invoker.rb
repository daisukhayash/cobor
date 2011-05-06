# -*- coding: utf-8 -*-
require 'dl/import'
require 'dl/struct'
require 'cobor/record'
require 'pp'

module Cobor
  module Invoker
    extend DL::Importable

    def self.call obj, *record
      raise ArgumentError, "no such object: #{obj}" unless File.exists? obj
      dlload obj
      id = File.basename obj, '.dll'
      decap = id.upcase.capitalize.swapcase
      args = record.map do |rec|
        arg = struct(rec.c_decl).malloc
        rec.fields.each do |field|
          name = field.name
          cname = field.cname
          instance_eval(<<-End, __FILE__, __LINE__ + 1)
            arg.#{cname} = rec.serial_#{name}
          End
        end
        arg
      end

      #pp args.map{|arg| arg.to_ptr}
      extern "void #{id}(#{(['void*'] * args.size).join(', ')})"
      send(decap, *args.map{|arg| arg.to_ptr})

      #pp args.map{|arg| arg.to_ptr}
      record.each_with_index do |rec, idx|
        rec.fields.each do |field|
          name = field.name
          cname = field.cname
          instance_eval(<<-End, __FILE__, __LINE__ + 1)
            rec.serial_#{name} = args[idx].#{cname}
          End
        end
      end

    end
  end
end
if __FILE__ == $0

  ENV['@CBR_CONSOLE'] = 'SYSTEM'

  rec1 = Cobor::Record.define [
           '03 CNT1-1 PIC S9(5) COMP-5.',
           '03 CNT1-2 PIC 9(4) COMP-3.',
           '03 MSG1 PIC X(20).',
           '03 CNT1-3 PIC 9(5).',
           '03 NMSG1 PIC N(10).'
                              ]
  rec2 = Cobor::Record.define [
           '03 カウンタ二の一 PIC 9(5) COMP-5.',
           '03 メッセージ PIC X(20).',
           '03 カウンタ二の二 PIC 9(5) COMP-3.',
           '03 CNT2-3 PIC S9(5).',
           '03 CNT2-4 PIC S9(5) COMP-3.',
           '03 CNT2-5 PIC S9(5) COMP-5.',
           '03 MSG3 PIC X(5).',
           '03 CNT2-6 PIC S9(5) COMP-5.',
           '03 MSG4 PIC X(15).'
                              ]

  puts rec1.c_decl
  puts rec2.c_decl

  puts "初期値----------------"
  p rec1.cnt1_1
  p rec1.cnt1_2
  p rec1.msg1
  p rec1.cnt1_3
  p rec1.nmsg1
  p rec2.カウンタ二の一
  p rec2.メッセージ
  p rec2.カウンタ二の二
  p rec2.cnt2_3
  p rec2.cnt2_4
  p rec2.cnt2_5
  p rec2.msg3
  p rec2.cnt2_6
  p rec2.msg4
  puts "初期値終わり----------"

  puts "設定開始--------------"
  rec1.cnt1_1 = -135
  rec1.cnt1_2 = 40
  rec1.msg1 = "hello"
  rec1.cnt1_3 = 100
  rec1.nmsg1 = "あいうえお"
  rec2.カウンタ二の一 = 20
  rec2.メッセージ = "12345678901234567890"
  rec2.カウンタ二の二 = 12345
  rec2.cnt2_3 = -9876543
  rec2.cnt2_4 = -245
  rec2.cnt2_5 = 2
  rec2.msg3 = "abcdefg"
  rec2.cnt2_6 = 30
  rec2.msg4 = "xyz"
  puts "設定終了--------------"

  puts "表示開始--------------"
  p rec1.cnt1_1
  p rec1.cnt1_2
  p rec1.msg1
  p rec1.cnt1_3
  p rec1.nmsg1
  p rec2.カウンタ二の一
  p rec2.メッセージ
  p rec2.カウンタ二の二
  p rec2.cnt2_3
  p rec2.cnt2_4
  p rec2.cnt2_5
  p rec2.msg3
  p rec2.cnt2_6
  p rec2.msg4
  puts "表示終了--------------"

  puts "-----------------------------"
  Cobor::Invoker.call('./cob/TESTCOB01.dll', rec1, rec2)
  puts "-----------------------------"
  p rec1.cnt1_1
  p rec1.cnt1_2
  p rec1.msg1
  p rec1.cnt1_3
  p rec1.nmsg1
  p rec2.カウンタ二の一
  p rec2.メッセージ
  p rec2.カウンタ二の二
  p rec2.cnt2_3
  p rec2.cnt2_4
  p rec2.cnt2_5
  p rec2.msg3
  p rec2.cnt2_6
  p rec2.msg4

  exit

  puts "-----------------------------"
  Cobor::Invoker.call('./cob/TESTCOB01.dll', rec1, rec2)
  puts "-----------------------------"

  p rec1.cnt1_1
  p rec1.cnt1_2
  p rec1.msg1
  p rec1.cnt1_3
  p rec1.nmsg1
  p rec2['カウンタ二の一']
  p rec2['メッセージ']
  p rec2['カウンタ二の二']
  p rec2['cnt2_3']
  p rec2['cnt2_4']

end
