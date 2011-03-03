# -*- coding: utf-8 -*-
require 'nkf'

module Cobor
  class Types

    PD_BIT = 'F'
    PD_POSITIVE = 'C'
    PD_NEGATIVE = 'D'
    ZONE_BIT = '3'
    ZONE_POSITIVE = '4'
    ZONE_NEGATIVE = '5'

    WAVEDASH_TABLE = {"1222"=>"0DFF"}

    class << self
      def num_to_pd n, size, signed=false
        bit = (if signed
                 if n >= 0
                   PD_POSITIVE
                 else
                   PD_NEGATIVE
                 end
               else
                 PD_BIT
               end)
        ret = '0' * (size - n.to_s.size).abs + n.to_s + bit
        ret = '0' + ret if ret.length.modulo(2) > 0
        ret = ret.split(/([0-9#{PD_BIT+PD_POSITIVE+PD_NEGATIVE}]{2})/).delete_if{|val| val == ""}.map{|val| val[0,1].hex*16**1 + val[1,1].hex*16**0}
        ret
      end

      def pd_to_num n
        ret = n
        ret = ret.map{|c| (c/16**1).abs.to_s(16)+(c.modulo(16**1)).abs.to_s(16)}.join('')
        ret = ret.chop.to_i * ((/#{PD_NEGATIVE}\z/i =~ ret) ? -1 : 1)
        ret
      end

      def num_to_upd n, size, signed=false
        bit = (if signed
                 if n >= 0
                   ZONE_POSITIVE
                 else
                   ZONE_NEGATIVE
                 end
               else
                 ZONE_BIT
               end)
        zones = ZONE_BIT * (size - 1) + bit
        ret = ('0' * size + n.abs.to_s)[-size, size]
        ret = zones.split(//).zip ret.split(//)
        ret = ret.map{|zone, val| zone.hex*16**1 + val.hex*16**0}
        ret
      end

      def upd_to_num n
        ret = n.map{|val| val.to_i.divmod(16)}
        ret = ret.map{|zone, val| val.to_s}.join('').to_i * (ret.last.first.to_s == ZONE_NEGATIVE ? -1 : 1)
        ret
      end

      def wave_dash str
        ret = str.dup
        ([4] * (str.length / 4)).map { |byte|
          nc = ret.slice!(0, byte)
          WAVEDASH_TABLE[nc] or nc
        }.join('')
      end

      def sjis_to_ucs2le str
        ret = NKF.nkf('-w16L0 -S -x -cp932', str)
        ret = [wave_dash(ret.unpack('H*')[0])].pack('H*')
        ret = ret.unpack('c*')
      end

      def ucs2le_to_sjis str
        ret = str.pack('c*')
        ret = NKF.nkf('-s -W16L0 -x -cp932', ret)
      end

      def str_to_nchar str, size
        ret = str
        paddingsize = size - str.split(//).size
        ret = ret + '　' * paddingsize if paddingsize > 0
        sjis_to_ucs2le ret
      end

      def nchar_to_str str
        ucs2le_to_sjis str
      end

      def d_to_n num, base, ary=[]
        #puts "d_to_n num=#{num} ary=#{ary.join(', ')}"
        #raise ArgumentError if num < 0
        raise ArgumentError if num.round != num
        quotient, remainder = num.divmod(base)
        #puts "quotient=#{quotient} remainder=#{remainder}"
        ary << remainder
        if quotient == 0 or quotient == -1
          return ary
        end
        d_to_n(quotient, base, ary)
      end

      def num_to_comp5 n, size
        width = case size
                when 1..4; 2*2
                when 5..9; 4*2
                when 10..18; 8*2
                else; 8*2
                end
        [sprintf("%.#{width}x", n)].pack('H*').unpack('c*').reverse
      end

      def comp5_to_num n
        ret = n.reverse.pack('c*').unpack('H*').first.hex
        if n.last[0] == 1
          ret -= 256**n.size
        end
        ret
      end
    end

    @@TYPES = [
               # FORMAT:
               # ['type name', ctype, default-value, bytesize-computing_method, charcountencoding_method, decoding_method]
               ['X',
                :char,
                nil,
                proc{|str, size| str = (str + ' ' * (size))[0, size]; str.unpack('c*')},
                proc{|str| (str.is_a?(Array) ? str : [str]).pack('c*')},
                ' '],
               ['9',
                :char,
                nil,
                proc{|val, size| num_to_upd(val.abs, size)},
                proc{|val| upd_to_num(val)},
                0],
               ['S9',
                :char,
                nil,
                proc{|val, size| num_to_upd(val, size, :signed)},
                proc{|val| upd_to_num(val)},
                0],
               ['9COMP-3',
                :char,
                proc{|n| (n.modulo(2) > 0) ? (n+1)/2 : (n+2)/2},
                proc{|val, size| num_to_pd(val.abs, size)},
                proc{|val| pd_to_num(val)},
                0],
               ['S9COMP-3',
                :char,
                proc{|n| (n.modulo(2) > 0) ? (n+1)/2 : (n+2)/2},
                proc{|val, size| num_to_pd(val, size, :signed)},
                proc{|val| pd_to_num(val)},
                0],
               ['9COMP-5',
                #proc{|n| case n
                #         when 1..4; :short
                #         when 5..9; :int
                #         when 10..18; :long
                #         else; :long
                #         end
                #},
                :char,
                proc{|n| case n
                         when 1..4; 2
                         when 5..9; 4
                         when 10..18; 8
                         else; 8
                         end
                },
                proc{|val, size| num_to_comp5(val, size)},
                proc{|val| comp5_to_num(val)},
                0],
               ['S9COMP-5',
                #proc{|n| case n
                #         when 1..4; :short
                #         when 5..9; :int
                #         when 10..18; :long
                #         else; :long
                #         end
                #},
                :char,
                proc{|n| case n
                         when 1..4; 2
                         when 5..9; 4
                         when 10..18; 8
                         else; 8
                         end
                },
                proc{|val, size| num_to_comp5(val, size)},
                proc{|val| comp5_to_num(val)},
                0],
               ['N',
                :char,
                proc{|n| n*2},
                proc{|str, size| str_to_nchar(str, size)},
                proc{|str| nchar_to_str(str)},
                '　'],
              ]

    def ctype(type, size)
      if (cty = @@TYPES.assoc(type)[1]).instance_of? Proc
        cty.call(size)
      else
        cty
      end
    end

    def bytesize(type, size)
      if _proc = @@TYPES.assoc(type)[2]
        _proc.call(size)
      else
        size
      end
    end

    def encoder(type)
      @@TYPES.assoc(type)[3]
    end

    def decoder(type)
      @@TYPES.assoc(type)[4]
    end

    #def default(type, size)
    #  val = @@TYPES.assoc(type)[5] * size
    #  val = encoder(type).call(val, size) if encoder(type)
    #  val
    #end

    def default(type)
      @@TYPES.assoc(type)[5]
    end
  end
end

if __FILE__ == $0
  p str = 'あかさたなはまやらわ'
  p nc = Cobor::Types.str_to_nchar(str, 10)
  p nc.pack('c*').unpack('H*')
  p Cobor::Types.nchar_to_str(nc)
end
