# -*- coding: utf-8 -*-
require 'nkf'

module Cobor
  class Type

    SIGNED = true
    UNSIGNED = false

    PD_NIBBLE = 'F'
    PD_POSITIVE = 'C'
    PD_NEGATIVE = 'D'
    ZONE_NIBBLE = '3'
    ZONE_POSITIVE = '4'
    ZONE_NEGATIVE = '5'

    WAVEDASH_TABLE = {"1222"=>"0DFF"}

    class << self
      def num_to_pd n, size, signed=UNSIGNED
        n = n.to_i
        nibble = (if signed
                    if n >= 0
                      PD_POSITIVE
                    else
                      PD_NEGATIVE
                    end
                  else
                    PD_NIBBLE
                  end)
        n = n.abs
        n = n % 10 ** size
        ret = '0' * (size - n.to_s.size).abs + n.to_s + nibble
        ret = '0' + ret if ret.length.modulo(2) > 0
        ret = ret.split(/([0-9#{PD_NIBBLE+PD_POSITIVE+PD_NEGATIVE}]{2})/).delete_if{|val| val == ""}.map{|val| val[0,1].hex*16**1 + val[1,1].hex*16**0}
        ret
      end

      def pd_to_num n
        ret = n
        ret = ret.map{|c| (c/16**1).abs.to_s(16)+(c.modulo(16**1)).abs.to_s(16)}.join('')
        ret = ret.chop.to_i * ((/#{PD_NEGATIVE}\z/i =~ ret) ? -1 : 1)
        ret
      end

      def num_to_zd n, size, signed=UNSIGNED
        n = n.to_i
        bit = (if signed
                 if n >= 0
                   ZONE_POSITIVE
                 else
                   ZONE_NEGATIVE
                 end
               else
                 ZONE_NIBBLE
               end)
        n = n.abs
        zones = ZONE_NIBBLE * (size - 1) + bit
        ret = ('0' * size + n.to_s)[-size, size]
        ret = zones.split(//).zip ret.split(//)
        ret = ret.map{|zone, val| zone.hex*16**1 + val.hex*16**0}
        ret
      end

      def zd_to_num n
        ret = n.map{|val| val.to_i.divmod(16)}
        ret = ret.map{|zone, val| val.to_s}.join('').to_i * (ret.last.first.to_s == ZONE_NEGATIVE ? -1 : 1)
        ret
      end

      def num_to_comp5 n, size, signed=UNSIGNED
        n = n.to_i
        n = n.abs % 10 ** size * ((signed and n < 0) ? -1 : 1)
        width = case size
                when 1..4; 2*2
                when 5..9; 4*2
                when 10..18; 8*2
                else; 8*2
                end
        ret = [sprintf("%.#{width}x", n)].pack('H*').unpack('c*').reverse
      end

      def comp5_to_num n
        ret = n.reverse.pack('c*').unpack('H*').first.hex
        # if n.last[0] == 1
        if n.last < 0
          ret -= 256**n.size
        end
        ret
      end

      def wave_dash str
        ret = str.dup
        ([4] * (str.length / 4)).map { |byte|
          nc = ret.slice!(0, byte)
          WAVEDASH_TABLE[nc] or nc
        }.join('')
      end

      KCODE2NKFOPT = {'UTF8' => 'w', 'SJIS' => 's', 'NONE' => 'w'}
      def external_opt
        KCODE2NKFOPT[$KCODE]
      end

      def n_whitespace
        NKF.nkf("-#{external_opt} -W -x", [0x3000].pack("U*"))
      end

      def n_charcount str
        $KCODE = 'UTF8' if $KCODE == 'NONE'
        str.split(//).size
      end

      def external_to_ucs2le str
        ret = NKF.nkf("-w16L0 -#{external_opt.upcase} -x", str)
        ret = ret.unpack('c*')
      end

      def ucs2le_to_external str
        ret = str.pack('c*')
        ret = NKF.nkf("-#{external_opt} -W16L0 -x", ret)
      end

      def str_to_nchar str, size
        ret = str.to_s
        paddingsize = size - n_charcount(ret)
        ret = ret + n_whitespace * paddingsize if paddingsize > 0
        external_to_ucs2le(ret)[0, bytesize('N', size)]
      end

      def nchar_to_str str
        ucs2le_to_external(str).gsub(/#{n_whitespace}*\z/, '')
      end

      def include? type
        @@TYPES.assoc(type) ? true : false
      end

      def ctype type, size
        if (cty = @@TYPES.assoc(type)[1]).instance_of? Proc
          cty.call(size)
        else
          cty
        end
      end

      def bytesize type, size
        if _proc = @@TYPES.assoc(type)[2]
          _proc.call(size)
        else
          size
        end
      end

      def decoder type
        @@TYPES.assoc(type)[3]
      end

      def encoder type
        @@TYPES.assoc(type)[4]
      end

      def default type
        @@TYPES.assoc(type)[5]
      end
    end

    @@TYPES = [
      # FORMAT:
      # ['type name',
      #  ctype,
      #  bytesize-computing_method,
      #  charcount,
      #  decoding_method,
      #  encoding_method,
      #  default-value]
      ['X',
        :char,
        nil,
        lambda{|str, size| str = (str.to_s + ' ' * size)[0, size]; str.unpack('c*')},
        lambda{|str| (str.is_a?(Array) ? str : [str]).pack('c*').rstrip},
        ' '],
      ['9',
        :char,
        nil,
        lambda{|val, size| num_to_zd(val, size)},
        lambda{|val| zd_to_num(val)},
        0],
      ['S9',
        :char,
        nil,
        lambda{|val, size| num_to_zd(val, size, SIGNED)},
        lambda{|val| zd_to_num(val)},
        0],
      ['9COMP-3',
        :char,
        lambda{|n| (n.modulo(2) > 0) ? (n+1)/2 : (n+2)/2},
        lambda{|val, size| num_to_pd(val, size)},
        lambda{|val| pd_to_num(val)},
        0],
      ['S9COMP-3',
        :char,
        lambda{|n| (n.modulo(2) > 0) ? (n+1)/2 : (n+2)/2},
        lambda{|val, size| num_to_pd(val, size, SIGNED)},
        lambda{|val| pd_to_num(val)},
        0],
      ['9COMP-5',
        #lambda{|n| case n
        #         when 1..4; :short
        #         when 5..9; :int
        #         when 10..18; :long
        #         else; :long
        #         end
        #},
        :char,
        lambda{|n| case n
                 when 1..4; 2
                 when 5..9; 4
                 when 10..18; 8
                 else; 8
                 end
        },
        lambda{|val, size| num_to_comp5(val, size)},
        lambda{|val| comp5_to_num(val)},
        0],
      ['S9COMP-5',
        #lambda{|n| case n
        #         when 1..4; :short
        #         when 5..9; :int
        #         when 10..18; :long
        #         else; :long
        #         end
        #},
        :char,
        lambda{|n| case n
                 when 1..4; 2
                 when 5..9; 4
                 when 10..18; 8
                 else; 8
                 end
        },
        lambda{|val, size| num_to_comp5(val, size, SIGNED)},
        lambda{|val| comp5_to_num(val)},
        0],
      ['N',
        :char,
        lambda{|n| n*2},
        lambda{|str, size| str_to_nchar(str, size)},
        lambda{|str| nchar_to_str(str)},
        n_whitespace],
    ]

  end
end
