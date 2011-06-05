# -*- coding: utf-8 -*-
require 'nkf'

def chars_to_hex_string var
  var.pack('c*').unpack('H*').first
end

def hex_string_to_chars var
  [var].pack('H*').unpack('c*')
end

OPT = { 'UTF8' => 'w', 'SJIS' => 's'}
def encoding_to_default str
  NKF.nkf("-#{OPT[$KCODE]}", str)
end

RSpec::Matchers.define :eql_to_hex_string do |expected|
  match do |actual|
    chars_to_hex_string(actual) == expected
  end
  failure_message_for_should do |actual|
    "expected that [#{actual.join(', ')}] (#{chars_to_hex_string(actual)}) would equal to #{expected} in hex string"
  end
  failure_message_for_should_not do |actual|
    "expected that [#{actual.join(', ')}] (#{chars_to_hex_string(actual)}) would not equal to #{expected} in hex string"
  end
  description do
    "equal to hex string"
  end
end

RSpec::Matchers.define :eql_in_encoding do |expected|
  match do |actual|
    actual == encoding_to_default(expected)
  end
  failure_message_for_should do |actual|
    <<-"EOT"
expected #{expected.inspect}
     got #{actual.inspect}

(compared using ==)
    EOT
  end
  failure_message_for_should_not do |actual|
    <<-"EOT"
expected #{expected.inspect} not to equal #{actual.inspect}

(compared using ==)
    EOT
  end
  description do
    "equal to hex string"
  end
end
