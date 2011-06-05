# -*- coding: utf-8 -*-
require (File.dirname(__FILE__) + '/spec_helper')
require 'cobor/record'

describe Cobor::Field do
  context 'initialized with no arguments' do
    it 'should raise ArgumentError' do
      lambda { Cobor::Field.new }.should raise_error ArgumentError
    end
  end

  context 'when type is invalid' do
    it 'should raise ArgumentError' do
      lambda { Cobor::Field.new 'test', 'A', 10 }.should raise_error ArgumentError
    end
  end

  describe 'clearing value' do
    describe 'X type' do
      subject do
        field = Cobor::Field.new 'test_X', 'X', 10
        field.value = 'testvalue'
        field
      end
      before :each do
        subject.clear
      end

      its(:value) { should eq '' }
      its(:serial_value) { should eq [32, 32, 32, 32, 32, 32, 32, 32, 32, 32] }
    end
    describe '9 type' do
      subject do
        field = Cobor::Field.new 'test_9', '9', 10
        field.value = 12345
        field
      end
      before :each do
        subject.clear
      end

      its(:value) { should eq 0 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 48] }
    end
    describe 'N type' do
      subject do
        field = Cobor::Field.new 'test_N', 'N', 10
        field.value = 'テスト値'
        field
      end
      before :each do
        subject.clear
      end

      its(:value) { should eq '' }
      its(:serial_value) { should eq [0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48] }
    end
  end

  describe 'X type' do
    subject do
      field = Cobor::Field.new 'test_X', 'X', 10
    end
    context 'when first created' do
      its(:name) { should eq 'test_X' }
      its(:cname) { should eq 'test_X' }
      its(:fieldtype) { should eq 'X' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:c_decl) { should eq 'char test_X[10]' }
      its(:value) { should eq '' }
      its(:serial_value) { should eq [32, 32, 32, 32, 32, 32, 32, 32, 32, 32] }
      its(:serialize) { should eq [32, 32, 32, 32, 32, 32, 32, 32, 32, 32] }
    end
    context 'when value is set' do
      before(:all) { subject.value = 'xyz123' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:value) { should eq 'xyz123' }
      its(:serial_value) { should eq [120, 121, 122, 49, 50, 51, 32, 32, 32, 32] }
      its(:serialize) { should eq [120, 121, 122, 49, 50, 51, 32, 32, 32, 32] }
    end
    context 'when overflow' do
      before(:all) { subject.value = 'xyz1234567890' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:value) { should eq 'xyz1234567' }
      its(:serial_value) { should eq [120, 121, 122, 49, 50, 51, 52, 53, 54, 55] }
      its(:serialize) { should eq [120, 121, 122, 49, 50, 51, 52, 53, 54, 55] }
    end
    context 'when numeric value is set' do
      before(:all) { subject.value = 123 }
      it 'should not raise Exception' do
        lambda { subject.value = 123 }.should_not raise_error Exception
      end
      its(:value) { should eq '123' }
      its(:serial_value) { should eq [49, 50, 51, 32, 32, 32, 32, 32, 32, 32] }
    end
    context 'when japanese character value is set' do
      before(:all) { subject.value = 'テスト' }
      it 'should not raise Exception' do
        lambda { subject.value = 'テスト' }.should_not raise_error Exception
      end
      its(:value) { should eq 'テスト' }
      its(:serial_value) { should eq [-29, -125, -122, -29, -126, -71, -29, -125, -120, 32] }
    end
  end

  describe '9 type' do
    subject do
      field = Cobor::Field.new 'test_9', '9', 10
    end
    context 'when first created' do
      its(:name) { should eq 'test_9' }
      its(:cname) { should eq 'test_9' }
      its(:fieldtype) { should eq '9' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:c_decl) { should eq 'char test_9[10]' }
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 48] }
      its(:serialize) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 48] }
    end
    context 'when value is set' do
      before(:all) { subject.value = 1234 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:value) { should eq 1234 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 49, 50, 51, 52] }
      its(:serialize) { should eq [48, 48, 48, 48, 48, 48, 49, 50, 51, 52] }
    end
    context 'when overflow' do
      before(:all) { subject.value = 12345678901 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:value) { should eq 2345678901 }
      its(:serial_value) { should eq [50, 51, 52, 53, 54, 55, 56, 57, 48, 49] }
      its(:serialize) { should eq [50, 51, 52, 53, 54, 55, 56, 57, 48, 49] }
    end
    context 'when negative numeric value is set' do
      before(:all) { subject.value = -123 }
      it 'should not raise Exception' do
        lambda { subject.value = -123 }.should_not raise_error Exception
      end
      its(:value) { should eq 123 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 49, 50, 51] }
    end
    context 'when numeric character value is set' do
      before(:all) { subject.value = '123' }
      it 'should not raise Exception' do
        lambda { subject.value = '123' }.should_not raise_error Exception
      end
      its(:value) { should eq 123 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 49, 50, 51] }
    end
    context 'when non-numeric value is set' do
      before(:all) { subject.value = 'abc' }
      it 'should not raise RuntimeError' do
        lambda { subject.value = 'abc' }.should_not raise_error RuntimeError
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 48] }
    end
    context 'when japanese character value is set' do
      before(:all) { subject.value = 'テスト' }
      it 'should not raise Exception' do
        lambda { subject.value = 'テスト' }.should_not raise_error Exception
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 48] }
    end
  end

  describe 'S9 type' do
    subject do
      field = Cobor::Field.new 'test_S9', 'S9', 10
    end
    context 'when first created' do
      its(:name) { should eq 'test_S9' }
      its(:cname) { should eq 'test_S9' }
      its(:fieldtype) { should eq 'S9' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:c_decl) { should eq 'char test_S9[10]' }
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 64] }
      its(:serialize) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 64] }
    end
    context 'when value is set' do
      before(:all) { subject.value = -1234 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:value) { should eq -1234 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 49, 50, 51, 84] }
      its(:serialize) { should eq [48, 48, 48, 48, 48, 48, 49, 50, 51, 84] }
    end
    context 'when overflow' do
      before(:all) { subject.value = -12345678901 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 10 }
      its(:value) { should eq -2345678901 }
      its(:serial_value) { should eq [50, 51, 52, 53, 54, 55, 56, 57, 48, 81] }
      its(:serialize) { should eq [50, 51, 52, 53, 54, 55, 56, 57, 48, 81] }
    end
    context 'when positive numeric value is set' do
      before(:all) { subject.value = 123 }
      it 'should not raise Exception' do
        lambda { subject.value = 123 }.should_not raise_error Exception
      end
      its(:value) { should eq 123 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 49, 50, 67] }
    end
    context 'when numeric character value is set' do
      before(:all) { subject.value = '-123' }
      it 'should not raise Exception' do
        lambda { subject.value = '-123' }.should_not raise_error Exception
      end
      its(:value) { should eq -123 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 49, 50, 83] }
    end
    context 'when non-numeric value is set' do
      before(:all) { subject.value = 'abc' }
      it 'should not raise RuntimeError' do
        lambda { subject.value = 'abc' }.should_not raise_error RuntimeError
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 64] }
    end
    context 'when japanese character value is set' do
      before(:all) { subject.value = 'テスト' }
      it 'should not raise Exception' do
        lambda { subject.value = 'テスト' }.should_not raise_error Exception
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [48, 48, 48, 48, 48, 48, 48, 48, 48, 64] }
    end
  end

  describe '9 COMP-3 type' do
    subject do
      field = Cobor::Field.new 'test_9COMP-3', '9COMP-3', 10
    end
    context 'when first created' do
      its(:name) { should eq 'test_9COMP_3' }
      its(:cname) { should eq 'test_9COMP_3' }
      its(:fieldtype) { should eq '9COMP-3' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 6 }
      its(:c_decl) { should eq 'char test_9COMP_3[6]' }
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [0, 0, 0, 0, 0, 15] }
      its(:serialize) { should eq [0, 0, 0, 0, 0, 15] }
    end
    context 'when value is set' do
      before(:all) { subject.value = 1234 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 6 }
      its(:value) { should eq 1234 }
      its(:serial_value) { should eq [0, 0, 0, 1, 35, 79] }
      its(:serialize) { should eq [0, 0, 0, 1, 35, 79] }
    end
    context 'when overflow' do
      before(:all) { subject.value = 12345678901 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 6 }
      its(:value) { should eq 2345678901 }
      its(:serial_value) { should eq [2, 52, 86, 120, 144, 31] }
      its(:serialize) { should eq [2, 52, 86, 120, 144, 31] }
    end
    context 'when negative numeric value is set' do
      before(:all) { subject.value = -123 }
      it 'should not raise Exception' do
        lambda { subject.value = -123 }.should_not raise_error Exception
      end
      its(:value) { should eq 123 }
      its(:serial_value) { should eq [0, 0, 0, 0, 18, 63] }
    end
    context 'when numeric character value is set' do
      before(:all) { subject.value = '123' }
      it 'should not raise Exception' do
        lambda { subject.value = '123' }.should_not raise_error Exception
      end
      its(:value) { should eq 123 }
      its(:serial_value) { should eq [0, 0, 0, 0, 18, 63] }
    end
    context 'when non-numeric value is set' do
      before(:all) { subject.value = 'abc' }
      it 'should not raise RuntimeError' do
        lambda { subject.value = 'abc' }.should_not raise_error RuntimeError
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [0, 0, 0, 0, 0, 15] }
    end
    context 'when japanese character value is set' do
      before(:all) { subject.value = 'テスト' }
      it 'should not raise Exception' do
        lambda { subject.value = 'テスト' }.should_not raise_error Exception
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [0, 0, 0, 0, 0, 15] }
    end
  end

  describe 'S9 COMP-3 type' do
    subject do
      field = Cobor::Field.new 'test_S9COMP-3', 'S9COMP-3', 10
    end
    context 'when first created' do
      its(:name) { should eq 'test_S9COMP_3' }
      its(:cname) { should eq 'test_S9COMP_3' }
      its(:fieldtype) { should eq 'S9COMP-3' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 6 }
      its(:c_decl) { should eq 'char test_S9COMP_3[6]' }
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [0, 0, 0, 0, 0, 12] }
      its(:serialize) { should eq [0, 0, 0, 0, 0, 12] }
    end
    context 'when value is set' do
      before(:all) { subject.value = -1234 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 6 }
      its(:value) { should eq -1234 }
      its(:serial_value) { should eq [0, 0, 0, 1, 35, 77] }
      its(:serialize) { should eq [0, 0, 0, 1, 35, 77] }
    end
    context 'when overflow' do
      before(:all) { subject.value = -12345678901 }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 6 }
      its(:value) { should eq -2345678901 }
      its(:serial_value) { should eq [2, 52, 86, 120, 144, 29] }
      its(:serialize) { should eq [2, 52, 86, 120, 144, 29] }
    end
    context 'when positive numeric value is set' do
      before(:all) { subject.value = 123 }
      it 'should not raise Exception' do
        lambda { subject.value = 123 }.should_not raise_error Exception
      end
      its(:value) { should eq 123 }
      its(:serial_value) { should eq [0, 0, 0, 0, 18, 60] }
    end
    context 'when numeric character value is set' do
      before(:all) { subject.value = '-123' }
      it 'should not raise Exception' do
        lambda { subject.value = '-123' }.should_not raise_error Exception
      end
      its(:value) { should eq -123 }
      its(:serial_value) { should eq [0, 0, 0, 0, 18, 61] }
    end
    context 'when non-numeric value is set' do
      before(:all) { subject.value = 'abc' }
      it 'should not raise RuntimeError' do
        lambda { subject.value = 'abc' }.should_not raise_error RuntimeError
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [0, 0, 0, 0, 0, 12] }
    end
    context 'when japanese character value is set' do
      before(:all) { subject.value = 'テスト' }
      it 'should not raise Exception' do
        lambda { subject.value = 'テスト' }.should_not raise_error Exception
      end
      its(:value) { should eq 0 }
      its(:serial_value) { should eq [0, 0, 0, 0, 0, 12] }
    end
  end

  describe '9 COMP-5 type' do
    describe '9-digit' do
      subject do
        field = Cobor::Field.new 'test_9COMP-5', '9COMP-5', 9
      end
      context 'when first created' do
        its(:name) { should eq 'test_9COMP_5' }
        its(:cname) { should eq 'test_9COMP_5' }
        its(:fieldtype) { should eq '9COMP-5' }
        its(:size) { should eq 9 }
        its(:bytesize) { should eq 4 }
        its(:c_decl) { should eq 'char test_9COMP_5[4]' }
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0] }
        its(:serialize) { should eq [0, 0, 0, 0] }
      end
      context 'when value is set' do
        before(:all) { subject.value = 1234 }
        its(:size) { should eq 9 }
        its(:bytesize) { should eq 4 }
        its(:value) { should eq 1234 }
        its(:serial_value) { should eq [-46, 4, 0, 0] }
        its(:serialize) { should eq [-46, 4, 0, 0] }
      end
      context 'when overflow' do
        before(:all) { subject.value = 12345678901 }
        its(:size) { should eq 9 }
        its(:bytesize) { should eq 4 }
        its(:value) { should eq 345678901 }
        its(:serial_value) { should eq [53, -92, -102, 20] }
        its(:serialize) { should eq [53, -92, -102, 20] }
      end
      context 'when negative numeric value is set' do
        before(:all) { subject.value = -123 }
        it 'should not raise Exception' do
          lambda { subject.value = -123 }.should_not raise_error Exception
        end
        its(:value) { should eq 123 }
        its(:serial_value) { should eq [123, 0, 0, 0] }
      end
      context 'when numeric character value is set' do
        before(:all) { subject.value = '123' }
        it 'should not raise Exception' do
          lambda { subject.value = '123' }.should_not raise_error Exception
        end
        its(:value) { should eq 123 }
        its(:serial_value) { should eq [123, 0, 0, 0] }
      end
      context 'when non-numeric value is set' do
        before(:all) { subject.value = 'abc' }
        it 'should not raise RuntimeError' do
          lambda { subject.value = 'abc' }.should_not raise_error RuntimeError
        end
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0] }
      end
      context 'when japanese character value is set' do
        before(:all) { subject.value = 'テスト' }
        it 'should not raise Exception' do
          lambda { subject.value = 'テスト' }.should_not raise_error Exception
        end
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0] }
      end
    end

    describe '10-digit' do
      subject do
        field = Cobor::Field.new 'test_9COMP-5', '9COMP-5', 10
      end
      context 'when first created' do
        its(:name) { should eq 'test_9COMP_5' }
        its(:cname) { should eq 'test_9COMP_5' }
        its(:fieldtype) { should eq '9COMP-5' }
        its(:size) { should eq 10 }
        its(:bytesize) { should eq 8 }
        its(:c_decl) { should eq 'char test_9COMP_5[8]' }
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0, 0, 0, 0, 0] }
        its(:serialize) { should eq [0, 0, 0, 0, 0, 0, 0, 0] }
      end
      context 'when value is set' do
        before(:all) { subject.value = 1234 }
        its(:size) { should eq 10 }
        its(:bytesize) { should eq 8 }
        its(:value) { should eq 1234 }
        its(:serial_value) { should eq [-46, 4, 0, 0, 0, 0, 0, 0] }
        its(:serialize) { should eq [-46, 4, 0, 0, 0, 0, 0, 0] }
      end
      context 'when overflow' do
        before(:all) { subject.value = 12345678901 }
        its(:size) { should eq 10 }
        its(:bytesize) { should eq 8 }
        its(:value) { should eq 2345678901 }
        its(:serial_value) { should eq [53, 56, -48, -117, 0, 0, 0, 0] }
        its(:serialize) { should eq [53, 56, -48, -117, 0, 0, 0, 0] }
      end
      context 'when negative numeric value is set' do
        before(:all) { subject.value = -123 }
        it 'should not raise Exception' do
          lambda { subject.value = -123 }.should_not raise_error Exception
        end
        its(:value) { should eq 123 }
        its(:serial_value) { should eq [123, 0, 0, 0, 0, 0, 0, 0] }
      end
      context 'when numeric character value is set' do
        before(:all) { subject.value = '123' }
        it 'should not raise Exception' do
          lambda { subject.value = '123' }.should_not raise_error Exception
        end
        its(:value) { should eq 123 }
        its(:serial_value) { should eq [123, 0, 0, 0, 0, 0, 0, 0] }
      end
      context 'when non-numeric value is set' do
        before(:all) { subject.value = 'abc' }
        it 'should not raise RuntimeError' do
          lambda { subject.value = 'abc' }.should_not raise_error RuntimeError
        end
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0, 0, 0, 0, 0] }
      end
      context 'when japanese character value is set' do
        before(:all) { subject.value = 'テスト' }
        it 'should not raise Exception' do
          lambda { subject.value = 'テスト' }.should_not raise_error Exception
        end
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0, 0, 0, 0, 0] }
      end
    end
  end

  describe 'S9 COMP-5 type' do
    describe '9-digit' do
      subject do
        field = Cobor::Field.new 'test_S9COMP-5', 'S9COMP-5', 9
      end
      context 'when first created' do
        its(:name) { should eq 'test_S9COMP_5' }
        its(:cname) { should eq 'test_S9COMP_5' }
        its(:fieldtype) { should eq 'S9COMP-5' }
        its(:size) { should eq 9 }
        its(:bytesize) { should eq 4 }
        its(:c_decl) { should eq 'char test_S9COMP_5[4]' }
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0] }
        its(:serialize) { should eq [0, 0, 0, 0] }
      end
      context 'when value is set' do
        before(:all) { subject.value = -1234 }
        its(:size) { should eq 9 }
        its(:bytesize) { should eq 4 }
        its(:value) { should eq -1234 }
        its(:serial_value) { should eq [46, -5, -1, -1] }
        its(:serialize) { should eq [46, -5, -1, -1] }
      end
      context 'when overflow' do
        pending 'now researching' do
          before(:all) { subject.value = -12345678901 }
          its(:size) { should eq 9 }
          its(:bytesize) { should eq 4 }
          its(:value) { should eq -345678901 }
          its(:serial_value) { should eq [0, 0, 0, 0] }
          its(:serialize) { should eq [0, 0, 0, 0] }
        end
      end
      context 'when positive numeric value is set' do
        before(:all) { subject.value = 123 }
        it 'should not raise Exception' do
          lambda { subject.value = 123 }.should_not raise_error Exception
        end
        its(:value) { should eq 123 }
        its(:serial_value) { should eq [123, 0, 0, 0] }
      end
      context 'when numeric character value is set' do
        before(:all) { subject.value = '-123' }
        it 'should not raise Exception' do
          lambda { subject.value = '-123' }.should_not raise_error Exception
        end
        its(:value) { should eq -123 }
        its(:serial_value) { should eq [-123, -1, -1, -1] }
      end
      context 'when non-numeric value is set' do
        before(:all) { subject.value = 'abc' }
        it 'should not raise RuntimeError' do
          lambda { subject.value = 'abc' }.should_not raise_error RuntimeError
        end
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0] }
      end
      context 'when japanese character value is set' do
        before(:all) { subject.value = 'テスト' }
        it 'should not raise Exception' do
          lambda { subject.value = 'テスト' }.should_not raise_error Exception
        end
        its(:value) { should eq 0 }
        its(:serial_value) { should eq [0, 0, 0, 0] }
      end
    end
  end

  describe 'N type' do
    subject do
      field = Cobor::Field.new 'test_N', 'N', 10
    end
    context 'when first created' do
      its(:name) { should eq 'test_N' }
      its(:cname) { should eq 'test_N' }
      its(:fieldtype) { should eq 'N' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 20 }
      its(:c_decl) { should eq 'char test_N[20]' }
      its(:value) { should eq '' }
      its(:serial_value) { should eq [0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48] }
      its(:serialize) { should eq [0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48] }
    end
    context 'when value is set' do
      before(:all) { subject.value = 'いろはにほへと' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 20 }
      its(:value) { should eq 'いろはにほへと' }
      its(:serial_value) { should eq [68, 48, -115, 48, 111, 48, 107, 48, 123, 48, 120, 48, 104, 48, 0, 48, 0, 48, 0, 48] }
      its(:serialize) { should eq [68, 48, -115, 48, 111, 48, 107, 48, 123, 48, 120, 48, 104, 48, 0, 48, 0, 48, 0, 48] }
    end
    context 'when overflow' do
      before(:all) { subject.value = 'いろはにほへとちりぬるを' }
      its(:size) { should eq 10 }
      its(:bytesize) { should eq 20 }
      its(:value) { should eq 'いろはにほへとちりぬ' }
      its(:serial_value) { should eq [68, 48, -115, 48, 111, 48, 107, 48, 123, 48, 120, 48, 104, 48, 97, 48, -118, 48, 108, 48] }
      its(:serialize) { should eq [68, 48, -115, 48, 111, 48, 107, 48, 123, 48, 120, 48, 104, 48, 97, 48, -118, 48, 108, 48] }
    end
    context 'when character value is set' do
      before(:all) { subject.value = 'test' }
      it 'should not raise Exception' do
        lambda { subject.value = 'test' }.should_not raise_error Exception
      end
      its(:value) { should eq 'test' }
      its(:serial_value) { should eq [116, 0, 101, 0, 115, 0, 116, 0, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48] }
    end
    context 'when numeric value is set' do
      before(:all) { subject.value = 123 }
      it 'should not raise Exception' do
        lambda { subject.value = 123 }.should_not raise_error Exception
      end
      its(:value) { should eq '123' }
      its(:serial_value) { should eq [49, 0, 50, 0, 51, 0, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48, 0, 48] }
    end
  end
end
