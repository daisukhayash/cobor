# -*- coding: utf-8 -*-
require (File.dirname(__FILE__) + '/spec_helper')
require 'cobor/type'

describe Cobor::Type do
  describe 'conversion of number into packed decimal' do
    context 'when positive number without signed flag' do
      before :all do
        @val = 123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_pd(@val, 1).should eql_to_hex_string('3f')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_pd(@val, 2).should eql_to_hex_string('023f')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_pd(@val, 3).should eql_to_hex_string('123f')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_pd(@val, 4).should eql_to_hex_string('00123f')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_pd(@val, 5).should eql_to_hex_string('00123f')
      end
    end
    context 'when negative number without signed flag' do
      before :all do
        @val = -123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_pd(@val, 1).should eql_to_hex_string('3f')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_pd(@val, 2).should eql_to_hex_string('023f')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_pd(@val, 3).should eql_to_hex_string('123f')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_pd(@val, 4).should eql_to_hex_string('00123f')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_pd(@val, 5).should eql_to_hex_string('00123f')
      end
    end
    context 'when positive number with signed flag' do
      before :all do
        @val = 123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_pd(@val, 1, Cobor::Type::SIGNED).should eql_to_hex_string('3c')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_pd(@val, 2, Cobor::Type::SIGNED).should eql_to_hex_string('023c')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_pd(@val, 3, Cobor::Type::SIGNED).should eql_to_hex_string('123c')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_pd(@val, 4, Cobor::Type::SIGNED).should eql_to_hex_string('00123c')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_pd(@val, 5, Cobor::Type::SIGNED).should eql_to_hex_string('00123c')
      end
    end
    context 'when negative number with signed flag' do
      before :all do
        @val = -123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_pd(@val, 1, Cobor::Type::SIGNED).should eql_to_hex_string('3d')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_pd(@val, 2, Cobor::Type::SIGNED).should eql_to_hex_string('023d')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_pd(@val, 3, Cobor::Type::SIGNED).should eql_to_hex_string('123d')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_pd(@val, 4, Cobor::Type::SIGNED).should eql_to_hex_string('00123d')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_pd(@val, 5, Cobor::Type::SIGNED).should eql_to_hex_string('00123d')
      end
    end
  end

  describe 'conversion of packed decimal into number' do
    context 'when unsigned' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('3f')
        Cobor::Type.pd_to_num(val).should == 3
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('023f')
        Cobor::Type.pd_to_num(val).should == 23
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('123f')
        Cobor::Type.pd_to_num(val).should == 123
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('00123f')
        Cobor::Type.pd_to_num(val).should == 123
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('00123f')
        Cobor::Type.pd_to_num(val).should == 123
      end
    end
    context 'when positive' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('3c')
        Cobor::Type.pd_to_num(val).should == 3
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('023c')
        Cobor::Type.pd_to_num(val).should == 23
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('123c')
        Cobor::Type.pd_to_num(val).should == 123
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('00123c')
        Cobor::Type.pd_to_num(val).should == 123
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('00123c')
        Cobor::Type.pd_to_num(val).should == 123
      end
    end
    context 'when negative' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('3d')
        Cobor::Type.pd_to_num(val).should == -3
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('023d')
        Cobor::Type.pd_to_num(val).should == -23
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('123d')
        Cobor::Type.pd_to_num(val).should == -123
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('00123d')
        Cobor::Type.pd_to_num(val).should == -123
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('00123d')
        Cobor::Type.pd_to_num(val).should == -123
      end
    end
  end

  describe 'conversion of number into zoned decimal' do
    context 'when positive number without signed flag' do
      before :all do
        @val = 123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_zd(@val, 1).should eql_to_hex_string('33')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_zd(@val, 2).should eql_to_hex_string('3233')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_zd(@val, 3).should eql_to_hex_string('313233')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_zd(@val, 4).should eql_to_hex_string('30313233')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_zd(@val, 5).should eql_to_hex_string('3030313233')
      end
    end
    context 'when negative number without signed flag' do
      before :all do
        @val = -123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_zd(@val, 1).should eql_to_hex_string('33')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_zd(@val, 2).should eql_to_hex_string('3233')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_zd(@val, 3).should eql_to_hex_string('313233')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_zd(@val, 4).should eql_to_hex_string('30313233')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_zd(@val, 5).should eql_to_hex_string('3030313233')
      end
    end
    context 'when positive number without signed flag' do
      before :all do
        @val = 123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_zd(@val, 1, Cobor::Type::SIGNED).should eql_to_hex_string('43')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_zd(@val, 2, Cobor::Type::SIGNED).should eql_to_hex_string('3243')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_zd(@val, 3, Cobor::Type::SIGNED).should eql_to_hex_string('313243')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_zd(@val, 4, Cobor::Type::SIGNED).should eql_to_hex_string('30313243')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_zd(@val, 5, Cobor::Type::SIGNED).should eql_to_hex_string('3030313243')
      end
    end
    context 'when positive number without signed flag' do
      before :all do
        @val = -123
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_zd(@val, 1, Cobor::Type::SIGNED).should eql_to_hex_string('53')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_zd(@val, 2, Cobor::Type::SIGNED).should eql_to_hex_string('3253')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_zd(@val, 3, Cobor::Type::SIGNED).should eql_to_hex_string('313253')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_zd(@val, 4, Cobor::Type::SIGNED).should eql_to_hex_string('30313253')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_zd(@val, 5, Cobor::Type::SIGNED).should eql_to_hex_string('3030313253')
      end
    end
  end

  describe 'conversion of zoned decimal into number' do
    context 'when unsigned' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('33')
        Cobor::Type.zd_to_num(val).should == 3
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('3233')
        Cobor::Type.zd_to_num(val).should == 23
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('313233')
        Cobor::Type.zd_to_num(val).should == 123
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('30313233')
        Cobor::Type.zd_to_num(val).should == 123
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('3030313233')
        Cobor::Type.zd_to_num(val).should == 123
      end
    end
    context 'when positive' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('43')
        Cobor::Type.zd_to_num(val).should == 3
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('3243')
        Cobor::Type.zd_to_num(val).should == 23
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('313243')
        Cobor::Type.zd_to_num(val).should == 123
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('30313243')
        Cobor::Type.zd_to_num(val).should == 123
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('3030313243')
        Cobor::Type.zd_to_num(val).should == 123
      end
    end
    context 'when negative' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('53')
        Cobor::Type.zd_to_num(val).should == -3
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('3253')
        Cobor::Type.zd_to_num(val).should == -23
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('313253')
        Cobor::Type.zd_to_num(val).should == -123
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('30313253')
        Cobor::Type.zd_to_num(val).should == -123
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('3030313253')
        Cobor::Type.zd_to_num(val).should == -123
      end
    end
  end

  describe 'conversion of number into comp-5' do
    context 'when positive number without signed flag' do
      before :all do
        @val = 1234
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_comp5(@val, 1).should eql_to_hex_string('0400')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_comp5(@val, 2).should eql_to_hex_string('2200')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_comp5(@val, 3).should eql_to_hex_string('ea00')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_comp5(@val, 4).should eql_to_hex_string('d204')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_comp5(@val, 5).should eql_to_hex_string('d2040000')
      end
    end
    context 'when negative number without signed flag' do
      before :all do
        @val = -1234
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_comp5(@val, 1).should eql_to_hex_string('0400')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_comp5(@val, 2).should eql_to_hex_string('2200')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_comp5(@val, 3).should eql_to_hex_string('ea00')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_comp5(@val, 4).should eql_to_hex_string('d204')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_comp5(@val, 5).should eql_to_hex_string('d2040000')
      end
    end
    context 'when positive number without signed flag' do
      before :all do
        @val = 1234
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_comp5(@val, 1, Cobor::Type::SIGNED).should eql_to_hex_string('0400')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_comp5(@val, 2, Cobor::Type::SIGNED).should eql_to_hex_string('2200')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_comp5(@val, 3, Cobor::Type::SIGNED).should eql_to_hex_string('ea00')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_comp5(@val, 4, Cobor::Type::SIGNED).should eql_to_hex_string('d204')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_comp5(@val, 5, Cobor::Type::SIGNED).should eql_to_hex_string('d2040000')
      end
    end
    context 'when positive number without signed flag' do
      before :all do
        @val = -1234
      end
      it 'in case of 1-digit' do
        Cobor::Type.num_to_comp5(@val, 1, Cobor::Type::SIGNED).should eql_to_hex_string('fcff')
      end
      it 'in case of 2-digit' do
        Cobor::Type.num_to_comp5(@val, 2, Cobor::Type::SIGNED).should eql_to_hex_string('deff')
      end
      it 'in case of 3-digit' do
        Cobor::Type.num_to_comp5(@val, 3, Cobor::Type::SIGNED).should eql_to_hex_string('16ff')
      end
      it 'in case of 4-digit' do
        Cobor::Type.num_to_comp5(@val, 4, Cobor::Type::SIGNED).should eql_to_hex_string('2efb')
      end
      it 'in case of 5-digit' do
        Cobor::Type.num_to_comp5(@val, 5, Cobor::Type::SIGNED).should eql_to_hex_string('2efbffff')
      end
    end
  end

  describe 'conversion of number into comp-5' do
    context 'when positive number' do
      before :all do
        @val = 1234
      end
    end
    context 'when negative number' do
      before :all do
        @val = -1234
      end
    end
  end

  describe 'conversion of comp-5 into number' do
    context 'when positive' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('0400')
        Cobor::Type.comp5_to_num(val).should == 4
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('2200')
        Cobor::Type.comp5_to_num(val).should == 34
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('ea00')
        Cobor::Type.comp5_to_num(val).should == 234
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('d204')
        Cobor::Type.comp5_to_num(val).should == 1234
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('d2040000')
        Cobor::Type.comp5_to_num(val).should == 1234
      end
    end
    context 'when negative' do
      it 'in case of 1-digit' do
        val = hex_string_to_chars('fcff')
        Cobor::Type.comp5_to_num(val).should == -4
      end
      it 'in case of 2-digit' do
        val = hex_string_to_chars('deff')
        Cobor::Type.comp5_to_num(val).should == -34
      end
      it 'in case of 3-digit' do
        val = hex_string_to_chars('16ff')
        Cobor::Type.comp5_to_num(val).should == -234
      end
      it 'in case of 4-digit' do
        val = hex_string_to_chars('2efb')
        Cobor::Type.comp5_to_num(val).should == -1234
      end
      it 'in case of 5-digit' do
        val = hex_string_to_chars('2efbffff')
        Cobor::Type.comp5_to_num(val).should == -1234
      end
    end
  end
end
