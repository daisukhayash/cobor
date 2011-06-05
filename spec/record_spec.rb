# -*- coding: utf-8 -*-
require (File.dirname(__FILE__) + '/spec_helper')
require 'cobor/record'

describe Cobor::Record do
  context 'when not defined' do
    subject do
      record = Cobor::Record.new
    end

    its(:size) { should eq 0 }
    its(:names) { should eq [] }
    its(:values) { should eq [] }
    its(:fields) { should eq [] }
    its(:serialize) { should eq [] }
    its(:c_decl) { should eq [] }

    describe 'method not exist' do
      it 'should raise NoMethodError' do
        lambda { subject.none }.should raise_error NoMethodError
      end
    end
  end

  context 'when defined' do
    subject do
      record = Cobor::Record.define [
                             '03 X1  PIC  X(05)',
                             '03 D1  PIC  9(05)',
                             '03 D2  PIC S9(05)',
                             '03 D3  PIC  9(05) COMP-3',
                             '03 D4  PIC S9(05) COMP-3',
                             '03 D5  PIC  9(05) COMP-5',
                             '03 D6  PIC S9(05) COMP-5',
                             '03 N1  PIC  N(05)',
                             '03 G1.',
                             '  05 X2  PIC  X(03)',
                             '  05 D7  PIC  9(07)',
                             '  05 G2.',
                             '    07 X3  PIC  X(01)',
                             '    07 D8  PIC  9(02)',
                             '03 X4  PIC  X(02) OCCURS 3',
                             '03 G3 OCCURS 2',
                             '  05 X5  PIC  X(04)',
                             '  05 D9  PIC  9(04)'
                             ]
    end

    context 'when items have no value' do
      its(:size) { should eq 74 }
      its(:names) { should eq ['X1', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'N1', 'X2', 'D7', 'X3', 'D8', 'X4_0', 'X4_1', 'X4_2', 'X5_0', 'D9_0', 'X5_1', 'D9_1'] }
      its(:X1) { should eq '' }
      its(:serial_X1) { should eq [32, 32, 32, 32, 32] }
      its(:D1) { should eq 0 }
      its(:serial_D1) { should eq [48, 48, 48, 48, 48] }
      its(:D2) { should eq 0 }
      its(:serial_D2) { should eq [48, 48, 48, 48, 64] }
      its(:D3) { should eq 0 }
      its(:serial_D3) { should eq [0, 0, 15] }
      its(:D4) { should eq 0 }
      its(:serial_D4) { should eq [0, 0, 12] }
      its(:D5) { should eq 0 }
      its(:serial_D5) { should eq [0, 0, 0, 0] }
      its(:D6) { should eq 0 }
      its(:serial_D6) { should eq [0, 0, 0, 0] }
      its(:N1) { should eq '' }
      its(:serial_N1) { should eq [0, 48, 0, 48, 0, 48, 0, 48, 0, 48] }
      its(:X2) { should eq '' }
      its(:serial_X2) { should eq [32, 32, 32] }
      its(:D7) { should eq 0 }
      its(:serial_D7) { should eq [48, 48, 48, 48, 48, 48, 48] }
      its(:X3) { should eq '' }
      its(:serial_X3) { should eq [32] }
      its(:D8) { should eq 0 }
      its(:serial_D8) { should eq [48, 48] }
      it 'X4[0] should eq prescribed value' do subject.X4[0].should eq '' end
      it 'serial_X4[0] should eq prescribed value' do subject.serial_X4[0].should eq [32, 32] end
      it 'X4[1] should eq prescribed value' do subject.X4[1].should eq '' end
      it 'serial_X4[1] should eq prescribed value' do subject.serial_X4[1].should eq [32, 32] end
      it 'X4[2] should eq prescribed value' do subject.X4[2].should eq '' end
      it 'serial_X4[2] should eq prescribed value' do subject.serial_X4[2].should eq [32, 32] end
      it 'X5[0] should eq prescribed value' do subject.X5[0].should eq '' end
      it 'serial_X5[0] should eq prescribed value' do subject.serial_X5[0].should eq [32, 32, 32, 32] end
      it 'D9[0] should eq prescribed value' do subject.D9[0].should eq 0 end
      it 'serial_D9[0] should eq prescribed value' do subject.serial_D9[0].should eq [48, 48, 48, 48] end
      it 'X5[0] should eq prescribed value' do subject.X5[1].should eq '' end
      it 'serial_X5[1] should eq prescribed value' do subject.serial_X5[1].should eq [32, 32, 32, 32] end
      it 'D9[1] should eq prescribed value' do subject.D9[1].should eq 0 end
      it 'serial_D9[1] should eq prescribed value' do subject.serial_D9[1].should eq [48, 48, 48, 48] end
      its(:values) { should eq ['', 0, 0, 0, 0, 0, 0, '', '', 0, '', 0, '', '', '', '', 0, '', 0] }
      its(:serialize) { should eq [[32, 32, 32, 32, 32], [48, 48, 48, 48, 48], [48, 48, 48, 48, 64], [0, 0, 15], [0, 0, 12], [0, 0, 0, 0], [0, 0, 0, 0], [0, 48, 0, 48, 0, 48, 0, 48, 0, 48], [32, 32, 32], [48, 48, 48, 48, 48, 48, 48], [32], [48, 48], [32, 32], [32, 32], [32, 32], [32, 32, 32, 32], [48, 48, 48, 48], [32, 32, 32, 32], [48, 48, 48, 48]] }
      its(:c_decl) { should eq ['char X1[5]', 'char D1[5]', 'char D2[5]', 'char D3[3]', 'char D4[3]', 'char D5[4]', 'char D6[4]', 'char N1[10]', 'char X2[3]', 'char D7[7]', 'char X3', 'char D8[2]', 'char X4_0[2]', 'char X4_1[2]', 'char X4_2[2]', 'char X5_0[4]', 'char D9_0[4]', 'char X5_1[4]', 'char D9_1[4]'] }
    end

    describe 'setting value' do
      before :each do
        subject.X1 = 'Test!'
        subject.D1 = 12345
        subject.D2 = -12345
        subject.D3 = 12345
        subject.D4 = -12345
        subject.D5 = 12345
        subject.D6 = -12345
        subject.N1 = 'テストです'
        subject.X2 = 'Hi!'
        subject.D7 = 1234567
        subject.X3 = '?'
        subject.D8 = 12
        subject.X4[0] = 'He'
        subject.X4[1] = 'll'
        subject.X4[2] = 'o,'
        subject.X5[0] = 'worl'
        subject.D9[0] = 1234
        subject.X5[1] = 'd.'
        subject.D9[1] = 5678
      end

      describe 'clearing value' do
        before do
          subject.clear
        end

        its(:X1) { should eq '' }
        its(:serial_X1) { should eq [32, 32, 32, 32, 32] }
        its(:D1) { should eq 0 }
        its(:serial_D1) { should eq [48, 48, 48, 48, 48] }
        its(:D2) { should eq 0 }
        its(:serial_D2) { should eq [48, 48, 48, 48, 64] }
        its(:D3) { should eq 0 }
        its(:serial_D3) { should eq [0, 0, 15] }
        its(:D4) { should eq 0 }
        its(:serial_D4) { should eq [0, 0, 12] }
        its(:D5) { should eq 0 }
        its(:serial_D5) { should eq [0, 0, 0, 0] }
        its(:D6) { should eq 0 }
        its(:serial_D6) { should eq [0, 0, 0, 0] }
        its(:N1) { should eq '' }
        its(:serial_N1) { should eq [0, 48, 0, 48, 0, 48, 0, 48, 0, 48] }
        its(:X2) { should eq '' }
        its(:serial_X2) { should eq [32, 32, 32] }
        its(:D7) { should eq 0 }
        its(:serial_D7) { should eq [48, 48, 48, 48, 48, 48, 48] }
        its(:X3) { should eq '' }
        its(:serial_X3) { should eq [32] }
        its(:D8) { should eq 0 }
        its(:serial_D8) { should eq [48, 48] }
        it 'X4[0] should eq prescribed value' do subject.X4[0].should eq '' end
        it 'serial_X4[0] should eq prescribed value' do subject.serial_X4[0].should eq [32, 32] end
        it 'X4[1] should eq prescribed value' do subject.X4[1].should eq '' end
        it 'serial_X4[1] should eq prescribed value' do subject.serial_X4[1].should eq [32, 32] end
        it 'X4[2] should eq prescribed value' do subject.X4[2].should eq '' end
        it 'serial_X4[2] should eq prescribed value' do subject.serial_X4[2].should eq [32, 32] end
        it 'X5[0] should eq prescribed value' do subject.X5[0].should eq '' end
        it 'serial_X5[0] should eq prescribed value' do subject.serial_X5[0].should eq [32, 32, 32, 32] end
        it 'D9[0] should eq prescribed value' do subject.D9[0].should eq 0 end
        it 'serial_D9[0] should eq prescribed value' do subject.serial_D9[0].should eq [48, 48, 48, 48] end
        it 'X5[0] should eq prescribed value' do subject.X5[1].should eq '' end
        it 'serial_X5[1] should eq prescribed value' do subject.serial_X5[1].should eq [32, 32, 32, 32] end
        it 'D9[1] should eq prescribed value' do subject.D9[1].should eq 0 end
        it 'serial_D9[1] should eq prescribed value' do subject.serial_D9[1].should eq [48, 48, 48, 48] end
        its(:values) { should eq ['', 0, 0, 0, 0, 0, 0, '', '', 0, '', 0, '', '', '', '', 0, '', 0] }
        its(:serialize) { should eq [[32, 32, 32, 32, 32], [48, 48, 48, 48, 48], [48, 48, 48, 48, 64], [0, 0, 15], [0, 0, 12], [0, 0, 0, 0], [0, 0, 0, 0], [0, 48, 0, 48, 0, 48, 0, 48, 0, 48], [32, 32, 32], [48, 48, 48, 48, 48, 48, 48], [32], [48, 48], [32, 32], [32, 32], [32, 32], [32, 32, 32, 32], [48, 48, 48, 48], [32, 32, 32, 32], [48, 48, 48, 48]] }
      end

      describe 'accessing it by methods of name' do
        its(:X1) { should eq 'Test!' }
        its(:serial_X1) { should eq [84, 101, 115, 116, 33] }
        its(:D1) { should eq 12345 }
        its(:serial_D1) { should eq [49, 50, 51, 52, 53] }
        its(:D2) { should eq -12345 }
        its(:serial_D2) { should eq [49, 50, 51, 52, 85] }
        its(:D3) { should eq 12345 }
        its(:serial_D3) { should eq [18, 52, 95] }
        its(:D4) { should eq -12345 }
        its(:serial_D4) { should eq [18, 52, 93] }
        its(:D5) { should eq 12345 }
        its(:serial_D5) { should eq [57, 48, 0, 0] }
        its(:D6) { should eq -12345 }
        its(:serial_D6) { should eq [-57,-49, -1, -1] }
        its(:N1) { should eq 'テストです' }
        its(:serial_N1) { should eq [-58, 48, -71, 48, -56, 48, 103, 48, 89, 48] }
        its(:X2) { should eq 'Hi!' }
        its(:serial_X2) { should eq [72, 105, 33] }
        its(:D7) { should eq 1234567 }
        its(:serial_D7) { should eq [49, 50, 51, 52, 53, 54, 55] }
        its(:X3) { should eq '?' }
        its(:serial_X3) { should eq [63] }
        its(:D8) { should eq 12 }
        its(:serial_D8) { should eq [49, 50] }
        it 'X4[0] should eq prescribed value' do subject.X4[0].should eq 'He' end
        it 'serial_X4[0] should eq prescribed value' do subject.serial_X4[0].should eq [72, 101] end
        it 'X4[1] should eq prescribed value' do subject.X4[1].should eq 'll' end
        it 'serial_X4[1] should eq prescribed value' do subject.serial_X4[1].should eq [108, 108] end
        it 'X4[2] should eq prescribed value' do subject.X4[2].should eq 'o,' end
        it 'serial_X4[2] should eq prescribed value' do subject.serial_X4[2].should eq [111, 44] end
        it 'X5[0] should eq prescribed value' do subject.X5[0].should eq 'worl' end
        it 'serial_X5[0] should eq prescribed value' do subject.serial_X5[0].should eq [119, 111, 114, 108] end
        it 'D9[0] should eq prescribed value' do subject.D9[0].should eq 1234 end
        it 'serial_D9[0] should eq prescribed value' do subject.serial_D9[0].should eq [49, 50, 51, 52] end
        it 'X5[0] should eq prescribed value' do subject.X5[1].should eq 'd.' end
        it 'serial_X5[1] should eq prescribed value' do subject.serial_X5[1].should eq [100, 46, 32, 32] end
        it 'D9[1] should eq prescribed value' do subject.D9[1].should eq 5678 end
        it 'serial_D9[1] should eq prescribed value' do subject.serial_D9[1].should eq [53, 54, 55, 56] end
      end

      describe 'accessing it by [name]' do
        its([:X1]) { should eq 'Test!' }
        its([:serial_X1]) { should eq [84, 101, 115, 116, 33] }
        its([:D1]) { should eq 12345 }
        its([:serial_D1]) { should eq [49, 50, 51, 52, 53] }
        its([:D2]) { should eq -12345 }
        its([:serial_D2]) { should eq [49, 50, 51, 52, 85] }
        its([:D3]) { should eq 12345 }
        its([:serial_D3]) { should eq [18, 52, 95] }
        its([:D4]) { should eq -12345 }
        its([:serial_D4]) { should eq [18, 52, 93] }
        its([:D5]) { should eq 12345 }
        its([:serial_D5]) { should eq [57, 48, 0, 0] }
        its([:D6]) { should eq -12345 }
        its([:serial_D6]) { should eq [-57,-49, -1, -1] }
        its([:N1]) { should eq 'テストです' }
        its([:serial_N1]) { should eq [-58, 48, -71, 48, -56, 48, 103, 48, 89, 48] }
        its([:X2]) { should eq 'Hi!' }
        its([:serial_X2]) { should eq [72, 105, 33] }
        its([:D7]) { should eq 1234567 }
        its([:serial_D7]) { should eq [49, 50, 51, 52, 53, 54, 55] }
        its([:X3]) { should eq '?' }
        its([:serial_X3]) { should eq [63] }
        its([:D8]) { should eq 12 }
        its([:serial_D8]) { should eq [49, 50] }
        it '[:X4][0] should eq prescribed value' do subject['X4'][0].should eq 'He' end
        it '[:serial_X4][0] should eq prescribed value' do subject['serial_X4'][0].should eq [72, 101] end
        it '[:X4][1] should eq prescribed value' do subject['X4'][1].should eq 'll' end
        it '[:serial_X4][1] should eq prescribed value' do subject['serial_X4'][1].should eq [108, 108] end
        it '[:X4][2] should eq prescribed value' do subject['X4'][2].should eq 'o,' end
        it '[:serial_X4][2] should eq prescribed value' do subject['serial_X4'][2].should eq [111, 44] end
        it '[:X5][0] should eq prescribed value' do subject['X5'][0].should eq 'worl' end
        it '[:serial_X5][0] should eq prescribed value' do subject['serial_X5'][0].should eq [119, 111, 114, 108] end
        it '[:D9][0] should eq prescribed value' do subject['D9'][0].should eq 1234 end
        it '[:serial_D9][0] should eq prescribed value' do subject['serial_D9'][0].should eq [49, 50, 51, 52] end
        it '[:X5][0] should eq prescribed value' do subject['X5'][1].should eq 'd.' end
        it '[:serial_X5][1] should eq prescribed value' do subject['serial_X5'][1].should eq [100, 46, 32, 32] end
        it '[:D9][1] should eq prescribed value' do subject['D9'][1].should eq 5678 end
        it '[:serial_D9][1] should eq prescribed value' do subject['serial_D9'][1].should eq [53, 54, 55, 56] end
      end

      describe 'accessing it by method not exist' do
        it 'should raise NoMethodError' do
          lambda{ subject.Z1 }.should raise_error NoMethodError
        end
      end

      describe 'accessing it by [name not exist]' do
        it 'should raise ArgumentError' do
          lambda{ subject['Z1'] }.should raise_error ArgumentError
        end
      end
    end

    describe 'setting serial value' do
      before :each do
        subject.serial_X1 = [84, 101, 115, 116, 33]
        subject.serial_D1 = [49, 50, 51, 52, 53]
        subject.serial_D2 = [49, 50, 51, 52, 85]
        subject.serial_D3 = [18, 52, 95]
        subject.serial_D4 = [18, 52, 93]
        subject.serial_D5 = [57, 48, 0, 0]
        subject.serial_D6 = [-57,-49, -1, -1]
        subject.serial_N1 = [-58, 48, -71, 48, -56, 48, 103, 48, 89, 48]
        subject.serial_X2 = [72, 105, 33]
        subject.serial_D7 = [49, 50, 51, 52, 53, 54, 55]
        subject.serial_X3 = [63]
        subject.serial_D8 = [49, 50]
        subject.serial_X4[0] = [72, 101]
        subject.serial_X4[1] = [108, 108]
        subject.serial_X4[2] = [111, 44]
        subject.serial_X5[0] = [119, 111, 114, 108]
        subject.serial_D9[0] = [49, 50, 51, 52]
        subject.serial_X5[1] = [100, 46, 32, 32]
        subject.serial_D9[1] = [53, 54, 55, 56]
      end

      its(:X1) { should eq 'Test!' }
      its(:serial_X1) { should eq [84, 101, 115, 116, 33] }
      its(:D1) { should eq 12345 }
      its(:serial_D1) { should eq [49, 50, 51, 52, 53] }
      its(:D2) { should eq -12345 }
      its(:serial_D2) { should eq [49, 50, 51, 52, 85] }
      its(:D3) { should eq 12345 }
      its(:serial_D3) { should eq [18, 52, 95] }
      its(:D4) { should eq -12345 }
      its(:serial_D4) { should eq [18, 52, 93] }
      its(:D5) { should eq 12345 }
      its(:serial_D5) { should eq [57, 48, 0, 0] }
      its(:D6) { should eq -12345 }
      its(:serial_D6) { should eq [-57,-49, -1, -1] }
      its(:N1) { should eq 'テストです' }
      its(:serial_N1) { should eq [-58, 48, -71, 48, -56, 48, 103, 48, 89, 48] }
      its(:X2) { should eq 'Hi!' }
      its(:serial_X2) { should eq [72, 105, 33] }
      its(:D7) { should eq 1234567 }
      its(:serial_D7) { should eq [49, 50, 51, 52, 53, 54, 55] }
      its(:X3) { should eq '?' }
      its(:serial_X3) { should eq [63] }
      its(:D8) { should eq 12 }
      its(:serial_D8) { should eq [49, 50] }
      it ':X4[0] should eq prescribed value' do subject.X4[0].should eq 'He' end
      it ':serial_X4[0] should eq prescribed value' do subject.serial_X4[0].should eq [72, 101] end
      it ':X4[1] should eq prescribed value' do subject.X4[1].should eq 'll' end
      it ':serial_X4[1] should eq prescribed value' do subject.serial_X4[1].should eq [108, 108] end
      it ':X4[2] should eq prescribed value' do subject.X4[2].should eq 'o,' end
      it ':serial_X4[2] should eq prescribed value' do subject.serial_X4[2].should eq [111, 44] end
      it ':X5[0] should eq prescribed value' do subject.X5[0].should eq 'worl' end
      it ':serial_X5[0] should eq prescribed value' do subject.serial_X5[0].should eq [119, 111, 114, 108] end
      it ':D9[0] should eq prescribed value' do subject.D9[0].should eq 1234 end
      it ':serial_D9[0] should eq prescribed value' do subject.serial_D9[0].should eq [49, 50, 51, 52] end
      it ':X5[0] should eq prescribed value' do subject.X5[1].should eq 'd.' end
      it ':serial_X5[1] should eq prescribed value' do subject.serial_X5[1].should eq [100, 46, 32, 32] end
      it ':D9[1] should eq prescribed value' do subject.D9[1].should eq 5678 end
      it ':serial_D9[1] should eq prescribed value' do subject.serial_D9[1].should eq [53, 54, 55, 56] end
    end
  end
end
