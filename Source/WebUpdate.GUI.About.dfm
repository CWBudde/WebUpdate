object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 393
  ClientWidth = 517
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    517
    393)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDualLicenses: TLabel
    Left = 8
    Top = 109
    Width = 64
    Height = 13
    Caption = 'Dual Licenses'
  end
  object Label1: TLabel
    Left = 8
    Top = 79
    Width = 442
    Height = 19
    Caption = 'Written for Delphi XE+. Uses the DWS, JEDI and Indy projects'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MemoLGPL: TMemo
    Left = 8
    Top = 131
    Width = 501
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '                   GNU LESSER GENERAL PUBLIC LICENSE'
      '                       Version 3, 29 June 2007'
      ''
      
        ' Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.o' +
        'rg/>'
      ' Everyone is permitted to copy and distribute verbatim copies'
      ' of this license document, but changing it is not allowed.'
      ''
      ''
      
        '  This version of the GNU Lesser General Public License incorpor' +
        'ates'
      'the terms and conditions of version 3 of the GNU General Public'
      
        'License, supplemented by the additional permissions listed below' +
        '.'
      ''
      '  0. Additional Definitions.'
      ''
      '  As used herein, "this License" refers to version 3 of the GNU '
      'Lesser'
      
        'General Public License, and the "GNU GPL" refers to version 3 of' +
        ' the '
      'GNU'
      'General Public License.'
      ''
      
        '  "The Library" refers to a covered work governed by this Licens' +
        'e,'
      'other than an Application or a Combined Work as defined below.'
      ''
      '  An "Application" is any work that makes use of an interface '
      'provided'
      'by the Library, but which is not otherwise based on the Library.'
      
        'Defining a subclass of a class defined by the Library is deemed ' +
        'a '
      'mode'
      'of using an interface provided by the Library.'
      ''
      
        '  A "Combined Work" is a work produced by combining or linking a' +
        'n'
      
        'Application with the Library.  The particular version of the Lib' +
        'rary'
      'with which the Combined Work was made is also called the "Linked'
      'Version".'
      ''
      
        '  The "Minimal Corresponding Source" for a Combined Work means t' +
        'he'
      
        'Corresponding Source for the Combined Work, excluding any source' +
        ' '
      'code'
      
        'for portions of the Combined Work that, considered in isolation,' +
        ' are'
      'based on the Application, and not on the Linked Version.'
      ''
      
        '  The "Corresponding Application Code" for a Combined Work means' +
        ' the'
      
        'object code and/or source code for the Application, including an' +
        'y '
      'data'
      
        'and utility programs needed for reproducing the Combined Work fr' +
        'om '
      'the'
      'Application, but excluding the System Libraries of the Combined '
      'Work.'
      ''
      '  1. Exception to Section 3 of the GNU GPL.'
      ''
      '  You may convey a covered work under sections 3 and 4 of this '
      'License'
      'without being bound by section 3 of the GNU GPL.'
      ''
      '  2. Conveying Modified Versions.'
      ''
      
        '  If you modify a copy of the Library, and, in your modification' +
        's, a'
      'facility refers to a function or data to be supplied by an '
      'Application'
      
        'that uses the facility (other than as an argument passed when th' +
        'e'
      'facility is invoked), then you may convey a copy of the modified'
      'version:'
      ''
      
        '   a) under this License, provided that you make a good faith ef' +
        'fort '
      'to'
      '   ensure that, in the event an Application does not supply the'
      '   function or data, the facility still operates, and performs'
      '   whatever part of its purpose remains meaningful, or'
      ''
      
        '   b) under the GNU GPL, with none of the additional permissions' +
        ' of'
      '   this License applicable to that copy.'
      ''
      
        '  3. Object Code Incorporating Material from Library Header File' +
        's.'
      ''
      
        '  The object code form of an Application may incorporate materia' +
        'l '
      'from'
      'a header file that is part of the Library.  You may convey such '
      'object'
      
        'code under terms of your choice, provided that, if the incorpora' +
        'ted'
      'material is not limited to numerical parameters, data structure'
      'layouts and accessors, or small macros, inline functions and '
      'templates'
      '(ten or fewer lines in length), you do both of the following:'
      ''
      
        '   a) Give prominent notice with each copy of the object code th' +
        'at '
      'the'
      '   Library is used in it and that the Library and its use are'
      '   covered by this License.'
      ''
      
        '   b) Accompany the object code with a copy of the GNU GPL and t' +
        'his '
      'license'
      '   document.'
      ''
      '  4. Combined Works.'
      ''
      
        '  You may convey a Combined Work under terms of your choice that' +
        ','
      'taken together, effectively do not restrict modification of the'
      
        'portions of the Library contained in the Combined Work and rever' +
        'se'
      
        'engineering for debugging such modifications, if you also do eac' +
        'h of'
      'the following:'
      ''
      
        '   a) Give prominent notice with each copy of the Combined Work ' +
        'that'
      
        '   the Library is used in it and that the Library and its use ar' +
        'e'
      '   covered by this License.'
      ''
      
        '   b) Accompany the Combined Work with a copy of the GNU GPL and' +
        ' '
      'this license'
      '   document.'
      ''
      '   c) For a Combined Work that displays copyright notices during'
      '   execution, include the copyright notice for the Library among'
      
        '   these notices, as well as a reference directing the user to t' +
        'he'
      '   copies of the GNU GPL and this license document.'
      ''
      '   d) Do one of the following:'
      ''
      
        '       0) Convey the Minimal Corresponding Source under the term' +
        's of '
      'this'
      '       License, and the Corresponding Application Code in a form'
      '       suitable for, and under terms that permit, the user to'
      
        '       recombine or relink the Application with a modified versi' +
        'on '
      'of'
      
        '       the Linked Version to produce a modified Combined Work, i' +
        'n '
      'the'
      
        '       manner specified by section 6 of the GNU GPL for conveyin' +
        'g'
      '       Corresponding Source.'
      ''
      
        '       1) Use a suitable shared library mechanism for linking wi' +
        'th '
      'the'
      
        '       Library.  A suitable mechanism is one that (a) uses at ru' +
        'n '
      'time'
      
        '       a copy of the Library already present on the user'#39's compu' +
        'ter'
      
        '       system, and (b) will operate properly with a modified ver' +
        'sion'
      
        '       of the Library that is interface-compatible with the Link' +
        'ed'
      '       Version.'
      ''
      '   e) Provide Installation Information, but only if you would '
      'otherwise'
      
        '   be required to provide such information under section 6 of th' +
        'e'
      '   GNU GPL, and only to the extent that such information is'
      '   necessary to install and execute a modified version of the'
      '   Combined Work produced by recombining or relinking the'
      
        '   Application with a modified version of the Linked Version. (I' +
        'f'
      
        '   you use option 4d0, the Installation Information must accompa' +
        'ny'
      
        '   the Minimal Corresponding Source and Corresponding Applicatio' +
        'n'
      
        '   Code. If you use option 4d1, you must provide the Installatio' +
        'n'
      
        '   Information in the manner specified by section 6 of the GNU G' +
        'PL'
      '   for conveying Corresponding Source.)'
      ''
      '  5. Combined Libraries.'
      ''
      '  You may place library facilities that are a work based on the'
      
        'Library side by side in a single library together with other lib' +
        'rary'
      'facilities that are not Applications and are not covered by this'
      'License, and convey such a combined library under terms of your'
      'choice, if you do both of the following:'
      ''
      
        '   a) Accompany the combined library with a copy of the same wor' +
        'k '
      'based'
      '   on the Library, uncombined with any other library facilities,'
      '   conveyed under the terms of this License.'
      ''
      
        '   b) Give prominent notice with the combined library that part ' +
        'of '
      'it'
      
        '   is a work based on the Library, and explaining where to find ' +
        'the'
      '   accompanying uncombined form of the same work.'
      ''
      '  6. Revised Versions of the GNU Lesser General Public License.'
      ''
      '  The Free Software Foundation may publish revised and/or new '
      'versions'
      
        'of the GNU Lesser General Public License from time to time. Such' +
        ' new'
      
        'versions will be similar in spirit to the present version, but m' +
        'ay'
      'differ in detail to address new problems or concerns.'
      ''
      '  Each version is given a distinguishing version number. If the'
      
        'Library as you received it specifies that a certain numbered ver' +
        'sion'
      'of the GNU Lesser General Public License "or any later version"'
      'applies to it, you have the option of following the terms and'
      
        'conditions either of that published version or of any later vers' +
        'ion'
      'published by the Free Software Foundation. If the Library as you'
      'received it does not specify a version number of the GNU Lesser'
      
        'General Public License, you may choose any version of the GNU Le' +
        'sser'
      'General Public License ever published by the Free Software '
      'Foundation.'
      ''
      '  If the Library as you received it specifies that a proxy can '
      'decide'
      
        'whether future versions of the GNU Lesser General Public License' +
        ' '
      'shall'
      
        'apply, that proxy'#39's public statement of acceptance of any versio' +
        'n is'
      'permanent authorization for you to choose that version for the'
      'Library.')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object MemoMIT: TMemo
    Left = 8
    Top = 131
    Width = 501
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'The MIT License (MIT)'
      ''
      'Copyright (c) 2014 Christian-W. Budde'
      ''
      'Permission is hereby granted, free of charge, to any person '
      
        'obtaining a copy of this software and associated documentation f' +
        'iles '
      '(the "Software"), to deal in the Software without restriction, '
      
        'including without limitation the rights to use, copy, modify, me' +
        'rge, '
      
        'publish, distribute, sublicense, and/or sell copies of the Softw' +
        'are, '
      
        'and to permit persons to whom the Software is furnished to do so' +
        ', '
      'subject to the following conditions:'
      ''
      'The above copyright notice and this permission notice shall be '
      'included in all copies or substantial portions of the Software.'
      ''
      'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, '
      
        'EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES ' +
        'OF '
      'MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND '
      
        'NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLD' +
        'ERS '
      
        'BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN ' +
        'AN '
      
        'ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR I' +
        'N '
      
        'CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE' +
        ' '
      'SOFTWARE.')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object RadioButtonLicenseMIT: TRadioButton
    Left = 78
    Top = 108
    Width = 35
    Height = 17
    Caption = 'MIT'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = RadioButtonLicenseMITClick
  end
  object RadioButtonLicenseLGPL: TRadioButton
    Left = 119
    Top = 108
    Width = 42
    Height = 17
    Caption = 'LGPL'
    TabOrder = 1
    OnClick = RadioButtonLicenseLGPLClick
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 517
    Height = 73
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkSoft
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 4
    object ImageHeader: TImage
      Left = 7
      Top = 5
      Width = 32
      Height = 32
      Picture.Data = {
        055449636F6E0000010004001818000001002000880900004600000020200000
        01002000A8100000CE090000101000000100200068040000761A000016160000
        0100200010080000DE1E00002800000018000000300000000100200000000000
        00090000120B0000120B00000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000005555550F55555527
        55555551555555765555559C555555C9555555EB555555FC555555FF555555FF
        C0C0C0FF9C9C9CFF868686FF555555FF555555FF555555FE555555F7555555DE
        555555AD5555557055555540555555260000000000000000C8C8C80FC8C8C827
        C8C8C84FC8C8C874C8C8C89BC8C8C8C6C8C8C8EAC8C8C8FBA0A0A0FF808080FF
        E2E2E2FFC8C8C8FFA0A0A0FF666666FF808080FFA0A0A0FEC8C8C8F6C8C8C8D8
        C8C8C8A1C8C8C860C8C8C832C8C8C81C00000000000000005555552455555545
        5555556E55555590555555B1535353D8545454F2555555FD555555FF555555FF
        E2E2E2FEE2E2E2FEC7C7C7FE555555FF0000CCFF0000CCFF0000CCFF0000CCFF
        0000CCFF0000CCFF0000CCFF1A1AA76E00000000000000000000000000000000
        000000000000000300000020000000380000005038271F875F4333DE604534FA
        5A5755FFA09C9AFF5B5755FF5F4533F83A2A63E60000CCFF0000CCFF0000CCFF
        0000CCFF0000CCFF0000CC4C0000000000000000000000000000000000000000
        0000000000000016000000310000004C866049DAA67C5DFFBC9A7FFFCCAF97FF
        948A80FFCFC4BAFF978B81FFCAAD93FFBA987CFFA57C5CFF0000CCFF0000CCFF
        0000CCFF0000926A000000030000000000000000000000000000000000000000
        00000000643C2D5B442E25509C7256F6B9977CFFD8C1ABFFB49477FFB68151FF
        B27947FFB07643FFAB7240FFA87447FFB28967FFD3B9A2FFB79476FF654A77F0
        0000CCFF0000CCFF0000BD520000000000000000000000000000000000000000
        7848364C6F4232529E7357F2C1A288FFD0BEAEFFDAD9D8FFA79C92FFBA875AFF
        BB8758FFB98454FFB67F4EFFB27947FFA97445FFB6B5B4FFC0AF9FFFBD9B80FF
        664B7BE60000CCFF0000CCFF0000CC4C00000000000000000000000000000000
        784836819A6E53BEBB997FFFD5B89EFFD1CDC9FFEEEEEEFFF1F1F1FFAB9785FF
        C5966AFFC39265FFBF8C5FFFBA8656FFA98C70FFC7C7C7FFB9B9B8FFC0A58EFF
        B79477FF5D4589B30000CCFF0000CC810000000000000000000000007848364C
        7C4C3A9AA67D5DFFDDC8B6FFCAC1BAFFFCFCFCFFFEFEFEFFFEFEFEFFBBB0A6FF
        CFA37CFFCC9F76FFC8996FFFC39265FFB5967AFFD7D7D7FFCACACAFF968A82FF
        D5BBA4FFA57C5CFF0000CCFF0000CCC1000000000000000000000000784836A7
        845641D7BFA087FFD5B596FFFEFEFEFFFEFEFEFFFEFEFEFFFDFCFCFFC4B6AAFF
        D8B18DFFD5AC87FFD0A67EFFCB9D74FFBBA896FFE6E6E6FFD7D7D7FF9E948CFF
        B68E6DFFBB997DFF2219B3D40000CCFF000000000000000000000000784836A7
        93664DF0D3BCA8FFD5AE8AFFE2DEDAFFFEFEFEFFEFEEEDFFCFB79FFFE4C2A2FF
        E2BF9EFFDEB997FFBEA38BFFBC9E82FFD3CCC5FFF3F3F3FFE3E3E3FFCFCECEFF
        967152FFCBAF96FF46349AE40000CCFF000000000000000000000000784836FF
        A47A5BFBE1CFBEFFC2B2A2FFCFBFB0FFD4CDC7FFD4BFABFFEED1B5FFEED0B4FF
        EBCDB1FFD6BFAAFFFCFBFBFFFEFEFEFFFEFEFEFFFEFEFEFFEDEDEDFFCDCBC9FF
        978676FFD5BDA7FF70527CF60000CCFF000000000000000000000000784836FF
        9C7053FFE6D8CAFFC9BCAFFFE9C9ABFFF0D3B8FFF5DBC1FFF9E2CCFFF9E5D2FF
        F6E2CDFFD7CDC4FFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFF5F5F5FFB8AEA4FF
        BFBDBCFFDAC4AFFFA37959FE0000CCFF000000000000000000000000784836FF
        966C51FDE2D2C5FFDFBFA1FFE3CAB1FFF7DDC4FFFEEAD7FFFFEEDEFFFFEFDEFF
        FDECDBFFEBDBCEFFFFFFFFFFFEFEFEFFFEFEFEFFFEFEFEFFE6E4E2FFCAC5C0FF
        B1AAA4FFD7C0AAFF70537CF40000CCA7000000000000000000000000784836FF
        8F644BF0D7C2B1FFEBE9E7FFBBB4AEFFFAE4CFFFFFEFDFFFFFEFDFFFFFEFE0FF
        FFEFE0FFFBE9D8FFDBD2CBFFFEFEFEFFEBE9E8FFD1C8C0FFCDC0B3FFEAEAEAFF
        D5D4D2FFCDB29BFF46349AE30000CCA7000000000000000000000000784836C1
        784836FFC3A68EFFF7F3EFFFFEFEFEFFBFB7AEFFFFEFE0FFFFF0E1FFFFF0E1FF
        FFF0E1FFFCEBDBFFD5CEC7FFBCB3AAFFCCBBACFFC0B6ACFFECEBEAFFE9E9E9FF
        DDD6CEFFBD9D82FF3628A6C10000CC4C00000000000000000000000078483681
        784836FFA47A5CFFE6D8CBFFFDFCFCFFFEFEFEFFC2BDB8FFF0E3D7FFFFF1E2FF
        FFF1E3FFFAEAD9FFF4E1CFFFE9E8E6FFF7F7F7FFFEFEFEFFF9F9F9FFE6E5E5FF
        DBC6B2FFA67D5DFF140EBE93000000000000000000000000000000007848364C
        784836FF784836FFC0A189FFECE4DDFFDBD2C9FFFEFEFEFFD8CFC6FFFEF0E2FF
        FBECDDFFF7E5D5FFE0D1C1FFD0C6BEFFCFC8C2FFF3F2F1FFF1F1F1FFE1D4C8FF
        BB9A7FFF5D458AB10000CC4C0000000000000000000000000000000000000000
        7848364C784836FF976A50E3C8AE99FFE4DAD1FFD8CDC1FFD1C6BCFFD1CBC5FF
        C7BCB1FFEBDACAFFE3CFBDFFE2CDB9FFE0DEDBFFD5D0CBFFDFD2C6FFC3A58CFF
        A37959C60000CC4C000000000000000000000000000000000000000000000000
        000000007848364C784836FF90634AE2C0A28AFFE8DBD0FFE6D8CBFFD9D5D1FF
        FCFCFCFFBAB6B2FFE9D1BAFFE0C5AAFFDEC8B3FFE1CFBDFFBC9C82FFA37959C6
        A47A5A1700000000000000000000000000000000000000000000000000000000
        7848364C784836FF784836FF784836FF83553FD3A67D5EFFC4A791FFD9C7B6FF
        E6D8CBFFEADDD2FFE3D5C7FFD6C1AFFFC2A38CFFA67D5EFFA47A5A9000000000
        000000000000000000000000000000000000000000000000000000007848364C
        784836FF784836FF784836FF784836FF784836FF784836FF784836FFA47A5AC6
        A37959F1A37959FEA37959F0A47A5AC6A47A5A82A47A5A240000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000FFFFFF008000010080000100
        80000100F0000300F0000300F0000300E0000100E0000100C0000100C0000100
        C0000100C0000100C0000100C0000100C0000100C0000100C0000300C0000300
        E0000700F0000F00E0003F00C0007F00FFFFFF00280000002000000040000000
        010020000000000000100000120B0000120B0000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000010000000200000002
        0000000200000001000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000040000001100000023000000330000003D0000004300000046
        00000046000000420000003A0000002A00000018000000080000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000006
        0000001E3B3B3B98444444FC434343FF434343FF434343FF434343FF434343FF
        434343FF434343FF434343FF444444FD353535AA000000430000002D0000000D
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000045454568474747D1474747FF474747FF
        474747FF444444FF6A6A6AFF6D6D6DFF6D6D6DFF6D6D6DFF6D6D6DFF6D6D6DFF
        6D6D6DFF6D6D6DFF6D6D6DFF6A6A6AFF444444FF474747FF474747FF474747FF
        474747FF464646F74646468E4E4E4E0D00000000000000000000000000000000
        000000000000000000000000A4A4A40EA0A0A04BA3A3A37F939393AD818181D1
        767676EE434343FF7C7C7CFF929292FF7C7C7CFF7B7B7BFF7B7B7BFF7B7B7BFF
        7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF434343FF686868FF6F6F6FFF777777EC
        838383CC9A9A9AA2ABABAB76AAAAAA1800000000000000000000000000000000
        000000000000000000000000D3D3D317D0D0D077DFDFDFBDE0E0E0DFD5D5D5ED
        CECECEF8434343FFC2C2C2FFEFEFEFFFC3C3C3FFE7E7E7FFEFEFEFFFEFEFEFFF
        EFEFEFFFEFEFEFFFE7E7E7FFB3B3B3FF434343FFC6C6C6FFC8C8C8FFCECECEF8
        D7D7D7EBE3E3E3DBE6E6E6BFCECECE2A00000000000000000000000000000000
        000000000000000000000000555555034747476B474747D0474747FF474747FF
        474747FF444444FF868686FFA4A4A4FF8C8C8CFF8A8A8AFF8A8A8AFF8A8A8AFF
        8A8A8AFF8A8A8AFF8A8A8AFF858585FF444444FF474747FF474747FF474747FF
        474747FF474747FF494949A14A4A4A1800000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000641414189444444FB434343FF434343FF8D8D8DFF949494FF7A7A7AFF
        7A7A7AFF434343FF434343FF444444FB3B3B3B98000000260000001500000002
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000062363634583E3EFF704A4AFF744848FF6E4242FF
        6F4848FF583E3EFF4F2929440000000E000055062B2BDE362A2AE12B0000FF02
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000155000003
        000000016C3B3B346B3838BE7C4E4EFF916B6BFFA28181FFA78989FFA68787FF
        9E7C7CFF8D6666FF7A4C4CFF6C3939C26D3B3B38000080022929DD692B2BDDD5
        2B2BDFA72A2AE16E2B2BDD354040BF0400000000000000000000000000000000
        0000000000000000000000000000000000000000623B140D5D3A171640400004
        6B393970754545FCA49494FFA89491FFA47A66FFA2704EFFA16B42FF9D6742FF
        98684BFF986F5EFFA07F7CFF977272FF744545FD6B3A3A77000000002B2BDF48
        2B2BDFDF2B2BDEE92A2ADEF12A2ADEEB2B2BDFB82A2ADF792B2BE13B00000000
        00000000000000000000000000000001663C111E60391328555500036D3A3A73
        7C4F4FFFB29696FFCBCAC9FFBCBAB8FFA2754DFFB57D4BFFB37B49FFB17744FF
        AD723EFFA56D3DFF9B6D49FF98908EFF9A8282FF794B4BFF6C38387B00000000
        2B2BDF5F2B2BDFDC2B2BDEE32A2ADEEB2A2ADEF22B2BDED12B2BDF6000000000
        0000000000000000000000006737122A6939103F71391C096B3A3A39754545FC
        B59A9AFFAD7F64FFD9D9D8FFDFDFDFFFA49487FFBB895CFFBD895AFFBA8555FF
        B7804FFFB27947FF9D7E62FFB8B8B8FF938D89FF9F7E7EFF734343FE6E3B3B41
        0000FF012A2ADF9D2B2BDFD62B2BDFDD2929E15D0000FF010000000000000000
        0000000000000000663C111E673B125767371525000000006B3939C5A68787FF
        AD8673FFB19781FFEEEEEEFFF3F3F3FFF5F5F5FFB19071FFC6966CFFC39266FF
        BF8C5FFFBB8556FFAD9E90FFC8C8C8FFADABA9FF876C61FF967171FF6C3A3ACC
        000000012C2CDE2E2A2ADEC82B2BDED02C2CDE5D000000000000000000000000
        0000000040400004683A1060643A115C555500036B3A3A397F5353FFBAA09DFF
        BE9977FFF6F6F6FFFEFEFEFFFEFEFEFFFEFEFEFFB3A191FFD0A47DFFCC9F77FF
        C8996FFFC39165FFB5A494FFD7D7D7FFC9C9C9FF897464FF9E7C77FF7A4C4CFF
        6C3A3A42000000002A2ADF9E2A2ADEC22B2BDE8A000000000000000000000000
        00000000653A10306539137964371245000000006B38388CA38383FFB99685FF
        CBB39DFFFEFEFEFFFEFEFEFFFEFEFEFFDDDAD6FFCAB199FFD9B28EFFD5AC87FF
        D0A57EFFCA9C73FFCCC4BDFFE5E5E5FFD6D6D6FF948579FF9B715CFF916B6BFF
        6C383895000000002A2ADD622A2ADFB52A2ADF9F000000000000000000000000
        0000000066391167663B138766391328000000006C3939CBBEA8A8FFC39B80FF
        D1AD8DFFF9F9F8FFFEFEFEFFCBC2BBFFE6C4A5FFE5C3A4FFE2BF9FFFDCB797FF
        BEAFA2FFB39F8EFFEDEBE9FFF2F2F2FFE1E1E1FFCBCACAFF8E694EFFA38484FF
        6C3939D5000000002A2ADE3D2B2BDFA72929DFA0000000000000000000000000
        FF00000166391193663A1396623B141A000000006C3939E8CEBDBDFFBAA391FF
        C3B6AAFFD4C6BAFFD2C3B5FFEACDB2FFF0D3B7FFF0D3B8FFEDD0B6FFCEC5BDFF
        FEFEFEFFFEFEFEFFFEFEFEFFFCFCFCFFEBEBEBFFC5C2BFFF90755DFFAC8F8FFF
        6C3939F0000000002828DD2D2A2ADE992A2ADE91000000000000000000000000
        60401010663B12AA673A13A46D37120E000000006C3939E9E8E4E4FFCFC6BEFF
        D4B79CFFECCDB0FFF3D6BCFFF8DFC7FFFBE6D3FFFAE6D3FFF7E1CDFFDAD5D1FF
        FEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFF0EFEFFFB6ACA3FFBAB8B6FFAC9393FF
        6D3A3AF2000000002B2BDA302A2ADE8C2929DC76000000000000000000000000
        623B141A673A12B8673A13B360351518000000006C3939CEE6E1E1FFB8AAA1FF
        CEB79FFFEACFB5FFFAE1CBFFFFEDDCFFFFEEDDFFFFEEDEFFFCEAD8FFE0D4C9FF
        FEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFD3CFCBFFDBDAD9FFABA199FFAEA3A3FF
        6B3939D8000000002A2ADF372B2BDF7E2929E051000000000000000000000000
        66400D14663A12C6663A12C1643C1433000000006C3A3A92C7B9B9FFFEFEFEFF
        FDFCFCFFBEB2A8FFFEEBD9FFFFEEDEFFFFEFDFFFFFEFDFFFFFEFDFFFF9E6D4FF
        D4CDC6FFEAE8E7FFCBC2B9FFCBBAAAFFC6BBB2FFE6E6E6FFDCDCDCFFA19494FF
        6D3A3A9B000000002A2AE0492B2BDD712B2BDC24000000000000000000000000
        80000002673B12CC663A12CF66391467000000006E3B3B418D6666FFFBFBFAFF
        FEFEFEFFF3F2F2FFC5BEB5FFF7E8D9FFFFEFE0FFFFEFE1FFFFEFE1FFFAE7D7FF
        DEDAD6FFBAAFA5FFC2B6ACFFD9D6D3FFF8F8F8FFE5E5E5FFDFDEDEFF7E5757FF
        6C3838494040BF042828DD592A2ADE5600008002000000000000000000000000
        00000000663B12AD663B12DE673A13BA00000001000000016C3939CEDAD2D2FF
        FEFEFEFFFDFDFDFFFAFAFAFFC8C3BFFFEADED2FFFFF0E2FFFEEEDFFFF8E6D5FF
        E3D2C3FFEDECEBFFFEFEFEFFFEFEFEFFF4F4F4FFE7E7E7FFB8AFAFFF6C3939D5
        800000022626E1222B2BDE4E2929DE1F00000000000000000000000000000000
        0000000065391474673A12EC673B13E7653C114C000000006B3939437E5252FE
        F6F4F4FFCBC2BCFFDCD6D1FFDFDDDBFFF7E9DBFFF9E9DCFFF9E9DAFFF5E2D1FF
        E1D2C5FFD0C3B8FFC7BEB6FFF6F5F4FFEFEFEFFFDAD8D8FF784B4BFF6D38384D
        3333FF052B2BD9362A2ADB2B000000000000000000000000000000006B3A101F
        673B1392673B13F4663A12FA663A13F5663B12D4693C0F11000000006B383883
        916D6DFFF2F0F0FFCCC2BDFFCCC1B6FFDED0C4FFD5D3D0FFCBBEB4FFDCCCBFFF
        E7D1BCFFDBC8B5FFD4D0CCFFECEBEAFFD8D3D3FF835E5EFF6B38388C0000FF01
        2C2CDC1D2828DF200000FF0200000000000000000000000000000000673B13A3
        663B13E5673B13FF673B13FF673B13FF663A12FE673A13B3552B000600000001
        6C3A3A84845858FED5C9C9FFE8E3E2FFD2C2BBFFECEBEAFFFEFEFEFFBDB8B3FF
        DDC5B2FFCEB4A7FFCBB7B4FFBDA8A8FF7B4D4DFF6C39398B555555031717E80B
        2727D80D0000FF01000000000000000000000000000000000000000000000000
        000000006A391524673B1263673B13A1673B13DF673B13FF683B14B660401010
        000000006D3A3A466C3939CF957272FFC1B1B1FFDCD2D2FFEAE5E5FFE0D9D9FF
        C8B5B5FFAA8C8CFF875D5DFF6C3939D56C383849000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000663C111E683C135D683A137B
        673D122A00000000000000016B3939436D3A3A966C3939D46C3939EF6C3939F0
        6C3939D56C3939996C3939478000000200000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000000000000000000000FFFFFFFF
        FFFC1FFFFFC001FFFF00007FF8000007F0000007F0000007F0000007FF80007F
        FFF0007FFE000007FC000100F0000080F0000001E2000003C0000023C4000023
        C40000238400002384000023840000238400002384000003C0000007C200000F
        0100000F0000001FC04003FFFC2007FFFFFFFFFFFFFFFFFFFFFFFFFF28000000
        1000000020000000010020000000000000040000120B0000120B000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000A4FF0000A4FF0000A4FF0000A4FF0000A4FF0000A4FF0000A4FF
        0000A44C00000000000000000000000021121006956F57A69F765AEEA3795AFE
        A4795AFFA27859FB9D7458D9211893C20000A4FF0000A4FF0000A4FF0000A44C
        00000000000000000000000089614B59A68369F9B69277FFCEB49FFFDCC8B7FF
        E1CFBEFFDCC7B4FFCCB29BFFB49175FF71546EE00B089EC10000A44C00000000
        000000000000000094695150A67D5DFFCBB09AFFE2D7CCFFB7906DFFBD8A5CFF
        BA8656FFB6804FFFB78556FFD0C1B3FFC7AB93FF886565F10B089EC10000A44C
        0000000000000000A87F61F3CBB29CFFE9E3DCFFF6F6F6FFC6BFB8FFC99B71FF
        C6966BFFC18F62FFBC8859FFD1D1D1FFCCC4BCFFC8AC94FF614877DA0000A4FF
        0000A44CA37A6095B7957AFFE9DFD6FFFEFEFEFFFEFEFEFFEFEEEDFFCEA783FF
        D1A780FFCB9F75FFC5966CFFE0E0E0FFD3D3D3FFC9B7A5FFAD8B76FF161199D0
        0000A481A1775CE8C2A795FFE5E1DCFFFEFEFEFFE3E1DFFFDABDA2FFE0BD9CFF
        DCB693FFCAA684FFC7A78BFFF5F5F5FFE2E2E2FFAEA69DFFCDB39DFF453384DA
        0000A4C1966A50FEC5AD9EFFCDC2B7FFD4CEC8FFE8CCB3FFEED1B5FFECCEB1FF
        D1BCA9FFF9F9F8FFF3F2F1FFFEFEFEFFEFEFEFFFD1D0CEFFDDC9B8FF74566FF0
        0000A47E784836FFCEB9ADFFE9CAACFFF2D7BCFFF9E1C9FFFCE8D5FFF9E5D2FF
        D5CFC8FFFEFEFEFFFEFEFEFFFEFEFEFFF9F9F9FFBCB5AEFFE3D2C3FF725470FD
        0000A458835540FFB19385FFD3BEAAFFFAE3CCFFFFEEDEFFFFEFDFFFFFEFDFFF
        EADBCEFFFDFDFDFFFEFEFEFFFDFDFDFFE6E4E2FFDFDDDCFFDFCDBCFF846268EE
        0000A43E906751F4946D5BFFDFDCD9FFEFDDCCFFFFEFE0FFFFF0E1FFFFF0E1FF
        FCEBDAFFD2CDC7FFD0C7BEFFC9B5A4FFC8BEB5FFE8E6E4FFD0B8A3FFA47A5AB6
        0000A42C8F644EA9784836FFCBB8AFFFE5E4E3FFDFD5CAFFFFF1E2FFFFF1E3FF
        FAEADAFFD8CFC6FFDBD9D7FFF5F4F4FFFBFBFBFFE9DFD5FFB69479FFA47A5A61
        0000000000000000997159F5936B5AFFD6CAC4FFD3D1CEFFFEEFE2FFFBEDDEFF
        F6E5D4FFD4C8BCFFCEC6C0FFE0DEDBFFF0EBE6FFCDB4A0FFA47A5AC700000000
        00000000000000009D715635986E54FE936B5AFFCDBCB4FFE2D9CFFFC4BFBAFF
        DACBBEFFEBD4BEFFD6CABEFFE9E1D9FFCEB6A3FFA3795AECA47A5A2E00000000
        00000000000000007848364C784836FF784836FF784836FF9F7B6BFFEDE4DCFF
        F4EBE6FFEBDFD5FFD6C2B1FFB9977EFFA37959CAA47A5A2E0000000000000000
        000000007848364C784836FF784836FF784836FF784836FF784836FF784836FF
        784836FFA27859F3A27758BDA47A5A6100000000000000000000000000000000
        00000000FF000000E0010000C003000080010000800000000000000000000000
        0000000000000000000000000000000000010000800300008003000080070000
        001F000028000000160000002C000000010020000000000090070000120B0000
        120B000000000000000000005555550F5555552755555551555555765555559C
        555555C9555555EB555555FC555555FF555555FFC0C0C0FF9C9C9CFF868686FF
        555555FF555555FF555555FE555555F7555555DE555555AD5555557055555540
        55555526C8C8C80FC8C8C827C8C8C84FC8C8C874C8C8C89BC8C8C8C6C8C8C8EA
        C8C8C8FBA0A0A0FF808080FFE2E2E2FFC8C8C8FFA0A0A0FF666666FF808080FF
        A0A0A0FEC8C8C8F6C8C8C8D8C8C8C8A1C8C8C860C8C8C832C8C8C81C55555524
        555555455555556E55555590555555B1535353D8545454F2555555FD555555FF
        555555FFE2E2E2FEE2E2E2FEC7C7C7FE555555FF0000CCFF0000CCFF0000CCFF
        0000CCFF0000CCFF0000CCFF0000CCFF1A1AA76E000000000000000000000000
        0000000300000020000000380000005038271F875F4333DE604534FA5A5755FF
        A09C9AFF5B5755FF5F4533F83A2A63E60000CCFF0000CCFF0000CCFF0000CCFF
        0000CCFF0000CC4C000000000000000000000000000000000000001600000031
        0000004C866049DAA67C5DFFBC9A7FFFCCAF97FF948A80FFCFC4BAFF978B81FF
        CAAD93FFBA987CFFA57C5CFF0000CCFF0000CCFF0000CCFF0000926A00000003
        00000000000000000000000000000000643C2D5B442E25509C7256F6B9977CFF
        D8C1ABFFB49477FFB68151FFB27947FFB07643FFAB7240FFA87447FFB28967FF
        D3B9A2FFB79476FF654A77F00000CCFF0000CCFF0000BD520000000000000000
        000000007848364C6F4232529E7357F2C1A288FFD0BEAEFFDAD9D8FFA79C92FF
        BA875AFFBB8758FFB98454FFB67F4EFFB27947FFA97445FFB6B5B4FFC0AF9FFF
        BD9B80FF664B7BE60000CCFF0000CCFF0000CC4C000000000000000078483681
        9A6E53BEBB997FFFD5B89EFFD1CDC9FFEEEEEEFFF1F1F1FFAB9785FFC5966AFF
        C39265FFBF8C5FFFBA8656FFA98C70FFC7C7C7FFB9B9B8FFC0A58EFFB79477FF
        5D4589B30000CCFF0000CC81000000007848364C7C4C3A9AA67D5DFFDDC8B6FF
        CAC1BAFFFCFCFCFFFEFEFEFFFEFEFEFFBBB0A6FFCFA37CFFCC9F76FFC8996FFF
        C39265FFB5967AFFD7D7D7FFCACACAFF968A82FFD5BBA4FFA57C5CFF0000CCFF
        0000CCC100000000784836A7845641D7BFA087FFD5B596FFFEFEFEFFFEFEFEFF
        FEFEFEFFFDFCFCFFC4B6AAFFD8B18DFFD5AC87FFD0A67EFFCB9D74FFBBA896FF
        E6E6E6FFD7D7D7FF9E948CFFB68E6DFFBB997DFF2219B3D40000CCFF00000000
        784836A793664DF0D3BCA8FFD5AE8AFFE2DEDAFFFEFEFEFFEFEEEDFFCFB79FFF
        E4C2A2FFE2BF9EFFDEB997FFBEA38BFFBC9E82FFD3CCC5FFF3F3F3FFE3E3E3FF
        CFCECEFF967152FFCBAF96FF46349AE40000CCFF00000000784836FFA47A5BFB
        E1CFBEFFC2B2A2FFCFBFB0FFD4CDC7FFD4BFABFFEED1B5FFEED0B4FFEBCDB1FF
        D6BFAAFFFCFBFBFFFEFEFEFFFEFEFEFFFEFEFEFFEDEDEDFFCDCBC9FF978676FF
        D5BDA7FF70527CF60000CCFF00000000784836FF9C7053FFE6D8CAFFC9BCAFFF
        E9C9ABFFF0D3B8FFF5DBC1FFF9E2CCFFF9E5D2FFF6E2CDFFD7CDC4FFFEFEFEFF
        FEFEFEFFFEFEFEFFFEFEFEFFF5F5F5FFB8AEA4FFBFBDBCFFDAC4AFFFA37959FE
        0000CCFF00000000784836FF966C51FDE2D2C5FFDFBFA1FFE3CAB1FFF7DDC4FF
        FEEAD7FFFFEEDEFFFFEFDEFFFDECDBFFEBDBCEFFFFFFFFFFFEFEFEFFFEFEFEFF
        FEFEFEFFE6E4E2FFCAC5C0FFB1AAA4FFD7C0AAFF70537CF40000CCA700000000
        784836FF8F644BF0D7C2B1FFEBE9E7FFBBB4AEFFFAE4CFFFFFEFDFFFFFEFDFFF
        FFEFE0FFFFEFE0FFFBE9D8FFDBD2CBFFFEFEFEFFEBE9E8FFD1C8C0FFCDC0B3FF
        EAEAEAFFD5D4D2FFCDB29BFF46349AE30000CCA700000000784836C1784836FF
        C3A68EFFF7F3EFFFFEFEFEFFBFB7AEFFFFEFE0FFFFF0E1FFFFF0E1FFFFF0E1FF
        FCEBDBFFD5CEC7FFBCB3AAFFCCBBACFFC0B6ACFFECEBEAFFE9E9E9FFDDD6CEFF
        BD9D82FF3628A6C10000CC4C0000000078483681784836FFA47A5CFFE6D8CBFF
        FDFCFCFFFEFEFEFFC2BDB8FFF0E3D7FFFFF1E2FFFFF1E3FFFAEAD9FFF4E1CFFF
        E9E8E6FFF7F7F7FFFEFEFEFFF9F9F9FFE6E5E5FFDBC6B2FFA67D5DFF140EBE93
        00000000000000007848364C784836FF784836FFC0A189FFECE4DDFFDBD2C9FF
        FEFEFEFFD8CFC6FFFEF0E2FFFBECDDFFF7E5D5FFE0D1C1FFD0C6BEFFCFC8C2FF
        F3F2F1FFF1F1F1FFE1D4C8FFBB9A7FFF5D458AB10000CC4C0000000000000000
        000000007848364C784836FF976A50E3C8AE99FFE4DAD1FFD8CDC1FFD1C6BCFF
        D1CBC5FFC7BCB1FFEBDACAFFE3CFBDFFE2CDB9FFE0DEDBFFD5D0CBFFDFD2C6FF
        C3A58CFFA37959C60000CC4C0000000000000000000000000000000000000000
        7848364C784836FF90634AE2C0A28AFFE8DBD0FFE6D8CBFFD9D5D1FFFCFCFCFF
        BAB6B2FFE9D1BAFFE0C5AAFFDEC8B3FFE1CFBDFFBC9C82FFA37959C6A47A5A17
        00000000000000000000000000000000000000007848364C784836FF784836FF
        784836FF83553FD3A67D5EFFC4A791FFD9C7B6FFE6D8CBFFEADDD2FFE3D5C7FF
        D6C1AFFFC2A38CFFA67D5EFFA47A5A9000000000000000000000000000000000
        00000000000000007848364C784836FF784836FF784836FF784836FF784836FF
        784836FF784836FFA47A5AC6A37959F1A37959FEA37959F0A47A5AC6A47A5A82
        A47A5A2400000000000000000000000000000000000000000000000000000000
        0000000000000000E0000400E0000400E0000400C0000000C000000080000000
        8000000080000000800000008000000080000000800000008000000080000400
        80000400C0000C00E0001C00C0007C008000FC00}
    end
    object LabelSubTitle: TLabel
      Left = 8
      Top = 45
      Width = 275
      Height = 19
      Caption = 'Copyright (c) 2014 Christian-W. Budde'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LabelTitle: TLabel
      Left = 45
      Top = 0
      Width = 418
      Height = 42
      Caption = 'WebUpdate Authoring Tool'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -35
      Font.Name = 'Tahoma'
      Font.Style = []
      Font.Quality = fqAntialiased
      ParentFont = False
    end
  end
end