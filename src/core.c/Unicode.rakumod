my class Unicode {

#?if moar
    my constant NFG = True;
#?endif
#?if !moar
    my constant NFG = False;
#?endif

    # This constant specifies the current Unicode version being supported
#?if jvm
    my constant VERSION = nqp::jvmgetunicodeversion.Version;
#?endif
#?if !jvm
    my constant VERSION = (
       '1.1' => 'a',
       '2.0' => 'ẛ',
       '2.1' => '€',
       '3.0' => 'ϟ',
       '3.1' => 'ϴ',
       '3.2' => '⁇',
       '4.0' => 'ȡ',
       '4.1' => 'ℼ',
       '5.0' => 'ↄ',
       '5.1' => 'Ϗ',
       '5.2' => 'Ɒ',
       '6.0' => '✅',
       '6.1' => 'Ɦ',
       '6.2' => '₺',
       '6.3' =>  0x061C.chr,
       '7.0' =>  0x037F.chr,
       '8.0' =>  0x218A.chr,
       '9.0' =>  0xA7AE.chr,
      '10.0' =>  0x20BF.chr,
      '11.0' =>  0xA7AF.chr,
      '12.0' =>  0xA7BA.chr,
      '12.1' =>  0x32FF.chr,
      '13.0' => 0x1F972.chr,
      '14.0' =>  0x061D.chr,
      '15.0' =>  0x0CF3.chr,
      '15.1' => 0x2FFC.chr,
      '16.0' => 0x0897.chr,
      '17.0' => 0x088F.chr,
    # PLEASE ADD NEWER UNICODE VERSIONS HERE, AS SOON AS THE UNICODE
    # CONSORTIUM HAS RELEASED A NEW VERSION
    ).first(*.value.uniprop('Age') ne 'Unassigned', :end).key.Version;
#?endif

    has Version $.version = VERSION;
    has Bool    $.NFG     = NFG;

    proto method version(|) {*}
    multi method version(Unicode:U:) { VERSION   }
    multi method version(Unicode:D:) { $!version }

    proto method NFG(|) {*}
    multi method NFG(Unicode:U:) { NFG   }
    multi method NFG(Unicode:D:) { $!NFG }
}

# vim: expandtab shiftwidth=4
