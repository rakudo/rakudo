my class Rat { ... }
my class X::Cannot::Capture       { ... }
my class X::Numeric::DivideByZero { ... }
my class X::NYI::BigInt { ... }

my class Int { ... }
my subset UInt of Int where {
    nqp::not_i(nqp::isconcrete($_)) || nqp::isge_I($_,0)
}

my class Int does Real { # declared in BOOTSTRAP
    # class Int is Cool
    #     has bigint $!value is box_target;

    multi method WHICH(Int:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Int),
              'Int|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::tostr_I(self)
          ),
          ValueObjAt
        )
    }

    proto method new(|) {*}
    multi method new(Any:U $type) {
        die "Cannot create an Int from a '$type.^name()' type object";
    }
    multi method new(Any:D \value --> Int:D) { self.new: value.Int }
    multi method new(int   \value --> Int:D) {
        # rebox the value, so we get rid of any potential mixins
        nqp::fromI_I(nqp::decont(value), self)
    }
    multi method new(Int:D \value = 0 --> Int:D) {
        # rebox the value, so we get rid of any potential mixins
        nqp::fromI_I(nqp::decont(value), self)
    }

    multi method raku(Int:D: --> Str:D) {
        self.Str;
    }
    multi method Bool(Int:D: --> Bool:D) {
        nqp::hllbool(nqp::bool_I(self));
    }

    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }

    method Int() { self }

    method sign(Int:D: --> Int:D) {
        nqp::isgt_I(self,0) || nqp::neg_i(nqp::islt_I(self,0))
    }

    multi method Str(Int:D: --> Str:D) {
        nqp::p6box_s(nqp::tostr_I(self));
    }

    method Num(Int:D: --> Num:D) {
        nqp::p6box_n(nqp::tonum_I(self));
    }

    method Rat(Int:D: $? --> Rat:D) {
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(Rat),Rat,'$!numerator',self),
          Rat,'$!denominator',1
        )
    }
    method FatRat(Int:D: $? --> FatRat:D) {
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(FatRat),FatRat,'$!numerator',self),
          FatRat,'$!denominator',1
        )
    }

    method abs(Int:D: --> Int:D) {
        nqp::abs_I(self, Int)
    }

    method Bridge(Int:D: --> Num:D) {
        nqp::p6box_n(nqp::tonum_I(self));
    }

    method chr(Int:D: --> Str:D) {
        nqp::if(
          nqp::isbig_I(self),
          die("chr codepoint %i (0x%X) is out of bounds".sprintf(self, self)),
          nqp::p6box_s(nqp::chr(nqp::unbox_i(self)))
        )
    }

    method sqrt(Int:D: --> Num:D) {
        nqp::p6box_n(nqp::sqrt_n(nqp::tonum_I(self)))
    }

    proto method base(|) {*}
    multi method base(Int:D: Int:D $base --> Str:D) {
        2 <= $base <= 36
          ?? nqp::p6box_s(nqp::base_I(self,nqp::unbox_i($base)))
          !! Failure.new(X::OutOfRange.new(
               what => "base argument to base", :got($base), :range<2..36>))
    }
    multi method base(Int:D: Int(Cool) $base, $digits? --> Str:D) {
        2 <= $base <= 36
          ?? $digits && ! nqp::istype($digits, Whatever)
            ?? $digits < 0
              ?? Failure.new(X::OutOfRange.new(
                   :what('digits argument to base'),:got($digits),:range<0..1073741824>))
              !!  nqp::p6box_s(nqp::base_I(self,nqp::unbox_i($base)))
                    ~ '.'
                    ~ '0' x $digits
            !! nqp::p6box_s(nqp::base_I(self,nqp::unbox_i($base)))
          !! Failure.new(X::OutOfRange.new(
               :what('base argument to base'),:got($base),:range<2..36>))
    }
    method !eggify($egg --> Str:D) { self.base(2).trans("01" => $egg) }
    multi method base(Int:D: "camel" --> Str:D) { self!eggify: "🐪🐫" }
    multi method base(Int:D: "beer"  --> Str:D) { self!eggify: "🍺🍻" }

    # If self is Int, we assume mods are Ints also.  (div fails otherwise.)
    # If do-not-want, user should cast invocant to proper domain.
    method polymod(Int:D: +@mods --> Seq:D) {
        fail X::OutOfRange.new(
          :what('invocant to polymod'), :got(self), :range<0..^Inf>
        ) if self < 0;

        gather {
            my $more = self;
            if @mods.is-lazy {
                for @mods -> $mod {
                    $more
                      ?? $mod
                        ?? take $more mod $mod
                        !! Failure.new(X::Numeric::DivideByZero.new:
                             using => 'polymod', numerator => $more)
                      !! last;
                    $more = $more div $mod;
                }
                take $more if $more;
            }
            else {
                for @mods -> $mod {
                    $mod
                      ?? take $more mod $mod
                      !! Failure.new(X::Numeric::DivideByZero.new:
                           using => 'polymod', numerator => $more);
                    $more = $more div $mod;
                }
                take $more;
            }
        }
    }

    method expmod(Int:D: Int:D \base, Int:D \mod --> Int:D) {
        nqp::expmod_I(self, nqp::decont(base), nqp::decont(mod), Int);
    }
    method is-prime(--> Bool:D) {
        my constant bits = 0x0000000000000C02_1000120240148200_9491012009000419_6010824020082800_5228801A00108040_0088011481280600_0400104032110800_0844004360848128_0041009409040212_2180C34816010400_1900240001305801_0048018240040043_0084082020922420_4228061100200B40_3000412002448002_0000812004010010_9001300204900828_800028861B040400_6120000100800810_0300005800B21160_020860000A410480_0426580000800C02_002010804002130C_8008408011000080_40B44240900200A0_400012802C00C221_244320009000A402_0000106400500084_0148845220820100_100101320005104A_0010C20806084410_0001008024200141_06020C2012480211_4100002430C84002_1225240000020145_0240411042280200_28A4030424110810_0020848008925009_82420C100108200A_45A0880002412900_010C040028101001_940A20145200200A_0404800030004424_084182C301000240_80992400C02000C8_2906020104400420_900010180014C200_809101001840A280_4502184580802002_030020030020080C_0200000440090040_2D00120096980800_0220800920001128_200B009618400082_2100826010912402_480000112000C840_90024002D1008411_0C20910102422430_C109828001041848_0000281082600082_4182D12520404810_0040225260204340_108008A002400211_00124001948A0104_0001200140869004_2050040088201000_0884000896000410_1024000B40108205_2458000200492200_0524400090802986_1900301800001021_044120020204A408_0080910C02120000_402004900C020908_300808000205A082_0114000C000B4026_0900001001108808_82102CA042040090_20101A24008B2904_9028020244320020_109061040A089000_0000110C34012022_C800B00049904204_0212081409242200_01040840144004A0_5204000101029240_A0C1210051250010_0810080902124420_0369940200241100_00102D22000180C0_0002002102400C80_0028204020000309_0041000601408200_0110000104004084_0004020B08141808_2048009440401002_25000808A0180522_880130C84006800D_26024C0211000212_40A0080080102502_414016000C200260_00C0418280208011_40060104108821A0_0000045224004000_8281081088050400_0006000822410026_0044204244100840_8443058402081680_49128001140A2100_9205800244000061_2088000000600481_00004A4C30010400_0804048820064028_2609401200402250_0080406886020004_1300060109000020_A448419000082041_0486900410424100_830902000900000C_3000203012001081_2090520102500C00_0040008804304A01_02912484090C1000_6000D22000080884_C208860200929000_1002648001200281_0002410882014102_100400814100100C_0410008418051048_2034010000100482_1248909848004060_008200161021A60A_0010000100404504_8A08008124040200_2001083200008400_6092402C001B0020_0001201025108029_840300224A0C9000_0400120410C20002_4008240148B08148_2210610000001202_0C845A0006810930_8060204048801008_00510410802C104A_01844220000129A0_4844000009008041_1000609482002243_0000090802C04424_0020805024801B08_B4104002100524C1_00008101821204B0_1061001040B4C120_0642280008401081_0030180180400802_0120241044008908_2098418081481640_00A0004802080400_C224800A0002C101_240B049610011040_00000368041001A4_5104201841200004_10002012820D0208_4414084402902010_C001124009A24008_8080080288000043_0000410804584014_0808124A60000101_1212000052008081_0930106004C10882_000C200100008024_004A402402011400_21824A4030100400_8021108108809100_8050002400281080_0414084102822982_1900209005200A05_24C80914030C8008_08300009108A2024_002086C12C004108_0281050000441040_0904902080030480_092002180084C201_0052050040401009_20300800A4020100_D008A0490880100C_0202218008400081_2006520804800002_C201200A21800008_80034CA080052010_0020404010400524_4800821068128024_1043200092050649_08800800004800B0_C108121120201044_0408293010419040_4004002800014416_0000008045200928_145020A400402218_2030D22080004000_8304204908029060_10080510026000D0_2500024814880100_0821344109848200_0640480280250080_04108A0002102422_1800A00008204800_9042419002240058_4482112012400024_8240105000221308_0280482040202400_0004400004490002_080C02C025040140_941220A4520C2000_09004000A0024882_4109841A00940001_008A040009410640_00840000A0810500_120084C06006C200_00590020004C3050_04A0820092822024_4044209824124041_804101004009A240_0880810000500120_C108008029804244_2608600292658400_4800D10500100802_9128108001104200_80820C86104C1019_0420C20530080010_8101001044008029_0082608048600411_0024190032084800_1064008009141100_80114480980D0240_6034820082420802_1348000804300820_040308164100020B_0030004120404400_0800100004020000_361129008A00304A_20005005000000B0_890402580020C808_84000124000C2211_00020200844B2180_8000064008128049_101820A042009002_04040800A2102432_5001A40000840200_8010080018040082_2580002102400182_0808241008001220_940A680042202042_0420904902C84420_4200800200820904_3088212208251400_48164308801004A0_0020101840808360_0641050611008009_0512104020014086_8228001240861004_10480020C2411050_2000420410112030_1045204220029100_8042009418290000_4400800080400082_5100100068120A00_94400010C0248019_0020102022CA0100_410100030D260840_128141108260248B_01801008001840A4_8104205000A04048_061008004200A680_2412886100884016_0224001008109804_20C80500012010C0_2020004404082C00_1245108308100001_0212000609000280_4400004880C02880_0048900060205800_2040210280050248_4010004010124000_0941808129044100_908849300A218041_2986100804490024_812802806104C109_1212058201400090_0020C02000402810_900922000C00006C_3000219009001442_0022010804110822_0040A00240921024_8402042400250252_20000901008000A0_0A08B00100001024_90C02000D3080201_08148801220A6090_4048040008840240_2400040048012408_09121224A0520080_1928008A04A0C801_001120060240A600_0122004400882000_0101841100220001_220A009000011090_2D06180420000420_1024340148808108_0618489290400000_2090414000112020_4040840109204005_0083200091000019_0424802C32400014_4140804204801100_0088080010010482_2012812022400CA2_0065200040940200_141001A04B003089_2110C84400000110_8029061840808844_02920424486004C3_04820A0090116510_0000008000060224_0448082488050008_0430024804900080_0140061821221821_A001011400008002_4C128940301A0100_C200125120A04008_1400280240601002_48840121A4400812_8941108205140001_12020C0018008480_2100020404484806_0300040A0C22100C_021824204A208211_0502094000094802_484130C160101020_245800A480212018_40908A0014020C80_100086884C10D204_2481011091080040_0820006810C00184_0228040221064900_0418001242008040_6900D100801244B4_8920020004148121_12C0098612042000_2410084080410180_0208044008B60100_2000212043490002_2020084480090530_0041100A4084400C_2001440201400082_2480800914000920_080C90100D028260_140B400401240653_0012902022880484_4020001001040A08_2410042200210400_61120008820A4032_9005100824008940_80020C0241001409_4930580100C00802_8008004140229005_009000A042018600_0980014406106010_5045844028001028_044A4810080C1280_6004014800810420_490C120120008A01_000A2910C1048410_00140100000A2130_0001104300225040_80104D0212009008_0106120024100816_004410122400C240_8610242018040019_0022902084804890_5020820148809904_10CA6480080106C1_00200A0090184102_D001048160040000_860A080011403048_0120406012110904_0004148104020200_208028020009A400_0CA0810900006010_0920029028045108_02810C000A408001_0092032884484406_810C009800040B09_8002048042088290_6800426080810106_8009204240000861_320020140A201413_01801A0026002100_C044000829901304_0203448010410258_2120C04012000020_120C30800D108260_94806000000D0418_00A20140220064A0_0129908104005840_8201050018201082_0880400522024002_1840209001004900_005120A240488010_4102402184830000_D005860100340848_1088412008491252_0002010830806402_1820044948840204_0408040010691288_0480032100100500_0040040804104244_00C328000304A00A_0082980812902010_C048840020A21048_12980020804000C1_0806400984004420_082C201244000A60_1043002400042209_4800000480484112_D000020A48100021_2052201080408601_0504420092100000_920420410100D10C_205100A081000048_6010032980002404_4000048028229020_8440488202280210_44160104100860A0_012910D000220204_9608252200050001_0004C20480000004_1969100040B04220_8003288418082410_65004A0530884010_030020400000116D_0090001480200443_0120584834000C00_402090012102022C_8000000219002208_4004080904100880_4040840028304024_2042091091200600_00A4184400520480_890884D124201300_0200413040040042_2812D225001A4824_0841220224300100_82C1052610402200_04121840200B4080_C10D240808948808_3200051001218011_0800090422902532_5020004A01920208_2042000608640048_44000201049021A4_4304B0104C205000_1401288092010041_0424800902820590_4A41020228820004_34000000C2010489_00808308224A0820_0168000200148800_0001240041480401_41121801A0824800_800C805004A20101_100040A449098640_0882110404884800_8A20008A08004120_84010CA000042280_2104086880C02504_4000021961108840_340240929108000B_00A0010120900434_0820124209040208_808061121A2404C0_6084032100194080_0004000065808000_101200805940A091_0400C82014490814_0009000204048800_205A00A480000211_0884480806080C00_0200040260009121_02080C269061104A_4520080900810402_1340301160005004_2009018442080202_4010800510806424_0240028329060848_108104101A609042_0114122480424400_8828228841A00140_9612010000048401_0C00922404482104_1100A00108120061_0212400401689091_2080420830084820_C801340020920300_0000480000010258_0500096906020004_1840A09104021020_A480218212002401_00020944104A2130_4260028000040304_200901024801B002_0800402C22110084_8920300804A0C800_049009860A0C1008_4402004190810890_D021264008160008_20C241A000000050_0982004000900522_0805008920060304_80584C2011080080_00944B4186512020_5100848845000205_2081009683048418_0000800822886010_C000920221805044_84092830406404C0_08040029800344A2_004000D204800048_1043202253001600_2920820030486100_5329040B04B00005_20500420C84080C0_0C021244B2006012_0844240000020221_0218001009003008_41B0020802912020_090084802C205801_144000000005A200_40149820004A2584_8A0014D009A20300_0218482090240009_2004520080410C26_1841120044240008_1291048412480001_0100D02104416006_0100A0020C008024_02920400C2410082_2424110034094930_0820804060048008_2009400019282050_2020400190402400_020C801140208245_8001280040210042_4482186802022480_8908040220841908_248121320040040A_0012D10002010404_9044008A20240148_1402098012089080_4D120A60A4826086_5005024040808940_32120000C0008611_2C00000422100C02_080020C309041200_A2014080912C00C0_4420002802912982_4A0CB40005301004_80812000C0008618_04009801104A0184_0009001100020348_2418002048200008_0806820526020812_0124009A00900861_80C1090441009008_01325864B0400912_0004804140200105_1080210489089202_0422014414002C30_4201204921104009_A2020004104502C0_41008A6090810120_1900201900228244_0408409210290413_04B0100502000000_C948808300804844_A0106C000044A442_0004832120094810_8101005804004328_10802C2010402290_04109040A0404004_1224820008820100_200A048442011400_2806480826080110_4000100028848024_0610081411692248_41204A0910520400_104C248941001A24_10404912C0080018_4C800168129040B4_4320041001020808_1008003040249081_6084020020134404_8808204020908B41_8652210043009010_2400486400012802_9100860944320840_22C06400006804C2_0084030404910010_484180480002822D_82080400014020D2_0520C06092820CA0_4248101009100804_108A218050282048_4830810402104180_016186112C001340_A69020001020948B_0002402D00024400_8024201A21244A01_024100260008B610_0C028221100B0890_1100001804060968_20C2041008018202_0900020C84106002_C00030802894010D_244A001080441018_2010094106812524_100882804C205824_3041089003002443_0024110102982084_4309004000001240_92090C20024124C9_0916D020A6400004_0844101221048268_00C0240601000099_4020480510024082_000D820004200800_0252040448209042_0884004010012800_1060340841005229_204140228104101A_4180416014920884_1004869801104061_B0004006520C0213_0C82100820C20080_C801061101200304_30002810C0650082_00841324A0120830_1169008844940260_001008801A0C8088_0102422404004916_420126020092900C_12880020C8200200_28241000A6010530_1820A00049120108_0049400681252000_4130424080400120_500C900168000060_80892816D0010009_0806080102924194_0200009124A04904_368000028A41B042_0900C10884080814_0124228204108941_84030922184CA011_2100920410432102_4225044008140008_104A212001288602_20A44040A2892522_5020204140964001_80034C9408081080_41848A2906010024_0B00841000320040_840140948000200B_0812014030C000A0_006112C02D02010C_00884402C80130C1_6984010122404810_8101021020844921_00520C0408003281_4120022110430002_9228A00904A40008_125001B00A489040_2CA2500004104C10_8840240A01109100_8249402610491012_6084892890120504_5108340028120041_1448618202240003_4480096500180134_0028024309064A04_908028009A44904B_0000820C06114010_010500522000C008_809320A00A408209_0810180114820890_5228004040161840_20D004A0C3080200_2122404430004832_0264848000844201_02520C028029208A_0494400880030106_0840B40160008801_04096110C101A24A_0082010512886400_8A6086400C001800_048044320240A083_20941220A41804A4_180110C04120CA41_020000A241081209_2C00D86424812004_D028804340101824_1282441481480482_0022130406980032_084520004802C10C_860A00A011252092_2514480906810C04_4200B09104000240_A0CA000611210010_4816814802586100_8229020024845904_24080130100020C1_4882402D20410490_014000D040A40A29_140128040A0C9418_6512422194032010_120100100C061061_020004A442681210_0880190480084102_8224148929860004_A2020C9011680218_6030014084C30906_4040208805321A01_1000211403002400_4424990912486084_C90804522C004208_861104309204A440_28024001020A0090_1865328244908A00_04D0048452043698_0800084524002982_C02C801348348924_000A042049019040_05A0104422812000_926094022080C329_2058489608481048_4000814196800880_530082090932C040_24880906002D2043_0092900C10480424_430800112186430C_32100100D0258082_60901300264B0400_802832CA01140868_10C0250219002488_4520582024894810_C20C26584822006D_10100480C0618283_2926000836004512_0A41A00101840128_241140A218003250_2084490880522502_034C108144309A25_A48B081051018200_4CA2100800522094_092094D204A6400C_14916022C044A002_0110D301821B0484_086122D22400C060_12410DA408088210_6900924430434006_9004265940A28948_220825B082689681_0804490000982D32_C02104C941124221_80124496804C3098_65048928125108A0_0B40B4086C304205_148A48844225064B_0834992132424030_4A2882D129861144_A48961205A0434C9_2196820D864A4C32_816D129A64B4CB6E;
        nqp::hllbool(
          (nqp::bitand_I(self,1,Int) || nqp::iseq_I(self,2))
            && nqp::isle_I(self,100000)
                 ?? nqp::bitand_I(
                      nqp::bitshiftr_I(bits,nqp::bitshiftr_I(self,1,Int),Int),
                      1,
                      Int
                    )
                 !! nqp::isprime_I(self,100)
        )
    }

    method floor(Int:D:) { self }
    method ceiling(Int:D:) { self }
    proto method round(|) {*}
    multi method round(Int:D:) { self }
    multi method round(Int:D: Real(Cool) $scale --> Real:D) {
        (self / $scale + 1/2).floor * $scale
    }

    method lsb(Int:D: --> Int:D) {
        nqp::unless(
          self, # short-circuit `0`, as it doesn't have any bits set…
          Nil,  # … and the algo we'll use requires at least one that is.
          nqp::stmts(
            (my int $lsb),
            (my $x := nqp::abs_I(self, Int)),
            nqp::while( # "fast-forward": shift off by whole all-zero-bit bytes
              nqp::isfalse(nqp::bitand_I($x, 0xFF, Int)),
              nqp::stmts(
                ($lsb += 8),
                ($x := nqp::bitshiftr_I($x, 8, Int)))),
            nqp::while( # our lsb is in the current byte; shift off zero bits
              nqp::isfalse(nqp::bitand_I($x, 0x01, Int)),
              nqp::stmts(
                ++$lsb,
                ($x := nqp::bitshiftr_I($x, 1, Int)))),
            $lsb)) # we shifted enough to get to the first set bit
    }

    method msb(Int:D: --> Int:D) {
        nqp::unless(
          self,
          Nil,
          nqp::if(
            nqp::iseq_I(self, -1),
            0,
            nqp::stmts(
              (my int $msb),
              (my $x := self),
              nqp::islt_I($x, 0) # handle conversion of negatives
                && ($x := nqp::mul_I(-2,
                  nqp::add_I($x, 1, Int), Int)),
              nqp::while(
                nqp::isgt_I($x, 0xFF),
                nqp::stmts(
                  ($msb += 8),
                  ($x := nqp::bitshiftr_I($x, 8, Int)))),
              nqp::isgt_I($x, 0x0F)
                && ($msb += 4) && ($x := nqp::bitshiftr_I($x, 4, Int)),
                 nqp::bitand_I($x, 0x8, Int) && ($msb += 3)
              || nqp::bitand_I($x, 0x4, Int) && ($msb += 2)
              || nqp::bitand_I($x, 0x2, Int) && ($msb += 1),
              $msb)))
    }

    method narrow(Int:D:) { self }

    method Range(Int:U: --> Range:D) {
        given self {
            when int  { $?BITS == 64 ??  int64.Range !!  int32.Range }
            when uint { $?BITS == 64 ?? uint64.Range !! uint32.Range }

            when int64  { Range.new(-9223372036854775808, 9223372036854775807) }
            when int32  { Range.new(         -2147483648, 2147483647         ) }
            when int16  { Range.new(              -32768, 32767              ) }
            when int8   { Range.new(                -128, 127                ) }
            # Bring back in a future Raku version, or just put on the type object
            #when int4   { Range.new(                  -8, 7                  ) }
            #when int2   { Range.new(                  -2, 1                  ) }
            #when int1   { Range.new(                  -1, 0                  ) }

            when uint64 { Range.new( 0, 18446744073709551615 ) }
            when uint32 { Range.new( 0, 4294967295           ) }
            when uint16 { Range.new( 0, 65535                ) }
            when uint8  { Range.new( 0, 255                  ) }
            when byte   { Range.new( 0, 255                  ) }
            # Bring back in a future Raku version, or just put on the type object
            #when uint4  { Range.new( 0, 15                   ) }
            #when uint2  { Range.new( 0, 3                    ) }
            #when uint1  { Range.new( 0, 1                    ) }

            default {  # some other kind of Int
                .^name eq 'UInt'
                  ?? Range.new(    0, Inf, :excludes-max )
                  !! Range.new( -Inf, Inf, :excludes-min, :excludes-max )
            }
        }
    }

    my $nuprop := nqp::null;
    my $deprop := nqp::null;
    method unival(Int:D:) {
        my str $de = nqp::getuniprop_str(
          self,
          nqp::ifnull(
            $deprop,
            $deprop := nqp::unipropcode("Numeric_Value_Denominator")
          )
        );
        nqp::if(
          nqp::chars($de),
          nqp::if(                                    # some string to work with
            nqp::iseq_s($de,"NaN"),
            NaN,                                       # no value found
            nqp::stmts(                                # value for denominator
              (my str $nu = nqp::getuniprop_str(
                self,
                nqp::ifnull(
                  $nuprop,
                  $nuprop := nqp::unipropcode("Numeric_Value_Numerator")
                )
              )),
              nqp::if(
                nqp::iseq_s($de,"1"),
                nqp::atpos(nqp::radix(10,$nu,0,0),0),   # just the numerator
                Rat.new(                                # spotted a Rat
                  nqp::atpos(nqp::radix(10,$nu,0,0),0),
                  nqp::atpos(nqp::radix(10,$de,0,0),0)
                )
              )
            )
          ),
          Nil                                          # no string, so no value
        )
    }
}

multi sub prefix:<++>(Int:D $a is rw --> Int:D) {
    $a = nqp::add_I(nqp::decont($a), 1, Int);
}
multi sub prefix:<++>(int $a is rw --> int) {
    $a = nqp::add_i($a, 1);
}
multi sub prefix:<-->(Int:D $a is rw --> Int:D) {
    $a = nqp::sub_I(nqp::decont($a), 1, Int);
}
multi sub prefix:<-->(int $a is rw --> int) {
    $a = nqp::sub_i($a, 1);
}
multi sub postfix:<++>(Int:D $a is rw --> Int:D) {
    my \b := nqp::decont($a);
    $a = nqp::add_I(b, 1, Int);
    b
}
multi sub postfix:<++>(int $a is rw --> int) {
    my int $b = $a;
    $a = nqp::add_i($b, 1);
    $b
}
multi sub postfix:<-->(Int:D $a is rw --> Int:D) {
    my \b := nqp::decont($a);
    $a = nqp::sub_I(b, 1, Int);
    b
}
multi sub postfix:<-->(int $a is rw --> int) {
    my int $b = $a;
    $a = nqp::sub_i($b, 1);
    $b
}

multi sub prefix:<->(Int:D \a --> Int:D) {
    nqp::neg_I(nqp::decont(a), Int);
}
multi sub prefix:<->(int $a --> int) {
    nqp::neg_i($a)
}

multi sub abs(Int:D \a --> Int:D) {
    nqp::abs_I(nqp::decont(a), Int);
}
multi sub abs(int $a --> int) {
    nqp::abs_i($a)
}

multi sub infix:<+>(Int:D \a, Int:D \b --> Int:D) {
    nqp::add_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<+>(int $a, int $b --> int) {
    nqp::add_i($a, $b)
}

multi sub infix:<->(Int:D \a, Int:D \b --> Int:D) {
    nqp::sub_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<->(int $a, int $b --> int) {
    nqp::sub_i($a, $b)
}

multi sub infix:<*>(Int:D \a, Int:D \b --> Int:D) {
    nqp::mul_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<*>(int $a, int $b --> int) {
    nqp::mul_i($a, $b);
}

multi sub infix:<eqv>(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(  # need to check types as enums such as Bool wind up here
      nqp::eqaddr($a.WHAT,$b.WHAT) && nqp::iseq_I($a,$b)
    )
}
multi sub infix:<eqv>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i($a,$b))
}

multi sub infix:<div>(Int:D \a, Int:D \b --> Int:D) {
    b
      ?? nqp::div_I(nqp::decont(a), nqp::decont(b), Int)
      !! Failure.new(X::Numeric::DivideByZero.new(:using<div>, :numerator(a)))
}
multi sub infix:<div>(int $a, int $b --> int) {
    # relies on opcode or hardware to detect division by 0
    nqp::div_i($a, $b)
}

multi sub infix:<%>(Int:D \a, Int:D \b --> Int:D) {
    nqp::if(
      nqp::isbig_I(nqp::decont(a)) || nqp::isbig_I(nqp::decont(b)),
      nqp::if(
        b,
        nqp::mod_I(nqp::decont(a),nqp::decont(b),Int),
        Failure.new(X::Numeric::DivideByZero.new(:using<%>, :numerator(a)))
      ),
      nqp::if(
        nqp::isne_i(b,0),
        nqp::mod_i(    # quick fix https://github.com/Raku/old-issue-tracker/issues/4999
          nqp::add_i(nqp::mod_i(nqp::decont(a),nqp::decont(b)),b),
          nqp::decont(b)
        ),
        Failure.new(X::Numeric::DivideByZero.new(:using<%>, :numerator(a)))
      )
    )
}
multi sub infix:<%>(int $a, int $b --> int) {
    # relies on opcode or hardware to detect division by 0
    nqp::mod_i(nqp::add_i(nqp::mod_i($a,$b),$b),$b) # quick fix https://github.com/Raku/old-issue-tracker/issues/4999
}

multi sub infix:<%%>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i(nqp::mod_i($a, $b), 0))
}

multi sub infix:<**>(Int:D \a, Int:D \b --> Real:D) {
    my $power := nqp::pow_I(nqp::decont(a), nqp::decont(b >= 0 ?? b !! -b), Num, Int);
    # when a**b is too big nqp::pow_I returns Inf
    nqp::istype($power, Num)
        ?? Failure.new(
            b >= 0 ?? X::Numeric::Overflow.new !! X::Numeric::Underflow.new
        ) !! b >= 0 ?? $power
            !! ($power := 1 / $power) == 0 && a != 0
                ?? Failure.new(X::Numeric::Underflow.new)
                    !! $power;
}

multi sub infix:<**>(int $a, int $b --> int) {
    nqp::pow_i($a, $b);
}

multi sub infix:<lcm>(Int:D \a, Int:D \b --> Int:D) {
    nqp::lcm_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<lcm>(int $a, int $b --> int) {
    nqp::lcm_i($a, $b)
}

multi sub infix:<gcd>(Int:D \a, Int:D \b --> Int:D) {
    nqp::gcd_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<gcd>(int $a, int $b --> int) {
    nqp::gcd_i($a, $b)
}

multi sub infix:<===>(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr($a.WHAT,$b.WHAT)
      && nqp::iseq_I($a, $b)
    )
}
multi sub infix:<===>(int $a, int $b --> Bool:D) {
    # hey, the optimizer is smart enough to figure that one out for us, no?
    $a == $b
}

multi sub infix:<==>(Int:D \a, Int:D \b --> Bool:D) {
    nqp::hllbool(nqp::iseq_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:<==>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i($a, $b))
}

multi sub infix:<!=>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isne_i($a, $b))
}
multi sub infix:<!=>(Int:D \a, Int:D \b --> Bool:D) {
    nqp::hllbool(nqp::isne_I(nqp::decont(a), nqp::decont(b)))
}

multi sub infix:«<»(Int:D \a, Int:D \b --> Bool:D) {
    nqp::hllbool(nqp::islt_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«<»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::islt_i($a, $b))
}

multi sub infix:«<=»(Int:D \a, Int:D \b --> Bool:D) {
    nqp::hllbool(nqp::isle_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«<=»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isle_i($a, $b))
}

multi sub infix:«>»(Int:D \a, Int:D \b --> Bool:D) {
    nqp::hllbool(nqp::isgt_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«>»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_i($a, $b))
}

multi sub infix:«>=»(Int:D \a, Int:D \b --> Bool:D) {
    nqp::hllbool(nqp::isge_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«>=»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($a, $b))
}

multi sub infix:<+|>(Int:D \a, Int:D \b --> Int:D) {
    nqp::bitor_I(nqp::decont(a), nqp::decont(b), Int)
}
multi sub infix:<+|>(int $a, int $b --> int) {
   nqp::bitor_i($a, $b)
}

multi sub infix:<+&>(Int:D \a, Int:D \b --> Int:D) {
    nqp::bitand_I(nqp::decont(a), nqp::decont(b), Int)
}
multi sub infix:<+&>(int $a, int $b --> int) {
   nqp::bitand_i($a, $b)
}

multi sub infix:<+^>(Int:D \a, Int:D \b --> Int:D) {
    nqp::bitxor_I(nqp::decont(a), nqp::decont(b), Int)
}
multi sub infix:<+^>(int $a, int $b --> int) {
   nqp::bitxor_i($a, $b);
}

multi sub infix:«+<»(Int:D \a, Int:D \b --> Int:D) {
    nqp::bitshiftl_I(nqp::decont(a), nqp::unbox_i(b), Int)
}
multi sub infix:«+<»(int $a, int $b --> int) {
   nqp::bitshiftl_i($a, $b);
}

multi sub infix:«+>»(Int:D \a, Int:D \b --> Int:D) {
    nqp::bitshiftr_I(nqp::decont(a), nqp::unbox_i(b), Int)
}
multi sub infix:«+>»(int $a, int $b --> int) {
   nqp::bitshiftr_i($a, $b)
}

multi sub prefix:<+^>(Int:D \a --> Int:D) {
    nqp::bitneg_I(nqp::decont(a), Int);
}
multi sub prefix:<+^>(int $a --> int) {
   nqp::bitneg_i($a);
}

proto sub chr($, *%) is pure  {*}
multi sub chr(Int:D  \x --> Str:D) { x.chr        }
multi sub chr(Cool \x   --> Str:D) { x.Int.chr    }
multi sub chr(int $x    --> str)   { nqp::chr($x) }

proto sub is-prime($, *%) is pure {*}
multi sub is-prime(\x --> Int:D) { x.is-prime }

proto sub expmod($, $, $, *%) is pure  {*}
multi sub expmod(Int:D \base, Int:D \exp, Int:D \mod --> Int:D) {
    nqp::expmod_I(nqp::decont(base), nqp::decont(exp), nqp::decont(mod), Int);
}
multi sub expmod(\base, \exp, \mod --> Int:D) {
    nqp::expmod_I(nqp::decont(base.Int), nqp::decont(exp.Int), nqp::decont(mod.Int), Int);
}

proto sub lsb($, *%) {*}
multi sub lsb(Int:D \i --> Int:D) { i.lsb }

proto sub msb($, *%) {*}
multi sub msb(Int:D \i --> Int:D) { i.msb }

# vim: expandtab shiftwidth=4
