# The EXPORTHOW declaration must survive the preceding load: the loaded
# unit's GLOBALish carries an EXPORTHOW entry, and importing that entry as a
# lexical would swallow this declaration in a merge.
need ExporthowDeclarer;
my package EXPORTHOW {
    package DECLARE {
        constant after-load-class = Metamodel::ClassHOW;
    }
}
