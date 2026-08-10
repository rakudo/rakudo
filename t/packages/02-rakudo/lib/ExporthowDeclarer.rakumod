# A unit that declares a my scoped EXPORTHOW. Declaring one also places an
# EXPORTHOW entry in the unit's GLOBALish, which loaders of this unit see.
my package EXPORTHOW {
    package DECLARE {
        constant declarer-class = Metamodel::ClassHOW;
    }
}
