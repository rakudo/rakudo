mijn $a = 41;
mijn $b = 666;
als $a {
    $a++
}
andersals $b {
    sterf("That is not the right number!")
}
anders {
    merk-op("Shouldn't see this")
}
zeg("incremented") als $a == 42
