sub shadow-helper() is export(:DEFAULT, :refine) { 'helper' }
my class Set is Set { method shadow-tag() { 'shadowed' } }
BEGIN EXPORT::refine::<Set> := Set;
