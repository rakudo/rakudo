## $Id$

=head1 NAME

src/builtins/enums.pir - implements various enumerations

=cut

.namespace
.sub 'setup' :load :init
    # Set up bool role.
    .local pmc bool_role
    bool_role = "!keyword_role"("bool")
    get_global $P21, "Object"
    "!keyword_has"(bool_role, "$!bool", $P21)
    get_global $P24, "bool_role_bool"
    bool_role."add_method"("bool", $P24)
    get_global $P30, "bool_role_False"
    bool_role."add_method"("False", $P30)
    get_global $P36, "bool_role_True"
    bool_role."add_method"("True", $P36)

    # Set up anonymous class.
    .local pmc bool_class
    bool_class = "!keyword_enum"(bool_role)
    new $P0, "Int"
    assign $P0, 1
    setprop bool_class, 'enum', $P0
    get_global $P44, "bool_class_invoke"
    bool_class."add_method"("invoke", $P44, 1 :named("vtable"))
    get_global $P49, "bool_class_string"
    bool_class."add_method"("get_string", $P49, 1 :named("vtable"))
    get_global $P54, "bool_class_number"
    bool_class."add_method"("get_bool", $P54, 1 :named("vtable"))
    bool_class."add_method"("get_integer", $P54, 1 :named("vtable"))
    bool_class."add_method"("get_number", $P54, 1 :named("vtable"))

    # Set up values.
    new $P0, "Int"
    assign $P0, 0
    $P1 = bool_class."new"($P0 :named("$!bool"))
    set_hll_global ["bool"], "False", $P1
    set_hll_global "False", $P1
    new $P0, "Int"
    assign $P0, 1
    $P1 = bool_class."new"($P0 :named("$!bool"))
    set_hll_global ["bool"], "True", $P1
    set_hll_global "True", $P1
.end


# bool Role methods
.sub "bool_role_bool" :method
    getattribute $P23, self, "$!bool"
    .return ($P23)
.end
.sub "bool_role_False" :method
    getattribute $P27, self, "$!bool"
    new $P28, "Int"
    assign $P28, 0
    $P29 = "infix:eq"($P27, $P28)
    .return ($P29)
.end
.sub "bool_role_True" :method
    getattribute $P33, self, "$!bool"
    new $P34, "Int"
    assign $P34, 1
    $P35 = "infix:eq"($P33, $P34)
    .return ($P35)
.end

# bool anonymous class methods
.sub "bool_class_invoke" :method
    getattribute $P43, self, "$!bool"
    .return ($P43)
.end
.sub "bool_class_string" :method
    getattribute $P47, self, "$!bool"
    $P48 = "prefix:~"($P47)
    .return ($P48)
.end
.sub "bool_class_number" :method :lexid("26")
    getattribute $P52, self, "$!bool"
    $P53 = "prefix:+"($P52)
    .return ($P53)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
