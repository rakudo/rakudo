unit module HeredocParameterDefault;

sub heredoc-default($text = q:to/END/) is export { $text }
from the module
END
