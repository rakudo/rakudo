unit module QualifiedCallGetter;
use QualifiedCallState;

our sub get-it()            { QualifiedCallState::get-state() }
our sub whoami-via-call()   { QualifiedCallState::whoami()    }
