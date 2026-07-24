unit module QualifiedCallSetter;
use QualifiedCallState;

our sub set-it($v)          { QualifiedCallState::set-state($v) }
our sub whoami-via-call()   { QualifiedCallState::whoami()      }
