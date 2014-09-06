my role IO {
    method umask { state $ = :8( qx/umask/.chomp ) }
}
