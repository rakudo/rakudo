my role IO {
    method umask { state $ = EVAL "0o" ~ qx/umask/ }
}
