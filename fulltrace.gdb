set breakpoint pending on
break MVM_gc_enter_from_allocator
commands
thread apply all bt
print MVM_dump_backtrace(MVM_running_threads_context)
c
end
break exit
commands
thread apply all bt
print MVM_dump_backtrace(MVM_running_threads_context)
c
end
break _exit
commands
thread apply all bt
print MVM_dump_backtrace(MVM_running_threads_context)
c
end
