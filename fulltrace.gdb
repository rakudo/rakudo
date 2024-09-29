set verbose on
set trace-commands on
set breakpoint pending on
set debug symfile on
set debug symtab-create 1
set debug separate-debug-file on
set debug solib on
symbol-file ../install/bin/moar.exe.pdb
add-symbol-file ../install/bin/moar.dll.pdb
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
