package org.perl6.rakudo;

import org.perl6.nqp.tools.EvalServer;
import org.perl6.nqp.runtime.LibraryLoader;

public class RakudoEvalServer extends EvalServer {

    private String appname = null;
    private Class<?> byteclass = null;

    public String run(String code) throws Exception {
        if( appname != null ) {
            return super.run(appname, code);
        }
        else {
            return super.run(byteclass, code);
        }
    }

    public String run(String[] argv) throws Exception {
        if( appname != null ) {
            return super.run(appname, argv);
        }
        else {
            return super.run(byteclass, argv);
        }
    }

    /*
        Neccessary elements in the CLASSPATH are as follows:

        $P6_INSTALL_PREFIX/share/nqp/lib
        $P6_INSTALL_PREFIX/share/nqp/runtime/*
        $P6_INSTALL_PREFIX/share/perl6/lib
        $P6_INSTALL_PREFIX/share/perl6/lib/Perl6
        $P6_INSTALL_PREFIX/share/perl6/runtime
        $P6_INSTALL_PREFIX/share/perl6/runtime/*
    */
    public RakudoEvalServer(boolean selfexjar) throws Exception {
        if( selfexjar ) {
            byteclass = LibraryLoader.loadFile(getClass().getResource("/lib/perl6.jar"), true);
        }
        else {
            String[] cps = System.getProperty("java.class.path").split("[;:]");
            for (String cfile : cps) {
                if (cfile.endsWith("perl6.jar")) {
                    appname = cfile;
                    break;
                }
            }
            if (appname == null) {
                throw new RuntimeException("CLASSPATH not set properly, couldn't find perl6.jar");
            }

        }

        System.setProperty("perl6.execname", "RakudoEvalServer");
    }
}
