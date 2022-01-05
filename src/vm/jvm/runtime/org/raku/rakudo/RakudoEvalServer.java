package org.raku.rakudo;

import org.raku.nqp.tools.EvalServer;

public class RakudoEvalServer extends EvalServer {

    private String appname = null;

    public String run(String code) throws Exception {
        return super.run(appname, code);
    }

    public String run(String[] argv) throws Exception {
        return super.run(appname, argv);
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
    public RakudoEvalServer() {
        String[] cps = System.getProperty("java.class.path").split("[;:]");
        for(String cfile : cps) {
            if(cfile.endsWith("rakudo.jar")) {
                appname = cfile;
                break;
            }
        }
        if(appname == null) {
            throw new RuntimeException("CLASSPATH not set properly, couldn't find rakudo.jar");
        }

        System.setProperty("perl6.prefix", appname.substring(0, appname.indexOf("/share")));
        System.setProperty("perl6.execname", "RakudoEvalServer");
    }
}
