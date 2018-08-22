package mill.scalalib.wrapper;

import java.io.PrintStream;

/**
 * Wraps the given main method, proxying both output streams over System.out to
 * avoid race conditions on the read-side.
 */
public class MillRun {
    public static void main(String[] args) throws Throwable{
        PrintStream out = System.out;
        System.setOut(new PrintStream(new mill.main.client.ProxyOutputStream(out, 1)));
        System.setErr(new PrintStream(new mill.main.client.ProxyOutputStream(out, -1)));
        String realMain = args[0];
        String[] realArgs = new String[args.length - 1];
        for(int i = 0; i < args.length-1; i++){
            realArgs[i] = args[i+1];
        }
        java.lang.reflect.Method main = Class.forName(realMain).getMethod("main", String[].class);
        try{
            main.invoke(null, (Object)realArgs);
        }catch(java.lang.reflect.InvocationTargetException e){
            StackTraceElement[] s = e.getCause().getStackTrace();
            int found = -1;
            for(int i = 0; i < s.length; i ++){
                if (s[i].getClassName().equals(realMain)) found = i;
            }
            e.getCause().setStackTrace(
                    java.util.Arrays.copyOfRange(s, 0, found == -1 ? s.length : found)
            );

            throw e.getCause();
        }
    }
}
