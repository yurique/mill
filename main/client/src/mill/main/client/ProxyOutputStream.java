package mill.main.client;

import java.io.IOException;

/**
 * An OutputStream that multiplexes it's output over the target OutputStream,
 * marked by the `key` value which is either 1 or -1, to be de-multiplexed on
 * the other side by the `ProxyStreamPumper` back into two separate streams
 * while properly preserving the order of the original writes.
 *
 * Useful for transmitting stderr/stdout between processes while avoiding race
 * conditions in reading/writing to the two pipes, which can otherwise cause
 * the stderr/stdout to appear jumbled on the other side.
 */
public class ProxyOutputStream extends java.io.OutputStream {
    private java.io.OutputStream out;
    private int key;
    public ProxyOutputStream(java.io.OutputStream out, int key){
        this.out = out;
        this.key = key;
    }
    @Override public void write(int b) throws IOException {
        synchronized (out){
            out.write(key);
            out.write(b);
        }
    }
    @Override public void write(byte[] b) throws IOException {
        synchronized (out) {
            write(b, 0, b.length);
        }
    }
    @Override public void write(byte[] b, int off, int len) throws IOException {
        synchronized (out) {
            int i = 0;
            while (i < len && i + off < b.length) {
                int chunkLength = Math.min(len - i, 127);
                out.write(chunkLength * key);
                out.write(b, off + i, Math.min(b.length - off - i, chunkLength));
                i += chunkLength;
            }
        }
    }
    @Override public void flush() throws IOException {
        synchronized (out) {
            out.flush();
        }
    }
    @Override public void close() throws IOException {
        synchronized (out) {
            out.close();
        }
    }
}
