package hello;
public class Hello{
    public static void main(String[] args) throws Exception{
        System.out.println("HelloWorld!");

        foo();
    }
    static void foo() throws Exception{
        Thread.sleep(1/0);
    }
}