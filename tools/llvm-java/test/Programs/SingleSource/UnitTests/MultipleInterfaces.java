public class MultipleInterfaces
{
    interface I1
    {
        public boolean i1();
    }

    interface I2
    {
        public int i2();
    }

    interface I3 extends I1, I2
    {
        public float i3();
    }

    private static class C1 implements I3
    {
        public boolean i1() { return true; }
        public int i2() { return 2; }
        public float i3() { return 3.0F; }
    }

    public static void main(String[] args) {
        C1 o = new C1();
        I1 i1 = o;
        I2 i2 = o;
        I3 i3 = o;

        Test.println(o.i1());
        Test.println(o.i2());
        Test.println(o.i3());

        Test.println(i1.i1());

        Test.println(i2.i2());

        Test.println(i3.i1());
        Test.println(i3.i2());
        Test.println(i3.i3());
    }
}
