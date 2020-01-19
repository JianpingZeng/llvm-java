import java.util.*;

public class Collections4
{
    public static void main(String[] args) {
        Collection c1 = new ArrayList();
        Collections.addRandomIntsToCollection(c1);
        Collection c2 = new TreeSet(c1);
        Collection c3 = new LinkedList();
        Collections.addRandomIntsToCollection(c3);
        Test.println(c1.containsAll(c2));
        Test.println(c1.containsAll(c3));
        Collections.addRandomIntsToCollection(c1);
        Test.println(c1.containsAll(c2));
        Test.println(c1.containsAll(c3));
        Collections.printIntCollection(c1);
        Collections.printIntCollection(c2);
        Collections.printIntCollection(c3);
    }
}
