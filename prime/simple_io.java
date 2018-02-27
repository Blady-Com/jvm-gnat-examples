import java.io.*;

public class simple_io {

    //  procedure New_Line;
    
    public static void new_line ()
    {
        System.out.println ();
    }

    //  procedure Put (B : Boolean);
    
    public static void put (boolean b)
    {
        System.out.print (b);
    }

    //  procedure Put (C : Character);
    
    public static void put (byte c)
    {
        System.out.print ((char) c);
    }

    //  procedure Wide_Put (W : Wide_Character);
    
    public static void wide_put (char w)
    {
        System.out.print (w);
    }

    //  procedure Put (I : Integer);
    
    public static void put (int i)
    {
        System.out.print (i);
    }

    //  procedure Put (L : Long_Integer);
    
    public static void put (long l)
    {
        System.out.print (l);
    }

    //  procedure Put (F : Float);
    
    public static void put (float f)
    {
        System.out.print (f);
    }

    //  procedure Put (D : Long_Float);
    
    public static void put (double d)
    {
        System.out.print (d);
    }

    //  procedure Put (S : String);
    
    public static void put (byte s [])
    {
        System.out.print (new String (s));
    }

    //  procedure Wide_Put (WS : Wide_String);
    
    public static void wide_put (char ws [])
    {
        System.out.print (ws);
    }

    //  procedure Put_Line (S : String);
    
    public static void put_line (byte s [])
    {
        System.out.println (new String (s));
    }

    //  procedure Wide_Put_Line (WS : Wide_String);
    
    public static void wide_put_line (char ws [])
    {
        System.out.println (ws);
    }
    //  procedure Wide_Put_Line (WS : Wide_String);

    public static int get_line (byte b [], int off, int len) throws IOException
    {
        return System.in.read (b, off, len);
    }

    public static int get_int () throws IOException
    {
        byte s [];
        int off, len, ret;

        off = 0;
        len = 128;
        s = new byte [len];
        ret = System.in.read (s, off, len);

	int end = ret - 1;

	if (s [ret - 2] < '0')
	    end--;

        return new Integer (new String (s).substring (0, end)).intValue ();
    }
}

