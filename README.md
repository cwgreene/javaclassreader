# Java Class Reader (jcr)

This project is to create a fast java class reader. It is intended to eventually have
feature parity with `javap`.

Example Java File:

`Test.java`
```
class Test {
    private int a;
    Test() {
        a = 3;
    }
}
```

Sample output:
```
$ javac Test.java
$ cat Test.class | jcr
ClassFile
  { magic = 3405691582
  , minor = 0
  , major = 52
  , constantPool =
      [ ConstMethodRef 4 13
      , ConstFieldRef 3 14
      , ConstClass 15
      , ConstClass 16
      , ConstUtf8 "a"
      , ConstUtf8 "I"
      , ConstUtf8 "<init>"
      , ConstUtf8 "()V"
      , ConstUtf8 "Code"
      , ConstUtf8 "LineNumberTable"
      , ConstUtf8 "SourceFile"
      , ConstUtf8 "Test.java"
      , ConstNameAndType 7 8
      , ConstNameAndType 5 6
      , ConstUtf8 "Test"
      , ConstUtf8 "java/lang/Object"
      ]
  , c_access_flags = 32
  , this_class = 3
  , super_class = 4
  , interfaces = []
  , fields =
      [ Field
          { f_access_flags = 2
          , name_index = 5
          , descriptor_index = 6
          , attributes = []
          }
      ]
  , methods =
      [ Method
          { m_access_flags = 0
          , m_name_index = 7
          , m_descriptor_index = 8
          , m_attributes =
              [ Attribute
                  { attribute_name_index = 9
                  , info =
                      "\NUL\STX\NUL\SOH\NUL\NUL\NUL\n*\183\NUL\SOH*\ACK\181\NUL\STX\177\NUL\NUL\NUL\SOH\NUL\n\NUL\NUL\NUL\SO\NUL\ETX\NUL\NUL\NUL\ETX\NUL\EOT\NUL\EOT\NUL\t\NUL\ENQ"
                  }
              ]
          }
      ]
  , cl_attributes =
      [ Attribute { attribute_name_index = 11 , info = "\NUL\f" } ]
  }
```

`javap` for comparison:
```
$ javap -v Test.class
Classfile /home/chris/projects/javaclassreader/Test.class
  Last modified Oct 12, 2017; size 221 bytes
  MD5 checksum ff962e2a245f190df9c02a85805cbdfc
  Compiled from "Test.java"
class Test
  minor version: 0
  major version: 52
  flags: ACC_SUPER
Constant pool:
   #1 = Methodref          #4.#13         // java/lang/Object."<init>":()V
   #2 = Fieldref           #3.#14         // Test.a:I
   #3 = Class              #15            // Test
   #4 = Class              #16            // java/lang/Object
   #5 = Utf8               a
   #6 = Utf8               I
   #7 = Utf8               <init>
   #8 = Utf8               ()V
   #9 = Utf8               Code
  #10 = Utf8               LineNumberTable
  #11 = Utf8               SourceFile
  #12 = Utf8               Test.java
  #13 = NameAndType        #7:#8          // "<init>":()V
  #14 = NameAndType        #5:#6          // a:I
  #15 = Utf8               Test
  #16 = Utf8               java/lang/Object
{
  Test();
    descriptor: ()V
    flags:
    Code:
      stack=2, locals=1, args_size=1
         0: aload_0
         1: invokespecial #1                  // Method java/lang/Object."<init>":()V
         4: aload_0
         5: iconst_3
         6: putfield      #2                  // Field a:I
         9: return
      LineNumberTable:
        line 3: 0
        line 4: 4
        line 5: 9
}
SourceFile: "Test.java"
```
