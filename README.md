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
                    , info = "*GIBBERISH FIX THIS*",
                    }
                ]
            }
        ]
    , cl_attributes =
        [ Attribute
            { attribute_name_index = 11
            , info = "*GIBBERISH FIX THIS*"
            }
        ]
    }
```

