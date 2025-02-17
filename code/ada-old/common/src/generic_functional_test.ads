with Formal_Algorithms;

generic
   with package Algorithms is new Formal_Algorithms (<>);
procedure Generic_Functional_Test with SPARK_Mode => On;
