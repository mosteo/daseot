generic
   with function Key (S : Scalar) return Keys is <>;
package Daseot.Filesystems with Preelaborate is

   --  Wrappers to represent filesystems

   subtype Item is Tree;
   --  Either a file or a folder

   subtype Dir is Item
   --  with Dynamic_Predicate => Dir.Kind = Dict_Kind  --  kills GNAT 14
   ;

   subtype File is Item
   --  with Dynamic_Predicate => File.Kind = Atom_Kind  --  kills GNAT 14
   ;

   subtype Name is Scalar;

   subtype Contents is Tree_Array;
   --  The contents of a directory are always given as an array

   Empty : constant Contents := (1 .. 0 => <>);

   function "/" (L : Name; R : Item) return Item;
   --  Dir that contains a single item

   function "/" (L : Name; R : Contents) return Item;
   --  Dir that contains many items

   function "+" (S : Name) return Item renames Set;
   --  Generate an item from its name, that can be put into Contents

end Daseot.Filesystems;
