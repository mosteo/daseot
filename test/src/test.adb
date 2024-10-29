with Daseot;

with GNAT.IO; use GNAT.IO;

procedure Test is

   function Image (S : String) return String is (S);

   package Trees is new Daseot (String); use Trees;

   procedure Report (Title : String; This : Trees.Tree) is
   begin
      Put_Line (Title & ":");
      Put_Line (This.Image);
   end Report;

   Tree : Trees.Tree;

begin

   --  Just a value

   pragma Assert (Tree.Is_Empty);
   Report ("Empty tree", Tree);

   Tree.Root.Set ("hello");
   pragma Assert (Tree.Is_Populated);
   pragma Assert (Tree.Root.Get = "hello");
   Report ("Atom tree", Tree);

   --  Replace root with a dict

   Tree.Root
     .Map ("key1", "value1", Retype => True)
     .Map ("key2", "value2");
   Report ("Dict tree", Tree);

   --  Replace root with a list
   Tree.Root
     .Append ("val1", Retype => True)
     .Append ("val2");
   Report ("List tree", Tree);
end Test;
