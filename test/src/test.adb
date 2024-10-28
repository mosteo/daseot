with Daseot;

with GNAT.IO; use GNAT.IO;

procedure Test is

   function Image (S : String) return String is (S);

   package Trees is new Daseot (String); use Trees;

   procedure Report (Title : String; This : Trees.Tree) is
   begin
      Put_Line (Title & ":");
      Put_Line (This.Image);
      New_Line;
   end Report;

   Tree : Trees.Tree;

begin

   --  Just a value

   pragma Assert (Tree.Is_Empty);
   Report ("Empty tree", Tree);

   Tree.Root.Set ("hello");
   pragma Assert (Tree.Is_Populated);
   pragma Assert (Tree.Root.Atom = "hello");
   Report ("Atom tree", Tree);

   --  Replace root with a map

   Tree.Root.Set (New_Dict.Set ("key", New_Atom ("value")).Element.all);
   Report ("Dict tree", Tree);
end Test;
