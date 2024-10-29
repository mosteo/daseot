with Daseot;
with Daseot.Filesystems;

with GNAT.IO; use GNAT.IO;

procedure Test is

   function Image (S : String) return String is (S);

   package Trees is new Daseot (String); use Trees;
   package FSs is new Trees.Filesystems (Image); use FSs;

   procedure Report (Title : String; This : Trees.Tree) is
   begin
      Put_Line (Title & ":");
      Put_Line (This.Image);
      New_Line;
   end Report;

   Tree : Trees.Tree;

   --  FS   : constant Trees.Tree :=
   --           "/"
   --           / ("share/"
   --              / "lib/",
   --              "src/"
   --              / (+"lib.ads",
   --                 +"lib.adb"),
   --              +"alire.toml",
   --              +"lib.gpr")
   --  ;

   FS   : constant Dir :=
            "/"
              / ("share/"
                 / (+"lib/"),
                 "src/"
                 / (+"lib.ads",
                    +"lib.adb"),
                 +"alire.toml",
                 +"lib.gpr")
   ;

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

   Report ("Empty dict", Dict);
   Report ("Empty list", List);

   Report ("List from array", To_List ((Set ("1"), Set ("2"))));

   Report ("Dict with list",
           Dict.Root.Map
             ("/",
             To_List ((Set ("1"), Set ("2"))).Root).Copy);

   Report ("List of dicts",
           List.Root
           .Append (Dict.Root.Map ("1", "a").Map ("2", "b"))
           .Append (Dict.Root.Map ("3", "c").Map ("4", "d")).Copy);

   Report ("Filesystem", FS);
end Test;
