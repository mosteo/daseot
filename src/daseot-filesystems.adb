package body Daseot.Filesystems is

   ---------
   -- "/" --
   ---------

   function "/" (L : Name; R : Item) return Item
   is (L / Contents'(1 => R));

   ---------
   -- "/" --
   ---------

   function "/" (L : Name; R : Contents) return Item
   is
      Result : Tree;
   begin
      Result.Root.Map (Key (L), To_List (R).Root);
      return Result;
   end "/";

end Daseot.Filesystems;
