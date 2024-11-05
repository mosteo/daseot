package body Daseot.Filesystems is

   ---------
   -- "/" --
   ---------

   function "/" (L : Name; R : Item) return Item is
   begin
      return L / Contents'(1 => R);
   end "/";

   ---------
   -- "/" --
   ---------

   function "/" (L : Name; R : Contents) return Item is
      Items : constant Tree := To_List (R);
   begin
      return Result : Tree do
         Result.Root.Map (Key (L),
                          Items.Root);
      end return;
   end "/";

end Daseot.Filesystems;
