package body Daseot_Helpers.Holders is

   ---------
   -- Get --
   ---------

   function Get (This : Holder) return Values
   is (This.Value.First_Element);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Holder) return Boolean
   is (This.Value.Is_Empty);

   ------------------
   -- Is_Populated --
   ------------------

   function Is_Populated (This : Holder) return Boolean
   is (not This.Is_Empty);

   ---------
   -- Ref --
   ---------

   function Ref (This : aliased Holder) return access Values
   is (This.Value.Constant_Reference (This.Value.First).Element);

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Holder; Value : Values) is
   begin
      This.Value.Clear;
      This.Value.Append (Value);
   end Set;

   ---------
   -- Set --
   ---------

   function Set (Value : Values) return Holder is
   begin
      return Result : Holder do
         Result.Value.Append (Value);
      end return;
   end Set;

end Daseot_Helpers.Holders;
