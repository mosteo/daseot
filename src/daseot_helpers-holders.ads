private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
   type Values (<>) is private;
   with function "=" (L, R : Values) return Boolean is <>;
package Daseot_Helpers.Holders with Preelaborate is

   --  A basic holder to avoid the standard holders, that raise suspicious
   --  errors in GNAT 13 only.

   type Holder is tagged private;

   function Is_Empty (This : Holder) return Boolean;

   function Is_Populated (This : Holder) return Boolean;

   procedure Set (This : in out Holder; Value : Values)
     with Post => This.Is_Populated;

   function Set (Value : Values) return Holder
     with Post => Set'Result.Is_Populated;

   function Get (This : Holder) return Values
     with Pre => This.Is_Populated;

   function Ref (This : aliased Holder) return access Values
     with Pre => This.Is_Populated;

private

   package Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Values, "=");

   type Holder is tagged record
      Value : Lists.List;
   end record
     with Type_Invariant => Holder.Value.Length in 0 | 1;

end Daseot_Helpers.Holders;
