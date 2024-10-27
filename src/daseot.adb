with Ada.Tags;

package body Daseot is

   ---------
   -- Set --
   ---------

   procedure Set (This : Ref; Value : Node'Class) is
   begin
      if This.Ptr.all in Root_Node then
         Root_Node (This.Ptr.all).Root.Replace_Element (Value);
      elsif This.Ptr.all in Real_Node then
         Real_Node (This.Ptr.all).Data := Real_Node (Value).Data;
      else
         raise Program_Error with Ada.Tags.External_Tag (This.Ptr.all'Tag);
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : Ref; Value : Scalar) is
   begin
      This.Set (New_Atom (Value));
   end Set;

end Daseot;
