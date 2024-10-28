with Ada.Strings.Unbounded;
with Ada.Tags;

package body Daseot is

   function Image (T : Ada.Tags.Tag) return String
                   renames Ada.Tags.External_Tag;

   -----------
   -- Image --
   -----------

   function Image (This : Tree) return String is
      use Ada.Strings.Unbounded;
      function "+" (S : String) return Unbounded_String
                    renames To_Unbounded_String;

      Result : Unbounded_String;

      --------------
      -- Traverse --
      --------------

      procedure Traverse (This : Node'Class; Prefix : String := "") is
         R : constant Ref := This.Edit;
      begin
         case This.Kind is
            when Atom_Kind =>
               for E of Ref'(R) loop -- Only one, but we test the iterator so
                  null;
                  --  Append (Result, Prefix & Image (E.Atom));
               end loop;
            when Dict_Kind =>
               raise Program_Error;
            when List_Kind =>
               raise Program_Error;
         end case;
      end Traverse;

   begin
      if This.Is_Empty then
         Result := +"(empty)";
      else
         Traverse (This.Root.Root.Constant_Reference);
      end if;

      return To_String (Result);
   end Image;

   ----------
   -- Kind --
   ----------

   function Kind (This : Node'Class) return Kinds
   is (if This in Atom_Node then Atom_Kind
       elsif This in Dict_Node then Dict_Kind
       elsif This in List_Node then List_Kind
       else raise Program_Error with Image (This'Tag));

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Dict_Node; Key : Keys; Val : Node'Class) is
   begin
      This.Data.Dict.Include (Key, Val);
   end Set;

   ---------
   -- Set --
   ---------

   function Set (This : aliased Dict_Node'Class;
                 Key  : Keys;
                 Val  : Node'Class)
                 return Ref
   is
   begin
      return Result : constant Ref := (Element => This'Unrestricted_Access) do
         Dict_Node (Result.Element.all).Set (Key, Val);
      end return;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : Ref; Value : Node'Class) is
   begin
      if This.Element.all in Root_Node then
         Root_Node (This.Element.all).Root.Replace_Element (Value);
      elsif This.Element.all in Real_Node then
         Real_Node (This.Element.all).Data := Real_Node (Value).Data;
      else
         raise Program_Error with Ada.Tags.External_Tag (This.Element.all'Tag);
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
