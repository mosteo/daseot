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
      begin
         case This.Kind is
            when Atom_Kind =>
               for E of This loop -- Only one, but we test the iterator so
                  Append (Result, Prefix & Image (E.Get));
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
         Traverse (This.Root.Root.Constant_Reference.Ref);
      end if;

      return To_String (Result);
   end Image;

   ----------
   -- Kind --
   ----------

   function Kind (This : Node) return Kinds
   is (if This.Ptr.all in Real_Node
       then Real_Node (This.Ptr.all).Data.Kind
       else raise Program_Error with Image (This.Ptr.all'Tag));

   ---------
   -- Set --
   ---------

   procedure Set (This : aliased Node; Key : Keys; Val : Scalar) is
   begin
      Real_Node (This.Ptr.all).Data.Dict.Include (Key, New_Atom (Val));
   end Set;

   ---------
   -- Set --
   ---------

   function Set (This : aliased Node;
                 Key  : Keys;
                 Val  : Scalar)
                 return Node
   is
   begin
      return Result : constant Node := (Ptr => This.Ptr) do
         Result.Set (Key, Val);
      end return;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : Node; Value : Scalar) is
   begin
      if This.Ptr.all in Root_Node then
         Root_Node (This.Ptr.all).Root.Replace_Element (New_Atom (Value));
      elsif This.Ptr.all in Real_Node then
         Real_Node (This.Ptr.all) := New_Atom (Value);
      else
         raise Program_Error with Image (This.Ptr.all'Tag);
      end if;
   end Set;

end Daseot;
