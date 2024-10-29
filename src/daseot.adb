with Ada.Strings.Unbounded;
with Ada.Tags;

package body Daseot is

   function Image (T : Ada.Tags.Tag) return String
                   renames Ada.Tags.External_Tag;

   ------------
   -- Append --
   ------------

   procedure Append (This   : aliased Node;
                     Val    : Scalar;
                     Retype : Boolean := False)
   is
   begin
      if This in Real_Node'Class then
         declare
            Real : Real_Node renames Real_Node (This.Ptr.all);
         begin
            if Real.Data.Kind = List_Kind then
               Real.Data.List.Append (New_Atom (Val));
            else
               if Retype then
                  --  Discard old node to replace with new one
                  Real.Data := (Kind => List_Kind, others => <>);
                  This.Append (Val, Retype);
               else
                  raise Constraint_Error with
                    "cannot assign list to node of type " & This.Kind'Image
                    & " without retype";
               end if;
            end if;
         end;
      elsif This in Root_Node'Class then
         --  Use the stored root node
         Root_Node (This.Ptr.all).Root.Reference.Ref.Append (Val, Retype);
      else
         raise Program_Error with Image (This.Ptr.all'Tag);
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   function Append (This   : aliased Node;
                    Val    : Scalar;
                    Retype : Boolean := False)
                    return Node
   is
   begin
      return Result : constant Node := (Ptr => This.Ptr) do
         Result.Append (Val, Retype);
      end return;
   end Append;

   -------------
   -- Element --
   -------------

   function Element (This : Node; C : Cursor) return Node is
      Real : Real_Node renames Real_Node (This.Ptr.all);
   begin
      return Result : constant Node :=
        (Ptr =>
           (case C.Kind is
               when Atom_Kind => This.Ptr,
               when Dict_Kind =>
                  Real.Data.Dict.Reference (C.Dict_Cursor).Element,
               when List_Kind =>
                 Real.Data.List.Reference (C.List_Cursor).Element
           ));
   end Element;

   ---------
   -- Get --
   ---------

   function Get (This : Node) return Scalar
   is (if This.Ptr.all in Real_Node'Class then
          Real_Node (This.Ptr.all).Data.Value.Element
       elsif This.Ptr.all in Root_Node then
          Root_Node (This.Ptr.all).Root.Constant_Reference.Ref.Get
       else
          raise Program_Error with Image (This.Ptr.all'Tag));

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Node; C : Cursor) return Boolean is
   begin
      case C.Kind is
         when Atom_Kind =>
            return not C.Visited;
         when Dict_Kind =>
            return Node_Maps.Has_Element (C.Dict_Cursor);
         when List_Kind =>
            return Node_Vectors.Has_Element (C.List_Cursor);
      end case;
   end Has_Element;

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
         NL : constant Character := ASCII.LF;
         --  Tab : constant String := "   ";
      begin
         case This.Kind is
            when Atom_Kind =>
               for E of This loop -- Only one, but we test the iterator so
                  Append (Result, Prefix & Image (E.Get) & NL);
               end loop;
            when Dict_Kind =>
               declare
                  Real : Real_Node renames Real_Node (This.Ptr.all);
                  C    : Node_Maps.Cursor := Real.Data.Dict.First;
                  use Node_Maps;
               begin
                  while Has_Element (C) loop
                     Append (Result, Prefix & Key (C) & " : ");
                     Traverse (Real.Data.Dict.Reference (C).Ref, Prefix);
                     C := Next (C);
                  end loop;
               end;
            when List_Kind =>
               declare
                  Real : Real_Node renames Real_Node (This.Ptr.all)
                    with Unreferenced;
               begin
                  for E of This loop
                     Append (Result, Prefix & "- ");
                     Traverse (E, Prefix);
                  end loop;
               end;
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
   is (if This.Ptr.all in Real_Node then
          Real_Node (This.Ptr.all).Data.Kind
       elsif This in Root_Node then
          Kind (Root_Node (This.Ptr.all).Root.Constant_Reference.Ref)
       else
          raise Program_Error with Image (This.Ptr.all'Tag));

   ---------
   -- Map --
   ---------

   procedure Map (This   : aliased Node;
                  Key    : Keys;
                  Val    : Scalar;
                  Retype : Boolean := False) is
   begin
      if This in Real_Node'Class then
         declare
            Real : Real_Node renames Real_Node (This.Ptr.all);
         begin
            if Real.Data.Kind = Dict_Kind then
               Real.Data.Dict.Include (Key, New_Atom (Val));
            else
               if Retype then
                  --  Discard old node to replace with new one
                  Real.Data := (Kind => Dict_Kind, others => <>);
                  This.Map (Key, Val, Retype);
               else
                  raise Constraint_Error with
                    "cannot assign dict to node of type " & This.Kind'Image
                    & " without retype";
               end if;
            end if;
         end;
      elsif This in Root_Node'Class then
         --  Use the stored root node
         Root_Node (This.Ptr.all).Root.Reference.Ref.Map (Key, Val, Retype);
      else
         raise Program_Error with Image (This.Ptr.all'Tag);
      end if;
   end Map;

   ---------
   -- Map --
   ---------

   function Map (This   : aliased Node;
                 Key    : Keys;
                 Val    : Scalar;
                 Retype : Boolean := False)
                 return Node
   is
   begin
      return Result : constant Node := (Ptr => This.Ptr) do
         Result.Map (Key, Val, Retype);
      end return;
   end Map;

   ---------
   -- Set --
   ---------

   procedure Set (This : Node; Value : Scalar; Retype : Boolean := False) is
   begin
      if This.Ptr.all in Root_Node then
         Root_Node (This.Ptr.all).Root.Replace_Element (New_Atom (Value));
      elsif This.Ptr.all in Real_Node then
         if This.Kind = Atom_Kind or else Retype then
            Real_Node (This.Ptr.all) := New_Atom (Value);
         else
            raise Constraint_Error with
              "cannot assign atom to node of type " & This.Kind'Image
              & " without retype";
         end if;
      else
         raise Program_Error with Image (This.Ptr.all'Tag);
      end if;
   end Set;

end Daseot;
