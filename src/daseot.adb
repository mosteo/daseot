with Ada.Strings.Unbounded;
with Ada.Tags;

package body Daseot is

   function Image (T : Ada.Tags.Tag) return String
                   renames Ada.Tags.External_Tag;

   ----------------
   -- Base_Nodes --
   ----------------

   package body Base_Nodes is

      ---------
      -- Ref --
      ---------

      function Ref (This : aliased Base_Node'Class) return Node is
      begin
         return Node'(Ptr => This'Unrestricted_Access);
      end Ref;

   end Base_Nodes;

   ----------------
   -- Root_Nodes --
   ----------------

   package body Root_Nodes is

      --------------
      -- Real_Ref --
      --------------

      function Real_Ref (This : aliased Root_Node'Class) return Node is
      begin
         if This.Root.Is_Empty then
            raise Constraint_Error with "Empty root node, cannot be cast";
         end if;

         return Node'
           (Ptr =>
              This.Root.Constant_Reference.Element.all'Unrestricted_Access);
      end Real_Ref;

      -----------
      -- Store --
      -----------

      procedure Store (This : in out Root_Node'Class;
                       Ref  : Node)
      is
      begin
         case Ref.Impl is
            when Real =>
               This.Root.Replace_Element (Ref.Ptr.all);
            when Root =>
               This.Root.Replace_Element (Ref.As_Root.Root.Constant_Reference);
         end case;
      end Store;

   end Root_Nodes;

   ------------
   -- Append --
   ------------

   procedure Append (This   : aliased Node;
                     Val    : Node;
                     Retype : Boolean := False)
   is
   begin
      if Val.Impl = Root then
         This.Append (Val.As_Root.Real_Ref, Retype);
         return;
      end if;

      if This in Real_Node'Class then
         declare
            Real : Real_Node renames Real_Node (This.Ptr.all);
         begin
            if Real.Data.Kind = List_Kind then
               Real.Data.List.Append (Val.Ptr.all);
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
         if This.As_Root.Is_Empty then
            This.As_Root.Store (Empty_List.Root);
         end if;

         --  Use the stored root node
         Root_Node (This.Ptr.all).Real_Ref.Append (Val, Retype);
      else
         raise Program_Error with Image (This.Ptr.all'Tag);
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (This   : aliased Node;
                     Val    : Scalar;
                     Retype : Boolean := False)
   is
   begin
      This.Append (New_Atom (Val).Ref, Retype);
   end Append;

   ------------
   -- Append --
   ------------

   function Append (This   : aliased Node;
                    Val    : Node;
                    Retype : Boolean := False)
                    return Node
   is
   begin
      return Result : constant Node := (Ptr => This.Ptr) do
         Result.Append (Val, Retype);
      end return;
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
   -- As_Real --
   -------------

   function As_Real (This : Node'Class) return access Real_Node
   is
   begin
      return Real_Node (This.Ptr.all)'Unchecked_Access;
   end As_Real;

   -------------
   -- As_Root --
   -------------

   function As_Root (This : Node'Class) return access Root_Node
   is
   begin
      return Root_Node (This.Ptr.all)'Unchecked_Access;
   end As_Root;

   -----------------------------
   -- Assert_Mutable_Contents --
   -----------------------------

   function Assert_Mutable_Contents (This : Mutable_Node) return Boolean
   is
   begin
      return
        (case This.Kind is
            when Atom_Kind =>
               not This.Value.Is_Empty,
            when Dict_Kind =>
              (for all E of This.Dict => E in Real_Node'Class),
            when List_Kind =>
              (for all E of This.List => E in Real_Node'Class));
   end Assert_Mutable_Contents;

   ----------
   -- Base --
   ----------

   function Base (This : Node'Class) return Base_Node'Class
   is
   begin
      return This.Ptr.all;
   end Base;

   ----------
   -- Copy --
   ----------

   function Copy (This : Node'Class) return Tree is
   begin
      return Result : Tree do
         Result.R.Root.Replace_Element
           (if This.Is_Root and then not This.Is_Empty then
               Root_Node (This.Ptr.all).Real_Ref.Ptr.all
            else
               This.Ptr.all
           );
      end return;
   end Copy;

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

   ----------------
   -- Empty_Dict --
   ----------------

   function Empty_Dict return Tree
   is
   begin
      return Result : Tree do
         Result.R.Root.Replace_Element (New_Dict);
      end return;
   end Empty_Dict;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List return Tree
   is
   begin
      return Result : Tree do
         Result.R.Root.Replace_Element (New_List);
      end return;
   end Empty_List;

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Tree
   is
   begin
      return Result : Tree;
   end Empty_Tree;

   -----------
   -- First --
   -----------

   function First (This : Node) return Cursor is
   begin
      if This.Ptr.all in Real_Node then
         case Real_Node (This.Ptr.all).Data.Kind is
            when Atom_Kind =>
               return (Kind => Atom_Kind, others => <>);
            when Dict_Kind =>
               raise Unimplemented;
            when List_Kind =>
               return (Kind        => List_Kind,
                      List_Cursor => Real_Node (This.Ptr.all).Data.List.First);
         end case;
      else
         raise Unimplemented;
      end if;
   end First;

   ---------
   -- Get --
   ---------

   function Get (This : Node) return Scalar
   is (case This.Impl is
          when Real => This.As_Real.Data.Value.Element,
          when Root => This.As_Root.Real_Ref.Get);

   ---------
   -- Get --
   ---------

   function Get (This : aliased Tree) return Scalar
   is
   begin
      return This.Root.Get;
   end Get;

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

   function Image (This : Tree; Compact : Boolean := False) return String is
      use Ada.Strings.Unbounded;
      function "+" (S : String) return Unbounded_String
                    renames To_Unbounded_String;

      Result : Unbounded_String;

      --------------
      -- Traverse --
      --------------

      procedure Traverse (This   : Node'Class;
                          Prefix : String;
                          Contd  : Boolean := False)
      is
         NL : constant Character := ASCII.LF;
         Tab : constant String := "   ";
         function WS (Str : String) return String
         is (1 .. Str'Length => ' ');
      begin
         case This.Kind is
            when Atom_Kind =>
               for E of This loop -- Only one, but we test the iterator so
                  Append (Result,
                          (if Contd then "" else Prefix) & Image (E.Get));
               end loop;

            when Dict_Kind =>
               declare
                  Real : Real_Node renames Real_Node (This.Ptr.all);
                  C    : Node_Maps.Cursor := Real.Data.Dict.First;
                  use Node_Maps;
                  Abbr : constant Boolean :=
                           Compact and then Real.Data.Dict.Length in 1;
               begin
                  if Real.Data.Dict.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix) & "{}");
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & "{"
                          & (if Abbr then ' ' else NL));

                  while Has_Element (C) loop
                     Append (Result,
                             (if Abbr then " " else Prefix & Tab)
                             & Key (C) & " : ");
                     Traverse (Real.Data.Dict.Reference (C).Ref,
                               WS (Prefix & Tab & Key (C) & " : "),
                               Contd => True);
                     if not Abbr then
                        Append (Result, NL);
                     end if;
                     C := Next (C);
                  end loop;

                  Append (Result,
                          (if Abbr then " " else Prefix) & "}");
               end;

            when List_Kind =>
               declare
                  Real : Real_Node renames Real_Node (This.Ptr.all);
                  Abbr : constant Boolean :=
                           Compact and then Real.Data.List.Length in 1;
                  I    : Natural := 0;
               begin
                  if Real.Data.List.Is_Empty then
                     Append (Result,
                             (if Contd then "" else Prefix) & "[]");
                     return;
                  end if;

                  Append (Result,
                          (if Contd then "" else Prefix)
                          & "["
                          & (if Abbr then ' ' else NL));

                  for E of This loop
                     Traverse (E,
                               Prefix & Tab,
                               Contd => Abbr);
                     I := I + 1;
                     Append (Result,
                             (if I = Natural (Real.Data.List.Length)
                              then ""
                              else ",")
                             & (if Abbr then ' ' else NL));
                  end loop;
                  Append (Result,
                          (if Abbr then " " else Prefix)
                          & "]");
               end;
         end case;
      end Traverse;

   begin
      if This.Is_Empty then
         Result := +"(empty)";
      else
         Traverse (This.R.Root.Constant_Reference.Ref, "");
      end if;

      return To_String (Result);
   end Image;

   ----------
   -- Impl --
   ----------

   function Impl (This : Node'Class) return Impls
   is
   begin
      return
        (if This.Ptr.all in Real_Node'Class then
            Real
         elsif This.Ptr.all in Root_Node'Class then
            Root
         else raise Program_Error);
   end Impl;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Node) return Boolean is
   begin
      return This.Ptr.Is_Empty;
   end Is_Empty;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (This : Real_Node) return Boolean is
   begin
      return This.Data.Dict.Is_Empty;
   end Is_Empty;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Tree) return Boolean is
   begin
      return This.R.Is_Empty;
   end Is_Empty;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Node) return Boolean is
   begin
      return This.Ptr.all in Root_Node;
   end Is_Root;

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

   ----------
   -- Kind --
   ----------

   function Kind (This : aliased Tree) return Kinds is
   begin
      return This.Root.Kind;
   end Kind;

   ---------
   -- Map --
   ---------

   procedure Map (This   : aliased Node;
                  Key    : Keys;
                  Val    : Node'Class;
                  Retype : Boolean := False)
   is
   begin
      if Val.Impl = Root then
         if Val.Is_Empty then
            raise Program_Error;
         end if;
         This.Map (Key, Val.As_Root.Real_Ref, Retype);
         return;
      end if;

      if This in Real_Node'Class then
         declare
            Real : Real_Node renames Real_Node (This.Ptr.all);
         begin
            if Real.Data.Kind = Dict_Kind then
               Real.Data.Dict.Include (Key, Val.Ptr.all);
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
         if This.As_Root.Is_Empty then
            This.As_Root.Store (Empty_Dict.Root);
         end if;

         --  Use the stored root node
         Root_Node (This.Ptr.all).Real_Ref.Map (Key, Val, Retype);
      else
         raise Program_Error with Image (This.Ptr.all'Tag);
      end if;
   end Map;

   ---------
   -- Map --
   ---------

   function Map (This   : aliased Node;
                 Key    : Keys;
                 Val    : Node'Class;
                 Retype : Boolean := False)
                 return Node
   is
   begin
      return Result : constant Node := (Ptr => This.Ptr) do
         Result.Map (Key, Val, Retype);
      end return;
   end Map;

   ---------
   -- Map --
   ---------

   procedure Map (This   : aliased Node;
                  Key    : Keys;
                  Val    : Scalar;
                  Retype : Boolean := False) is
   begin
      Map (This, Key, New_Atom (Val).Ref, Retype);
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

   --------------
   -- New_Atom --
   --------------

   function New_Atom (Value : Scalar) return Real_Node
   is
   begin
      return
        (Data => (Kind  => Atom_Kind,
                  Value => Scalar_Holders.To_Holder (Value)));
   end New_Atom;

   --------------
   -- New_Dict --
   --------------

   function New_Dict return Real_Node
   is
   begin
      return
        (Data => (Kind   => Dict_Kind,
                  others => <>));
   end New_Dict;

   --------------
   -- New_List --
   --------------

   function New_List return Real_Node
   is
   begin
      return
        (Data => (Kind   => List_Kind,
                  others => <>));
   end New_List;

   ----------
   -- Next --
   ----------

   function Next (This : Node; C : Cursor) return Cursor
   is
   begin
      return
        (case C.Kind is
            when Atom_Kind => (Kind => Atom_Kind, Visited => True),
            when List_Kind =>
              (Kind        => List_Kind,
               List_Cursor => Node_Vectors.Next (C.List_Cursor)),
            when others    => raise Unimplemented
        );
   end Next;

   ----------
   -- Root --
   ----------

   function Root (This : aliased Tree'Class) return Node
   is
   begin
      return (Ptr => This.R'Unrestricted_Access);
   end Root;

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

   ---------
   -- Set --
   ---------

   function Set (Value : Scalar) return Tree
   is
   begin
      return New_Atom (Value).Ref.Copy;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This   : Tree;
                  Value  : Scalar;
                  Retype : Boolean := False)
   is
   begin
      This.Root.Set (Value, Retype);
   end Set;

   ---------
   -- Set --
   ---------

   function Set (This   : Tree;
                 Value  : Scalar;
                 Retype : Boolean := False) return Tree
   is
   begin
      return Result : constant Tree := This do
         Result.Root.Set (Value, Retype);
      end return;
   end Set;

   -------------
   -- To_List --
   -------------

   function To_List (This : Tree_Array) return Tree is
   begin
      return Result : constant Tree := Empty_List do
         for E of This loop
            Result.Root.Append (E.Root);
         end loop;
      end return;
   end To_List;

end Daseot;
